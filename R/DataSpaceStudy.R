#' The DataSpaceStudy class
#'
#' @details
#'
#' @section Constructor:
#' \code{DataSpaceConnection$getStudy()}
#'
#' @section Fields:
#' \describe{
#'   \item{\code{study}}{
#'     A character. The study name.
#'   }
#'   \item{\code{config}}{
#'     A list. Stores configuration of the connection object such as
#'     URL, path and username.
#'   }
#'   \item{\code{availableDatasets}}{
#'     A data.frame. The table of datasets available in the study object.
#'   }
#'   \item{\code{cache}}{
#'     A list. Stores the data to avoid downloading the same tables multiple
#'     times.
#'   }
#'   \item{\code{treatmentArm}}{
#'     A data.frame. The table of treatment arm information for the connected
#'     study. Not available for cross study connection.
#'   }
#'   \item{\code{group}}{
#'     A character. The group name.
#'   }
#'   \item{\code{studyInfo}}{
#'     A list. Stores the information about the study.
#'   }
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(study = NULL, config = NULL, group = NULL, studyInfo = NULL)}}{
#'     Initialize \code{DataSpaceStudy} class.
#'     See \code{\link{DataSpaceConnection}}.
#'   }
#'   \item{\code{print()}}{
#'     Print \code{DataSpaceStudy} class.
#'   }
#'   \item{\code{getDataset(datasetName, colFilter = NULL,
#'   reload = FALSE, ...)}}{
#'     Get a dataset from the connection.
#'
#'     \code{datasetName}: A character. Name of the dataset to retrieve.
#'
#'     \code{colFilter}: A matrix. A filter as returned by Rlabkey's
#'     \code{\link[Rlabkey]{makeFilter}}.
#'
#'     \code{reload}: A logical. If set to TRUE, download the dataset, whether
#'     a cached version exist or not.
#'
#'     \code{...}: Extra arguments to be passed to
#'     \code{\link[Rlabkey]{labkey.selectRows}}
#'   }
#'   \item{\code{clearCache()}}{
#'     Clear \code{cache}. Remove downloaded datasets.
#'   }
#'   \item{\code{getVariableInfo(datasetName)}}{
#'     Get variable information.
#'
#'     \code{datasetName}: A character. Name of the dataset to retrieve.
#'   }
#'   \item{\code{refresh()}}{
#'     Refresh \code{DataSpaceStudy} class.
#'   }
#' }
#' @seealso \code{\link{connectDS}} \code{\link{DataSpaceConnection}}
#' @examples
#' \dontrun{
#' # Create a connection (Initiate a DataSpaceConnection object)
#' con <- connectDS()
#'
#' # Connect to cvd408 (Initiate a DataSpaceStudy object)
#' cvd408 <- con$getStudy("cvd408")
#'
#' # Retrieve Neutralizing antibody dataset (NAb) for cvd408 from DataSpace
#' cvd408$getDataset("NAb")
#' }
#' @docType class
#' @importFrom digest digest
DataSpaceStudy <- R6Class(
  classname = "DataSpaceStudy",
  public = list(
    initialize = function(study = NULL, config = NULL, group = NULL, studyInfo = NULL) {
      assert_that(length(study) <= 1,
                  msg = "For multiple studies, use an empty string and filter the connection.")
      assert_that(!is.null(config))

      # get primary fields
      config$labkey.url.path <- getUrlPath(study)

      # fix study
      study <- fixStudy(study, config$labkey.url.base, config$labkey.url.path)

      # set primary fields
      private$.study <- tolower(study)
      private$.config <- config
      private$.group <- group
      private$.studyInfo <- studyInfo

      # get extra fields if available
      self$refresh()

      NULL
    },
    print = function() {
      study <- ifelse(private$.study == "", "CAVD", private$.study)
      url <- file.path(gsub("/$", "", private$.config$labkey.url.base),
                       gsub("^/", "", private$.config$labkey.url.path))

      cat("<DataSpaceStudy>")
      if (is.null(private$.group)) {
        cat("\n  Study:", study)
      } else {
        cat("\n  Group:", private$.group)
      }
      cat("\n  URL:", url)
      cat("\n  Available datasets:")
      if (nrow(private$.availableDatasets) > 0) {
        cat(paste0("\n    - ", private$.availableDatasets$name), sep = "")
      }
    },
    getDataset = function(datasetName,
                          colFilter = NULL,
                          reload = FALSE,
                          ...) {
      assert_that(is.character(datasetName))
      assert_that(length(datasetName) == 1)
      assert_that(datasetName %in% private$.availableDatasets$name,
                  msg = paste0(datasetName, " is invalid dataset"))
      assert_that(is.null(colFilter) | is.matrix(colFilter),
                  msg = "colFilter is not a matrix")

      args <- list(datasetName = datasetName,
                   colFilter = colFilter,
                   ...)
      digestedArgs <- digest(args)
      if (digestedArgs %in% names(private$.cache)) {
        if (!reload) {
          return(private$.cache[[digestedArgs]]$data)
        }
      }

      viewName <- NULL
      if (!is.null(private$.group)) {
        colFilter <- rbind(colFilter,
                           makeFilter(c(paste0("SubjectId/", private$.group),
                                        "EQUAL",
                                        private$.group)))
      }

      dataset <- labkey.selectRows(
        baseUrl = private$.config$labkey.url.base,
        folderPath = private$.config$labkey.url.path,
        schemaName = "study",
        queryName = datasetName,
        viewName = viewName,
        colNameOpt = "fieldname",
        colFilter = colFilter,
        ...)

      # caching
      private$.cache[[digestedArgs]] <- list(args = args, data = dataset)

      dataset
    },
    clearCache = function() {
      private$.cache <- list()
    },
    getVariableInfo = function(datasetName) {
      assert_that(is.character(datasetName))
      assert_that(length(datasetName) == 1)
      assert_that(datasetName %in% private$.availableDatasets$name,
                  msg = paste0(datasetName, " is not a available dataset"))

      varInfo <- labkey.getQueryDetails(
        baseUrl = private$.config$labkey.url.base,
        folderPath = private$.config$labkey.url.path,
        schemaName = "study",
        queryName = datasetName)

      extraVars <- c("Created", "CreatedBy", "Modified", "ModifiedBy",
                     "SequenceNum", "date")

      varFilter <- varInfo$isHidden == "FALSE" &
        varInfo$isSelectable == "TRUE" &
        !varInfo$fieldName %in% extraVars

      varInfo <- varInfo[varFilter,
                         c("fieldName", "caption", "type", "description")]
      rownames(varInfo) <- NULL

      varInfo
    },
    refresh = function() {
      tries <- c(
        class(try(private$.getAvailableDatasets(), silent = private$.config$verbose)),
        class(try(private$.getTreatmentArm(), silent = private$.config$verbose))
      )

      invisible(!"try-error" %in% tries)
    }
  ),
  active = list(
    study = function() {
      private$.study
    },
    config = function() {
      private$.config
    },
    availableDatasets = function() {
      private$.availableDatasets
    },
    cache = function() {
      private$.cache
    },
    treatmentArm = function() {
      private$.treatmentArm
    },
    group = function() {
      private$.group
    },
    studyInfo = function() {
      private$.studyInfo
    }
  ),
  private = list(
    .study = character(),
    .config = list(),
    .availableDatasets = data.frame(),
    .cache = list(),
    .treatmentArm = data.frame(),
    .group = character(),
    .studyInfo = list(),

    .getAvailableDatasets = function() {
      datasetQuery <-
        paste("SELECT",
                "DataSets.Name as name,",
                "DataSets.Label as label,",
                "dataset_n.n",
              "FROM",
                "(",
                  makeCountQuery("ICS", private$.group),
                  "UNION",
                  makeCountQuery("BAMA", private$.group),
                  "UNION",
                  makeCountQuery("ELISPOT", private$.group),
                  "UNION",
                  makeCountQuery("NAb", private$.group),
                  "UNION",
                  makeCountQuery("Demographics", private$.group),
                ") AS dataset_n,",
                "DataSets",
              "WHERE",
                "Datasets.Name = dataset_n.Name AND dataset_n.n > 0")

      private$.availableDatasets <-
        suppressWarnings(
          labkey.executeSql(
            baseUrl = private$.config$labkey.url.base,
            folderPath = private$.config$labkey.url.path,
            schemaName = "study",
            sql = datasetQuery,
            colNameOpt = "fieldname"
          )
        )
    },
    .getTreatmentArm = function() {
      colSelect <- c("arm_id", "arm_part", "arm_group", "arm_name",
                     "randomization", "coded_label", "last_day", "description")

      private$.treatmentArm <-
        suppressWarnings(
          labkey.selectRows(
            baseUrl = private$.config$labkey.url.base,
            folderPath = private$.config$labkey.url.path,
            schemaName = "CDS",
            queryName = "treatmentarm",
            colSelect = colSelect,
            colNameOpt = "fieldname",
            colFilter = makeFilter(c("arm_id", "CONTAINS", private$.study))
          )
        )
    }
  )
)
