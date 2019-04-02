#' The DataSpaceStudy class
#'
#'
#' @return an instance of \code{DataSpaceStudy}
#'
#' @section Constructor:
#' \code{DataSpaceConnection$getStudy()}
#' \code{DataSpaceConnection$getGroup()}
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
#'     A data.table. The table of datasets available in the study object.
#'   }
#'   \item{\code{cache}}{
#'     A list. Stores the data to avoid downloading the same tables multiple
#'     times.
#'   }
#'   \item{\code{treatmentArm}}{
#'     A data.table. The table of treatment arm information for the connected
#'     study. Not available for all study connection.
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
#'   \item{\code{initialize(study = NULL, config = NULL, group = NULL,
#'   studyInfo = NULL)}}{
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
#'     \code{mergeExtra}: A logical. If set to TRUE, merge extra information.
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
#'   \item{\code{getDatasetDescription(datasetName)}}{
#'     Get variable information.
#'
#'     \code{datasetName}: A character. Name of the dataset to retrieve.
#'   }
#'   \item{\code{refresh()}}{
#'     Refresh the study object to update available datasets and treatment info.
#'   }
#' }
#'
#' @seealso \code{\link{connectDS}} \code{\link{DataSpaceConnection}}
#'
#' @examples
#' \dontrun{
#' # Create a connection (Initiate a DataSpaceConnection object)
#' con <- connectDS()
#'
#' # Connect to cvd408 (Initiate a DataSpaceStudy object)
#' # https://dataspace.cavd.org/cds/CAVD/app.view#learn/learn/Study/cvd408?q=408
#' cvd408 <- con$getStudy("cvd408")
#' cvd408
#'
#' # Retrieve Neutralizing antibody dataset (NAb) for cvd408 from DataSpace
#' NAb <- cvd408$getDataset("NAb")
#'
#' # Get variable information of the NAb dataset
#' cvd408$getDatasetDescription("NAb")
#'
#' # Take a look at cvd408's treatment arm information
#' cvd408$treatmentArm
#'
#' # Clear cache of a study object
#' cvd408$clearCache()
#'
#' # Connect to the NYVAC durability comparison group
#' # https://dataspace.cavd.org/cds/CAVD/app.view#group/groupsummary/220
#' nyvac <- con$getGroup(220)
#'
#' # Connect to all studies
#' cvd <- con$getStudy("")
#'
#' # Refresh the study object to update available datasets and treatment info
#' cvd$refresh()
#' }
#'
#' @docType class
#' @format NULL
#'
#' @importFrom digest digest
#' @importFrom Rlabkey labkey.getQueryDetails labkey.executeSql
DataSpaceStudy <- R6Class(
  classname = "DataSpaceStudy",
  public = list(
    initialize = function(study = NULL,
                              config = NULL,
                              group = NULL,
                              studyInfo = NULL) {
      assert_that(
        length(study) <= 1,
        msg = "For multiple studies, create a group in the portal."
      )
      assert_that(!is.null(config))

      # get primary fields
      config$labkeyUrlPath <- getUrlPath(study)

      # fix study
      study <- fixStudy(study, config$labkeyUrlBase, config$labkeyUrlPath)

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
      url <- file.path(
        gsub("/$", "", private$.config$labkeyUrlBase),
        gsub("^/", "", private$.config$labkeyUrlPath)
      )

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
      cat("\n")
    },
    getDataset = function(datasetName,
                              mergeExtra = FALSE,
                              colFilter = NULL,
                              reload = FALSE,
                              ...) {
      assert_that(is.character(datasetName))
      assert_that(length(datasetName) == 1)
      assert_that(
        datasetName %in% private$.availableDatasets$name,
        msg = paste0(datasetName, " is invalid dataset")
      )
      assert_that(is.logical(mergeExtra))
      assert_that(
        is.null(colFilter) | is.matrix(colFilter),
        msg = "colFilter is not a matrix"
      )
      assert_that(is.logical(reload))

      # build a list of arguments to digest and compare
      args <- list(
        datasetName = datasetName,
        mergeExtra = mergeExtra,
        colFilter = colFilter,
        ...
      )

      # retrieve dataset from cache if arguments match
      digestedArgs <- digest(args)
      if (digestedArgs %in% names(private$.cache)) {
        if (!reload) {
          return(private$.cache[[digestedArgs]]$data)
        }
      }

      # build a colFilter for group
      if (!is.null(private$.group)) {
        colFilter <- rbind(
          colFilter,
          makeFilter(c(
            paste0("SubjectId/", names(private$.group)),
            "EQUAL",
            private$.group
          ))
        )
      }

      # retrieve dataset
      dataset <- labkey.selectRows(
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = private$.config$labkeyUrlPath,
        schemaName = "study",
        queryName = datasetName,
        viewName = NULL,
        colNameOpt = "fieldname",
        colFilter = colFilter,
        method = "GET",
        ...
      )

      # convert to data.table
      setDT(dataset)

      # merge extra information
      if (args$mergeExtra) {
        if (!identical(datasetName, "Demographics")) {
          dem <- self$getDataset("Demographics")

          subj <- ifelse(private$.study == "", "Subject", "Participant")
          cols <- c(paste0(subj, "Visit/Visit"), "study_prot")
          dem <- dem[, -cols, with = FALSE]

          key <- paste0(subj, "Id")
          setkeyv(dem, key)
          setkeyv(dataset, key)

          dataset <- dataset[dem, nomatch = 0]
        }

        # create arm_id column with demographics and set it as key
        dataset[, arm_id := paste(
          study_prot, study_part, study_group, study_arm,
          sep = "-"
        )]
        setkey(dataset, arm_id)

        dataset[private$.treatmentArm, nomatch = 0]
        setkey(dataset, NULL)
      }

      # caching
      private$.cache[[digestedArgs]] <- list(
        args = args,
        data = dataset
      )

      dataset
    },
    clearCache = function() {
      private$.cache <- list()
    },
    getDatasetDescription = function(datasetName) {
      assert_that(is.character(datasetName))
      assert_that(length(datasetName) == 1)
      assert_that(
        datasetName %in% private$.availableDatasets$name,
        msg = paste0(datasetName, " is not a available dataset")
      )

      varInfo <- labkey.getQueryDetails(
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = private$.config$labkeyUrlPath,
        schemaName = "study",
        queryName = datasetName
      )

      # convert to data.table and set key
      setDT(varInfo)
      setkey(varInfo, fieldName)

      extraVars <- c(
        "Created", "CreatedBy", "Modified", "ModifiedBy",
        "SequenceNum", "date"
      )

      varInfo <- varInfo[
        isHidden == "FALSE" &
          isSelectable == "TRUE" &
          !fieldName %in% extraVars,
        .(fieldName, caption, type, description)
      ]

      varInfo
    },
    refresh = function() {
      tries <- c(
        class(try(
          private$.getAvailableDatasets(),
          silent = private$.config$verbose
        )),
        class(try(
          private$.getTreatmentArm(),
          silent = private$.config$verbos
        ))
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
    .availableDatasets = data.table(),
    .cache = list(),
    .treatmentArm = data.table(),
    .group = character(),
    .studyInfo = list(),

    .getAvailableDatasets = function() {
      datasetQuery <-
        paste(
          "SELECT",
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
          "Datasets.Name = dataset_n.Name AND dataset_n.n > 0"
        )

      availableDatasets <- suppressWarnings(
        labkey.executeSql(
          baseUrl = private$.config$labkeyUrlBase,
          folderPath = private$.config$labkeyUrlPath,
          schemaName = "study",
          sql = datasetQuery,
          colNameOpt = "fieldname"
        )
      )

      # convert to data.table
      setDT(availableDatasets)

      private$.availableDatasets <- availableDatasets[order(name)]
    },
    .getTreatmentArm = function() {
      colSelect <- c(
        "arm_id", "arm_part", "arm_group", "arm_name",
        "randomization", "coded_label", "last_day", "description"
      )

      treatmentArm <- suppressWarnings(
        labkey.selectRows(
          baseUrl = private$.config$labkeyUrlBase,
          folderPath = private$.config$labkeyUrlPath,
          schemaName = "CDS",
          queryName = "treatmentarm",
          colSelect = colSelect,
          colNameOpt = "fieldname",
          colFilter = makeFilter(c("arm_id", "CONTAINS", private$.study)),
          method = "GET"
        )
      )

      # convert to data.table
      setDT(treatmentArm)

      # set key
      setkey(treatmentArm, arm_id)

      private$.treatmentArm <- treatmentArm
    }
  )
)
