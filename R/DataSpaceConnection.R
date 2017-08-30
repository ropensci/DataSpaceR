#' The DataSpaceConnection class
#'
#' @details
#' Uses global variables \code{labkey.url.base}, and \code{labkey.url.path}, to
#' access a study. \code{labkey.url.base} should be
#' \url{https://dataspace.cavd.org/}. \code{labkey.url.path} should be
#' \code{/CAVD/studyname}, where \code{studyname} is the accession number of the
#' study.
#'
#' \code{DataSpaceConnection} will initialize itself, and look for a
#' \code{.netrc} file in \code{"~/"} the user's home directory. The
#' \code{.netrc} file should contain a \code{machine}, \code{login}, and
#' \code{password} entry to allow access to DataSpace, where \code{machine} is
#' the host name like "dataspace.cavd.org".
#'
#' @section Constructor:
#' \code{\link{connectDS}}
#'
#' @section Fields:
#' \describe{
#'   \item{\code{study}}{
#'     A character. The study name. Use an empty string ("") to create
#'     a connection at the project level.
#'   }
#'   \item{\code{config}}{
#'     A list. Stores configuration of the connection object such as
#'     URL, path and username.
#'   }
#'   \item{\code{availableDatasets}}{
#'     A data.frame. The table of datasets available in
#'     the connection object.
#'   }
#'   \item{\code{cache}}{
#'     A list. Stores the data to avoid downloading the same tables multiple
#'     times.
#'   }
#'   \item{\code{treatmentArm}}{
#'     A data.frame. The table of treatment arm information for the connected
#'     study. Not available for cross study connection.
#'   }
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(study = NULL, login = NULL, password = NULL,
#'   verbose = FALSE, onStaging = FALSE)}}{
#'     Initialize \code{DataSpaceConnection} class. See \code{\link{connectDS}}.
#'   }
#'   \item{\code{print()}}{
#'     Print \code{DataSpaceConnection} class.
#'   }
#'   \item{\code{getDataset(datasetName,
#'   colFilter = NULL, reload = FALSE, ...)}}{
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
#'   \item{\code{getVariableInfo()}}{
#'     Get variable information.
#'
#'     \code{datasetName}: A character. Name of the dataset to retrieve.
#'   }
#' }
#' @seealso \code{\link{connectDS}} \code{\link{DataSpaceR-package}}
#' @examples
#' \dontrun{
#' # Create a connection (Initiate a DataSpaceConnection object)
#' con <- connectDS("cvd408")
#'
#' # Retrieve Neutralizing antibody dataset (NAb) from DataSpace
#' con$getDataset("NAb")
#' }
#' @docType class
#' @import R6
#' @importFrom digest digest
DataSpaceConnection <- R6Class(
  classname = "DataSpaceConnection",
  public = list(
    initialize = function(study = NULL,
                          login = NULL,
                          password = NULL,
                          verbose = FALSE,
                          onStaging = FALSE) {
      assert_that(length(study) <= 1,
                  msg = "For multiple studies, use an empty string and filter the connection.")
      assert_that((is.null(login) && is.null(password)) || (!is.null(login) && !is.null(password)),
                  msg = "Enter both `login` and `password` or use netrc file.")
      assert_that(is.logical(verbose))
      assert_that(is.logical(onStaging))

      # get primary fields
      labkey.url.base <- getUrlBase(onStaging)
      labkey.user.email <- getUserEmail(labkey.url.base, login)
      labkey.url.path <- getUrlPath(study, labkey.url.path)

      # set Curl options
      netrcFile <- getNetrc(login, password, onStaging)
      curlOptions <- setCurlOptions(netrcFile)

      # fix study
      study <- fixStudy(study, labkey.url.base, labkey.url.path)

      # set primary fields
      private$.study <- tolower(study)
      private$.config <-
        list(
          labkey.url.base = labkey.url.base,
          labkey.url.path = labkey.url.path,
          labkey.user.email = labkey.user.email,
          curlOptions = curlOptions,
          verbose = verbose
        )

      # get extra fields if available
      try(private$.getAvailableDatasets(), silent = TRUE)
      try(private$.getTreatmentArm(), silent = TRUE)

      NULL
    },
    print = function() {
      study <- ifelse(private$.study == "", "CAVD", private$.study)
      url <- file.path(gsub("/$", "", private$.config$labkey.url.base),
                       gsub("^/", "", private$.config$labkey.url.path))

      cat(paste0("DataSpaceR Connection to ", study))
      cat(paste0("\nURL: ", url))
      cat(paste0("\nUser: ", private$.config$labkey.user.email))
      cat("\nAvailable datasets:")
      cat(paste0("\n\t", private$.availableDatasets$name), sep = "")
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
                  msg = paste0(datasetName, " is invalid dataset"))

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
    }
  ),
  private = list(
    .study = character(),
    .config = list(),
    .availableDatasets = data.frame(),
    .cache = list(),
    .treatmentArm = data.frame(),

    .getAvailableDatasets = function() {
      datasetQuery <-
        "
          SELECT
            DataSets.Name as name,
            DataSets.Label as label,
            -- DataSets.CategoryId AS category,
            -- DataSets.DataSetId AS id,
            dataset_n.n
          FROM
          (
            SELECT COUNT(participantid) AS n, 'ICS' AS Name
            FROM ICS
            UNION
            SELECT COUNT(participantid) AS n, 'BAMA' AS Name
            FROM BAMA
            UNION
            SELECT COUNT(participantid) AS n, 'ELISPOT' AS Name
            FROM ELISPOT
            UNION
            SELECT COUNT(participantid) AS n, 'NAb' AS Name
            FROM NAb
            UNION
            SELECT COUNT(participantid) AS n, 'Demographics' AS Name
            FROM Demographics
          ) AS dataset_n,
          DataSets
          WHERE Datasets.Name = dataset_n.Name AND dataset_n.n > 0
        "
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
      colSelect <- c("arm_part", "arm_group", "arm_name", "randomization",
                     "coded_label", "last_day")

      private$.treatmentArm <-
        suppressWarnings(
          labkey.selectRows(
            baseUrl = private$.config$labkey.url.base,
            folderPath = private$.config$labkey.url.path,
            schemaName = "CDS",
            queryName = "ds_treatmentarm",
            colSelect = colSelect,
            colNameOpt = "fieldname"
          )
        )
    }
  )
)
