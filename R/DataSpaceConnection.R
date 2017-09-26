#' The DataSpaceConnection class
#'
#' @details
#'
#' @section Constructor:
#' \code{\link{connectDS}}
#'
#' @section Fields:
#' \describe{
#'   \item{\code{config}}{
#'     A list. Stores configuration of the connection object such as
#'     URL, path and username.
#'   }
#'   \item{\code{availableStudies}}{
#'     A data frame. The table of studies available in the connection object.
#'   }
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(login = NULL, password = NULL, verbose = FALSE,
#'   onStaging = FALSE)}}{
#'     Initialize \code{DataSpaceConnection} class. See \code{\link{connectDS}}.
#'   }
#'   \item{\code{print()}}{
#'     Print \code{DataSpaceConnection} class.
#'   }
#'   \item{\code{getStudy(study)}}{
#'     Create a \code{DataSpaceStudy} class.
#'
#'     \code{study}: A character. Name of the study to retrieve.
#'   }
#' }
#' @seealso \code{\link{connectDS}} \code{\link{DataSpaceR-package}}
#' @examples
#' \dontrun{
#' # Create a connection (Initiate a DataSpaceConnection object)
#' con <- connectDS()
#' }
#' @docType class
DataSpaceConnection <- R6Class(
  classname = "DataSpaceConnection",
  public = list(
    initialize = function(login = NULL,
                          password = NULL,
                          verbose = FALSE,
                          onStaging = FALSE) {
      assert_that((is.null(login) && is.null(password)) || (!is.null(login) && !is.null(password)),
                  msg = "Enter both `login` and `password` or use netrc file.")
      assert_that(is.logical(verbose))
      assert_that(is.logical(onStaging))

      # get primary fields
      labkey.url.base <- getUrlBase(onStaging)
      labkey.user.email <- getUserEmail(labkey.url.base, login)

      # set Curl options
      netrcFile <- getNetrc(login, password, onStaging)
      curlOptions <- setCurlOptions(netrcFile)

      # set primary fields
      private$.config <-
        list(
          labkey.url.base = labkey.url.base,
          labkey.user.email = labkey.user.email,
          curlOptions = curlOptions,
          verbose = verbose
        )

      # get extra fields if available
      try(private$.getAvailableStudies(), silent = TRUE)
      try(private$.getStats(), silent = TRUE)

      NULL
    },
    print = function() {
      cat("<DataSpaceConnection>")
      cat("\n  URL:", private$.config$labkey.url.base)
      cat("\n  User:", private$.config$labkey.user.email)
      cat("\n  Available studies:", private$.stats$studies)
      cat("\n    -", private$.stats$subjectlevelstudies, "studies with data")
      cat("\n    -", private$.stats$subjects, "subjects")
      cat("\n    -", private$.stats$assays, "Assays")
      cat("\n    -", private$.stats$datacount, "Data points")
    },
    getStudy = function(study) {
      DataSpaceStudy$new(study, private$.config)
    }
  ),
  active = list(
    config = function() {
      private$.config
    },
    availableStudies = function() {
      private$.availableStudies
    }
  ),
  private = list(
    .config = list(),
    .availableStudies = data.frame(),
    .stats = data.frame(),

    .getAvailableStudies = function() {
      private$.availableStudies <-
        labkey.selectRows(
          baseUrl = private$.config$labkey.url.base,
          folderPath = "/CAVD",
          schemaName = "CDS",
          queryName = "study",
          colSelect = c("study_name", "title"),
          colNameOpt = "fieldname"
        )
    },
    .getStats = function() {
      private$.stats <-
        labkey.selectRows(
          baseUrl = private$.config$labkey.url.base,
          folderPath = "/CAVD",
          schemaName = "CDS",
          queryName = "ds_properties",
          colNameOpt = "fieldname"
        )
    }
  )
)
