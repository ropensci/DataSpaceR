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
#'     A data.table. The table of available studies.
#'   }
#'   \item{\code{availableGroups}}{
#'     A data.table. The table of available groups.
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
#'   \item{\code{getStudy(study, groupId = NULL)}}{
#'     Create a \code{DataSpaceStudy} class.
#'
#'     \code{study}: A character. Name of the study to retrieve.
#'
#'     \code{groupId}: An integer. ID of the group to retrieve.
#'   }
#'   \item{\code{refresh()}}{
#'     Refresh \code{DataSpaceConnection} class.
#'   }
#' }
#' @seealso \code{\link{connectDS}} \code{\link{DataSpaceR-package}}
#' @examples
#' \dontrun{
#' # Create a connection (Initiate a DataSpaceConnection object)
#' con <- connectDS()
#' }
#' @docType class
#' @importFrom rjson fromJSON
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
      self$refresh()

      NULL
    },
    print = function() {
      cat("<DataSpaceConnection>")
      cat("\n  URL:", private$.config$labkey.url.base)
      cat("\n  User:", private$.config$labkey.user.email)
      cat("\n  Available studies:", private$.stats$studies)
      cat("\n    -", private$.stats$subjectlevelstudies, "studies with data")
      cat("\n    -", private$.stats$subjects, "subjects")
      cat("\n    -", private$.stats$assays, "assays")
      cat("\n    -", private$.stats$datacount, "data points")
      cat("\n  Available groups:", nrow(private$.availableGroups))
    },
    getStudy = function(study, groupId = NULL) {
      assert_that(is.number(groupId) || is.null(groupId),
                  msg = "groupId should be an integer or null.")
      assert_that((study == "" && is.null(groupId)) || (study == "" && is.number(groupId)) || (study != "" && is.null(groupId)),
                  msg = "Use empty string if you are using a group filter")

      if (is.number(groupId)) {
        assert_that(groupId %in% private$.availableGroups$id,
                    msg = paste(groupId, "is not a valid group ID"))
        group <- private$.availableGroups[.(groupId), label]
      } else {
        group <- NULL
      }

      if (study != "") {
        studyInfo <- as.list(
          private$.availableStudies[.(study)]
        )
      } else {
        studyInfo <- NULL
      }

      DataSpaceStudy$new(study, private$.config, group, studyInfo)
    },
    refresh = function() {
      tries <- c(
        class(try(private$.getAvailableStudies(), silent = !private$.config$verbose)),
        class(try(private$.getStats(), silent = !private$.config$verbose)),
        class(try(private$.getAvailableGroups(), silent = !private$.config$verbose))
      )

      invisible(!"try-error" %in% tries)
    }
  ),
  active = list(
    config = function() {
      private$.config
    },
    availableStudies = function() {
      private$.availableStudies
    },
    availableGroups = function() {
      private$.availableGroups
    }
  ),
  private = list(
    .config = list(),
    .availableStudies = data.table(),
    .stats = data.table(),
    .availableGroups = data.table(),

    .getAvailableStudies = function() {
      colSelect <- c("study_name", "short_name", "title", "type", "status",
                     "stage", "species", "start_date", "strategy")

      availableStudies <- labkey.selectRows(
        baseUrl = private$.config$labkey.url.base,
        folderPath = "/CAVD",
        schemaName = "CDS",
        queryName = "study",
        colSelect = colSelect,
        colNameOpt = "fieldname"
      )

      setDT(availableStudies)
      setkey(availableStudies, study_name)

      private$.availableStudies <- availableStudies
    },
    .getStats = function() {
      stats <- labkey.selectRows(
        baseUrl = private$.config$labkey.url.base,
        folderPath = "/CAVD",
        schemaName = "CDS",
        queryName = "ds_properties",
        colNameOpt = "fieldname"
      )

      setDT(stats)

      private$.stats <- stats
    },
    .getAvailableGroups = function() {
      participantGroupApi <- paste0(
        private$.config$labkey.url.base,
        "/participant-group",
        "/CAVD",
        "/browseParticipantGroups.api?",
        "distinctCatgories=false&",
        "type=participantGroup&",
        "includeUnassigned=false&",
        "includeParticipantIds=false"
      )

      # execute via Rlabkey's standard GET function
      response <- Rlabkey:::labkey.get(participantGroupApi)

      # parse JSON response via rjson's fromJSON parsing function
      parsed <- fromJSON(response)

      # construct a data.table for each group
      groupsList <- lapply(parsed$groups, function(group) {
        data.table(
          id = group$id,
          label = group$label,
          description = ifelse(is.null(group$description), NA, group$description),
          createdBy = group$createdBy$displayValue,
          shared = group$category$shared,
          n = length(group$category$participantIds)
        )
      })

      # merge the list to data.table
      availableGroups <- rbindlist(groupsList)

      # set order by id
      setorder(availableGroups, id)
      setkey(availableGroups, id)

      private$.availableGroups <- availableGroups
    }
  )
)
