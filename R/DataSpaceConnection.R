#' The DataSpaceConnection class
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
#'     Create a \code{\link{DataSpaceStudy}} object.
#'
#'     \code{study}: A character. Name of the study to retrieve.
#'
#'     \code{groupId}: DEPRECATED. Use \code{getGroup} method.
#'   }
#'   \item{\code{getGroup(groupId)}}{
#'     Create a \code{\link{DataSpaceStudy}} object.
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
#' con
#'
#' # Connect to cvd408
#' # https://dataspace.cavd.org/cds/CAVD/app.view#learn/learn/Study/cvd408?q=408
#' cvd408 <- con$getStudy("cvd408")
#'
#' # Connect to all studies
#' cvd <- con$getStudy("cvd408")
#'
#' # Connect to the NYVAC durability comparison group
#' # https://dataspace.cavd.org/cds/CAVD/app.view#group/groupsummary/220
#' nyvac <- con$getGroup(220)
#'
#' # Refresh the connection object to update available studies and groups
#' con$refresh()
#' }
#' @docType class
#' @importFrom rjson fromJSON
#' @importFrom curl has_internet nslookup
DataSpaceConnection <- R6Class(
  classname = "DataSpaceConnection",
  public = list(
    initialize = function(login = NULL,
                          password = NULL,
                          verbose = FALSE,
                          onStaging = FALSE) {
      assert_that(
        (is.null(login) && is.null(password)) || (!is.null(login) && !is.null(password)),
        msg = "Enter both `login` and `password` or use netrc file."
      )
      assert_that(is.logical(verbose))
      assert_that(is.logical(onStaging))
      assert_that(
        has_internet(),
        msg = "No internet connection. Please connect to internet and try again."
      )

      # check if the portal is up
      assert_that(
        !is.null(nslookup(ifelse(onStaging, STAGING, PRODUCTION), error = FALSE)),
        msg = "The portal is currently down. Try again later."
      )

      # get primary fields
      labkey.url.base <- getUrlBase(onStaging)
      labkey.user.email <- getUserEmail(labkey.url.base, login)

      # set Curl options
      netrcFile <- getNetrc(login, password, onStaging)
      curlOptions <- setCurlOptions(netrcFile)

      # check credential
      checkCredential(onStaging, verbose)

      # set primary fields
      private$.config <-
        list(
          labkey.url.base = labkey.url.base,
          labkey.user.email = labkey.user.email,
          curlOptions = curlOptions,
          verbose = verbose,
          packageVersion = packageVersion("DataSpaceR")
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
      cat("\n")
    },
    getStudy = function(study, groupId = NULL) {
      if (!is.null(groupId)) {
        warning(
          "`groupId` argument is deprecated. ",
          "Use `getGroup()` method to retrieve a group.",
          immediate. = TRUE
        )
      }

      assert_that(
        is.number(groupId) || is.null(groupId),
        msg = "groupId should be an integer or null."
      )
      assert_that(
        (study == "" && is.null(groupId)) ||
          (study == "" && is.number(groupId)) ||
          (study != "" && is.null(groupId)),
        msg = "Use empty string if you are using a group filter"
      )

      if (is.number(groupId)) {
        assert_that(
          groupId %in% private$.availableGroups$id,
          msg = paste(groupId, "is not a valid group ID")
        )
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
    getGroup = function(groupId) {
      assert_that(
        is.number(groupId),
        msg = "groupId should be an integer."
      )
      assert_that(
        groupId %in% private$.availableGroups$id,
        msg = paste(groupId, "is not a valid group ID")
      )

      group <- private$.availableGroups[.(groupId), label]
      names(group) <- private$.availableGroups[.(groupId), originalLabel]

      DataSpaceStudy$new("", private$.config, group, NULL)
    },
    filterMabGrid = function(using, value) {
      assertColumn(using)

      column <- switchColumn(using)
      gridBase <- ifelse(
        isFromMabGrid(column),
        ".mabGridBase",
        ".mabMetaGridBase"
      )

      assert_that(all(value %in% private[[gridBase]][[column]]), msg = "The `value` and `using` parameter combination are not found in the mAb grid.")

      if (isFromMabGrid(column)) {
        private$.mabGridBase <- private$.mabGridBase[
          get(column) %in% value
        ]

        if (column == "mab_mix_name_std") {
        private$.mabMetaGridBase <- private$.mabMetaGridBase[
          get(column) %in% value
        ]
        }
      } else {
        private$.mabMetaGridBase <- private$.mabMetaGridBase[
          ,
          valid := any(get(column) == value, na.rm = TRUE),
          by = mab_mix_name_std
        ][valid == TRUE]
        private$.mabGridBase <- private$.mabGridBase[
          mab_mix_name_std %in% private$.mabMetaGridBase$mab_mix_name_std
        ]
      }

      # cache filter
      private$.mabFilters[[column]] <- c(private$.mabFilters[[column]], value)

      invisible(self)
    },
    retrieveMabGridValue = function(using, mAb_mixture = "") {
      assertColumn(using)
      assert_that(is.character(mAb_mixture))

      column <- switchColumn(using)
      gridBase <- ifelse(isFromMabGrid(column), ".mabGridBase", ".mabMetaGridBase")
      if (mAb_mixture == "") {
        unique(private[[gridBase]][[column]])
      } else {
        assert_that(all(mAb_mixture %in% private$.mabGridBase$mab_mix_name_std))
        unique(private[[gridBase]][mab_mix_name_std %in% mAb_mixture][[column]])
      }
    },
    resetMabGrid = function() {
      private$.mabGridBase <- private$.cache$mabGridBase
      private$.mabMetaGridBase <- private$.cache$mabMetaGridBase
      private$.mabFilters <- list()

      invisible(self)
    },
    getMab = function() {
      DataSpaceMab$new(self$mabGrid$mAb_mixture, private$.mabFilters, private$.config)
    },
    refresh = function() {
      tries <- c(
        class(try(private$.getAvailableStudies(), silent = !private$.config$verbose)),
        class(try(private$.getStats(), silent = !private$.config$verbose)),
        class(try(private$.getAvailableGroups(), silent = !private$.config$verbose)),
        class(try(private$.getMabGrid(), silent = !private$.config$verbose))
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
    },
    mabGrid = function() {
      mabGridBase <- private$.mabGridBase
      mabMetaGridBase <- private$.mabMetaGridBase

      mabGridBase[
        ,
        `:=`(
          mAb_mixture = mab_mix_name_std,
          n_viruses = length(unique(virus)),
          n_clades = length(unique(clade[!is.na(clade)])),
          n_tiers = length(unique(neutralization_tier[!is.na(neutralization_tier)])),
          geometric_mean_curve_ic50 = exp(mean(log(as.numeric(titer_curve_ic50)))),
          n_studies = length(unique(study))
        ),
        by = mab_mix_name_std
      ]
      mabGrid <- unique(mabGridBase[, .(mAb_mixture, n_viruses, n_clades, n_tiers, geometric_mean_curve_ic50, n_studies)])
      setkey(mabGrid, mAb_mixture)

      mabMetaGridBase[
        ,
        `:=`(
          mAb_mixture = mab_mix_name_std,
          donor_species = paste(unique(mab_donor_species[!is.na(mab_donor_species)]), collapse = ", "),
          hxb2_location = paste(unique(mab_hxb2_location[!is.na(mab_hxb2_location)]), collapse = ", "),
          isotype = paste(unique(mab_isotype[!is.na(mab_isotype)]), collapse = ", ")
        ),
        by = mab_mix_name_std
      ]
      mabMetaGrid <- unique(mabMetaGridBase[, .(mAb_mixture, donor_species, isotype, hxb2_location)])
      setkey(mabMetaGrid, mAb_mixture)

      # left join mabGrid with mabMetaGrid
      mabGrid <- mabGrid[mabMetaGrid, nomatch = 0]

      mabGrid[, .(mAb_mixture, donor_species, isotype, hxb2_location, n_viruses, n_clades, n_tiers, geometric_mean_curve_ic50, n_studies)]
    }
  ),
  private = list(
    .config = list(),
    .availableStudies = data.table(),
    .stats = data.table(),
    .availableGroups = data.table(),
    .mabGridBase = data.table(),
    .mabMetaGridBase = data.table(),
    .mabFilters = list(),
    .cache = list(),

    .getAvailableStudies = function() {
      colSelect <- c("study_name", "short_name", "title", "type", "status",
                     "stage", "species", "start_date", "strategy")

      availableStudies <- labkey.selectRows(
        baseUrl = private$.config$labkey.url.base,
        folderPath = "/CAVD",
        schemaName = "CDS",
        queryName = "study",
        colSelect = colSelect,
        colNameOpt = "fieldname",
        method = "GET"
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
        colNameOpt = "fieldname",
        method = "GET"
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
          originalLabel = group$category$label,
          description = ifelse(is.null(group$description), NA, group$description),
          createdBy = group$createdBy$displayValue,
          shared = group$category$shared,
          n = length(group$category$participantIds),
          studies = list(unique(substr(group$category$participantIds, 1, 6)))
        )
      })

      # merge the list to data.table
      availableGroups <- rbindlist(groupsList)

      # set order by id
      setorder(availableGroups, id)
      setkey(availableGroups, id)

      private$.availableGroups <- availableGroups
    },
    .getMabGrid = function() {
      mabGridBase <- labkey.selectRows(
        baseUrl = private$.config$labkey.url.base,
        folderPath = "/CAVD",
        schemaName = "CDS",
        queryName = "mAbGridBase",
        colNameOpt = "fieldname",
        method = "GET"
      )
      mabMetaGridBase <- labkey.selectRows(
        baseUrl = private$.config$labkey.url.base,
        folderPath = "/CAVD",
        schemaName = "CDS",
        queryName = "mAbMetaGridBase",
        colNameOpt = "fieldname",
        method = "GET"
      )

      setDT(mabGridBase)
      setDT(mabMetaGridBase)

      private$.mabGridBase <- mabGridBase
      private$.mabMetaGridBase <- mabMetaGridBase
      private$.cache$mabGridBase <- data.table::copy(mabGridBase)
      private$.cache$mabMetaGridBase <- data.table::copy(mabMetaGridBase)

      invisible(NULL)
    }
  )
)
