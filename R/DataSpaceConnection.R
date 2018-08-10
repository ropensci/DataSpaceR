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
      assert_that((is.null(login) && is.null(password)) || (!is.null(login) && !is.null(password)),
                  msg = "Enter both `login` and `password` or use netrc file.")
      assert_that(is.logical(verbose))
      assert_that(is.logical(onStaging))
      assert_that(has_internet(),
                  msg = "No internet connection. Please connect to internet and try again.")

      # check if the portal is up
      assert_that(!is.null(nslookup(ifelse(onStaging, STAGING, PRODUCTION), error = FALSE)),
                  msg = "The portal is currently down. Try again later.")

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
    filterMAbGrid = function(using, value) {
      assert_that(using %in% c("mAb_mixture", "donor_species", "isotype", "hxb2_location", "viruses", "clades", "tiers", "curve_ic50", "studies"))

      if (using == "mAb_mixture") {
        assert_that(all(value %in% private$.mAbGridBase$mab_mix_name_std))
        private$.mAbGridBase <- private$.mAbGridBase[mab_mix_name_std %in% value]
        private$.mAbMetaGridBase <- private$.mAbMetaGridBase[mab_mix_name_std %in% value]
      }
      if (using == "donor_species") {
        assert_that(all(value %in% private$.mAbMetaGridBase$mab_donor_species))
        private$.mAbMetaGridBase <- private$.mAbMetaGridBase[mab_donor_species %in% value]
        private$.mAbGridBase <- private$.mAbGridBase[mab_mix_name_std %in% private$.mAbMetaGridBase$mab_mix_name_std]
      }
      if (using == "isotype") {
        assert_that(all(value %in% private$.mAbMetaGridBase$mab_isotype))
        private$.mAbMetaGridBase <- private$.mAbMetaGridBase[mab_isotype %in% value]
        private$.mAbGridBase <- private$.mAbGridBase[mab_mix_name_std %in% private$.mAbMetaGridBase$mab_mix_name_std]
      }
      if (using == "hxb2_location") {
        assert_that(all(value %in% private$.mAbMetaGridBase$mab_hxb2_location))
        private$.mAbMetaGridBase <- private$.mAbMetaGridBase[mab_hxb2_location %in% value]
        private$.mAbGridBase <- private$.mAbGridBase[mab_mix_name_std %in% private$.mAbMetaGridBase$mab_mix_name_std]
      }
      if (using == "viruses") {
        assert_that(all(value %in% private$.mAbGridBase$virus))
        private$.mAbGridBase <- private$.mAbGridBase[virus %in% value]
      }
      if (using == "clades") {
        assert_that(all(value %in% private$.mAbGridBase$clade))
        private$.mAbGridBase <- private$.mAbGridBase[clade %in% value]
      }
      if (using == "tiers") {
        assert_that(all(value %in% private$.mAbGridBase$neutralization_tier))
        private$.mAbGridBase <- private$.mAbGridBase[neutralization_tier %in% value]
      }
      if (using == "studies") {
        assert_that(all(value %in% private$.mAbGridBase$study))
        private$.mAbGridBase <- private$.mAbGridBase[study %in% value]
      }

      invisible(NULL)
    },
    clearMAbGridFilters = function() {
      private$.mAbGridBase <- private$.cache$mAbGridBase
      private$.mAbMetaGridBase <- private$.cache$mAbMetaGridBase
    },
    refresh = function() {
      tries <- c(
        class(try(private$.getAvailableStudies(), silent = !private$.config$verbose)),
        class(try(private$.getStats(), silent = !private$.config$verbose)),
        class(try(private$.getAvailableGroups(), silent = !private$.config$verbose)),
        class(try(private$.getMAbGrid(), silent = !private$.config$verbose))
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
    mAbGrid = function() {
      mAbGridBase <- private$.mAbGridBase
      mAbMetaGridBase <- private$.mAbMetaGridBase

      mAbGridBase[, n_viruses := length(unique(virus)), by = mab_mix_name_std]
      mAbGridBase[, n_clades := length(unique(clade[!is.na(clade)])), by = mab_mix_name_std]
      mAbGridBase[, n_tiers := length(unique(neutralization_tier[!is.na(neutralization_tier)])), by = mab_mix_name_std]
      mAbGridBase[, goemetric_mean_curve_ic50 := exp(mean(log(as.numeric(titer_curve_ic50)))), by = mab_mix_name_std]
      mAbGridBase[, n_studies := length(unique(study)), by = mab_mix_name_std]
      mAbGrid <- unique(mAbGridBase[, .(mab_mix_name_std, n_viruses, n_clades, n_tiers, goemetric_mean_curve_ic50, n_studies)])

      mAbMetaGridBase[, donor_species := paste(unique(mab_donor_species[!is.na(mab_donor_species)]), collapse = ", "), by = mab_mix_name_std]
      mAbMetaGridBase[, hxb2_location := paste(unique(mab_hxb2_location[!is.na(mab_hxb2_location)]), collapse = ", "), by = mab_mix_name_std]
      mAbMetaGridBase[, isotype := paste(unique(mab_isotype[!is.na(mab_isotype)]), collapse = ", "), by = mab_mix_name_std]
      mAbMetaGrid <- unique(mAbMetaGridBase[, .(donor_species, mab_mix_name_std, isotype, hxb2_location)])

      setkey(mAbGrid, mab_mix_name_std)
      setkey(mAbMetaGrid, mab_mix_name_std)
      mAbGrid <- mAbGrid[mAbMetaGrid, nomatch = 0]

      mAbGrid[, .(mAb_mixture = mab_mix_name_std, donor_species, isotype, hxb2_location, n_viruses, n_clades, n_tiers, goemetric_mean_curve_ic50, n_studies)]
    }
  ),
  private = list(
    .config = list(),
    .availableStudies = data.table(),
    .stats = data.table(),
    .availableGroups = data.table(),
    .mAbGridBase = data.table(),
    .mAbMetaGridBase = data.table(),
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
    .getMAbGrid = function() {
      mAbGridBase <- labkey.selectRows(
        baseUrl = private$.config$labkey.url.base,
        folderPath = "/CAVD",
        schemaName = "CDS",
        queryName = "mAbGridBase",
        colNameOpt = "fieldname",
        method = "GET"
      )
      mAbMetaGridBase <- labkey.selectRows(
        baseUrl = private$.config$labkey.url.base,
        folderPath = "/CAVD",
        schemaName = "CDS",
        queryName = "mAbMetaGridBase",
        colNameOpt = "fieldname",
        method = "GET"
      )

      setDT(mAbGridBase)
      setDT(mAbMetaGridBase)

      private$.mAbGridBase <- mAbGridBase
      private$.mAbMetaGridBase <- mAbMetaGridBase
      private$.cache$mAbGridBase <- data.table::copy(mAbGridBase)
      private$.cache$mAbMetaGridBase <- data.table::copy(mAbMetaGridBase)

      invisible(NULL)
    }
  )
)
