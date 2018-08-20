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
    filterMabGrid = function(using, value) {
      assert_that(using %in% c("mAb_mixture", "donor_species", "isotype", "hxb2_location", "viruses", "clades", "tiers", "curve_ic50", "studies"))

      if (using == "mAb_mixture") {
        assert_that(all(value %in% private$.mabGridBase$mab_mix_name_std))
        private$.mabGridBase <- private$.mabGridBase[mab_mix_name_std %in% value]
        private$.mabMetaGridBase <- private$.mabMetaGridBase[mab_mix_name_std %in% value]
      }
      if (using == "donor_species") {
        assert_that(all(value %in% private$.mabMetaGridBase$mab_donor_species))
        private$.mabMetaGridBase <- private$.mabMetaGridBase[mab_donor_species %in% value]
        private$.mabGridBase <- private$.mabGridBase[mab_mix_name_std %in% private$.mabMetaGridBase$mab_mix_name_std]
        private$.mabFilters$mab_donor_species <- c(private$.mabFilters$mab_donor_species, value)
      }
      if (using == "isotype") {
        assert_that(all(value %in% private$.mabMetaGridBase$mab_isotype))
        private$.mabMetaGridBase <- private$.mabMetaGridBase[mab_isotype %in% value]
        private$.mabGridBase <- private$.mabGridBase[mab_mix_name_std %in% private$.mabMetaGridBase$mab_mix_name_std]
      }
      if (using == "hxb2_location") {
        assert_that(all(value %in% private$.mabMetaGridBase$mab_hxb2_location))
        private$.mabMetaGridBase <- private$.mabMetaGridBase[mab_hxb2_location %in% value]
        private$.mabGridBase <- private$.mabGridBase[mab_mix_name_std %in% private$.mabMetaGridBase$mab_mix_name_std]
        private$.mabFilters$mab_hxb2_location <- c(private$.mabFilters$mab_hxb2_location, value)
      }
      if (using == "viruses") {
        assert_that(all(value %in% private$.mabGridBase$virus))
        private$.mabGridBase <- private$.mabGridBase[virus %in% value]
        private$.mabFilters$virus <- c(private$.mabFilters$virus, value)
      }
      if (using == "clades") {
        assert_that(all(value %in% private$.mabGridBase$clade))
        private$.mabGridBase <- private$.mabGridBase[clade %in% value]
      }
      if (using == "tiers") {
        assert_that(all(value %in% private$.mabGridBase$neutralization_tier))
        private$.mabGridBase <- private$.mabGridBase[neutralization_tier %in% value]
      }
      if (using == "studies") {
        assert_that(all(value %in% private$.mabGridBase$study))
        private$.mabGridBase <- private$.mabGridBase[study %in% value]
      }

      invisible(self)
    },
    retrieveMabGridValue = function(using, mAb_mixture = "") {
      assert_that(using %in% c("mAb_mixture", "donor_species", "isotype", "hxb2_location", "viruses", "clades", "tiers", "curve_ic50", "studies"))

      switch(using,
        "mAb_mixture" = unique(private$.mabGridBase$mab_mix_name_std),
        "donor_species" = unique(private$.mabMetaGridBase$mab_donor_species),
        "isotype" = unique(private$.mabMetaGridBase$mab_isotype),
        "hxb2_location" = unique(private$.mabMetaGridBase$mab_hxb2_location),
        "viruses" = unique(private$.mabGridBase$virus),
        "clades" = unique(private$.mabGridBase$clade),
        "tiers" = unique(private$.mabGridBase$neutralization_tier),
        "curve_ic50" = unique(private$.mabGridBase$titer_curve_ic50),
        "studies" = unique(private$.mabGridBase$study)
      )
    },
    resetMabGridFilters = function() {
      private$.mabGridBase <- private$.cache$mabGridBase
      private$.mabMetaGridBase <- private$.cache$mabMetaGridBase
      private$.mabFilters <- list()
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

      mabGridBase[, n_viruses := length(unique(virus)), by = mab_mix_name_std]
      # mabGridBase[, viruses := paste(unique(virus), collapse = ", "), by = mab_mix_name_std]
      mabGridBase[, n_clades := length(unique(clade[!is.na(clade)])), by = mab_mix_name_std]
      # mabGridBase[, clades := paste(unique(clade[!is.na(clade)]), collapse = ", "), by = mab_mix_name_std]
      mabGridBase[, n_tiers := length(unique(neutralization_tier[!is.na(neutralization_tier)])), by = mab_mix_name_std]
      # mabGridBase[, tiers := paste(unique(neutralization_tier[!is.na(neutralization_tier)]), collapse = ", "), by = mab_mix_name_std]
      mabGridBase[, geometric_mean_curve_ic50 := exp(mean(log(as.numeric(titer_curve_ic50)))), by = mab_mix_name_std]
      mabGridBase[, n_studies := length(unique(study)), by = mab_mix_name_std]
      # mabGridBase[, studies := paste(unique(study), collapse = ", "), by = mab_mix_name_std]
      mabGrid <- unique(mabGridBase[, .(mab_mix_name_std, n_viruses, n_clades, n_tiers, geometric_mean_curve_ic50, n_studies)])
      # mabGrid <- unique(mabGridBase[, .(mab_mix_name_std, n_viruses, viruses, n_clades, clades, n_tiers, tiers, geometric_mean_curve_ic50, n_studies, studies)])

      mabMetaGridBase[, donor_species := paste(unique(mab_donor_species[!is.na(mab_donor_species)]), collapse = ", "), by = mab_mix_name_std]
      mabMetaGridBase[, hxb2_location := paste(unique(mab_hxb2_location[!is.na(mab_hxb2_location)]), collapse = ", "), by = mab_mix_name_std]
      mabMetaGridBase[, isotype := paste(unique(mab_isotype[!is.na(mab_isotype)]), collapse = ", "), by = mab_mix_name_std]
      mabMetaGrid <- unique(mabMetaGridBase[, .(donor_species, mab_mix_name_std, isotype, hxb2_location)])

      setkey(mabGrid, mab_mix_name_std)
      setkey(mabMetaGrid, mab_mix_name_std)
      mabGrid <- mabGrid[mabMetaGrid, nomatch = 0]

      # mabGrid[, .(mAb_mixture = mab_mix_name_std, donor_species, isotype, hxb2_location, n_viruses, viruses, n_clades, clades, n_tiers, tiers, geometric_mean_curve_ic50, n_studies, studies)]
      mabGrid[, .(mAb_mixture = mab_mix_name_std, donor_species, isotype, hxb2_location, n_viruses, n_clades, n_tiers, geometric_mean_curve_ic50, n_studies)]
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
