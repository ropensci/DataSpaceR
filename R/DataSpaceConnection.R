#' The DataSpaceConnection class
#'
#' @return an instance of \code{DataSpaceConnection}
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
#'   \item{\code{mabGrid}}{
#'     A data.table. The filtered mAb grid.
#'   }
#'   \item{\code{mabGridSummary}}{
#'     A data.table. The filtered grid with updated \code{n_} columns and
#'     \code{geometric_mean_curve_ic50}.
#'   }
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(login = NULL, password = NULL, verbose = FALSE,
#'   onStaging = FALSE)}}{
#'     Initialize a \code{DataSpaceConnection} object.
#'     See \code{\link{connectDS}}.
#'   }
#'   \item{\code{print()}}{
#'     Print the \code{DataSpaceConnection} object.
#'   }
#'   \item{\code{getStudy(study, groupId = NULL)}}{
#'     Create a \code{\link{DataSpaceStudy}} object.
#'
#'     \code{study}: A character. Name of the study to retrieve.
#'   }
#'   \item{\code{getGroup(groupId)}}{
#'     Create a \code{\link{DataSpaceStudy}} object.
#'
#'     \code{groupId}: An integer. ID of the group to retrieve.
#'   }
#'   \item{\code{refresh()}}{
#'     Refresh the connection object to update available studies and groups.
#'   }
#'   \item{\code{filterMabGrid(using, value)}}{
#'     Filter rows in the mAb grid by specifying the values to keep in the
#'     columns found in the \code{mabGrid} field. It takes the column and the
#'     values and filters the underlying tables.
#'
#'     \code{using}: A character. Name of the column to filter.
#'
#'     \code{value}: A character vector. Values to keep in the mAb grid.
#'   }
#'   \item{\code{getMab()}}{
#'     Create a \code{\link{DataSpaceMab}} object.
#'   }
#'   \item{\code{resetMabGrid()}}{
#'     Reset the mAb grid to the unfiltered state.
#'   }
#' }
#'
#' @seealso \code{\link{connectDS}} \code{\link{DataSpaceR-package}}
#'
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
#'
#' @docType class
#' @format NULL
#'
#' @importFrom jsonlite fromJSON
#' @importFrom curl has_internet nslookup
#' @importFrom Rlabkey labkey.selectRows
#' @importFrom data.table copy
DataSpaceConnection <- R6Class(
  classname = "DataSpaceConnection",
  public = list(
    initialize = function(login = NULL,
                          password = NULL,
                          verbose = FALSE,
                          onStaging = FALSE) {
      assert_that(
        (is.null(login) && is.null(password)) ||
          (!is.null(login) && !is.null(password)),
        msg = "Enter both `login` and `password` or use netrc file."
      )
      assert_that(is.logical(verbose))
      assert_that(is.logical(onStaging))
      assert_that(
        has_internet(),
        msg = "No internet connection. Connect to internet and try again."
      )

      # check if the portal is up
      assert_that(
        !is.null(nslookup(
          ifelse(onStaging, STAGING, PRODUCTION),
          error = FALSE
        )),
        msg = "The portal is currently down. Try again later."
      )

      # get primary fields
      labkeyUrlBase <- getUrlBase(onStaging)
      labkeyUserEmail <- getUserEmail(labkeyUrlBase, login)

      # set Curl options
      netrcFile <- getNetrc(login, password, onStaging)
      curlOptions <- setCurlOptions(netrcFile)

      # check netrc file
      if (!exists("labkey.sessionCookieName")) {
        checkNetrc(netrcFile, onStaging, verbose = FALSE)
      }

      # check credential
      checkCredential(labkeyUrlBase, verbose)

      # set primary fields
      private$.config <-
        list(
          labkeyUrlBase = labkeyUrlBase,
          labkeyUserEmail = labkeyUserEmail,
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
      cat("\n  URL:", private$.config$labkeyUrlBase)
      cat("\n  User:", private$.config$labkeyUserEmail)
      cat("\n  Available studies:", private$.stats$studies)
      cat("\n    -", private$.stats$subjectlevelstudies, "studies with data")
      cat("\n    -", private$.stats$subjects, "subjects")
      cat("\n    -", private$.stats$datacount, "data points")
      cat("\n  Available groups:", nrow(private$.availableGroups))
      cat("\n")
    },
    getStudy = function(study) {
      if (study != "") {
        studyInfo <- as.list(
          private$.availableStudies[.(study)]
        )
      } else {
        studyInfo <- NULL
      }

      DataSpaceStudy$new(study, private$.config, NULL, studyInfo)
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
      names(group) <- private$.availableGroups[.(groupId), original_label]

      DataSpaceStudy$new("", private$.config, group, NULL)
    },
    filterMabGrid = function(using, value) {
      assertColumn(using, self)

      column <- switchColumn(using)
      gridBase <- ifelse(
        isFromMabGrid(column),
        ".mabGridBase",
        ".mabMetaGridBase"
      )

      if (!all(value %in% private[[gridBase]][[column]])) {
        missingValue <- unique(value[!value %in% private[[gridBase]][[column]]])
        value <- unique(value[value %in% private[[gridBase]][[column]]])
        if (length(missingValue) > 3) {
          msgText <- paste(c(missingValue[c(1, 2, 3)], "and others"), collapse = ", ")
        } else {
          msgText <- paste(missingValue, collapse = ", ")
        }
        assert_that(length(value) != 0, msg = paste0(msgText, " set to the `value` argument is/are not found in the column set in the `using` argument.\nOnly returning values found."))
        warning(msgText, " set to the `value` argument is/are not found in the column set in the `using` argument.\nOnly returning values found.")
      }
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
          valid := any(get(column) %in% value, na.rm = TRUE),
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
    resetMabGrid = function() {
      private$.mabGridBase <- private$.cache$mabGridBase
      private$.mabMetaGridBase <- private$.cache$mabMetaGridBase
      private$.mabFilters <- list()

      invisible(self)
    },
    getMab = function() {
      DataSpaceMab$new(self$mabGridSummary$mab_mixture, private$.mabFilters, private$.config)
    },
    refresh = function() {
      tries <- c(
        class(try(
          private$.getAvailableStudies(),
          silent = !private$.config$verbose
        )),
        class(try(
          private$.getStats(),
          silent = !private$.config$verbose
        )),
        class(try(
          private$.getAvailableGroups(),
          silent = !private$.config$verbose
        )),
        class(try(
          private$.getMabGrid(),
          silent = !private$.config$verbose
        ))
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
    mabGridSummary = function() {
      mabGridBase <- copy(private$.mabGridBase)
      mabMetaGridBase <- copy(private$.mabMetaGridBase)
      mabGridBase[
        ,
        `:=`(
          mab_mixture = mab_mix_name_std,
          n_viruses = length(unique(virus)),
          n_clades = length(unique(clade[!is.na(clade)])),
          n_tiers = length(unique(neutralization_tier[!is.na(neutralization_tier)])),
          geometric_mean_curve_ic50 = as.numeric({
            if (all(titer_curve_ic50 %in% c(-Inf, Inf))) {
              NA
            } else {
              exp(mean(log(as.numeric(titer_curve_ic50[!titer_curve_ic50 %in% c(-Inf, Inf)]))))
            }
          }),
          n_studies = length(unique(study))
        ),
        by = mab_mix_name_std
      ]

      mabGrid <- unique(mabGridBase[, .(mab_mixture, n_viruses, n_clades, n_tiers, geometric_mean_curve_ic50, n_studies)])
      setkey(mabGrid, mab_mixture)

      mabMetaGridBase[
        ,
        `:=`(
          mab_mixture = mab_mix_name_std,
          donor_species = paste(sort(unique(mab_donor_species[!is.na(mab_donor_species)])), collapse = ", "),
          hxb2_location = paste(sort(unique(mab_hxb2_location[!is.na(mab_hxb2_location)])), collapse = ", "),
          isotype = paste(sort(unique(mab_isotype[!is.na(mab_isotype)])), collapse = ", ")
        ),
        by = mab_mix_name_std
      ]
      mabMetaGrid <- unique(mabMetaGridBase[, .(mab_mixture, donor_species, isotype, hxb2_location)])
      setkey(mabMetaGrid, mab_mixture)

      # left join mabGrid with mabMetaGrid
      mabGrid <- mabGrid[mabMetaGrid, nomatch = 0]

      mabGrid[, .(mab_mixture, donor_species, isotype, hxb2_location, n_viruses, n_clades, n_tiers, geometric_mean_curve_ic50, n_studies)]
    },
    mabGrid = function() {
      mabGridBase <- copy(private$.mabGridBase)
      mabMetaGridBase <- copy(private$.mabMetaGridBase)

      mabGridBase <- unique(
        mabGridBase[
          ,
          .(
            mab_mix_id = mab_mix_id,
            mab_mixture = mab_mix_name_std,
            virus = virus,
            clade = clade,
            tier = neutralization_tier,
            curve_ic50 = titer_curve_ic50,
            study = study
          )
        ]
      )

      mabMetaGridBase <- unique(
        mabMetaGridBase[
          ,
          .(
            mab_mix_id = mab_mix_id,
            mab_mixture = mab_mix_name_std,
            donor_species = mab_donor_species,
            hxb2_location = mab_hxb2_location,
            isotype = mab_isotype
          ),
        ]
      )


      setkey(mabGridBase, mab_mix_id)
      setkey(mabMetaGridBase, mab_mix_id)

      # left join mabGrid with mabMetaGrid
      mabGrid <- merge(
        mabGridBase,
        mabMetaGridBase[, .(mab_mix_id, donor_species, hxb2_location, isotype)],
        allow.cartesian = TRUE
      )

      mabGrid[, .(mab_mixture, donor_species, isotype, hxb2_location, virus, clade, tier, curve_ic50, study)]
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
      colSelect <- c(
        "study_name", "short_name", "title", "type", "status",
        "stage", "species", "start_date", "strategy",
        "network", "data_availability"
      )

      availableStudies <- labkey.selectRows(
        baseUrl = private$.config$labkeyUrlBase,
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
        baseUrl = private$.config$labkeyUrlBase,
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
        private$.config$labkeyUrlBase,
        "/participant-group",
        "/CAVD",
        "/browseParticipantGroups.api?",
        "distinctCatgories=false&",
        "type=participantGroup&",
        "includeUnassigned=false&",
        "includeParticipantIds=false"
      )

      # execute via Rlabkey's standard GET function
      response <- labkey.get(participantGroupApi)

      # parse JSON response via jsonlite's fromJSON parsing function
      parsed <- fromJSON(response, simplifyDataFrame = FALSE)

      # construct a data.table for each group
      groupsList <- lapply(parsed$groups, function(group) {
        data.table(
          id = group$id,
          label = group$label,
          original_label = group$category$label,
          description = ifelse(
            is.null(group$description),
            NA,
            group$description
          ),
          created_by = group$createdBy$displayValue,
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
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "CDS",
        queryName = "mAbGridBase",
        colNameOpt = "fieldname",
        method = "GET"
      )
      mabMetaGridBase <- labkey.selectRows(
        baseUrl = private$.config$labkeyUrlBase,
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
      private$.mabFilters <- list()
      private$.cache$mabGridBase <- copy(mabGridBase)
      private$.cache$mabMetaGridBase <- copy(mabMetaGridBase)

      invisible(NULL)
    }
  )
)
