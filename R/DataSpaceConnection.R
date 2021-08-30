#' The DataSpaceConnection class
#'
#' @section Constructor:
#' \code{\link{connectDS}}
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
#' @importFrom jsonlite fromJSON
#' @importFrom curl has_internet nslookup
#' @importFrom Rlabkey labkey.selectRows
#' @importFrom data.table copy
DataSpaceConnection <- R6Class(
  classname = "DataSpaceConnection",
  public = list(

    #' @description
    #' Initialize a \code{DataSpaceConnection} object.
    #' See \code{\link{connectDS}}.
    #' @param login A character. Optional argument. If there is no netrc file a
    #' temporary one can be written by passing login and password of an active
    #' DataSpace account.
    #' @param password A character. Optional. The password for the selected
    #' login.
    #' @param verbose A logical. Whether to print the extra details for
    #' troubleshooting.
    #' @param onStaging A logical. Whether to connect to the staging server
    #' instead of the production server.
    #' @return A new `DataSpaceConnection` object.
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
      if (!exists("labkey.apiKey")) {
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

    #' @description
    #' Print the \code{DataSpaceConnection} object.
    print = function() {
      cat("<DataSpaceConnection>")
      cat("\n  URL:", private$.config$labkeyUrlBase)
      cat("\n  User:", private$.config$labkeyUserEmail)
      cat("\n  Available studies:", private$.stats$studies)
      cat("\n    -", private$.stats$subjectlevelstudies, "studies with data")
      cat("\n    -", private$.stats$subjects, "subjects")
      cat("\n    -", private$.stats$datacount, "data points")
      cat("\n  Available groups:", nrow(private$.availableGroups))
      cat("\n  Available publications:", nrow(private$.availablePublications))
      cat("\n    -", sum(private$.availablePublications$publication_data_available), "publications with data")
      cat("\n")
    },

    #' @description
    #' Create a \code{\link{DataSpaceStudy}} object.
    #' @param studyName A character. Name of the study to retrieve.
    getStudy = function(studyName) {
      if (studyName != "") {
        studyInfo <- as.list(
          private$.availableStudies[.(study_name)]
        )
      } else {
        studyInfo <- NULL
      }

      DataSpaceStudy$new(studyName, private$.config, NULL, studyInfo)
    },

    #' @description
    #' Create a \code{\link{DataSpaceStudy}} object.
    #' @param groupId An integer. ID of the group to retrieve.
    getGroup = function(groupId) {
      assert_that(
        is.number(groupId),
        msg = "groupId should be an integer."
      )
      assert_that(
        groupId %in% private$.availableGroups$group_id,
        msg = paste(groupId, "is not a valid group ID. See `group_id` field in `availableGroups`.")
      )

      group <- private$.availableGroups[.(groupId), label]
      names(group) <- private$.availableGroups[.(groupId), original_label]

      DataSpaceStudy$new("", private$.config, group, NULL)
    },

    #' @description
    #' Filter rows in the mAb grid by specifying the values to keep in the
    #' columns found in the \code{mabGrid} field. It takes the column and the
    #' values and filters the underlying tables.
    #' @param using A character. Name of the column to filter.
    #' @param value A character vector. Values to keep in the mAb grid.
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

    #' @description
    #' Reset the mAb grid to the unfiltered state.
    resetMabGrid = function() {
      private$.mabGridBase <- private$.cache$mabGridBase
      private$.mabMetaGridBase <- private$.cache$mabMetaGridBase
      private$.mabFilters <- list()

      invisible(self)
    },

    #' @description
    #' Create a \code{\link{DataSpaceMab}} object.
    getMab = function() {
      DataSpaceMab$new(self$mabGridSummary$mab_mixture, private$.mabFilters, private$.config)
    },

    #' @description
    #' Download publication data for a chosen publication.
    #' @param publicationId A character/integer. ID for the publication to
    #' download data for.
    #' @param outputDir A character. Path to directory to download publication
    #' data.
    #' @param unzip A logical. If TRUE, unzip publication data to outputDir.
    #' @param verbose A logical. Default TRUE.
    downloadPublicationData = function(publicationId,
                                       outputDir = getwd(),
                                       unzip = TRUE,
                                       verbose = TRUE) {
      assert_that(
        dir.exists(outputDir),
        msg = paste0(outputDir, " is not a directory")
      )
      assert_that(
        publicationId %in% private$.availablePublications$publication_id,
        msg = paste0(publicationId, " is not a valid publication ID. See the `publication_id` field in `availablePublications`.")
      )
      assert_that(
        private$.availablePublications[publication_id == publicationId]$publication_data_available,
        msg = paste0("No publication data available for publication ", publicationId)
      )
      assert_that(is.logical(verbose))

      remotePath <- private$.availablePublications[publication_id == publicationId]$remotePath
      fileName <- basename(remotePath)
      localZipPath <- file.path(outputDir, fileName)
      fullOutputDir <- file.path(outputDir, gsub(".zip", "", fileName))
      if (verbose) message("downloading ", fileName, " to ", outputDir, " ...")

      # Use getStudyDocumentUrl.view to download
      getStudyDocumentUrl <- paste0(
        private$.config$labkeyUrlBase,
        "/cds/CAVD/getStudyDocument.view?",
        "&documentId=", private$.availablePublications[publication_id == publicationId]$document_id,
        "&filename=", gsub("/", "%2F", remotePath),
        "&publicAccess=true"
      )

      # Use labkey.webdav.getByUrl which includes filesystem and permissions checks
      ret <- Rlabkey:::labkey.webdav.getByUrl(getStudyDocumentUrl, localFilePath = localZipPath, overwrite = TRUE)
      if (!is.null(ret) && !is.na(ret) && ret == FALSE) {
        success <- FALSE
        stop("failed to download ", fileName)
      } else {
        success <- file.exists(localZipPath)
        if (!success) stop("failed to download to", localZipPath)
      }

      if (verbose) {
        message("File Contents: ")
        fileContents <- capture.output(unzip(localZipPath, list = TRUE))
        message(paste0(fileContents, collapse = "\n"))
      }

      if (unzip) {
        if (verbose) message("unzipping ", fileName, " to ", fullOutputDir)
        unzippedFiles <- unzip(localZipPath, exdir = fullOutputDir)
        unlink(localZipPath)
        # Return vector of file paths
        return(invisible(unzippedFiles))
      }

      # Return path of zip file
      invisible(localZipPath)
    },

    #' @description
    #' Refresh the connection object to update available studies and groups.
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
        )),
        class(try(
          private$.getVirusMetadata(),
          silent = !private$.config$verbose
        )),
        class(try(
          private$.getAvailablePublications(),
          silent = !private$.config$verbose
        ))
      )

      invisible(!"try-error" %in% tries)
    }
  ),
  active = list(

    #' @field config A list. Stores configuration of the connection object such as
    #' URL, path and username.
    config = function() {
      private$.config
    },

    #' @field availableStudies A data.table. The table of available studies.
    availableStudies = function() {
      private$.availableStudies
    },

    #' @field availableGroups A data.table. The table of available groups.
    availableGroups = function() {
      private$.availableGroups
    },

    #' @field availablePublications A data.table. The table of available
    #' publications.
    availablePublications = function() {
      private$.availablePublications[, !c("document_id", "remotePath")]
    },

    #' @field mabGridSummary A data.table. The filtered grid with updated
    #' \code{n_} columns and \code{geometric_mean_curve_ic50}.
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

    #' @field mabGrid A data.table. The filtered mAb grid.
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
    },

    #' @field virusMetadata A data.table. Metadata about all viruses in the
    #' DataSpace.
    virusMetadata = function() {
      private$.virusMetadata
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
    .virusMetadata = data.table(),
    .availablePublications = data.table(),
    .cache = list(),
    .getAvailableStudies = function() {
      colSelect <- c(
        "study_name", "short_name", "title", "type", "status",
        "stage", "species", "start_date", "strategy",
        "network", "data_availability"
      )

      niData <- setDT(
        merge(
          labkey.selectRows(
            baseUrl = private$.config$labkeyUrlBase,
            folderPath = "/CAVD",
            schemaName = "CDS",
            queryName = "document",
            colNameOpt = "fieldname",
            colSelect = c("document_id", "label", "filename", "document_type", "assay_identifier")
          ),
          labkey.selectRows(
            baseUrl = private$.config$labkeyUrlBase,
            folderPath = "/CAVD",
            schemaName = "CDS",
            queryName = "studydocument",
            colNameOpt = "fieldname",
            colSelect = c("document_id", "prot")
          ),
          by = "document_id"
        )
      )[document_type == "Non-Integrated Assay"][, .(ni_data_availability = paste(label, collapse = ", ")), by = "prot"]

      setnames(niData, "prot", "study_name")

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

      availableStudies <- merge(availableStudies, niData, all.x = TRUE)
      availableStudies[, data_availability := gsub("This study has assay data \\(", "", gsub("\\) in the DataSpace\\.", "", data_availability))]

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
          group_id = group$id,
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
      setorder(availableGroups, group_id)
      setkey(availableGroups, group_id)

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
    },
    .getVirusMetadata = function() {
      colSelect <- c(
        "assay_identifier", "virus", "virus_type", "neutralization_tier", "clade",
        "antigen_control", "virus_full_name", "virus_name_other", "virus_species",
        "virus_host_cell", "virus_backbone", "panel_names"
      )

      virusMetadata <- labkey.selectRows(
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = "/CAVD/",
        schemaName = "CDS",
        queryName = "nabAntigenWithPanelMeta",
        colSelect = colSelect,
        colNameOpt = "fieldname",
        method = "GET"
      )
      setDT(virusMetadata)
      setkey(virusMetadata, virus)

      private$.virusMetadata <- virusMetadata
    },
    .getAvailablePublications = function() {
      sqlQuery <-
        "
SELECT publication.id as publication_id, author_first as first_author, author_all as all_authors, title, journal_short journal, date publication_date,
link, pmid as pubmed_id, related_studies, studies_with_data, filename IS NOT NULL as publication_data_available, document_id,
filename as remote_path
FROM publication
LEFT OUTER JOIN
  (
    SELECT GROUP_CONCAT(DISTINCT prot, ', ') AS related_studies, publication_id
    FROM studypublication
    GROUP BY publication_id
  ) sp
  ON sp.publication_id = publication.id
LEFT OUTER JOIN
  (
  SELECT GROUP_CONCAT(DISTINCT prot, ', ') AS studies_with_data, publication_id
  FROM (
    studypublication
    LEFT OUTER JOIN
      (
        SELECT study_name, data_availability FROM study
      ) sdy
      ON sdy.study_name = prot
    )
  WHERE data_availability IS NOT NULL
  GROUP BY publication_id
  ) da
  ON da.publication_id = publication.id
LEFT OUTER JOIN
  (
    SELECT filename, document_id, publication_id from learn_publicationdata
  ) pd
  ON pd.publication_id = publication.id
"

      availablePublications <- labkey.executeSql(
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = "/CAVD/",
        schemaName = "CDS",
        sql = sqlQuery,
        colNameOpt = "fieldname"
      )

      setDT(availablePublications)
      setorder(availablePublications, "first_author")
      setnames(availablePublications, "remote_path", "remotePath")
      private$.availablePublications <- availablePublications
    }
  )
)
