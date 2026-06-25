#' @title The DataSpaceConnection class
#'
#' @description
#' An R6 class for DataSpace browsing and fetching data in DataSpace.
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
#'
#' # View available data
#'
#' con$availableStudies
#' con$availableGroups
#' con$availablePublications
#' con$availableMabs
#' con$availableMabMixtures
#' con$availableDonors
#' con$availableViruses
#'
#' # Pass an available object to a "get" method to get data
#'
#' cvd408 <- con$availableStudies[study_id == "cvd408"] |>
#'   con$getStudies()
#'
#' cd4Mabs <- con$availableMabs[grepl("CD4bs", mab_ab_binding_type)] |>
#'   con$getMabs()
#'
#' }
#'
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
      private$.shared$.config <-
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
      cat("\n  URL:", private$.shared$.config$labkeyUrlBase)
      cat("\n  User:", private$.shared$.config$labkeyUserEmail)
      cat("\n  Available Studies:", private$.stats$studies)
      cat("\n    -", private$.stats$subjectlevelstudies, "studies with data")
      cat("\n    -", private$.stats$subjects, "subjects")
      cat("\n    -", private$.stats$datacount, "data points")
      cat("\n  Available Groups:", nrow(self$availableGroups))
      cat("\n  Available Publications:", nrow(self$availablePublications))
      cat("\n    -", sum(self$availablePublications$publication_data_available), "publications with data")
      cat("\n  Available Connection objects:")
      cat(paste0("\n    - ", DataSpaceConnection$active |> names() |> cleanReservedDataSpaceR6()), sep = "")
      cat("\n  Available Connection methods:")
      cat(paste0("\n    - ", DataSpaceConnection$public_methods |> names() |> cleanReservedDataSpaceR6()), sep = "")
      cat("\n")
    },

    #' @description
    #' Create a `DataSpaceStudies` object.
    #' @param availableStudies an `availableStudies` object, or a vector of `study_id` values.
    getStudies = function(availableStudies = self$availableStudies) {
      if(is.character(availableStudies))
        availableStudies <- self$availableStudies[study_id %in% availableStudies]
      if(!"availableStudies" %in% class(availableStudies))
        stop("Argument must be `character` or `availableStudies`.")
      if(nrow(availableStudies) == 0)
        stop("No vaild `availableStudies`, or `study_id` vector passed.")

      DataSpaceStudies$new(availableStudies[,study_id])
    },

    #' @description
    #' Create a `DataSpaceGroups` object.
    #' @param availableGroups an `availableGroups` object, or a vector of `group id` values.
    getGroups = function(availableGroups = self$availableGroups) {
      if(is.numeric(availableGroups))
        availableGroups <- self$availableGroups[group_id %in% availableGroups]
      if(!"availableGroups" %in% class(availableGroups))
        stop("Argument must be `numeric` or `avilableGroups`.")
      if(nrow(availableGroups) == 0)
        stop("No vaild `availableGroups`, or `group_id` vector passed.")

      DataSpaceGroups$new(availableGroups[,group_id])
    },

    #' @description
    #' Create a `DataSpaceMabs` object.
    #' @param availableMabs an `availableMabs` or
    #' `availableMabMixtures` object, or a vector of `mab id` values. `mab_id` values are
    #' inferred from `availableMabMixtures` objects.
    #' @param includeMixtures Whether or not to include mab mixtures. "yes", "no", or "only"
    #' are valid. The default, "yes", will return any available mAb mixtures for any mAb passed here.
    getMabs = function(availableMabs = self$availableMabs, includeMixtures = "yes"){
      if(is.character(availableMabs))
        availableMabs <- self$availableMabs[
          mab_id %in% c(
            private$.shared$.mabMix[mab_mix_id %in% availableMabs, mab_id],
            private$.shared$.mabMix[mab_id %in% availableMabs, mab_id]
          )
        ]

      if(!any(class(availableMabs) %in% c("availableMabs", "availableMabMixtures")))
        stop("Argument must be `character`, `availableMabs`, or `availableMabMixtures`.")

      if(length(includeMixtures) != 1 || (!includeMixtures %in% c("yes", "no", "only")))
        stop("`includeMixtures` must by 1 of 3 values: 'yes', 'no', or 'only'.")

      if(nrow(availableMabs) == 0)
        stop("No mAbs available from `availableMabs` argument.")

      if("availableMabMixtures" %in% class(availableMabs)){
        message("All mabs in all mixtures passed with be returned.")
        availableMabs <-
          merge(availableMabs, private$.shared$.mabMix, by= "mab_mix_id")
      }

      DataSpaceMabs$new(availableMabs[,unique(mab_id)], includeMixtures)
    },

    #' @description
    #' Create a `DataSpaceDonors` object.
    #' @param availableDonors an `availableDonors` object, or a vector of `donor_id` values.
    getDonors = function(availableDonors = self$availableDonors){
      if(is.character(availableDonors))
        availableGroups <- self$availableDonors[donor_id %in% availableDonors]

      if(!"availableDonors" %in% class(self$availableDonors))
        stop("Argument must be `character` or `availableDonors`.")

      if(nrow(availableDonors) == 0)
        stop("No donors available from `availableDonors` argument.")

      DataSpaceDonors$new(availableDonors[,unique(donor_id)])
    },

    #' @description
    #' Create a `DataSpaceDaash` object.
    #' @param availableDaash an `availableMabs`, or `availableDonors` object, or a vector of `sequnce_id` values.
    getDaash = function(availableDaash = NULL){
      DataSpaceDaash$new(availableDaash)
    },

    #' @description
    #' Download study related publication datasets.
    #' @param availablePublications an `availablePublications` object or a vector of `publication_id` values.
    #' @param downloadDir A character. Optional, specifies directory to download nonstandard datasets.
    #' Default is use to the R session temp directory
    downloadPublicationData = function(availablePublications = NULL, downloadDir = tempdir()){

      if(is.character(availablePublications))
        availablePublications <- self$availablePublications[publication_id == availablePublications]

      if(!"availablePublications" %in% class(availablePublications))
        stop("Argument must be `character` or `availablePublications`.")

      if(nrow(availablePublications) == 0)
        stop("No datasets to download from `availablePublications`.")

      availablePublications <- private$.shared$.availablePublications[
        publication_id %in% availablePublications$publication_id
      ]

      downloadDocument <- invisible(
        availablePublications |>
          downloadDocuments(downloadDir)
      )

      message("Publications have been downloaded to `", downloadDir, "`.")
      if(any(!downloadDocument$success))
        warning("Not all publication ids successfully downloaded: ", paste(downloadDocument[success == FALSE, publication_id]))

      return(invisible(downloadDocument$destination))

    },

    #' @description
    #' Defunct. Use `getStudies`.
    getStudy      = function() .Defunct("getStudies"),

    #' @description
    #' Defunct. Use `getGroups`.
    getGroup      = function() .Defunct("getGroups"),

    #' @description
    #' Defunct. Use `getMabs`.
    getMab        = function() .Defunct("getMabs"),

    #' @description
    #' Defunct. Use `availableMabs`.
    filterMabGrid = function() .Defunct("availableMabs"),

    #' @description
    #' Defunct. Use `availableMabs`.
    resetMabGrid  = function() .Defunct("availableMabs"),

    #' @description
    #' Refresh the connection object to update available studies and groups.
    refresh = function() {
      private$.loadAll()
      private$.studyIds       <- private$.shared$.availableStudies$study_id
      private$.groupIds       <- private$.shared$.availableGroups$group_id
      private$.donorIds       <- private$.shared$.donorMetadata$donor_id
      private$.mabIds         <- private$.shared$.mabMetadata$mab_id
      private$.mabMixIds      <- private$.shared$.mabMix$mab_mix_id
      private$.publicationIds <- private$.shared$.availablePublications$publication_id
      NULL
    }
  ),
  active = list(

    #' @field config A list. Stores configuration of the connection object such as
    #' URL, path and username.
    config = function() {
      private$.shared$.config
    },

    #' @field availableStudies A data.tabl of available studies.
    availableStudies = function() {
      private$.shared$.availableStudies[study_id %in% private$.studyIds]
    },

    #' @field availableGroups A data.table of available groups.
    availableGroups = function() {
      private$.shared$.availableGroups[group_id %in% private$.groupIds]
    },

    #' @field availableMabs A data.table of available mAbs.
    availableMabs = function() {
      private$.shared$.availableMabs[
        mab_id %in% private$.mabIds
      ]
    },

    #' @field availableMabMixtures A data.table. Metadata of available
    #' mAb mixtures.
    availableMabMixtures = function(){
      private$.shared$.availableMabMixtures[
        mab_mix_id %in% private$.mabMixIds
      ]
    },

    #' @field availableDonors A data.table. Metadata about all mAb
    #' donors in the DataSpace.
    availableDonors = function(){
      private$.shared$.availableDonors[
        donor_id %in% private$.donorIds
      ]
    },

    #' @field availableViruses A data.table of metadata about all virsues
    #' in the DataSpace and virus name synonyms. 
    availableViruses = function(){
      private$.shared$.availableViruses
    },

    #' @field availablePublications A data.table of available
    #' publications metadata and available datasets.
    availablePublications = function() {
      private$.shared$.availablePublications[
        publication_id %in% private$.publicationIds,
        -c("url", "remote_path", "document_id")
      ]
    },

    #' @field virusNameMappingTables A list of data.tables containing virus name mappings.
    virusNameMappingTables = function() {
      private$.virusNameMappingTables
    },

    #' @field mabGridSummary Defunct. Use `availableMabs`.
    mabGridSummary = function() stop("Defunct. Use `availableMabs`."),

    #' @field mabGrid Defunct. Use `availableMabs`.
    mabGrid        = function() stop("Defunct. Use `availableMabs`."),

    #' @field virusMetadata Defunct. Use `virusNameMappingTables`.
    virusMetadata  = function() stop("Defunct. Use `virusNameMappingTables`.")

  ),
  private = list(
    .studyIds       = character(),
    .groupIds       = character(),
    .donorIds       = character(),
    .mabIds         = character(),
    .mabMixIds      = character(),
    .publicationIds = character(),
    .shared         = new.env(),

    .virusNameMappingTables = list(),

    .stats = data.table(),

    .loadAll = function() {

      private$.loadSharedMetadata()
      private$.loadAvailableStudies()
      private$.loadAvailableGroups()
      private$.loadAvailableDonors()
      private$.loadAvailableMabs()
      private$.loadAvailableMabMixtures()
      private$.loadAvailableViruses()
      private$.loadAvailablePublications()
      private$.loadStats()

    },
    .loadAvailableStudies = function() {

      study <- labkey.selectRows(
        baseUrl = private$.shared$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "CDS",
        queryName = "study",
        colSelect = c(
          "study_name", "short_name", "title", "type", "status",
          "stage", "species", "start_date", "strategy",
          "network"
        ),
        colNameOpt = "fieldname",
        method = "GET"
      ) |>
        setDT() |>
        setnames("study_name", "study_id")

      studyassay <- labkey.selectRows(
        baseUrl = private$.shared$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "CDS",
        queryName = "studyassay",
        colNameOpt = "fieldname",
        colSelect = c("prot", "assay_identifier", "assay_status"),
        method = "GET"
      ) |>
        setDT() |>
        setnames("prot", "study_id") |>
        _[
          assay_identifier %in% c("NAB", "NAB MAB", "PK MAB", "BAMA", "ICS", "IFNg ELS", "BCR sequencing") &
            assay_status == "Data added to DataSpace",
        ] |>
        _[,.(data_availability = paste(sort(assay_identifier), collapse = ", ")), by = .(study_id)]

      niData <- setDT(
        merge(
          labkey.selectRows(
            baseUrl = private$.shared$.config$labkeyUrlBase,
            folderPath = "/CAVD",
            schemaName = "CDS",
            queryName = "document",
            colNameOpt = "fieldname",
            colSelect = c("document_id", "label", "filename", "document_type", "assay_identifier")
          ),
          labkey.selectRows(
            baseUrl = private$.shared$.config$labkeyUrlBase,
            folderPath = "/CAVD",
            schemaName = "CDS",
            queryName = "studydocument",
            colNameOpt = "fieldname",
            colSelect = c("document_id", "prot")
          ),
          by = "document_id"
        )
      )[document_type == "Non-Integrated Assay"
        ][, .(ni_data_availability = paste(label, collapse = ", ")), by = "prot"] |>
        setnames("prot", "study_id")

      study <- study |>
        merge(studyassay, all.x = TRUE) |>
        merge(niData, all.x = TRUE)

      private$.shared$.availableStudies <- study
      class(private$.shared$.availableStudies) <- c(class(private$.shared$.availableStudies), "availableStudies")

    },
    .loadAvailableGroups = function() {

      response <- labkey.get(
        paste0(
          private$.shared$.config$labkeyUrlBase,
          "/participant-group",
          "/CAVD",
          "/browseParticipantGroups.api?",
          "distinctCatgories=false&",
          "type=participantGroup&",
          "includeUnassigned=false&",
          "includeParticipantIds=false"
        )
      )

      parsed <- jsonlite::fromJSON(response, simplifyDataFrame = FALSE)

      private$.shared$.availableGroups <- lapply(parsed$groups, function(group) {
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
          studies = paste((unique(substr(group$category$participantIds, 1, 6))), collapse = ", ")
        )
      }) |>
        rbindlist() |>
        setorder(group_id) |>
        setkey(group_id)

      class(private$.shared$.availableGroups) <- c(class(private$.shared$.availableGroups), "availableGroups")

    },
    .loadAvailableDonors = function() {

      donors <-
        merge(
          private$.shared$.donorMetadata,
          private$.shared$.donorMabSequence,
          by = "donor_id",
          all.x = TRUE
        ) |>
        merge(
          private$.shared$.mabMetadata,
          by = "mab_id",
          all.x = TRUE
        )

      donors[, `:=`(
        mab_count = length(na.omit(unique(mab_id))),
        sequence_count = length(na.omit(unique(sequence_id)))
      ), donor_id]

      donors[, lineage_sequences_available := ifelse(
        donor_id %in% private$.shared$.donorMabSequence[is.na(mab_id), .N, donor_id]$donor_id,
        TRUE,
        FALSE
      )]

      private$.shared$.availableDonors <-
        donors[,c(names(private$.shared$.donorMetadata), "lineage_sequences_available", grep("_count", names(donors), value = TRUE)), with = FALSE] |>
        unique()

      class(private$.shared$.availableDonors) <- c(class(private$.shared$.availableDonors), "availableDonors")

    },
    .loadAvailableMabs = function() {

      concatField <- function(field)
        ifelse(all(is.na(field)), NA, paste(na.omit(sort(unique(unlist(strsplit(field, ", "))))), collapse = ", ")) |>
          as.character()

      private$.shared$.availableMabs <-
        private$.shared$.mabMetadata |>
        merge(
          private$.shared$.mabMix,
          by = "mab_id",
          all.x = TRUE
        ) |>
        merge(
          private$.shared$.mabStudies,
          all.x = TRUE,
          by = "mab_mix_id"
        ) |>
        merge(
          private$.shared$.donorMabSequence,
          by = "mab_id",
          all.x = TRUE
        ) |>
        _[, sequences_available := !is.na(sequence_id)] |>
        _[,-c("sequence_id", "mab_mix_id")] |>
        unique() |>
        _[, `:=`(
          studies_available = concatField(studies_available),
          assays_available  = concatField(assays_available)
        ), mab_id] |>
        unique()

      class(private$.shared$.availableMabs) <- c(class(private$.shared$.availableMabs), "availableMabs")
    
    },
    .loadAvailableMabMixtures = function() {

      private$.shared$.availableMabMixtures <- private$.shared$.mabMixMetadata[
       ,
         .(
           mab_mix_name_std = mab_mix_label,
           mab_mix_name_other = paste0(
             mab_mix_name_std[mab_mix_name_std != mab_mix_label],
             na.omit(mab_mix_name_other),
             collapse = ", "
           ),
           mab_mix_type
         ), mab_mix_id] |>
        merge(
          private$.shared$.mabNeutralization[
           ,
             .(
               virus_count = length(virus),
               titer_50_geomean = 10^(mean(log10(ifelse(titer_curve_ic50 %in% c(Inf, -Inf), -1, titer_curve_ic50)), na.rm = T)) |> suppressWarnings()
             ),
             mab_mix_id
          ],
          by = "mab_mix_id",
          all.x = TRUE
        ) |>
          unique()

      class(private$.shared$.availableMabMixtures) <- c(class(private$.shared$.availableMabMixtures), "availableMabMixtures")

    },
    .loadAvailableViruses = function() {

      virusMetadata <- suppressWarnings(
        labkey.selectRows(
          baseUrl = private$.shared$.config$labkeyUrlBase,
          folderPath = "/CAVD/",
          schemaName = "CDS",
          queryName = "nabAntigenWithPanelMeta",
          colNameOpt = "fieldname",
          method = "GET"
        )
      ) |> # Rlabkey version 3.4.1 returns warnings for GROUP_CONCAT field in sql expressions
        setDT() |>
        setcolorder(
          c(
            "assay_identifier", "cds_virus_id", "virus", "virus_type", "neutralization_tier", "clade",
            "antigen_control", "virus_full_name", "virus_name_other", "virus_species",
            "virus_host_cell", "virus_backbone", "panel_names"
          )
        ) |>
        setkey(cds_virus_id)

      virusMetadataAll <- labkey.selectRows(
        baseUrl = private$.shared$.config$labkeyUrlBase,
        folderPath = "/CAVD/",
        schemaName = "CDS",
        queryName = "virus_metadata_all",
        colNameOpt = "fieldname",
        colSelect = c("cds_virus_id", "virus", "virus_full_name",
                      "virus_backbone", "virus_host_cell", "virus_plot_label",
                      "virus_type", "virus_species", "clade",
                      "neutralization_tier"),
        method = "GET"
      ) |>
        setDT() |>
        setkey(cds_virus_id)

      virusLabId <- labkey.selectRows(
        baseUrl = private$.shared$.config$labkeyUrlBase,
        folderPath = "/CAVD/",
        schemaName = "CDS",
        queryName = "virus_lab_id",
        colNameOpt = "fieldname",
        colSelect = c("cds_virus_id", "lab_code", "lab_virus_id",
                      "lab_virus_id_variable_name", "harvest_date"),
        method = "GET"
      ) |>
        setDT() |>
        setkey(cds_virus_id)

      virusSynonym <- labkey.selectRows(
        baseUrl = private$.shared$.config$labkeyUrlBase,
        folderPath = "/CAVD/",
        schemaName = "CDS",
        queryName = "virus_synonym",
        colNameOpt = "fieldname",
        colSelect = c("cds_virus_id", "virus_synonym"),
        method = "GET"
      ) |>
        setDT() |>
        setkey(cds_virus_id)

      private$.virusNameMappingTables <- list(
        virus_metadata_all = virusMetadataAll,
        virus_lab_id = virusLabId,
        virus_synonym = virusSynonym
      )

      private$.shared$.availableViruses <- merge(
        virusMetadataAll,
        virusSynonym[
          !virus_synonym %in% virusMetadataAll$virus &
            !virus_synonym %in% virusMetadataAll$virus_full_name,
          .(virus_name_other=paste(virus_synonym, collapse = ", ")),
          by = cds_virus_id
        ],
        all.x = TRUE
      )

    },
    .loadAvailablePublications = function() {

      pubs <- suppressWarnings(
        labkey.executeSql(
          baseUrl = private$.shared$.config$labkeyUrlBase,
          folderPath = "/CAVD/",
          schemaName = "CDS",
          sql = publicationSqlCall(),
          colNameOpt = "fieldname"
        )
      ) |> # Rlabkey version 3.4.1 returns warnings for GROUP_CONCAT field in sql expressions
        setDT() |>
        setorder("first_author")

      pubs[,url := paste0(
        private$.shared$.config$labkeyUrlBase,
        "/cds/CAVD/getStudyDocument.view?",
        "&documentId=", document_id,
        "&filename=", gsub("/", "%2F", remote_path),
        "&publicAccess=true"
      )]

      class(pubs) <- c(class(pubs), "availablePublications")
      private$.shared$.availablePublications <- pubs

    },
    .loadSharedMetadata = function() {

      private$.shared$.mabMetadata <-
        labkey.selectRows(
          baseUrl = private$.shared$.config$labkeyUrlBase,
          folderPath = "/CAVD",
          schemaName = "CDS",
          queryName = "mab_metadata",
          colNameOpt = "fieldname",
          method = "GET"
        ) |>
        setDT() |>
        setkey("mab_id") |>
        removeContainerId()

      private$.shared$.mabNeutralization <-
        labkey.selectRows(
          baseUrl = private$.shared$.config$labkeyUrlBase,
          folderPath = "/CAVD",
          schemaName = "CDS",
          queryName = "mAbGridBase",
          colNameOpt = "fieldname",
          colSelect = "mab_mix_id,virus,clade,neutralization_tier,titer_curve_ic50",
          method = "GET"
        ) |>
        setDT() |>
        setkey("mab_mix_id")

      private$.shared$.donorMetadata <-
        labkey.selectRows(
          baseUrl = private$.shared$.config$labkeyUrlBase,
          folderPath = "/CAVD",
          schemaName = "CDS",
          queryName = "donor_metadata",
          colNameOpt = "fieldname",
          method = "GET"
        ) |>
        setDT() |>
        setkey("donor_id") |>
        removeContainerId()

      private$.shared$.donorMabSequence <- labkey.selectRows(
        baseUrl = private$.shared$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "CDS",
        queryName = "donor_mab_sequence",
        colNameOpt = "fieldname",
        method = "GET"
      ) |>
        setDT() |>
        setkey("donor_id", "mab_id", "sequence_id") |>
        removeContainerId()

      private$.shared$.mabMix <- labkey.selectRows(
        baseUrl = private$.shared$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "CDS",
        queryName = "MAbMix",
        colNameOpt = "fieldname",
        method = "GET"
      ) |>
        setDT() |>
        setkey("mab_id", "mab_mix_id") |>
        removeContainerId()

      private$.shared$.mabMixMetadata <- labkey.selectRows(
        baseUrl = private$.shared$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "CDS",
        queryName = "MAbMixMetadata",
        colNameOpt = "fieldname",
        method = "GET"
      ) |>
        setDT() |>
        setkey("mab_mix_id") |>
        removeContainerId()

      private$.shared$.mabStudies <- labkey.selectRows(
        baseUrl = private$.shared$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "study",
        queryName = "availableMabStudies",
        colNameOpt = "fieldname",
        method = "GET"
      ) |>
        setDT() |>
        setkey("mab_mix_id") |>
        removeContainerId() |>
        _[, .(
          assays_available = paste(sort(unique(assay_id)), collapse = ", "),
          studies_available = paste(sort(unique(prot)), collapse = ", ")
        ), .(mab_mix_id)]

    },
    .loadStats = function() {

      private$.stats <- labkey.selectRows(
        baseUrl = private$.shared$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "CDS",
        queryName = "ds_properties",
        colNameOpt = "fieldname",
        method = "GET"
      ) |>
        setDT()
      
    }
  )
)
