#' @title The DataSpaceStudies class
#'
#' @description
#' An R6 class for DataSpace Study data.
#'
#' @section Constructor:
#' \code{DataSpaceConnection$getStudies()}
#' 
#' @seealso \code{\link{connectDS}} \code{\link{DataSpaceConnection}}
#'
#' @examples
#' \dontrun{
#' # Create a connection (Initiate a DataSpaceConnection object)
#' con <- connectDS()
#'
#' # Get group by `study_id` or pass a filtered `availableStudies` object.
#' studies <- con$getStudies(c("vtn505", "cvd408"))
#' studies <- con$getStudies(
#'   con$availableStudies[grepl("BAMA", data_availability) & species == "Human"]
#' )
#' 
#' # Load BAMA to the studies object.
#' studies$loadAssayDatasets("BAMA")
#' studies$datasets$BAMA
#' 
#' # Inspect variable information of the BAMA dataset
#' studies$datasetDescriptions$BAMA
#'
#' # Inspect treatment arm information for all studies in study object
#' studies$treatmentArm
#'
#' }
#'
DataSpaceStudies <- R6Class(
  classname = "DataSpaceStudies",
  inherit = DataSpaceConnection,
  public = list(

    #' @description
    #' Initialize \code{DataSpaceStudy} class.
    #' See \code{\link{DataSpaceConnection}}.
    #' @param studyIds A character. Name of the study to retrieve.
    #' as URL, path and username.
    initialize = function(studyIds) {

      studyIds <- studyIds[studyIds != ""]
      private$.studyIds <- checkStudies(studyIds, private$.shared$.config$labkeyUrlBase)
      private$.publicationIds <- private$.shared$.availablePublications[
        sapply(strsplit(related_studies, ", "), \(.) any(. %in% private$.studyIds)) &
          publication_data_available == TRUE,
        publication_id
      ]

      private$.mabMixIds <- private$.shared$.mabStudies[
        strsplit(studies_available, ", ") |> sapply(\(sa) any(sa %in% private$.studyIds)),
        mab_mix_id
      ] |>
        unique()

      private$.mabIds <- private$.shared$.mabMix[mab_mix_id %in% private$.mabMixIds, mab_id] |>
        unique()

      private$.shared$.config$labkeyStudyUrlPaths <- getStudyUrlPaths(private$.studyIds)

      self$refresh()

      NULL
    },

    #' @description
    #' Print \code{DataSpaceStudy} class.
    print = function() {

      cat("<DataSpaceStudies>")
      cat("\n  Studies:", paste(private$.studyIds, collapse = ", "))
      cat("\n  Available integrated datasets:")
      if (nrow(private$.availableIntegratedDatasets) > 0) {
        cat(paste("\n    -", unique(private$.availableIntegratedDatasets$assay_label)), sep = "")
      }
      cat("\n  Available non-integrated datasets:")
      if (nrow(private$.availableNIDatasets) > 0) {
        cat(paste("\n    -", unique(private$.availableNIDatasets$assay_label)), sep = "")
      }
      cat("\n  Available publication datasets:")
      if (length(private$.publicationIds) > 0) {
        cat(paste("\n    -", unique(private$.shared$.availablePublications[publication_id %in% private$.publicationIds, label])), sep = "")
      }
      cat("\n  Available Studies objects:")
      cat(paste0("\n    - ", DataSpaceStudies$active |> names() |> cleanReservedDataSpaceR6()), sep = "")
      cat("\n  Available Studies methods:")
      cat(paste0("\n    - ", DataSpaceStudies$public_methods |> names() |> cleanReservedDataSpaceR6()), sep = "")
      cat("\n  Available Connection objects:")
      cat(paste0("\n    - ", DataSpaceConnection$active |> names() |> cleanReservedDataSpaceR6()), sep = "")
      cat("\n  Available Connection methods:")
      cat(paste0("\n    - ", DataSpaceConnection$public_methods |> names() |> cleanReservedDataSpaceR6()), sep = "")
      cat("\n")
    },

    #' @description
    #' Load datasets to the studies object from an availableDatasets object.
    #' @param availableDatasets An `availableDatasets` object or vector of `study_id` values.
    #' @param downloadDir Optional, a character path specifying a directory to download.
    #' nonstandard datasets. The default is the working temp directory.
    loadAvailableDatasets = function(availableDatasets = self$availableDatasets, downloadDir = tempdir()) {

      if(is.character(availableDatasets)){
        availableDatasets = self$availableDatasets[assay_identifier %in% availableDatasets]
        if(nrow(availableDatasets) == 0)
          stop("Charactor vector of assay identifiers passed did not return any available datasets from `availableDatasets`.")
      }
      
      if(any(availableDatasets$dataset_type == "Publication Data")){
        warning("Publication data in `availalableDatasets` ignored. Use `getPublicationDatasets` to download those.")
      }

      availableDatasets <- availableDatasets[dataset_type %in% c("Integrated Assay", "Non-Integrated Assay")]

      if(nrow(availableDatasets) == 0) stop("No integrated or non-integrated assays to get.")

      if(any(availableDatasets$dataset_type == "Integrated Assay")){

        assays <- availableDatasets[dataset_type == "Integrated Assay"]$assay_identifier |>
          unique()

        filter <- makeFilter(c("study_prot", "IN", paste(unique(availableDatasets$study_id), collapse = ";")))
        assayData <- assays |>
          lapply(\(assay)
                 labkey.selectRows(
                   baseUrl = private$.shared$.config$labkeyUrlBase,
                   folderPath = "CAVD",
                   schemaName = "study",
                   queryName = assay,
                   viewName = NULL,
                   colFilter = filter,
                   colNameOpt = "fieldname",
                   method = "GET",
                   ) |> setDT()
                 )

        names(assayData) <- assays
      }

      if(any(availableDatasets$dataset_type == "Non-Integrated Assay")){

        niData <-  
          availableDatasets[
            private$.availableDocuments[
              assay_identifier %in% availableDatasets$assay_identifier &
                document_type == "Non-Integrated Assay"
            ] |>
              downloadDocuments(downloadDir) |>
              unzipDocuments() |>
              loadDocuments(),
            on = "assay_identifier"
          ]

        appendData        <- niData$datasets
        names(appendData) <- niData$assay_identifier

        assayData <- append(assayData, appendData)

        availableDatasets[niData[,.(assay_identifier, unzipDir)], on = "assay_identifier", unzipDir := i.unzipDir]

      } else {
        availableDatasets[,unzipDir := NA]
      }

      private$.datasets <- assayData

      if("BCR_Sequence" %in% assays){
        bcr_sequence <- labkey.selectRows(
          baseUrl = private$.shared$.config$labkeyUrlBase,
          folderPath="CAVD",
          schemaName="CDS",
          queryName="bcr_sequence_alignment_prot",
          colFilter=makeFilter(c("prot", "IN", paste(private$.studyIds, collapse = ";"))),
          colNameOpt="rname"
        ) |> setDT()

        private$.datasets <- append(
          private$.datasets,
          list(
            alignments = bcr_sequence[,.(alignment_id,sequence_id,stop_codon,vj_in_frame,productive,rev_comp,complete_vdj,v_call,d_call,j_call,sequence_alignment,germline_alignment,sequence_alignment_aa,germline_alignment_aa,v_alignment_start,v_alignment_end,d_alignment_start,d_alignment_end,j_alignment_start,j_alignment_end,v_sequence_alignment,v_sequence_alignment_aa,v_germline_alignment,v_germline_alignment_aa,d_sequence_alignment,d_sequence_alignment_aa,d_germline_alignment,d_germline_alignment_aa,j_sequence_alignment,j_sequence_alignment_aa,j_germline_alignment,j_germline_alignment_aa,fwr1,fwr1_aa,cdr1,cdr1_aa,fwr2,fwr2_aa,cdr2,cdr2_aa,fwr3,fwr3_aa,fwr4,fwr4_aa,cdr3,cdr3_aa,junction,junction_length,junction_aa,junction_aa_length,v_score,d_score,j_score,v_cigar,d_cigar,j_cigar,v_support,d_support,j_support,v_identity,d_identity,j_identity,v_sequence_start,v_sequence_end,v_germline_start,v_germline_end,d_sequence_start,d_sequence_end,d_germline_start,d_germline_end,j_sequence_start,j_sequence_end,j_germline_start,j_germline_end,fwr1_start,fwr1_end,cdr1_start,cdr1_end,fwr2_start,fwr2_end,cdr2_start,cdr2_end,fwr3_start,fwr3_end,fwr4_start,fwr4_end,cdr3_start,cdr3_end,np1,np1_length,np2,np2_length,run_application,d_frame,c_call,c_alignment_start,c_alignment_end,c_sequence_alignment,c_sequence_alignment_aa,c_germline_alignment,c_germline_alignment_aa,c_score,c_cigar,c_support,c_identity,c_sequence_start,c_sequence_end,c_germline_start,c_germline_end)],
            sequences = bcr_sequence[,.(sequence_id,sequence_nt,sequence_aa,chain)],
            runInformation = labkey.selectRows(
              baseUrl = private$.shared$.config$labkeyUrlBase,
              folderPath="CAVD",
              schemaName="CDS",
              queryName="alignment_run",
              colSelect="run_application,run_information",
              colFilter=makeFilter(c("run_application", "IN", paste(unique(bcr_sequence$run_application), collapse = ";"))),
              colNameOpt="rname"
            ) |> setDT()
          )
        )
      }

      setDatasetNames(private$.datasets)

      varInfo <- availableDatasets[, .N, .(assay_identifier, dataset_type, unzipDir)][
       ,.(
         varInfo = ifelse(
           dataset_type == "Integrated Assay",
           list(
             getVarInfo(assay_identifier, names(private$.datasets[[assay_identifier]]), private$.shared$.config$labkeyUrlBase)
           ),
           list(sprintf("Non-integrated data variable info found at %s", unzipDir))
         )
       ), assay_identifier
      ]

      private$.variableDefinitions <- varInfo$varInfo
      names(private$.variableDefinitions) <- varInfo$assay_identifier

      if("BCR_Sequence" %in% assays){
        private$.variableDefinitions <- append(
          private$.variableDefinitions, list(
            sequence = getVarInfo("sequence", names(private$.datasets$sequence), private$.shared$.config$labkeyUrlBase, "CDS"),
            alignment = getVarInfo("alignment", names(private$.datasets$alignment), private$.shared$.config$labkeyUrlBase, "CDS"),
            runInformation = getVarInfo("alignment_run", names(private$.datasets$runInformation), private$.shared$.config$labkeyUrlBase, "CDS")
          )
        )
      }

    },

    #' @description
    #' Refresh the study object to update available datasets and treatment info.
    refresh = function() {
      private$.fetchAvailableDatasets()
      NULL
    }
  ),
  
  active = list(

    #' @field studies A character vector of `study_id` values found in the object.
    studies = function() {
      private$.studyIds
    },

    #' @field availableDatasets A table of datasets available in
    #' the \code{DataSpaceStudies} object.
    availableDatasets = function() {
      datasets <- rbind(
        private$.availableIntegratedDatasets,
        private$.availableNIDatasets
      )
      class(datasets) <- c("availableDatasets", class(datasets))
      return(datasets)
    },
    
    #' @field datasets A list of data.table objects containing the
    #' availableDatasets that were loaded.
    datasets = function() {
      if(all(private$.shared$.availableStudies[study_id %in% private$.studyIds, is.na(data_availability)]))
        warning("No study data is associated with this object")
      else if(length(private$.datasets) == 0)
        warning("No datasets loaded. Please run the `loadAvailableDatasets` method")
      else private$.datasets
    },

    #' @field variableDefinitions A list of data.table objects containing the
    #' data dictionaries of the integrated data loaded.
    variableDefinitions = function() {
      private$.variableDefinitions
    },
    
    #' @field treatmentArm A data.table. The table of treatment arm
    #' information for the connected study. Not available for all study
    #' connection.
    treatmentArm = function() {
      private$.treatmentArm
    },

    #' @field studyInfo A list. Stores the information about the study.
    studyInfo = function() {
      private$.shared$.availableStudies[study_id %in% private$.studyIds]
    }

  ),

  private = list(
    .studyIds = character(),
    .publicationIds = character(),
    .mabMixIds = character(),
    .mabIds = character(),
    .availableIntegratedDatasets = data.table(),
    .availableDocuments = data.table(),
    .availableNIDatasets = data.table(),
    .datasets = list(),
    .variableDefinitions = list(),
    .dataDir = character(),
    .treatmentArm = data.table(),
    .fetchAvailableDatasets = function(){
      private$.fetchAvailableIntegratedDatasets()
      private$.fetchAvailableDocuments()
      private$.fetchTreatmentArm()
      private$.fetchAvailableNIDatasets()
    },

    .fetchAvailableIntegratedDatasets = function() {

      private$.availableIntegratedDatasets <-
        Map(\(path, study_id) {
          labkey.executeSql(
            baseUrl = private$.shared$.config$labkeyUrlBase,
            folderPath = path,
            schemaName = "study",
            sql = datasetCountQuery(),
            colNameOpt = "fieldname"
          ) |>
            suppressWarnings() |>
            setDT() |>
            _[,`:=`(study_id = study_id, dataset_type = "Integrated Assay")] |> 
            _[,.(study_id, dataset_type, assay_identifier, assay_label)]
        },
        private$.shared$.config$labkeyStudyUrlPaths,
        names(private$.shared$.config$labkeyStudyUrlPaths)) |>
        rbindlist() |>
        _[order(study_id, assay_identifier)]

    },
    .fetchAvailableNIDatasets = function(){
      private$.availableNIDatasets <-
        private$.availableDocuments[
          document_type == "Non-Integrated Assay",
          .(study_id, dataset_type = document_type, assay_identifier, assay_label = document_label)
        ]
    },
    .fetchAvailableDocuments = function() {

      colSelect <- c("document_id", "label", "filename", "document_type", "assay_identifier")
      private$.availableDocuments <- merge(
        labkey.selectRows(
          private$.shared$.config$labkeyUrlBase,
          folderPath = "CAVD",
          schemaName = "CDS",
          queryName  = "document",
          colNameOpt = "fieldname",
          colSelect  = colSelect
        ) |>
          setDT(),
        labkey.selectRows(
          private$.shared$.config$labkeyUrlBase,
          folderPath = "CAVD",
          schemaName = "CDS",
          queryName = "studydocument",
          colNameOpt = "fieldname",
          colSelect = c("document_id", "prot")
        ) |>
          setDT(),
        by = "document_id"
      ) |>
        setcolorder(c("prot", colSelect)) |>
        setnames(c("prot", "filename", "label"), c("study_id", "remote_path", "document_label")) |>
        _[study_id %in% private$.studyIds] |>
        _[, url := URLencode(
          paste0(
            private$.shared$.config$labkeyUrlBase,
            "/cds/CAVD/getStudyDocument.view?",
            "study=", study_id,
            "&documentId=", document_id,
            "&filename=", remote_path
          )
        )]

    },
    .fetchTreatmentArm = function() {

      colSelect <- c(
        "arm_id", "arm_part", "arm_group", "arm_name",
        "randomization", "coded_label", "last_day", "description"
      )

      private$.treatmentArm <-
        labkey.selectRows(
          baseUrl = private$.shared$.config$labkeyUrlBase,
          folderPath = "CAVD",
          schemaName = "CDS",
          queryName = "treatmentarm",
          colSelect = colSelect,
          colNameOpt = "fieldname",
          method = "GET"
        ) |>
        suppressWarnings() |>
        setDT() |>
        setkey(arm_id) |>
        _[,study_id := gsub("^([a-z]{3}[0-9]+)-.+", "\\1", arm_id, )] |>
        setcolorder(c("study_id", colSelect)) |>
        _[study_id %in% private$.studyIds]

    }

  )
)
