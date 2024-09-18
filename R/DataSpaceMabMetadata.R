#' The DataSpaceMabMetadata class
#'
#' @section Constructor:
#' \code{DataSpaceConnection$getMabMetadata()}
#'
#' @seealso \code{\link{connectDS}} \code{\link{DataSpaceConnection}}
#'
#' @examples
#' \dontrun{
#' # Create a connection (Initiate a DataSpaceConnection object)
#' con <- connectDS()
#'
#' # Browse the mAb Grid
#' con$mabMetadata
#'
#' # Filter the grid by viruses
#' mabMeta <- con$getMabMetadata(mabIds = "cds_mab_32")
#'
#' # Or get all the mab metadata
#' mabMeta <- con$getMabMetadata()
#'
#' # Load all alignments
#' mabMeta$loadDaash()
#'
#' # Or just a subset of the alignments for the metadata extracted so far
#' mabMeta$loadDaash(mabIds = "cds_mab_32")
#' 
#' # Inspect the `topCalls` field
#' mabMeta$topCalls
#'
#' }
#'
#' @importFrom data.table setnames
DataSpaceMabMetadata <- R6Class(
  classname = "DataSpaceMabMetadata",
  public = list(
    #' @description
    #' Initialize \code{DataSpaceMabMetadata} object.
    #' See \code{\link{DataSpaceConnection}}.
    #' @param mabIds A character vector of cds mab ids.
    #' @param config A list.
    initialize = function(mabIds, config) {
      assert_that(!is.null(config))

      checkMabIdFormat(mabIds)

      ## set primary fields
      private$.mabIds <- mabIds
      if(length(mabIds) > 0) {
        private$.mabFilter <- makeFilter(
          c("mab_id", "IN", paste(mabIds, collapse = ";"))
        )
      } else {
        private$.mabFilter <- makeFilter(
          c("mab_id", "NOT_EQUAL", "NULL")
        )
      }
      private$.config <- config

      ## get extra fields if available
      self$refresh()

      NULL
    },

    #' @description
    #' Print the \code{DataSpaceMab} object summary.
    print = function() {
      cat("<DataSpaceMabMetadata>")
      cat("\n  URL:", private$.config$labkeyUrlBase)
      cat("\n  User:", private$.config$labkeyUserEmail)
      cat("\n  Summary:")
      cat("\n    -", length(unique(private$.mabMetadata$mab_name_std)), "mAbs")
      cat("\n  MAbs in metadata: ")
      cat("\n    ", truncatePrintable(paste(private$.mabNames, collapse = ", ")))
      cat("\n  MAb sequences available:")
      cat("\n    ",
          truncatePrintable(
            paste( private$.mabMetadata[sequence_available==TRUE, mab_name_std], collapse=", ")
          ))
      cat("\n  MAb sequences loaded:")
      cat("\n    ",
      {
        if(length(private$.daash) == 0){
          "No mAbs sequences loaded. Please run `loadDaash()` to load sequences and alignments."
        } else {
          truncatePrintable(
            paste(
              private$.mabMetadata[mab_id %in% private$.daash$sequences$mab_id, mab_name_std],
              collapse=", "
            )
          )
        }
      })

      cat("\n")

    },

    #' @description
    #' Refresh the \code{DataSpaceMabMetadata} object to update datasets.
    refresh = function() {
      private$.getMabMetadata()
      private$.getVariableDefinitions()
    },

  #' @description
  #' Applies LANL metadata to mabMetadata table.
  loadLanlMetadata = function(){
    private$.mabMetadata[, lanl_metadata := lapply(mab_lanl_id, fetchLanlMetadata)]
  },

  #' @description
  #' Return a fasta file for available daash sequences that have been loaded.
  #' @param seqIds character CDS Sequence IDs, found in the `sequence_id` column of the donorMetadata table in a DataSpaceDonorMetadata object. These are in the format "cds_seq_###".
  #' @param sequenceType character What type of fasta file to return: nt = nucleotide, aa = amino acid.
  #' @param originalHeaders boolean T/F if the original fasta headers should be provided, or 
  getFastaFromSequences = function(seqIds=NULL, sequenceType="nt", originalHeaders=FALSE){
    sequences <- copy(private$.daash$sequences)
    sequences[, cds_header:=paste(sequence_id, mab_id, donor_id, mab_name_std, donor_code, collapse="|")]
    generateFasta(sequences, seqIds, sequenceType, originalHeaders)
  },

  #' @description
  #' Applies DAASH tables to the this object
  #' @param mabIds character CDS MAb IDs, found in the `mab_id` column of the mabMetadata table in a DataSpaceMabMetadata object. These are in the format "cds_mab_###".
  #' @param filter character An additional filter to add to the query for getting daash results. This any valid filter for the view `donor_mab_sequence_header_source`, a view found on the DataSpace LabKey server.
  loadDaash = function(mabIds=c(), filter=NULL){
    if(length(mabIds) != 0){

      checkMabIdFormat(mabIds)
      
      if(all(!(mabIds %in% private$.mabSequence$mab_id)))
        stop("None of the `mabIds` elements provided are found in this object.")
      if(all(private$.mabSequence[mab_id %in% mabIds, is.na(sequence_id)]))
        stop("No sequences are available for the `mabIds` provided")
      
      if(any(private$.mabSequence[mab_id %in% mabIds, is.na(sequence_id)]))
        message("Note: Some sequences are not available for the `mabIds` provided")
      if(!all(mabIds %in% private$.mabSequence$mab_id))
        message("Note: At least one element of `mabIds` is not available in this object.")

      if(length(private$.mabIds) != 0){
        private$.daashMabIds <- private$.mabIds[private$.mabIds %in% mabIds]
      } else {
        private$.daashMabIds <- mabIds
      }
      
    } else {
      if(length(private$.mabIds) != 0){
        private$.daashMabIds <- private$.mabIds
      }
      
    }

    if(length(private$.daashMabIds) == 0){
      private$.mabDaashFilter <- NULL
    } else {
      private$.mabDaashFilter <- makeFilter(c("mab_id", "IN", paste(private$.daashMabIds, collapse=";")))
    }
    
    daashFilter <- rbind(private$.mabFilter, private$.mabDaashFilter, filter) |> unique()
    private$.daash <- fetchDaash(daashFilter, private$.config)
   
  }
  
  ),

  active = list(
    #' @field config A list. Stores configuration of the connection object such
    #' as URL, path and username.
    config = function() {
      private$.config
    },

    #' @field mabMetadata A data.table. The table of the basic metadata from a mab metadata filter.
    mabMetadata = function(){
      private$.mabMetadata
    },

    #' @field mabMix A data.table. The table of mAb mixture IDs and mAb IDs for merging data reported as mAb mixures and mAb metadata.
    mabMix = function(){
      private$.mabMix
    },

    #' @field topCalls A data.table. A table of top allele matches from both IgBLAST and V-Quest.  
    topCalls = function() {
      if(length(private$.daash) != 0){
        return(private$.daash$topCalls)
      }
      message("Please run `loadDaash()` to access top matches.");
    },

    #' @field alignments A data.table. The table of alignments of germline genes to a given sequence.
    #' attributes.
    alignments = function() {
      if(length(private$.daash) != 0){
        return(private$.daash$alignments)
      }
      message("Please run `loadDaash()` to access alignments.")
    },

    #' @field sequences A data.table. The table of information concerning the runs from the two alignment tools.
    #' measurements against viruses.
    sequences = function() {
      if(length(private$.daash) != 0){
        return(private$.daash$sequences)
      }
      message("Please run `loadDaash()` to access sequences.")
    },
    
    #' @field alleleSequences A data.table. The table of allele sequences from reported allele matches.
    #' measurements against viruses.
    alleleSequences = function() {
      if(length(private$.daash) != 0){
        return(private$.daash$alleleSequences)
      }
      message("Please run `loadDaash()` to access allele sequences.")
    },

    #' @field runInformation A data.table. The table of information concerning the runs from the two alignment tools.
    #' measurements against viruses.
    runInformation = function() {
      if(length(private$.daash) != 0){
        return(private$.daash$runInformation)
      }
      message("Please run `loadDaash()` to access run info.")
    },
    
    #' @field variableDefinitions A data.table. The table of variable definitions for the metadata.
    #' definitions.
    variableDefinitions = function() {
      private$.variableDefinitions
    }
  ),

  private = list(
    .config = list(),
    .mabNames = character(),
    .mabIds = character(),
    .daashMabIds = character(),
    .mabDaashFilter = character(),
    .mabMetadata = data.table(),
    .mabSequence = data.table(),
    .mabMix = data.table(),
    .mabFilter = character(),
    .daash = list(),
    .variableDefinitions = data.table(),
    
    .getMabMetadata = function() {

      mabMetadata <- labkey.selectRows(
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "CDS",
        queryName = "mab_metadata",
        colSelect = "mab_id,mab_name_std,mab_lanl_id,mab_hxb2_location,mab_ab_binding_type,mab_isotype",
        colNameOpt = "fieldname",
        colFilter = private$.mabFilter,
        method = "GET"
      ) |>
        suppressWarnings() |>
        setDT()

      if(nrow(mabMetadata) == 0){
        stop("None of the `mabIds` arguments provided are found in the database.")
      }
      
      donorMabSequence <- labkey.selectRows(
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "CDS",
        queryName = "donor_mab_sequence",
        colSelect = "donor_id,mab_id,sequence_id",
        colNameOpt = "fieldname",
        colFilter = private$.mabFilter, 
        method = "GET"
      ) |>
        suppressWarnings() |> 
        setDT()    

      mabMix <- labkey.selectRows(
        baseUrl = private$.config$labkeyUrlBase, 
        folderPath = "/CAVD", 
        schemaName = "CDS", 
        queryName = "MAbMix", 
        viewName = "", 
        colSelect = "mab_mix_id,mab_id", 
        colNameOpt = "fieldname",
        colFilter = private$.mabFilter,
        method = "GET"
      ) |> setDT()
      
      mabMetaSequence <- merge(
        mabMetadata,
        donorMabSequence,
        by = "mab_id",
        all.x = TRUE
      ) |> (\(.){
        .[!is.na(sequence_id), sequence_available:=TRUE ]
        .[ is.na(sequence_id), sequence_available:=FALSE]
        .[,`:=`(sequence_id = NULL)]
        unique(.)
      })()

      checkDaash <- c(
        "mabMetaSequence",
        "mabMix",
        "donorMabSequence"
      )
      for(ck in checkDaash)
        if(nrow(get(ck)) == 0)
          stop(
            sprintf(
              "Something went wrong. Contact `dataspace.support@scharp.org` for assistance: `%s` is empty",
              ck
            )
          )
      
      private$.mabMetadata <- mabMetaSequence
      private$.mabSequence <- donorMabSequence[,.(mab_id, sequence_id)] |> unique()
      private$.mabMix   <- mabMix
      private$.mabNames <- unique(mabMetadata$mab_name_std)
      
    },
    .getVariableDefinitions = function() {

      varInfo <- lapply(
        list(
          c("mabMetadata"    , "mabMetadata"                     , "mab_id,sequence_id,mab_name_std,mab_lanlid,mab_hxb2_location,mab_ab_binding_type,mab_isotype,mab_donorid,mab_donor_species,mab_donor_clade"),
          c("topCalls"       , "sequence_germline"               , "mab_id,allele,sequence_id,percent_identity,matches,alignment_length,score,run_application"),
          c("alignments"     , "alignment"                       , ""),
          c("sequences"      , "donor_mab_sequence_header_source", ""),
          c("alleleSequences", "allele_sequence"                 , "allele,allele_sequence_nt"),
          c("runInformation" , "alignment_run"                   , "run_application,run_information")
        ), \(.) {
          labkey.getQueryDetails(
            baseUrl = private$.config$labkeyUrlBase,
            folderPath = "/CAVD",
            schemaName = "CDS",
            queryName = .[2]
          ) |>
            setDT() |>
            ( \(tab) {
              if(nchar(.[3]) != 0)
                tab[fieldName %in% ( strsplit(.[3], ",") |> unlist() ), ]
              tab <- tab[fieldName != "container",]
              return(tab[,queryName:=.[1]])
            }
            )()        
        }) |>
        rbindlist() |>
        setnames(c("queryName", "fieldName"), c("name", "field_name"))
      private$.variableDefinitions <- varInfo[, .(name, field_name, caption, description)]
      
    })
)
