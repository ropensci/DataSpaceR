#' The DataSpaceDonorMetadata class
#'
#' @section Constructor:
#' \code{DataSpaceConnection$getDonorMetadata()}
#'
#' @seealso \code{\link{connectDS}} \code{\link{DataSpaceConnection}}
#'
#' @examples
#' \dontrun{
#' # Create a connection (Initiate a DataSpaceConnection object)
#' con <- connectDS()
#'
#' # Get all the donor metadata
#' donorMeta <- con$getDonorMetadata()
#'
#' # Load all alignments
#' donorMeta$loadDaash()
#'
#' # Or just a subset of the alignments for the metadata extracted so far
#' donorMeta$loadDaash(donorIds = "cds_donor_32")
#' 
#' # Inspect the `topCalls` field
#' donorMeta$topCalls
#'
#' }
#'
#' @importFrom data.table setnames
DataSpaceDonorMetadata <- R6Class(
  classname = "DataSpaceDonorMetadata",
  public = list(
    #' @description
    #' Initialize \code{DataSpaceDonorMetadata} object.
    #' See \code{\link{DataSpaceConnection}}.
    #' @param donorIds A character vector of cds donor ids
    #' @param config A list.
    initialize = function(donorIds, config) {
      assert_that(!is.null(config))

      ## set primary fields
      private$.donorIds <- donorIds
      if(length(donorIds) > 0) {
        private$.donorFilter <- makeFilter(
          c("donor_id", "IN", paste(donorIds, collapse = ";"))
        )
      } else {
        private$.donorFilter <- makeFilter(
          c("donor_id", "NOT_EQUAL", "NULL")
        )
      }
      private$.config <- config

      ## get extra fields if available
      self$refresh()

      NULL
    },

    #' @description
    #' Print the \code{DataSpaceDonor} object summary.
    print = function() {
      cat("<DataSpaceDonorMetadata>")
      cat("\n  URL:", private$.config$labkeyUrlBase)
      cat("\n  User:", private$.config$labkeyUserEmail)
      cat("\n  Summary:")
      cat("\n    -", length(unique(private$.donorMetadata$donor_code)), "Donors")
      cat("\n  Donors in metadata: ")
      cat("\n    ", truncatePrintable(paste(private$.donorMetadata$donor_code, collapse = ", ")))
      cat("\n  Donor sequences available:")
      cat("\n    ",
          truncatePrintable(
            paste( private$.donorMetadata[sequence_available==TRUE, donor_code], collapse=", ")
          ))
      cat("\n  Donor sequences loaded:")
      cat("\n    ",
      {
        if(length(private$.daash) == 0){
          "No donor sequences loaded. Please run `loadDaash()` to load sequences and alignments."
        } else {
          truncatePrintable(
            paste(
              private$.donorMetadata[donor_id %in% private$.daash$sequences$donor_id, donor_code],
              collapse=", "
            )
          )
        }
      })

        cat("\n")

      },

  #' @description
  #' Refresh the \code{DataSpaceDonorMetadata} object to update datasets.
  refresh = function() {
      private$.getDonorMetadata()
      private$.getVariableDefinitions()
  },

  #' @description
  #' Applies DAASH tables to the this object
  loadDaash = function(donorIds=c(), filter=NULL){
    if(length(donorIds) != 0){

      ## donor_id not implemented to use this format yet
      ## if(!all(grepl("^cds_donor_[0-9]+$", donorIds)))
      ##   stop("All `donorIds` must be in the format `^cds_donor_[0-9]+$`")

      if(all(!(donorIds %in% private$.donorSequence$donor_id)))
        stop("None of the `donorIds` elements provided are found in this object.")

      if(!all(donorIds %in% private$.donorSequence$donor_id))
        message("Note: At least one element of `donorIds` is not available in this object.")

      if(length(private$.donorIds) != 0){
        private$.daashDonorIds <- private$.donorIds[private$.donorIds %in% donorIds]
      } else {
        private$.daashDonorIds <- donorIds
      }
      
    } else {
      if(length(private$.donorIds) != 0){
        private$.daashDonorIds <- private$.donorIds
      }
      
    }

    if(length(private$.daashDonorIds) == 0){
      private$.donorDaashFilter <- NULL
    } else {
      private$.donorDaashFilter <- makeFilter(c("donor_id", "IN", paste(private$.daashDonorIds, collapse=";")))
    }
    
    daashFilter <- rbind(private$.donorFilter, private$.donorDaashFilter, filter)
    private$.daash <- fetchDaash(daashFilter, private$.config)
   
  }
  
  ),

  active = list(
    #' @field config A list. Stores configuration of the connection object such
    #' as URL, path and username.
    config = function() {
      private$.config
    },

    #' @field donorMetadata A data.table. The table of the basic metadata from a donor metadata filter.
    donorMetadata = function(){
      private$.donorMetadata
    },

    #' @field topCalls A data.table. A table of top allele matches from both IgBLAST and V-Quest.  
    topCalls = function() {
      if(length(private$.daash$topCalls) != 0){
        return(private$.daash$topCalls)
      }
      message("Please run `loadDaash()` to access top matches.");
    },

    #' @field alignments A data.table. The table of alignments of germline genes to a given sequence.
    #' attributes.
    alignments = function() {
      if(length(private$.daash$alignments) != 0){
        return(private$.daash$alignments)
      }
      message("Please run `loadDaash()` to access alignments.")
    },

    #' @field  A data.table. The table of information concerning the runs from the two alignment tools.
    #' measurements against viruses.
    sequences = function() {
      if(length(private$.daash$sequences) != 0){
        return(private$.daash$sequences)
      }
      message("Please run `loadDaash()` to access sequences.")
    },
    
    #' @field  A data.table. The table of allele sequences from reported allele matches.
    #' measurements against viruses.
    alleleSequences = function() {
      if(length(private$.daash$alleleSequences) != 0){
        return(private$.daash$alleleSequences)
      }
      message("Please run `loadDaash()` to access allele sequences.")
    },

    #' @field  A data.table. The table of information concerning the runs from the two alignment tools.
    #' measurements against viruses.
    runInformation = function() {
      if(length(private$.daash$runInformation) != 0){
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
    .donorNames = character(),
    .donorIds = character(),
    .daashDonorIds = character(),
    .donorDaashFilter = character(),
    .donorMetadata = data.table(),
    .donorSequence = data.table(),
    .donorMix = data.table(),
    .donorFilter = character(),
    .daash = list(),
    .variableDefinitions = data.table(),
    
    .getDonorMetadata = function() {

      suppressWarnings({
        donorMetadata <- labkey.selectRows(
          baseUrl = private$.config$labkeyUrlBase,
          folderPath = "/CAVD",
          schemaName = "CDS",
          queryName = "donor_metadata",
          ## colSelect = "",
          colNameOpt = "fieldname",
          colFilter = private$.donorFilter,
          method = "GET"
        ) |> setDT()
      })

      if(nrow(donorMetadata) == 0){
        stop("None of the `donorIds` elements provided are found in the database.")
      }
      
      donorMabSequence <- labkey.selectRows(
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "CDS",
        queryName = "donor_mab_sequence",
        colSelect = "mab_id,donor_id,sequence_id",
        colNameOpt = "fieldname",
        colFilter = private$.donorFilter, 
        method = "GET"
      ) |>
        suppressWarnings() |> 
        setDT()    
      
      donorMetaSequence <- merge(
        donorMetadata,
        donorMabSequence,
        by = "donor_id",
        all.x = TRUE
      ) |> (\(.){
        .[!is.na(sequence_id), sequence_available:=TRUE ]
        .[ is.na(sequence_id), sequence_available:=FALSE]
        unique(.)
      })()
      
      private$.donorMetadata <- donorMetaSequence
      private$.donorSequence <- donorMabSequence[,.(donor_id, sequence_id)] |> unique()
      private$.donorNames <- unique(donorMetadata$donor_code)
      
    },
    .getVariableDefinitions = function() {

      varInfo <- lapply(
        list(
          c("donorMetadata"  , "donor_metadata"                 , "mab_id,sequence_id,donor_id,donor_lanl_id,mab_hxb2_location,mab_ab_binding_type,mab_isotype,mab_donorid,mab_donor_species,mab_donor_clade"),
          c("topCalls"       , "sequence_germline"              , "mab_id,allele,sequence_id,percent_identity,matches,alignment_length,score,run_application"),
          c("alignments"     , "alignment"                      , ""),
          c("sequences"      , "antibody_sequence_header_source", ""),
          c("alleleSequences", "allele_sequence"                , "allele,allele_sequence_nt"),
          c("runInformation" , "alignment_run"                  , "run_application,run_information")
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
