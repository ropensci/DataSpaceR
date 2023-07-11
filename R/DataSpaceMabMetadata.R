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
#' mabMeta$loadAlignments()
#'
#' # Or just a subset of the alignments for the metadata extracted so far
#' mabMeta$loadAlignments(mabAlignmentIds = "cds_mab_32")
#' 
#' # Inspect the `topMatches` field
#' mabMeta$topMatches
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
    #' @param mabMixture A character vector.
    #' @param mabFilters A list.
    #' @param config A list.
    initialize = function(mabIds, lineage=FALSE, config) {
      assert_that(!is.null(config))

      ## set primary fields
      private$.mabIds <- mabIds
      private$.lineageFilter <- makeFilter(c("lineage", "EQUALS", tolower(as.character(lineage))))
      if(lineage) message("Note: When `lineage` is TRUE, only lineage sequences are able to be extracted from DataSpace. Large amounts of system memory are recommended when accessing those data.")
      if(length(mabIds) > 0) {
        private$.mabFilter <- makeFilter(c("mab_id", "IN", paste(mabIds, collapse = ";")))
      } else {
        private$.mabFilter <- NULL
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
      cat("\n    -", length(unique(private$.mabMetadata$mab_name_std)), "mabs")
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
        if(nrow(private$.sequences) == 0){
          "No mAbs loaded. Please run `loadAlignments()` to load sequences and alignments."
        } else {
          truncatePrintable(
            paste(
              private$.mabMetadata[mab_id %in% private$.sequences$mab_id, mab_name_std],
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
    tries <- c(
      class(try(
        private$.getMabMetadata(),
        silent = !private$.config$verbose
      )),
      class(try(
        private$.getMabSequence(),
        silent = !private$.config$verbose
      )),
      class(try(
        private$.getVariableDefinitions(),
        silent = !private$.config$verbose
      ))

    )
    invisible(!"try-error" %in% tries)
  },

  #' @description
  #' Applies LANL metadata to mabs table.
  getLanlMetadata = function(){
    checkList <- function(x){
      if(any(c("list", "data.frame") %in% class(x))){
        lapply(x, function(y){
          if("data.frame" %in% class(y)) {
            setDT(y)
            checkList(y)
          } else {
            checkList(y)
          }
        })
      }
    }
    pullForLanlId <- function(lanl_id){
      url <- paste0("https://www.hiv.lanl.gov/mojo/immunology/api/v1/epitope/ab?id=", lanl_id)
      if(is.na(lanl_id)) return(NA)
      res <- httr::GET(url)
      if(res$status == 200){
        json <- fromJSON(content(res, as="text")[[1]])
        json[["epitopes"]] <- data.table(json[["epitopes"]])
        json$source <- url
        lapply(json$epitopes, checkList)
        return(json)
      } else {
        return(paste0("No LANL metadata found at '", url, "'."))
      }
    }
    private$.mabMetadata[, lanl_metadata := lapply(mab_lanlid, pullForLanlId)]
  },

  loadAlignments = function(mabAlignmentIds=c()){

    if(length(mabAlignmentIds) != 0){

      if(!all(grepl("^cds_mab_[0-9]+$", mabAlignmentIds)))
        stop("All `mabAlignmentIds` must be in the format `^cds_mab_[0-9]+$`")

      if(all(!(mabAlignmentIds %in% private$.mabSequence$mab_id)))
        stop("None of the `mabAlignmentIds` elements provided exist in database.")

      if(!all(mabAlignmentIds %in% private$.mabSequence$mab_id))
        message("Note: At least one element of `mabAlignmentIds` is not available.")

      if(length(private$.mabIds) != 0){
        private$.mabAlignmentIds <- private$.mabIds[private$.mabIds %in% mabAlignmentIds]
      } else {
        private$.mabAlignmentIds <- mabAlignmentIds
      }
      
    } else {
      if(length(private$.mabIds) != 0){
        private$.mabAlignmentIds <- private$.mabIds
      }
      
    }

    if(length(private$.mabAlignmentIds) == 0){
      private$.mabAlignFilter <- NULL
      ## private$.seqAlignFilter <- NULL
    } else {
      private$.mabAlignFilter <- makeFilter(c("mab_id", "IN", paste(private$.mabAlignmentIds, collapse=";")))
      ## private$.seqAlignFilter <- makeFilter(c("sequence_id", "IN", paste(private$.seqAlignmentIds, collapse = ";")))
    }
    
    ## private$.seqAlignmentIds <- private$.mabSequence[mab_id %in% private$.mabAlignmentIds, sequence_id]    

    private$.topMatches <-  merge(
      private$.mabMetadata[,-c("sequence_available", "lineage_available")],
      labkey.selectRows(
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "cds",
        queryName = "lineage_sequence_germline",
        colNameOpt = "fieldname",
        colSelect = "mab_id,allele,sequence_id,percent_identity,matches,alignment_length,score,preferred_status,run_application",
        colFilter = rbind(private$.mabAlignFilter, private$.lineageFilter),
        method = "GET"
      ) |> setDT(),
      by = "mab_id"
    )[order(run_application, mab_name_std, allele)]
    
    private$.alignments <- merge(
      private$.mabMetadata[,.(mab_name_std, mab_id, mab_lanlid)],
      labkey.selectRows(
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "cds",
        queryName = "lineage_alignment",
        colNameOpt = "fieldname",
        colFilter = rbind(private$.mabAlignFilter, private$.lineageFilter),
        method = "GET"
      ) |> setDT(),
      by = "mab_id"
    )[order(run_application, mab_name_std), -"lineage"]

    private$.sequences <- merge(
      private$.alignments[,.(mab_id, sequence_id, locus)] |> unique(),
      labkey.selectRows(
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "cds",
        queryName = "antibody_sequence_header_source",
        colNameOpt = "fieldname",
        colFilter = rbind(private$.mabAlignFilter, private$.lineageFilter),
        method = "GET"
      )
    ) |>
      setDT() |>
      ( \(.) .[,-"lineage"] )()
    
    alleleFilter <- makeFilter(
      c(
        "allele", "IN", paste(unique(private$.topMatches$allele), collapse = ";")
      )
    )
    
    private$.alleleSequences <- labkey.selectRows(
      baseUrl = private$.config$labkeyUrlBase,
      folderPath = "/CAVD",
      schemaName = "cds",
      queryName = "allele_sequence",
      colNameOpt = "fieldname",
      colSelect = "allele,allele_sequence_nt",
      colFilter = alleleFilter,
      method = "GET"
    ) |>
      setDT()

    private$.runInformation <- labkey.selectRows(
      baseUrl = private$.config$labkeyUrlBase,
      folderPath = "/CAVD",
      schemaName = "cds",
      queryName = "alignment_run",
      colSelect = "run_application,run_information",
      colNameOpt = "fieldname",
      method = "GET"
    ) |>
      setDT()

    private$.runInformation[,run_information:=lapply(gsub("\"\"", "\"", run_information), fromJSON)]
  },
  
  getFastaFromSequences = function(originalHeaders = F){
    if(length(private$.sequences) == 0) stop("Please run `loadAlignments()` to access fasta file for sequences available.")

    if(originalHeaders){
      fastaParse <- private$.sequences[,.(header, sequence_nt)]
    } else {
      fastaParse <-
        merge(
          private$.mabMetadata,
          private$.sequences[,.(mab_id, locus, sequence_id, sequence_nt)] |> unique()
        ) |>
        ( \(.) .[, .(
          header =
            paste(
              mab_name_std, mab_id, sequence_id, mab_isotype, mab_donor_species, locus,
              sep = "|"
            ),
          sequence_nt
        )] )()
    }
    
    lines <- apply(fastaParse, 1, \(l) {
      header <- l[1]
      sets <- nchar(l[2]) %/% 60
      c(
        ifelse(!grepl("^>", header), paste0(">", header), header), 
        sapply( 1:sets, \(set) substr(l[2], (set - 1) * 60 + 1, set * 60) ),
        substr(l[2], sets * 60 + 1, nchar(l[2]))
      )
    }) |> unlist()
    names(lines) <- NULL
    return(lines)
    
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
  
  #' @field topMatches A data.table. A table of top allele matches from both IgBLAST and V-Quest.  
  topMatches = function() {
    if(length(private$.topMatches) != 0){
      return(private$.topMatches)
    }
    message("Please run `loadAlignments()` to access top matches.");
  },

  #' @field alignments A data.table. The table of alignments of germline genes to a given sequence.
  #' attributes.
  alignments = function() {
    if(length(private$.alignments) != 0){
      return(private$.alignments)
    }
    message("Please run `loadAlignments()` to access alignments.")
  },

  #' @field  A data.table. The table of information concerning the runs from the two alignment tools.
  #' measurements against viruses.
  sequences = function() {
    if(length(private$.sequences) != 0){
      return(private$.sequences)
    }
    message("Please run `loadAlignments()` to access sequences.")
  },

  
  #' @field  A data.table. The table of allele sequences from reported allele matches.
  #' measurements against viruses.
  alleleSequences = function() {
    if(length(private$.alleleSequences) != 0){
      return(private$.alleleSequences)
    }
    message("Please run `loadAlignments()` to access allele sequences.")
  },

  #' @field  A data.table. The table of information concerning the runs from the two alignment tools.
  #' measurements against viruses.
  runInformation = function() {
    if(length(private$.runInformation) != 0){
      return(private$.runInformation)
    }
    message("Please run `loadAlignments()` to access run info.")
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
  .seqIds = character(),
  .mabAlignmentIds = character(),
  .seqAlignmentIds = character(),
  .mabMetadata = data.table(),
  .mabSequence = data.table(),
  .mabFilter = character(),
  .mabAlignFilter = character(),
  .seqAlignFilter = character(),
  .lineageFilter = character(),
  .topMatches = data.table(),
  .alignments = data.table(),
  .sequences = data.table(),
  .alleleSequences = data.table(),
  .runInformation = data.table(),
  .variableDefinitions = data.table(),
  
  .getMabMetadata = function() {
    mabSequence <- labkey.selectRows(
      baseUrl = private$.config$labkeyUrlBase,
      folderPath = "/CAVD",
      schemaName = "cds",
      queryName = "antibody_sequence",
      colSelect = "mab_id,sequence_id,lineage",
      colNameOpt = "fieldname",
      colFilter = private$.mabFilter,
      method = "GET"
    ) |>
      setDT()    

    mabMetadata <- labkey.selectRows(
      baseUrl = private$.config$labkeyUrlBase,
      folderPath = "/CAVD",
      schemaName = "cds",
      queryName = "MAbMetadata",
      colSelect = "mab_id,mab_name_std,mab_lanlid,mab_hxb2_location,mab_ab_binding_type,mab_isotype,mab_donorid,mab_donor_species,mab_donor_clade",
      colNameOpt = "fieldname",
      colFilter = private$.mabFilter,
      method = "GET"
    ) |> setDT()

    mabMetadata <- merge(
      mabMetadata,
      mabSequence[,.(sequence_available = TRUE, lineage_available = any(lineage)), mab_id],
      all.x = TRUE
    )

    mabMetadata[is.na(sequence_available), sequence_available:=FALSE]
    mabMetadata[is.na(lineage_available), lineage_available:=FALSE]

    private$.mabMetadata <- mabMetadata
    private$.mabSequence <- mabSequence[lineage == as.logical(gsub(".+=([a-z])", "\\1", private$.lineageFilter))]
    private$.mabNames <- unique(mabMetadata$mab_name_std)
    private$.seqIds   <- unique(private$.mabSequence$sequence_id)
    
  },
  .getVariableDefinitions = function() {

    varInfo <- lapply(
      list(
        c("mabMetadata"    , "mabMetadata"                    , "mab_id,sequence_id,lineage,mab_name_std,mab_lanlid,mab_hxb2_location,mab_ab_binding_type,mab_isotype,mab_donorid,mab_donor_species,mab_donor_clade"),
        c("topMatches"     , "lineage_sequence_germline"      , "mab_id,allele,sequence_id,percent_identity,matches,alignment_length,score,run_application"),
        c("alignments"     , "lineage_alignment"              , ""),
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
