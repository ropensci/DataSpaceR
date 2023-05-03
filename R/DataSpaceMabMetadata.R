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
#' con$getMabMetadata(using = "mab_name_std", value = c("PGT121", "PGDM1400", "VRC01", "1-18", "10-1074"))
#' # Create a DataSpaceMab object that contains the filtered mAb data
#'
#' mabMeta <- con$getMabMetadata()
#' mabMeta
#'
#' # Inspect the `topMatches` field
#' mabMeta$topMatches
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
    initialize = function(mabIds, config) {
      assert_that(!is.null(config))

      ## set primary fields
      private$.mabIds <- mabIds
      if(length(mabIds) > 0) {
        private$.mabFilter <- makeFilter(c("mab_id", "IN", mabIds))
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
      cat("\n  MAbs: ")
      cat("\n    ",
      {
        mabs <- paste(private$.mabs, collapse = ", ")
        if( nchar(mabs) > 70 ){
          paste0(substr(mabs, 1, 77), "...")
        } else {
          mabs
        }
      })
      cat(" \n")
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

    loadAlignments = function(){
      
      private$.topMatches <- labkey.selectRows(
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "cds",
        queryName = "sequence_germline",
        colNameOpt = "fieldname",
        colFilter = private$.mabFilter,
        method = "GET"
      ) |>
        setDT() |>
        merge(private$.mabSequence, by = "sequence_id") |>
        merge(private$.mabMetadata, by = "mab_id") |>
        (\(.){
          .[,!grepl("container", names(.)), with = F]
        })()

      private$.alignments <- labkey.selectRows(
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "cds",
        queryName = "alignment",
        colNameOpt = "fieldname",
        colFilter = private$.mabFilter,
        method = "GET"
      ) |>
        setDT() |>
        merge(private$.mabSequence, by = "sequence_id") |>
        merge(private$.mabMetadata, by = "mab_id") |>
        (\(.){
          .[,!grepl("container", names(.)), with = F]
        })()

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
        colFilter = alleleFilter,
        method = "GET"
      ) |>
        setDT()

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

    #' @field  A data.table. The table of allele sequences from reported allele matches.
    #' measurements against viruses.
    alleleSequences = function() {
      if(length(private$.alleleSequences) == 0){
        return(private$.alleleSequences)
      }
      message("Please run `loadAlignments()` to access allele sequences.")
    },

    #' @field variableDefinitions A data.table. The table of variable definitions for the metadata.
    #' definitions.
    variableDefinitions = function() {
      private$.variableDefinitions
    }
  ),
  
  private = list(
    .config = list(),
    .mabs = character(),
    .mabIds = character(),
    .mabMetadata = data.table(),
    .mabSequence = data.table(),
    .mabFilter = character(),
    .topMatches = data.table(),
    .alignments = data.table(),
    .alleleSequences = data.table(),
    .variableDefinitions = data.table(),
    
    .getMabMetadata = function() {
      mabMetadata <- labkey.selectRows(
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "cds",
        queryName = "MAbMetadata",
        colSelect = "mab_id,mab_name_std,mab_lanlid,mab_hxb2_location,mab_ab_binding_type,mab_isotype,mab_donorid,mab_donor_species,mab_donor_clade",
        colNameOpt = "fieldname",
        colFilter = private$.mabFilter,
        method = "GET"
      )

      private$.mabMetadata <- mabMetadata
      private$.mabs <- unique(private$.mabMetadata$mab_name_std)
    },

    .getMabSequence = function() {
      private$.mabSequence <- labkey.selectRows(
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "cds",
        queryName = "antibody_sequence",
        ## colSelect = "mab_id,mab_name_std,mab_lanlid,mab_hxb2_location,mab_ab_binding_type,mab_isotype,mab_donorid,mab_donor_species,mab_donor_clade",
        colNameOpt = "fieldname",
        colFilter = private$.mabFilter,
        method = "GET"
      ) |> setDT()
    },

    .getVariableDefinitions = function() {
      varInfo <- rbind(
        labkey.getQueryDetails(
          baseUrl = private$.config$labkeyUrlBase,
          folderPath = "/CAVD",
          schemaName = "CDS",
          queryName = "MAbMetadata"
        ),
        labkey.getQueryDetails(
          baseUrl = private$.config$labkeyUrlBase,
          folderPath = "/CAVD",
          schemaName = "CDS",
          queryName = "sequence_germline_mab_metadata"
        ),
        labkey.getQueryDetails(
          baseUrl = private$.config$labkeyUrlBase,
          folderPath = "/CAVD",
          schemaName = "CDS",
          queryName = "alignment_mab_metadata"
        ),
        labkey.getQueryDetails(
          baseUrl = private$.config$labkeyUrlBase,
          folderPath = "/CAVD",
          schemaName = "CDS",
          queryName = "allele_sequences"
        )    
      )
      setDT(varInfo)
      setnames(varInfo, "fieldName", "field_name")
      private$.variableDefinitions <- varInfo[, .(field_name, caption, description)]
    }
    
  )
)
