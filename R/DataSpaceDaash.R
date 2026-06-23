#' @title The DataSpaceDAASH class
#'
#' @description
#' An R6 class for DataSpace DAASH data.
#'
#' @section Constructor:
#' \code{DataSpaceConnection$getDaash()}
#'
#' @seealso \code{\link{connectDS}} \code{\link{DataSpaceConnection}}
#'
#' @examples
#' \dontrun{
#' # Create a connection (Initiate a DataSpaceConnection object)
#' con <- connectDS()
#'
#' # Get the daash object using either an availableMabs or
#' # availableDonors object.
#' daash <- con$availableMabs[mab_ab_binding_type %like% "CD4"] |>
#'   con$getDaash()
#'
#' # To get lineage sequences, query donors, then pipe available
#' # donors to the connection getDaash object.
#' daash <- con$availableDonors[
#'   lineage_sequences_available == TRUE & mab_count < 10,
#'   ] |>
#'   con$getDaash()
#'
#' # Inspect what datasets are available
#' names(daash$datasets)
#'
#' # Inspect the `topCalls` dataset
#' daash$datasets$topCalls
#'
#' }
#'
DataSpaceDaash <- R6Class(
  classname = "DataSpaceDaash",
  inherit = DataSpaceConnection,
  public = list(
    #' @description
    #' Initialize \code{DataSpaceMabMetadata} object.
    #' See \code{\link{DataSpaceConnection}}.
    #' @param availableDaash availableDaash an `availableMabs`, or `availableDonors` object, or a vector of `sequnce_id` values.
    #' @param config A list.
    initialize = function(availableDaash) {
      if( "availableDonors" %in% class(availableDaash) ){
        private$.sequenceIds <- private$.shared$.donorMabSequence[donor_id %in% availableDaash$donor_id, sequence_id]
        private$.donorIds    <- availableDaash[, unique(donor_id)]

      } else if( "availableMabs" %in% class(availableDaash) ){
        private$.sequenceIds <- private$.shared$.donorMabSequence[mab_id %in% availableDaash$mab_id, sequence_id]
        private$.mabIds      <- availableDaash[, unique(mab_id)]

      } else if( is.character(availableDaash) && all(grepl("^cds_seq_[0-9]+", availableDaash)) ){
        private$.sequenceIds <- availableDaash

      } else{
        stop("Must pass an `availableMabs` or `availableDonors` object, or a character vector of cds sequence ids to get DAASH data from a connection object.")
      }

      self$refresh()
      NULL
    },

    #' @description
    #' Print the \code{DataSpaceMab} object summary.
    print = function() {
      cat("<DataSpaceDaash>")
      cat("\n  URL:", private$.shared$.config$labkeyUrlBase)
      cat("\n  User:", private$.shared$.config$labkeyUserEmail)
      cat("\n  Summary:")
      cat("\n    -", length(unique(na.omit(self$daashMetadata$mab_id))), "mAbs")
      cat("\n    -", length(unique(na.omit(self$daashMetadata$donor_id))), "donors")
      cat("\n    -", length(unique(na.omit(self$availableStructures$mab_id))), "predicted structures")
      cat("\n  MAbs loaded to object:")
      cat("\n    ", truncatePrintable(paste(self$daashMetadata$mab_name_std |> unique() |> sort(), collapse = ", ")))
      cat("\n  Donors loaded to object:")
      cat("\n    ", truncatePrintable(paste(self$daashMetadata$donor_code |> unique() |> sort(), collapse = ", ")))
      cat("\n  MAb structures to download:")
      cat("\n    ",
          truncatePrintable(
            paste( self$availableStructures$mab_name_std, collapse=", ")
          ))
      cat("\n  Available DAASH objects:")
      cat(paste0("\n    - ", DataSpaceDaash$active |> names() |> cleanReservedDataSpaceR6()), sep = "")
      cat("\n  Available DAASH methods:")
      cat(paste0("\n    - ", DataSpaceDaash$public_methods |> names() |> cleanReservedDataSpaceR6()), sep = "")
      cat("\n  Available Connection objects:")
      cat(paste0("\n    - ", DataSpaceConnection$active |> names() |> cleanReservedDataSpaceR6()), sep = "")
      cat("\n  Available Connection methods:")
      cat(paste0("\n    - ", DataSpaceConnection$public_methods |> names() |> cleanReservedDataSpaceR6()), sep = "")
      cat("\n")
    },

    #' @description
    #' Return a fasta file for available daash sequences that have been loaded to the current object.
    #' @param sequenceType character the type of fasta file to return: nt = nucleotide, aa = amino acid.
    #' @param originalHeaders boolean if the original fasta headers should be provided
    #' @param path The path where to save the fasta files to. If using the default value, NULL, then a
    #' fasta file is returned as a character vector.
    getFastaFromSequences = function(sequenceType="nt", originalHeaders=FALSE, path=NULL){
      sequences <- copy(private$.datasets$sequences)

      if(originalHeaders & is.null(private$.shared$.originalHeaders)){
        if(is.null(private$.shared$.originalHeaders))
          private$.shared$.originalHeaders <- labkey.selectRows(
            baseUrl=private$.shared$.config$labkeyUrlBase,
            folderPath="/CAVD",
            schemaName="CDS",
            queryName="sequence_header",
            colSelect="header,sequence_id",
            colNameOpt="rname"
          )
        sequences[private$.shared$.originalHeaders, on="sequence_id", header:=i.header]
      } else {
        sequences[, header:=paste(mab_name_std, chain, sequence_id, mab_id, donor_id, donor_code, sep="|")]
      }

      fasta <- generateFasta(sequences, seqIds, sequenceType)
      if( !is.null(path) ){
        writeLines(fasta, path)
      } else {
        return(fasta)
      }
    },

    #' @description
    #' Saves all antibody structures associated with the daash object's `availableStuctures` object.
    #' @param path The directory to export fasta files to.
    #' @param mab_id A subset of mab_ids to export. If using the default, NULL, all structures in
    #' availableStuctures are downloaded.
    downloadAntibodyStructures = function(path=tempdir(), mab_id=NULL){
      structures <- self$availableStructures
      if(!is.null(mab_id))
        structures <- structures[mab_id %in% mab_id,]
      for(i in structures$mab_id){
        out <- paste0(
          paste(self$daashMetadata[mab_id == i, .(mab_name_std, mab_id, mab_lanl_id)] |> unique(), collapse = "^^^"),
          ".pdb"
        )
        pdb <- paste0(i, ".pdb")
        message("Saving `", pdb, "` to `", path, "`.") 
        labkey.webdav.get(
          baseUrl = private$.shared$.config$labkeyUrlBase,
          folderPath = "CAVD Files/ab_structures",
          remoteFilePath = pdb,
          localFilePath = file.path(path, out)
        )
      }
    },
    
    #' @description
    #' Refresh the \code{DataSpaceMabMetadata} object to update datasets.
    refresh = function() {
      private$.loadDaash()
      private$.loadAvailableStructures()
    }

  ),

  active = list(

    #' @field mabMetadata A data.table of mAbs with metadata found in
    #' the object.
    mabMetadata = function(){
      self$availableMabs
    },

    #' @field donorMetadata A data.table of donors with metadata
    #' found in the object.
    donorMetadata = function(){
      self$availableDonors
    },

    #' @field daashMetadata A data.table showing the donor and mAb
    #' metadata with CDS sequence_id values for the loaded DAASH dataset.
    daashMetadata = function() {
      private$.shared$.donorMabSequence[sequence_id %in% private$.sequenceIds,] |>
        merge(private$.shared$.mabMetadata,   by = "mab_id",   all.x = T) |>
        merge(private$.shared$.donorMetadata, by = "donor_id", all.x = T)
    },

    #' @field availableStructures A data.table showing the mAb
    #' structures available to download.
    availableStructures = function() {
      private$.availableStructures
    },

    #' @field datasets A list of DAASH datastets loaded to the DAASH object.
    datasets = function() {
        private$.datasets
    },
    
    #' @field variableDefinitions A data.table of variable definitions.
    variableDefinitions = function() {
      private$.variableDefinitions
    }
  ),

  private = list(
    .config      = list(),
    .sequenceIds = character(),
    .mabIds      = character(),
    .donorIds    = character(),
    .datasets = list(),
    .availableStructures = data.table(),
    .variableDefinitions = data.table(),
    
    .loadDaash = function(){

      filter <- list(
        if(length(private$.sequenceIds) != 0) c("sequence_id", "IN", paste(private$.sequenceIds, collapse = ";")) else NULL,
        if(length(private$.donorIds)    != 0) c("donor_id",    "IN", paste(private$.donorIds,    collapse = ";")) else NULL,
        if(length(private$.mabIds)      != 0) c("mab_id",      "IN", paste(private$.mabIds,      collapse = ";")) else NULL
      )

      filter <- filter[lapply(filter, \(f) !is.null(f)) |> unlist()]

      filter <- Reduce(
        \(a,b) rbind(a, makeFilter(b)),
        filter[2:length(filter)],
        makeFilter(filter[[1]])
      )

      private$.datasets <- fetchDaash(
        filter,
        private$.shared$.config
      )

      private$.variableDefinitions <- fetchDaashVariableDefinitions(
        private$.datasets,
        private$.shared$.config
      )

      private$.sequenceIds <- private$.datasets$sequences$sequence_id |> unique()
      private$.mabIds      <- private$.datasets$sequences$mab_id      |> unique()
      private$.donorIds    <- private$.datasets$sequences$donor_id    |> unique()

    },
    
    .loadAvailableStructures = function(){

      private$.availableStructures <- labkey.webdav.listDir(
        baseUrl = private$.shared$.config$labkeyUrlBase,
        folderPath = "CAVD Files/ab_structures",
        remoteFilePath = ""
      )[["files"]] |>
        lapply(setDT) |>
        rbindlist() |>
        _[,mab_id := gsub("(.+)\\.pdb", "\\1", basename(id))] |>
        merge(self$daashMetadata, by = "mab_id") |>
        _[, names(private$.shared$.mabMetadata), with = FALSE] |>
        unique()
      
    }

  )

)
