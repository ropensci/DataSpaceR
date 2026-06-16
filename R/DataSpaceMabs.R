#' @title  The DataSpaceMab class
#'
#' @description
#' An R6 class for DataSpace MAb data.
#'
#' @section Constructor:
#' \code{DataSpaceConnection$getMab()}
#'
#' @seealso \code{\link{connectDS}} \code{\link{DataSpaceConnection}}
#'
#' @examples
#' \dontrun{
#' # Create a connection (Initiate a DataSpaceConnection object)
#' con <- connectDS()
#'
#' # Inspect available mabs, then pass subset to the `getMabs` method.
#' vrc01 <- con$availableMabs[mab_name_std == "VRC01"] |>
#'   con$getMabs()
#'
#' # Inspect the `NABMAb` assay data.
#' vrc01$datasets$NABMAb
#'
#' # Load DAASH data from mab object
#' vrc01$loadDaash()
#'
#' # Inspect DAASH datasets
#' vrc01$datasets$daash |> names()
#'
#' }
#'
DataSpaceMabs <- R6Class(
  classname = "DataSpaceMabs",
  inherit = DataSpaceConnection,
  public = list(

    #' @description
    #' Initialize \code{DataSpaceMab} object.
    #' See \code{\link{DataSpaceConnection}}.
    #' @param mabIds A character vector of `mab_id` values.
    #' @param includeMixtures Whether or not to include mab mixtures. "yes", "no", or "only" are valid.
    initialize = function(mabIds, includeMixtures) {

      private$.incMix <- ifelse(includeMixtures == "yes" | includeMixtures == "only", TRUE, FALSE)
      private$.incMab <- ifelse(includeMixtures == "yes" | includeMixtures == "no",   TRUE, FALSE)

      mabMixMab <-
        merge(
          private$.shared$.mabMixMetadata,
          private$.shared$.mabMix,
          by = "mab_mix_id"
        )

      if(includeMixtures == "no") {
        mabs <- mabMixMab[
          mab_id %in% mabIds & mab_mix_type == "Single mAb",
          mab_mix_id, mab_id
        ]
      } else if(includeMixtures == "only") {
        mabs <- mabMixMab[
          mab_id %in% mabIds & mab_mix_type != "Single mAb",
          mab_mix_id, mab_id
        ]
      } else {
        mabs <- mabMixMab[
          mab_id %in% mabIds,
          mab_mix_id, mab_id
        ]
      }

      private$.mabIds          <- mabIds
      private$.mabMixIds       <- mabs[,mab_mix_id]
      private$.includeMixtures <- includeMixtures
      private$.donorIds        <- private$.shared$.donorMabSequence[mab_id %in% mabIds, unique(donor_id)]

      assays <- private$.shared$.mabStudies[
        mab_mix_id %in% private$.mabMixIds, assays_available
      ] |> unique()

      private$.assays <- ifelse(assays == "NAB MAB", "NABMAb", "PKMAb")

      private$.studyIds <- private$.shared$.mabStudies[
        mab_mix_id %in% private$.mabMixIds, studies_available
      ] |> strsplit(", ") |> unlist() |> unique()

      self$refresh()
      NULL
    },

    #' @description
    #' Print the \code{DataSpaceMab} object summary.
    print = function() {
      cat("<DataSpaceMabs>")
      cat("\n  URL:", private$.shared$.config$labkeyUrlBase)
      cat("\n  User:", private$.shared$.config$labkeyUserEmail)
      cat("\n  Summary:")
      cat("\n    -", length(private$.studyIds), "studies")
      cat("\n    -", length(private$.mabIds), "mAbs")
      cat("\n    -", length(self$mabMixMetadata[mab_mix_type != "Single mAb", unique(mab_mix_id)]), "mAb mixtures")
      cat("\n    -", length(unique(private$.datasets$NABMAb$neutralization_tier)), "nAb mAb assay neutralization tiers")
      cat("\n    -", length(unique(private$.datasets$NABMAb$clade)), "nAb mAb assay virus clades")
      cat("\n  Available MAb objects:")
      cat(paste0("\n    - ", DataSpaceMabs$active |> names() |> cleanReservedDataSpaceR6()), sep = "")
      cat("\n  Available MAb methods:")
      cat(paste0("\n    - ", DataSpaceMabs$public_methods |> names() |> cleanReservedDataSpaceR6()), sep = "")
      cat("\n  Available Connection objects:")
      cat(paste0("\n    - ", DataSpaceConnection$active |> names() |> cleanReservedDataSpaceR6()), sep = "")
      cat("\n  Available Connection methods:")
      cat(paste0("\n    - ", DataSpaceConnection$public_methods |> names() |> cleanReservedDataSpaceR6()), sep = "")
      cat("\n")
    },

    #' @description
    #' Load any available DAASH datasets.
    loadDaash = function(){

      private$.datasets$daash <- fetchDaash(
        makeFilter(c("mab_id", "IN", paste(private$.mabIds, collapse = ";"))),
        private$.shared$.config
      )

      private$.variableDefinitions$daash <- fetchDaashVariableDefinitions(
        private$.datasets$daash,
        private$.shared$.config
      )

    },

    #' @description
    #' Refresh the \code{DataSpaceMab} object to update datasets.
    refresh = function() {

      for(assay in private$.assays)
        private$.getMabAssayData(assay)

      setDatasetNames(private$.datasets)

      for(assay in private$.assays)
        private$.variableDefinitions[[assay]] <- getVarInfo(
          assay,
          names(private$.datasets[[assay]]),
          private$.shared$.config$labkeyUrlBase
        )

      NULL

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
    
    #' @field mabMixMetadata A data.table. A table of mAb mixtures
    #' with metadata found in this DataSpaceMab instance.
    mabMixMetadata = function() {
      self$availableMabMixtures
    },

    #' @field mabMix A data.table. A mapping table of mab_mix_id to mab_id.
    #' with metadata found in this DataSpaceMab instance.
    mabMix = function() {
      private$.shared$.mabMix[mab_mix_id %in% private$.mabMixIds]
    },

    #' @field datasets A list of data.table objects containing the
    #' mab related that were loaded.
    datasets = function() {
      private$.datasets
    },

    #' @field variableDefinitions A data.table of variable definitions.
    variableDefinitions = function() {
      private$.variableDefinitions
    }

  ),
  private = list(
    .studyIds = character(),
    .mabIds = character(),
    .mabMixIds = character(),
    .datasets = list(),
    .variableDefinitions = list(),
    .includeMixtures = character(),
    .incMix = logical(),
    .incMab = logical(),
    .studies = data.table(),
    .assays = data.table(),
    .getMabAssayData = function(assay_identifier) {

      assayNm <- switch(
        assay_identifier,
        "NABMAb" = "NAB MAB",
        "PKMAb" = "PK MAB"
      )

      if(
        nrow( private$.shared$.mabStudies[
          mab_mix_id %in% private$.mabMixIds &
            grepl(assayNm, assays_available)
        ] ) != 0
      ) {

        private$.datasets[[assay_identifier]] <- labkey.selectRows(
          baseUrl = private$.shared$.config$labkeyUrlBase,
          folderPath = "/CAVD",
          schemaName = "study",
          queryName = assay_identifier,
          colNameOpt = "fieldname",
          colFilter = makeFilter(
            c("mab_mix_id", "IN", paste(private$.mabMixIds, collapse=";"))
          ),
          method = "GET"
        ) |>
          setDT()

      }

    }
  )
)
