#' @title The DataSpaceDonors class
#'
#' @description
#' An R6 class for DataSpace MAb Donor data.
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
#' # Print available donors to the console
#' con$availableDonors
#'
#' # Query the available donors object and pass that to `getDonors` to get a DataSpaceDonors object
#' donors <- con$availableDonors[lineage_sequences_available == TRUE & donor_clade == "B",] |>
#'   con$getDonors()
#'
#' # Load DAASH data to the object
#' donors$loadDaash()
#'
#' }
DataSpaceDonors <- R6Class(
  classname = "DataSpaceDonors",
  inherit = DataSpaceConnection,
  public = list(

    #' @description
    #' Initialize \code{DataSpaceMab} object.
    #' See \code{\link{DataSpaceConnection}}.
    #' @param donorIds a character vector of `donor_id` values.
    initialize = function(donorIds) {

      # set primary fields
      private$.donorIds <- donorIds
      private$.mabIds <- private$.shared$.donorMabSequence[ donor_id %in% private$.donorIds, na.omit(mab_id) ]
      private$.mabMixIds <-  merge(
        private$.shared$.donorMabSequence[ donor_id %in% private$.donorIds ],
        private$.shared$.mabMix,
        by = "mab_id"
      )[,unique(mab_mix_id)]

      self$refresh()
      NULL
    },

    #' @description
    #' Print the \code{DataSpaceMab} object summary.
    print = function() {
      cat("<DataSpaceDonors>")
      cat("\n  URL:", private$.shared$.config$labkeyUrlBase)
      cat("\n  User:", private$.shared$.config$labkeyUserEmail)
      cat("\n  Summary:")
      cat("\n    -", ifelse(is.character(self$donorStudies), "No available studies", paste(length(unique(self$donorStudies$study_id)), "studies")))
      cat("\n    -", length(unique(private$.donorIds)), "donors")
      cat("\n    -", length(unique(private$.mabIds)), "mAbs")
      cat("\n    -", length(private$.shared$.mabMixMetadata[mab_mix_type != "Single mAb" & mab_mix_id %in% private$.mabMixIds, unique(mab_mix_id)]), "mAb mixtures")
      cat("\n    -", sum(private$.shared$.availableDonors[donor_id %in% private$.donorIds, lineage_sequences_available]), "donors with lineage sequences")      
      cat("\n  Available Donors objects:")
      cat(paste0("\n    - ", DataSpaceDaash$active |> names() |> cleanReservedDataSpaceR6()), sep = "")
      cat("\n  Available Donors methods:")
      cat(paste0("\n    - ", DataSpaceDaash$public_methods |> names() |> cleanReservedDataSpaceR6()), sep = "")
      cat("\n  Available Connection objects:")
      cat(paste0("\n    - ", DataSpaceConnection$active |> names() |> cleanReservedDataSpaceR6()), sep = "")
      cat("\n  Available Connection methods:")
      cat(paste0("\n    - ", DataSpaceConnection$public_methods |> names() |> cleanReservedDataSpaceR6()), sep = "")
      cat("\n")
    },

    #' @description
    #' Load DAASH data to the object.
    loadDaash = function(){

      private$.datasets$daash <- fetchDaash(
        makeFilter(c("donor_id", "IN", paste(private$.donorIds, collapse = ";"))),
        private$.shared$.config
      )

      private$.variableDefinitions$daash <- fetchDaashVariableDefinitions(
        private$.datasets$daash,
        private$.shared$.config
      )

    },

    #' @description
    #' Refresh the `DataSpaceDonors` object to update datasets.
    refresh = function() {
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

    #' @field datasets A list of data.table objects containing the
    #' related data loaded.
    datasets = function() {
      private$.datasets
    },

    #' @field variableDefinitions A data.table of variable definitions.
    variableDefinitions = function() {
      private$.variableDefinitions
    }

  ),
  private = list(
    .mabIds = character(),
    .mabMixIds = character(),
    .donorIds = character(),
    .datasets = list(),
    .variableDefinitions = list()
  )
)
