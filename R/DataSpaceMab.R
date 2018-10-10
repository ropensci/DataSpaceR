#' The DataSpaceMab class
#'
#' @section Constructor:
#' \code{DataSpaceConnection$getMab()}
#'
#' @section Fields:
#' \describe{
#' }
#'
#' @section Methods:
#' \describe{
#' }
#' @seealso \code{\link{connectDS}} \code{\link{DataSpaceConnection}}
#' @examples
#' \dontrun{
#' }
#' @docType class
DataSpaceMab <- R6Class(
  classname = "DataSpaceMab",
  public = list(
    studyAndMabs = data.table(),
    mabs = data.table(),
    nabMab = data.table(),
    metadata = data.table(),
    studies = data.table(),
    assays = data.table(),
    variableDefinitions = data.table(),

    initialize = function(mAb_mixture, filters, config) {
      private$.config <- config
      private$.filters <- filters

      mabFilters <- Rlabkey::makeFilter(
        c("titer_curve_ic50", "GREATER_THAN", "0"),
        c("titer_curve_ic50", "NOT_MISSING", "")
      )
      if (length(filters) > 0) {
        filters <- lapply(names(filters), function(x) {
          Rlabkey::makeFilter(c(x, "IN", paste(unique(filters[[x]]), collapse = ";")))
        })
        mabFilters <- rbind(
          mabFilters,
          Rlabkey::makeFilter(c("mab_mix_name_std", "IN", paste(mAb_mixture, collapse = ";"))),
          do.call(rbind, filters)
        )
      }

      nabMab <- labkey.selectRows(
        baseUrl = config$labkey.url.base,
        folderPath = "/CAVD",
        schemaName = "study",
        queryName = "NAbMAbWithMixMeta",
        colNameOpt = "fieldname",
        colFilter = mabFilters,
        method = "GET"
      )

      setDT(nabMab)

      self$nabMab <- nabMab
    },
    print = function() {
      cat("<DataSpaceMab>")
      cat("\n  URL:", private$.config$labkey.url.base)
      cat("\n  User:", private$.config$labkey.user.email)
      cat("\n  Summary:")
      cat("\n   ", length(unique(self$nabMab$prot)), "studies")
      cat("\n   ", length(unique(self$nabMab$mab_mix_name_std)), "mAb mixtures")
      cat("\n   ", length(unique(self$nabMab$neutralization_tier)), "neutralization tiers")
      cat("\n   ", length(unique(self$nabMab$clade)), "clades")
      cat("\n  Filters:")
      if (length(private$.filters) > 0) {
        lapply(names(private$.filters), function(x) {
          cat("\n    ", x, ": ", paste(private$.filters[[x]], collapse = ", "), sep = "")
        })
      } else {
        cat(" NA")
      }
      cat(" \n")
    }
  ),
  active = list(),
  private = list(
    .config = list(),
    .filters = list()
  )
)
