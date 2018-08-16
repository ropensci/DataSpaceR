#' The DataSpaceMAb class
#'
#' @section Constructor:
\code{DataSpaceConnection$getMAb()}
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
DataSpaceMAb <- R6Class(
  classname = "DataSpaceMAb",
  public = list(
    nAbMAb = data.table(),
    initialize = function(mAb_mixture, filters, config) {
      mAbFilters <- Rlabkey::makeFilter(
        c("titer_curve_ic50", "GREATER_THAN", "0"),
        c("titer_curve_ic50", "NOT_MISSING", "")
      )
      if (length(filters) > 0) {
        filters <- lapply(names(filters), function(x) {
          Rlabkey::makeFilter(c(x, "IN", paste(unique(filters[[x]]), collapse = ";")))
        })
        mAbFilters <- rbind(
          mAbFilters,
          Rlabkey::makeFilter(c("mab_mix_name_std", "IN", paste(mAb_mixture, collapse = ";"))),
          do.call(rbind, filters)
        )
      }

      c("mab_mix_name_std", "IN", paste(mAb_mixture, collapse = ";"))
      nAbMAb <- labkey.selectRows(
        baseUrl = config$labkey.url.base,
        folderPath = "/CAVD",
        schemaName = "study",
        queryName = "NAbMAbWithMixMeta",
        colNameOpt = "fieldname",
        colFilter = mAbFilters,
        method = "GET"
      )

      setDT(nAbMAb)

      self$nAbMAb <- nAbMAb
    }
  ),
  active = list(),
  private = list()
)
