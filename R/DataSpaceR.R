#' @title DataSpaceR
#'
#' @description DataSpaceR provides a convenient API for accessing datasets
#' within the DataSpace database.
#'
#' @details Uses the Rlabkey package to connect to DataSpace. Implements
#' convenient methods for accessing datasets.
#'
#' @name DataSpaceR-package
#' @aliases DataSpaceR
#' @seealso \code{\link{connectDS}}
#' @author Ju Yeong Kim
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that is.number
#' @importFrom data.table data.table setDT := rbindlist setorder setkey setkeyv
NULL

# Global Variables
PRODUCTION <- "dataspace.cavd.org"
STAGING <- "dataspace-staging.cavd.org"
