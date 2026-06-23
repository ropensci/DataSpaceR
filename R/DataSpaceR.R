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
#' @importFrom Rlabkey getSession labkey.executeSql labkey.getQueryDetails labkey.selectRows labkey.setCurlOptions labkey.webdav.get labkey.webdav.listDir lsFolders makeFilter
#' @importFrom assertthat assert_that is.number
#' @importFrom curl has_internet nslookup
#' @importFrom data.table %like% copy data.table fread rbindlist setDT setkey setkeyv setnames setorder setcolorder shift
#' @importFrom digest digest
#' @importFrom httr content GET
#' @importFrom jsonlite fromJSON
#' @importFrom utils getFromNamespace packageVersion unzip
NULL

# Global Variables
PRODUCTION <- "dataspace.cavd.org"
STAGING <- "dataspace-staging.cavd.org"
