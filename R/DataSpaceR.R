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
#' @importFrom data.table %like% copy data.table fread rbindlist setDT setkey setkeyv setnames setorder setcolorder shift := .SD
#' @importFrom digest digest
#' @importFrom httr content GET
#' @importFrom jsonlite fromJSON
#' @importFrom utils getFromNamespace packageVersion unzip
NULL

# Global Variables
PRODUCTION <- "dataspace.cavd.org"
STAGING <- "dataspace-staging.cavd.org"

utils::globalVariables(
  c(
    ".", ".SD", ":=", "allele", "caption", "chain", "d_score", "description", "destination", "donor_code",
    "donor_id", "fieldName", "field_name", "header", "j_score", "mab_id", "mab_name_std",
    "machine", "remote_path", "run_application", "run_information", "score", "sequence_aa",
    "sequence_id", "sequence_nt", "success", "total_score", "unzipDir", "v_score"
  )
)
