#' CreateConnection
#'
#' @param study A \code{"character"} vector naming the study.
#' @param login A \code{"character"}. Optional argument. If there is no netrc
#' file a temporary one can be written by passing login and password of an
#' active DataSpace account.
#' @param password A \code{"character"}. Optional. The password for the selected
#' login.
#' @param verbose A \code{"logical"} whether to print the extra details for
#' troubleshooting.
#'
#' @return an instance of an \code{DataSpaceConnection}
#'
#' @examples
#' \dontrun{
#' # Single study
#' con <- CreateConnection("cvd408")
#' # Cross study
#' con <- CreateConnection("")
#' }
#' @export
#' @importFrom utils packageVersion
CreateConnection <- function(study = NULL,
                             login = NULL,
                             password = NULL,
                             verbose = FALSE) {
  assert_that(length(study) <= 1, msg = "For multiple studies, use an empty string and filter the connection.")
  assert_that(is.logical(verbose))

  labkey.url.base <- try(get("labkey.url.base", .GlobalEnv), silent = TRUE)
  if(inherits(labkey.url.base, "try-error")) labkey.url.base <- "https://dataspace-staging.cavd.org"
  labkey.url.base <- gsub("http:", "https:", labkey.url.base)
  if(length(grep("^https://", labkey.url.base)) == 0) labkey.url.base <- paste0("https://", labkey.url.base)

  labkey.user.email <- try(get("labkey.user.email", .GlobalEnv), silent = TRUE)
  if(inherits(labkey.user.email, "try-error")) labkey.user.email <- "unknown_user at not_a_domain.com"

  if(!is.null(login) & !is.null(password)) {
    # nf <- write_netrc(login, password)
    stop("please manually write .netrc file.")
  } else {
    nf <- try(get("labkey.netrc.file", .GlobalEnv), silent = TRUE)
  }
  if(!inherits(nf, "try-error") && !is.null(nf)) {
    curlOptions <- labkey.setCurlOptions(ssl.verifyhost = 2,
                                         sslversion = 1,
                                         netrc.file = nf,
                                         useragent = paste("DataSpaceR", packageVersion("DataSpaceR")))
  } else {
    curlOptions <- labkey.setCurlOptions(ssl.verifyhost = 2,
                                         sslversion = 1,
                                         useragent = paste("DataSpaceR", packageVersion("DataSpaceR")))
  }

  study <- tolower(study)
  labkey.url.path <- try(get("labkey.url.path", .GlobalEnv), silent = TRUE)
  if(inherits(labkey.url.path,"try-error")) {
    if(is.null(study)) {
      stop("study cannot be NULL")
    }
    labkey.url.path <- paste0("/CAVD/", study)
  } else if(!is.null(study)) {
    labkey.url.path <- file.path(dirname(labkey.url.path), study)
  }

  validStudies <- grep("\\w+\\d+", basename(lsFolders(getSession(labkey.url.base, "CAVD"))), value = T)
  req_study <- basename(study)
  if(!req_study %in% validStudies) {
    if(!verbose) {
      stop(paste0(req_study, " is not a valid study"))
    } else {
      stop(paste0(req_study, " is not a valid study\nValid studies: ",
                  paste(validStudies, collapse=", ")))
    }
  }

  config <- list(labkey.url.base = labkey.url.base,
                 labkey.url.path = labkey.url.path,
                 labkey.user.email = labkey.user.email,
                 curlOptions = curlOptions,
                 verbose = verbose)

  available_datasets <- labkey.selectRows(baseUrl = config$labkey.url.base,
                                          folderPath = config$labkey.url.path,
                                          schemaName = "study",
                                          queryName = "Datasets",
                                          colNameOpt = "fieldname",
                                          colSelect = c("Name", "Label", "Category", "Description", "Modified", "DataSetId"))

  .DSCon(study = basename(labkey.url.path),
         config = config,
         available_datasets = available_datasets)
}
