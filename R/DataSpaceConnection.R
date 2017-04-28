#' A Reference Class to represent a bank account.
#'
#' @field study A length-one numeric vector.
#' @field config A length-one numeric vector.
#' @field available_datasets A length-one numeric vector.
#' @export
.DSCon <- setRefClass(
  Class = "DataSpaceConnection",
  fields = list(
    study = "character",
    config = "list",
    available_datasets = "data.frame"
  ),
  methods = list(
    initialize = function(study = NULL,
                          login = NULL,
                          password = NULL,
                          verbose = FALSE) {
      "Initialize class"
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

      study <<- tolower(study)
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
      if(!req_study %in% c("", validStudies)) {
        if(!verbose) {
          stop(paste0(req_study, " is not a valid study"))
        } else {
          stop(paste0(req_study, " is not a valid study\nValid studies: ",
                      paste(validStudies, collapse=", ")))
        }
      }

      config <<- list(labkey.url.base = labkey.url.base,
                      labkey.url.path = labkey.url.path,
                      labkey.user.email = labkey.user.email,
                      curlOptions = curlOptions,
                      verbose = verbose)
      getAvailableDatasets()
    },
    show = function() {
      "Show class"
      url <- file.path(gsub("/$", "", config$labkey.url.base), "project", gsub("^/", "", config$labkey.url.path))
      cat(paste0("DataSpace Connection to ", study))
      cat(paste0("\nURL: ", url))
      cat(paste0("\nUser: ", config$labkey.user.email))
      cat("\nAvailable datasets")
      for(name in available_datasets$Name) {
        cat(paste0("\n\t", name))
      }
    },
    getAvailableDatasets = function() {
      "Get availabel datasets"
      datasetQuery <-
        "
        SELECT
        DataSets.Name,
        DataSets.Label,
        DataSets.CategoryId AS Category,
        DataSets.DataSetId AS Id,
        dataset_n.n
        FROM
        (
        SELECT COUNT(participantid) AS n, 'ICS' AS Name
        FROM ICS
        UNION
        SELECT COUNT(participantid) AS n, 'BAMA' AS Name
        FROM BAMA
        UNION
        SELECT COUNT(participantid) AS n, 'ELISPOT' AS Name
        FROM ELISPOT
        UNION
        SELECT COUNT(participantid) AS n, 'NAb' AS Name
        FROM NAb
        UNION
        SELECT COUNT(participantid) AS n, 'Demographics' AS Name
        FROM Demographics
        ) AS dataset_n,
        DataSets
        WHERE Datasets.Name = dataset_n.Name AND dataset_n.n > 0
       "
      available_datasets <<- suppressWarnings(labkey.executeSql(baseUrl = config$labkey.url.base,
                                                                     folderPath = config$labkey.url.path,
                                                                     schemaName = "study",
                                                                     sql = datasetQuery,
                                                                     colNameOpt = "fieldname"))
    },
    getDataset = function(dataset_name,
                          original_view = FALSE,
                          colFilter = NULL,
                          ...) {
      "get dataset!"
      assert_that(is.character(dataset_name))
      assert_that(length(dataset_name) == 1)
      assert_that(dataset_name %in% available_datasets$Name, msg = paste0(dataset_name, " is invalid dataset."))
      assert_that(is.logical(original_view))

      # if(!is.null(data_cache[[cache_name]]) & !reload & is.null(colFilter) & nOpts == 0) { # Serve cache
      #   data <- data_cache[[cache_name]]
      #   #if(!is.null(colFilter)) {
      #   #  data <- filter_cached_copy(colFilter, data)
      #   #  return(data)
      #   #} else {
      #   #}
      # } else { # Download the data
      #   viewName <- NULL
      #   if(original_view) {
      #     viewName <- "full"
      #   }
      #   if(!is.null(colFilter)) {
      #     colFilter <- .check_filter(config$labkey.url.base,
      #                                config$labkey.url.path,
      #                                "study",
      #                                x,
      #                                viewName,
      #                                colFilter)
      #     cache <- FALSE
      #   } else if(length(nOpts) > 0) {
      #     cache <- FALSE
      #   } else {
      #       cache <- TRUE
      #   }
      labkey.selectRows(baseUrl = config$labkey.url.base,
                        folderPath = config$labkey.url.path,
                        schemaName = "study",
                        queryName = dataset_name,
                        # viewName = viewName,
                        colNameOpt = "fieldname",
                        colFilter = colFilter,
                        ...)
    }
  )
)
