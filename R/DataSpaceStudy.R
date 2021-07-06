#' The DataSpaceStudy class
#'
#' @section Constructor:
#' \code{DataSpaceConnection$getStudy()}
#' \code{DataSpaceConnection$getGroup()}
#'
#' @seealso \code{\link{connectDS}} \code{\link{DataSpaceConnection}}
#'
#' @examples
#' \dontrun{
#' # Create a connection (Initiate a DataSpaceConnection object)
#' con <- connectDS()
#'
#' # Connect to cvd408 (Initiate a DataSpaceStudy object)
#' # https://dataspace.cavd.org/cds/CAVD/app.view#learn/learn/Study/cvd408?q=408
#' cvd408 <- con$getStudy("cvd408")
#' cvd408
#'
#' # Retrieve Neutralizing antibody dataset (NAb) for cvd408 from DataSpace
#' NAb <- cvd408$getDataset("NAb")
#'
#' # Get variable information of the NAb dataset
#' cvd408$getDatasetDescription("NAb")
#'
#' # Take a look at cvd408's treatment arm information
#' cvd408$treatmentArm
#'
#' # Clear cache of a study object
#' cvd408$clearCache()
#'
#' # Connect to the NYVAC durability comparison group
#' # https://dataspace.cavd.org/cds/CAVD/app.view#group/groupsummary/220
#' nyvac <- con$getGroup(220)
#'
#' # Connect to all studies
#' cvd <- con$getStudy("")
#'
#' # Refresh the study object to update available datasets and treatment info
#' cvd$refresh()
#' }
#'
#' @importFrom data.table fread
#' @importFrom digest digest
#' @importFrom Rlabkey labkey.getQueryDetails labkey.executeSql labkey.webdav.get
DataSpaceStudy <- R6Class(
  classname = "DataSpaceStudy",
  public = list(

    #' @description
    #' Initialize \code{DataSpaceStudy} class.
    #' See \code{\link{DataSpaceConnection}}.
    #' @param study A character. Name of the study to retrieve.
    #' @param config A list. Stores configuration of the connection object such
    #' as URL, path and username.
    #' @param group An integer. ID of the group to retrieve.
    #' @param studyInfo A list. Stores the information about the study.
    initialize = function(study = NULL,
                          config = NULL,
                          group = NULL,
                          studyInfo = NULL) {
      assert_that(
        length(study) <= 1,
        msg = "For multiple studies, create a group in the portal."
      )
      assert_that(!is.null(config))

      # get primary fields
      config$labkeyUrlPath <- getUrlPath(study)

      # fix study
      study <- fixStudy(study, config$labkeyUrlBase, config$labkeyUrlPath)

      # set primary fields
      private$.study <- study
      private$.config <- config
      private$.group <- group
      private$.studyInfo <- studyInfo
      private$.dataDir <- tempdir()

      # get extra fields if available
      self$refresh()

      NULL
    },

    #' @description
    #' Print \code{DataSpaceStudy} class.
    print = function() {
      study <- ifelse(private$.study == "", "CAVD", private$.study)
      url <- file.path(
        gsub("/$", "", private$.config$labkeyUrlBase),
        gsub("^/", "", private$.config$labkeyUrlPath)
      )

      cat("<DataSpaceStudy>")
      if (is.null(private$.group)) {
        cat("\n  Study:", study)
      } else {
        cat("\n  Group:", private$.group)
      }
      cat("\n  URL:", url)
      cat("\n  Available datasets:")
      if (nrow(private$.availableDatasets) > 0) {
        cat(paste0("\n    - ", private$.availableDatasets$label), sep = "")
      }
      cat("\n  Available non-integrated datasets:")
      if (nrow(private$.availableNIDatasets) > 0) {
        cat(paste0("\n    - ", private$.availableNIDatasets$label), sep = "")
      }
      cat("\n")
    },

    #' @description
    #' Get a dataset from the connection.
    #' @param datasetName A character. Name of the dataset to retrieve.
    #' Accepts the value in either the "name" or "label" field from \code{availableDatasets}.
    #' @param mergeExtra A logical. If set to TRUE, merge extra information.
    #' Ignored for non-integrated datasets.
    #' @param colFilter A matrix. A filter as returned by Rlabkey's
    #' \code{\link[Rlabkey]{makeFilter}}.
    #' @param reload A logical. If set to TRUE, download the dataset, whether
    #' a cached version exist or not.
    #' @param outputDir A character. Optional, specifies directory to download
    #' nonstandard datasets. If \code{NULL}, data will be downloaded to
    #' \code{dataDir}, set with \code{setDataDir(dataDir)}. If \code{dataDir}
    #' is not set, and \code{outputDir} is \code{NULL}, a tmp directory will be
    #' used.
    #' @param ... Extra arguments to be passed to
    #' \code{\link[Rlabkey]{labkey.selectRows}}
    getDataset = function(datasetName,
                          mergeExtra = FALSE,
                          colFilter = NULL,
                          reload = FALSE,
                          outputDir = NULL,
                          ...) {
      assert_that(is.character(datasetName))
      assert_that(length(datasetName) == 1)
      assert_that(
        datasetName %in% self$availableDatasets$name | datasetName %in% self$availableDatasets$label,
        msg = paste0(datasetName, " is invalid dataset")
      )
      assert_that(is.logical(mergeExtra))
      assert_that(
        is.null(colFilter) | is.matrix(colFilter),
        msg = "colFilter is not a matrix"
      )
      assert_that(is.logical(reload))

      # Use dataset name instead of label
      if (datasetName %in% self$availableDatasets$label) {
        datasetName <- self$availableDatasets[label == datasetName]$name
      }

      # build a list of arguments to digest and compare
      args <- list(
        datasetName = datasetName,
        mergeExtra = mergeExtra,
        colFilter = colFilter,
        ...
      )

      # retrieve dataset from cache if arguments match
      digestedArgs <- digest(args)
      if (digestedArgs %in% names(private$.cache)) {
        if (!reload) {
          return(private$.cache[[digestedArgs]]$data)
        }
      }

      # Retrieve dataset
      if (self$availableDatasets[name == datasetName]$integrated) {
        # build a colFilter for group
        if (!is.null(private$.group)) {
          colFilter <- rbind(
            colFilter,
            makeFilter(c(
              paste0("SubjectId/", names(private$.group)),
              "EQUAL",
              private$.group
            ))
          )
        }

        # retrieve dataset
        dataset <- labkey.selectRows(
          baseUrl = private$.config$labkeyUrlBase,
          folderPath = private$.config$labkeyUrlPath,
          schemaName = "study",
          queryName = datasetName,
          viewName = NULL,
          colNameOpt = "fieldname",
          colFilter = colFilter,
          method = "GET",
          ...
        )

        ## set variable names that are returned as camel case with slash to snake case
        setnames(
          dataset,
          names(dataset),
          tolower(
            gsub("/[A-z]+", "", 
                 gsub("_+", "_",
                      gsub("([A-z]+|[0-9]+)_?([A-Z])", "\\1_\\2",
                           gsub("([A-Z])([A-Z])", "\\1\\L\\2", names(dataset), perl = TRUE)
                           )
                      )
                 )
          )
        )
          
        # convert to data.table
        setDT(dataset)
      } else {
        datasetDir <- private$.availableNIDatasets[name == datasetName]$localPath
        dataset <- NULL

        # First check to see if it already exists
        if (!reload) {
          if (is.na(datasetDir)) {
            remotePath <- private$.availableNIDatasets[name == datasetName]$remotePath
            datasetDir <- file.path(private$.getOutputDir(outputDir), gsub(".zip", "", basename(remotePath)))
          }

          try(
            {
              files <- list.files(datasetDir)
              datacsv <- grep(".csv", files, value = TRUE)
              dataset <- fread(file.path(datasetDir, datacsv))
              private$.availableNIDatasets[name == datasetName, localPath := datasetDir]
            },
            silent = TRUE
          )
        }

        # if loading fails, commence to download.
        if (reload || !"data.table" %in% class(dataset)) {
          datasetDir <- private$.downloadNIDataset(datasetName, outputDir)
          files <- list.files(datasetDir)
          datacsv <- grep(".csv", files, value = TRUE)
          dataset <- fread(file.path(datasetDir, datacsv))
        }

        # update 'n'
        private$.availableNIDatasets[name == datasetName, n := nrow(dataset)]

        # change "subject_id" to "participant_id" to be consistent with other datasets
        setnames(dataset, c("subject_id", "prot"), c("participant_id", "study_prot"), skip_absent = TRUE)
      }

      # merge extra information
      if (args$mergeExtra) {

        # mergeExtra is only allowed for integrated datasets
        if (!self$availableDatasets[name == datasetName]$integrated) {
          message("'mergeExtra' is not allowed for non-integrated datsets. Skipping.")
        } else {
          if (!identical(datasetName, "Demographics")) {
            dem <- self$getDataset("Demographics")

            subj <- ifelse(private$.study == "", "subject", "participant")
            cols <- c(paste0(subj, "_visit"), "study_prot")
            dem <- dem[, -cols, with = FALSE]

            key <- paste0(subj, "_id")
            setkeyv(dem, key)
            setkeyv(dataset, key)

            dataset <- dataset[dem, nomatch = 0]
          }

          # create arm_id column with demographics and set it as key
          dataset[, arm_id := paste(
            study_prot, study_part, study_group, study_arm,
            sep = "-"
          )]
          setkey(dataset, arm_id)

          dataset[private$.treatmentArm, nomatch = 0]
          setkey(dataset, NULL)
        }
      }

      # caching
      private$.cache[[digestedArgs]] <- list(
        args = args,
        data = dataset
      )

      dataset
    },

    #' @description
    #' Clear \code{cache}. Remove downloaded datasets.
    clearCache = function() {
      private$.cache <- list()
    },

    #' @description
    #' Get variable information.
    #' @param datasetName A character. Name of the dataset to retrieve.
    #' Accepts the value in either the "name" or "label" field from \code{availableDatasets}.
    #' @param outputDir A character. Directory path.
    getDatasetDescription = function(datasetName, outputDir = NULL) {
      assert_that(is.character(datasetName))
      assert_that(length(datasetName) == 1)
      assert_that(
        datasetName %in% self$availableDatasets$name | datasetName %in% self$availableDatasets$label,
        msg = paste0(datasetName, " is not a available dataset")
      )

      # Use dataset name instead of label
      if (datasetName %in% self$availableDatasets$label) {
        datasetName <- self$availableDatasets[label == datasetName]$name
      }

      if (self$availableDatasets[name == datasetName]$integrated) {
        varInfo <- labkey.getQueryDetails(
          baseUrl = private$.config$labkeyUrlBase,
          folderPath = private$.config$labkeyUrlPath,
          schemaName = "study",
          queryName = datasetName
        )

        # convert to data.table and set key
        setDT(varInfo)
        setkey(varInfo, fieldName)

        extraVars <- c(
          "Created", "CreatedBy", "Modified", "ModifiedBy",
          "SequenceNum", "date"
        )

        varInfo <- varInfo[
          isHidden == "FALSE" &
            isSelectable == "TRUE" &
            !fieldName %in% extraVars,
          .(fieldName, caption, type, description)
        ]

        return(varInfo)
      } else {
        # Download and unzip dataset if not already downloaded
        datasetDir <- private$.availableNIDatasets[name == datasetName]$localPath

        if (is.na(datasetDir)) {
          remotePath <- private$.availableNIDatasets[name == datasetName]$remotePath
          datasetDir <- file.path(private$.getOutputDir(outputDir), gsub(".zip", "", basename(remotePath)))
        }

        # Check to see if it already exists
        files <- list.files(datasetDir)
        fileFormatPdf <- grep("file_format.pdf", files, value = TRUE)
        if (length(fileFormatPdf) == 1) {
          private$.availableNIDatasets[name == datasetName]$localPath <- datasetDir
        } else {
          datasetDir <- private$.downloadNIDataset(datasetName, outputDir)
          files <- list.files(datasetDir)
          fileFormatPdf <- grep("file_format.pdf", files, value = TRUE)
        }

        # View pdf, using method borrowed from Biobase::openPDF
        # https://github.com/Bioconductor/Biobase/blob/6017663b35b7380c7d8b09e6ec8a1c1087a7bd62/R/tools.R#L39
        if (isWindows()) {
          shell.exec(file.path(datasetDir, fileFormatPdf))
        } else {
          pdf <- getOption("pdfviewer")
          msg <- NULL
          if (is.null(pdf)) {
            msg <- "getOption('pdfviewer') is NULL"
          } else if (length(pdf) == 1 && nchar(pdf[[1]]) == 0) {
            msg <- "getOption('pdfviewer') is ''"
          }
          if (!is.null(msg)) {
            stop(msg, "; please use 'options(pdfviewer=...)'")
          }
          cmd <- paste(pdf, file.path(datasetDir, fileFormatPdf))
          system(cmd)
        }
      }
    },

    #' @description
    #' Set default directory to download non-integrated datasets. If no
    #' \code{dataDir} is set, a tmp directory will be used.
    #' @param dataDir A character. Directory path.
    setDataDir = function(dataDir) {
      if (length(dataDir) == 0) {
        private$.dataDir <- character()
      } else {
        assert_that(file.exists(dataDir))
        assert_that(dir.exists(dataDir))

        private$.dataDir <- normalizePath(dataDir)
      }

      invisible(self)
    },

    #' @description
    #' Refresh the study object to update available datasets and treatment info.
    refresh = function() {
      tries <- c(
        class(try(
          private$.getAvailableDatasets(),
          silent = private$.config$verbose
        )),
        class(try(
          private$.getAvailableNIDatasets(),
          silent = private$.config$verbose
        )),
        class(try(
          private$.getTreatmentArm(),
          silent = private$.config$verbose
        ))
      )

      invisible(!"try-error" %in% tries)
    }
  ),

  active = list(

    #' @field study A character. The study name.
    study = function() {
      private$.study
    },

    #' @field config A list. Stores configuration of the connection object such
    #' as URL, path and username.
    config = function() {
      private$.config
    },

    #' @field availableDatasets A data.table. The table of datasets available in
    #' the \code{DataSpaceStudy} object.
    availableDatasets = function() {
      rbind(
        private$.availableDatasets[, .(name,
          label,
          n,
          integrated = rep(TRUE, nrow(private$.availableDatasets))
        )],
        private$.availableNIDatasets[, .(name,
          label,
          n,
          integrated = rep(FALSE, nrow(private$.availableNIDatasets))
        )]
      )
    },

    #' @field cache A list. Stores the data to avoid downloading the same tables
    #' multiple times.
    cache = function() {
      private$.cache
    },

    #' @field dataDir A character. Default directory for storing nonstandard
    #' datasets. Set with \code{setDataDir(dataDir)}.
    dataDir = function() {
      private$.dataDir
    },

    #' @field treatmentArm A data.table. The table of treatment arm
    #' information for the connected study. Not available for all study
    #' connection.
    treatmentArm = function() {
      private$.treatmentArm
    },

    #' @field group A character. The group name.
    group = function() {
      private$.group
    },

    #' @field studyInfo A list. Stores the information about the study.
    studyInfo = function() {
      private$.studyInfo
    }
  ),

  private = list(
    .study = character(),
    .config = list(),
    .availableDatasets = data.table(),
    .availableNIDatasets = data.table(),
    .cache = list(),
    .dataDir = character(),
    .treatmentArm = data.table(),
    .group = character(),
    .studyInfo = list(),

    .getAvailableDatasets = function() {
      datasetQuery <-
        paste(
          "SELECT",
          "DataSets.Name as name,",
          "DataSets.Label as label,",
          "dataset_n.n",
          "FROM",
          "(",
          makeCountQuery("ICS", private$.group),
          "UNION",
          makeCountQuery("BAMA", private$.group),
          "UNION",
          makeCountQuery("ELISPOT", private$.group),
          "UNION",
          makeCountQuery("NAb", private$.group),
          "UNION",
          makeCountQuery("Demographics", private$.group),
          "UNION",
          makeCountQuery("PKMAb", private$.group),
          ") AS dataset_n,",
          "DataSets",
          "WHERE",
          "Datasets.Name = dataset_n.Name AND dataset_n.n > 0"
        )

      availableDatasets <- suppressWarnings(
        labkey.executeSql(
          baseUrl = private$.config$labkeyUrlBase,
          folderPath = private$.config$labkeyUrlPath,
          schemaName = "study",
          sql = datasetQuery,
          colNameOpt = "fieldname"
        )
      )

      # convert to data.table
      setDT(availableDatasets)

      private$.availableDatasets <- availableDatasets[order(name)]
    },

    .getAvailableNIDatasets = function() {
      document <- labkey.selectRows(
        private$.config$labkeyUrlBase,
        folderPath = private$.config$labkeyUrlPath,
        schemaName = "CDS",
        queryName = "document",
        colNameOpt = "fieldname",
        colSelect = c("document_id", "label", "filename", "document_type", "assay_identifier")
      )
      studydocument <- labkey.selectRows(
        private$.config$labkeyUrlBase,
        folderPath = private$.config$labkeyUrlPath,
        schemaName = "CDS",
        queryName = "studydocument",
        colNameOpt = "fieldname",
        colSelect = c("document_id", "prot")
      )
      niDatasets <- merge(document, studydocument, by = "document_id")

      # convert to data.table
      setDT(niDatasets)

      # need to subset by study because document tables don't use study filters
      niDatasets <- niDatasets[
        document_type == "Non-Integrated Assay" & prot == private$.study,
        .(
          name = assay_identifier,
          label,
          remotePath = filename,
          localPath = as.character(NA),
          n = as.integer(NA),
          document_id
        )
      ]
      private$.availableNIDatasets <- niDatasets
    },

    .getTreatmentArm = function() {
      colSelect <- c(
        "arm_id", "arm_part", "arm_group", "arm_name",
        "randomization", "coded_label", "last_day", "description"
      )

      treatmentArm <- suppressWarnings(
        labkey.selectRows(
          baseUrl = private$.config$labkeyUrlBase,
          folderPath = private$.config$labkeyUrlPath,
          schemaName = "CDS",
          queryName = "treatmentarm",
          colSelect = colSelect,
          colNameOpt = "fieldname",
          colFilter = makeFilter(c("arm_id", "CONTAINS", private$.study)),
          method = "GET"
        )
      )

      # convert to data.table
      setDT(treatmentArm)

      # set key
      setkey(treatmentArm, arm_id)

      private$.treatmentArm <- treatmentArm
    },

    .downloadNIDataset = function(datasetName, outputDir = NULL) {
      remotePath <- private$.availableNIDatasets[name == datasetName]$remotePath
      outputDir <- private$.getOutputDir(outputDir)

      fileName <- basename(remotePath)
      localZipPath <- file.path(outputDir, fileName)
      fullOutputDir <- file.path(outputDir, gsub(".zip", "", fileName))

      message("downloading ", fileName, " to ", outputDir, "...")
      # use getStudyDocument.view
      # https://dataspace.cavd.org/cds/CAVD/getStudyDocument.view?
      # study=vtn505&documentId=916&filename=study_documents%2Fvtn505_adcp.zip

      # Use getStudyDocumentUrl.view to download
      getStudyDocumentUrl <- paste0(
        private$.config$labkeyUrlBase,
        "/cds/CAVD/getStudyDocument.view?",
        "study=", private$.study,
        "&documentId=", private$.availableNIDatasets[name == datasetName]$document_id,
        "&filename=", gsub("/", "%2F", private$.availableNIDatasets[name == datasetName]$remotePath)
      )

      # Use labkey.webdav.getByUrl which includes filesystem and permissions checks
      ret <- Rlabkey:::labkey.webdav.getByUrl(getStudyDocumentUrl, localFilePath = localZipPath, overwrite = TRUE)
      if (!is.null(ret) && !is.na(ret) && ret == FALSE) {
        success <- FALSE
      } else {
        success <- file.exists(localZipPath)
      }

      if (success) {
        message("unzipping ", fileName, " to ", fullOutputDir)
        unzip(localZipPath, exdir = fullOutputDir)
        unlink(localZipPath)
      } else {
        stop(paste0("Could not create ", fullOutputDir))
      }

      private$.availableNIDatasets[name == datasetName, localPath := fullOutputDir]

      return(fullOutputDir)
    },

    .getOutputDir = function(outputDir = NULL) {
      if (!is.null(outputDir)) {
        if (dir.exists(outputDir)) {
          return(outputDir)
        } else {
          stop(paste0(outputDir, " is not a directory."))
        }
      } else if (length(private$.dataDir) == 1) {
        return(private$.dataDir)
      } else {
        return(tempdir())
      }
    }
  )
)
