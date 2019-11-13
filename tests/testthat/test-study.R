library(testthat)
onStaging <- TRUE

con <- connectDS(onStaging = onStaging)

test_study <- function(study, datasets, niDatasets = c(), groupId = NULL, groupLabel = NULL) {
  datasets <- sort(datasets)
  niDatasets <- sort(niDatasets)
  target <- ifelse(
    study != "",
    study,
    ifelse(
      is.null(groupLabel),
      "CAVD",
      groupLabel
    )
  )

  context(paste0("DataSpaceStudy (", target, ")"))

  if (is.null(groupId)) {
    cavd <- try(con$getStudy(study), silent = TRUE)
  } else {
    cavd <- try(con$getGroup(groupId), silent = TRUE)
  }

  test_that("can connect to studies", {
    expect_is(cavd, "DataSpaceStudy")
    expect_is(cavd, "R6")
  })

  if ("DataSpaceStudy" %in% class(cavd)) {
    con_names <- c(
      ".__enclos_env__",
      "studyInfo",
      "group",
      "treatmentArm",
      "dataDir",
      "cache",
      "availableDatasets",
      "config",
      "study",
      "clone",
      "setDataDir",
      "refresh",
      "getDatasetDescription",
      "clearCache",
      "getDataset",
      "print",
      "initialize"
    )
    test_that("`DataSpaceStudy` contains correct fields and methods", {
      expect_equal(names(cavd), con_names)
    })

    if (identical(names(cavd), con_names)) {
      test_that("`print`", {
        path <- ifelse(study == "", study, paste0("/", study))
        con_output <- c(
          "<DataSpaceStudy>",
          ifelse(is.null(groupLabel),
            paste0("  Study: ", ifelse(study == "", "CAVD", study)),
            paste0("  Group: ", groupLabel)
          ),
          paste0("  URL: https://dataspace-staging.cavd.org/CAVD", path),
          "  Available datasets:",
          strwrap(datasets, prefix = "    - "),
          "  Available non-integrated datasets:",
          strwrap(niDatasets, prefix = "    - ")
        )
        cap_output <- capture.output(cavd$print())
        expect_equal(cap_output, con_output)
      })

      test_that("`config`", {
        configs <- c(
          "labkeyUrlBase",
          "labkeyUserEmail",
          "curlOptions",
          "verbose",
          "packageVersion",
          "labkeyUrlPath"
        )

        expect_is(cavd$config, "list")
        expect_equal(names(cavd$config), configs)
      })

      test_that("`availableDatasets`", {
        expect_is(cavd$availableDatasets, "data.table")
        expect_equal(
          names(cavd$availableDatasets),
          c("name", "label", "n", "integrated")
        )
        expect_equal(
          cavd$availableDatasets$name,
          c(datasets, niDatasets)
        )
      })

      test_that("`study`", {
        expect_equal(cavd$study, study)
      })

      test_that("`cache`", {
        expect_is(cavd$cache, "list")
        expect_length(cavd$cache, 0)
      })

      test_that("`treatmentArm`", {
        expect_is(cavd$treatmentArm, "data.table")
        expect_equal(
          names(cavd$treatmentArm),
          c(
            "arm_id", "arm_part", "arm_group", "arm_name",
            "randomization", "coded_label", "last_day", "description"
          )
        )
        expect_gt(nrow(cavd$treatmentArm), 0)
      })

      test_that("`group`", {
        expect_equal(cavd$group, groupLabel)
      })


      test_that("`studyInfo`", {
        if (study == "") {
          expect_null(cavd$studyInfo)
        } else {
          expect_is(cavd$studyInfo, "list")
          expect_gt(length(cavd$studyInfo), 0)
        }
      })

      test_that("`setDataDir`, `getOutputDir`", {
        getOutputDir <- cavd$.__enclos_env__$private$.getOutputDir
        expect_equal(getOutputDir(), tempdir())
        expect_equal(getOutputDir("."), getwd())

        cavd$setDataDir(".")
        expect_equal(cavd$dataDir, getwd())
        expect_equal(getOutputDir(), getwd())
        expect_equal(getOutputDir(tempdir()), tempdir())

        cavd$setDataDir(NULL)
        expect_equal(getOutputDir(), tempdir())

      })

      test_that("`.downloadNIDataset`", {
        niDatasets <- cavd$.__enclos_env__$private$.availableNIDatasets
        downloadNIDataset <- cavd$.__enclos_env__$private$.downloadNIDataset
        if (nrow(niDatasets) > 0) {
          for (datasetName in niDatasets$name) {
            path <- downloadNIDataset(datasetName)
            expect_equal(path, niDatasets[name == datasetName]$localPath)
            expect_equal(cavd$.__enclos_env__$private$.getOutputDir(), dirname(path))
            expect_true(dir.exists(path))
            expect_gt(length(list.files(path)), 0)

            path <- downloadNIDataset(datasetName, outputDir = ".")
            expect_equal(getwd(), dirname(path))
            expect_equal(path, niDatasets[name == datasetName]$localPath)
            expect_true(dir.exists(path))
            expect_gt(length(list.files(path)), 0)
            unlink(path, recursive = TRUE)

            cavd$setDataDir(".")
            path <- downloadNIDataset(datasetName)
            expect_equal(getwd(), dirname(path))
            expect_equal(cavd$dataDir, dirname(path))
            expect_equal(cavd$.__enclos_env__$private$.getOutputDir(), dirname(path))
            expect_equal(niDatasets[name == datasetName]$localPath, path)
            expect_true(dir.exists(path))
            expect_gt(length(list.files(path)), 0)
            unlink(path, recursive = TRUE)

          }
        }
      })

      test_that("`getDataset`", {
        for (datasetName in cavd$availableDatasets[integrated == FALSE]$name) {
          dataset <- try(cavd$getDataset(datasetName), silent = TRUE)
          expect_is(dataset, "data.table", info = datasetName)
          expect_gt(nrow(dataset), 0)
        }
      })

      test_that("`getDataset` (access cache)", {
        for (i in seq_len(nrow(cavd$availableDatasets))) {
          datasetName <- cavd$availableDatasets$name[i]
          datasetN <- cavd$availableDatasets$n[i]
          dataset <- try(cavd$getDataset(datasetName), silent = TRUE)
          expect_is(dataset, "data.table", info = datasetName)
          expect_equal(nrow(dataset), datasetN)
        }
      })

      test_that("`getDataset` (mergeExtra)", {
        for (datasetName in cavd$availableDatasets$name) {
          dataset <- try(
            cavd$getDataset(datasetName, mergeExtra = TRUE),
            silent = TRUE
          )
          expect_is(dataset, "data.table", info = datasetName)
          expect_gt(nrow(dataset), 0)
          expect_true("arm_id" %in% names(dataset))
        }
      })

      test_that("`clear_cache`", {
        skip_if_not_installed("pryr")

        expect_gt(length(cavd$cache), 0)

        before <- pryr::object_size(cavd)
        clearCache <- try(cavd$clearCache(), silent = TRUE)
        after <- pryr::object_size(cavd)

        expect_is(clearCache, "list")
        expect_length(cavd$cache, 0)
        expect_lte(after, before)
      })

      test_that("`getDatasetDescription`", {
        for (datasetName in cavd$availableDatasets$name) {
          dataset <- try(
            cavd$getDatasetDescription(datasetName = datasetName),
            silent = TRUE
          )
          expect_is(dataset, "data.table", info = datasetName)
          expect_gt(nrow(dataset), 0)
          expect_equal(
            names(dataset),
            c("fieldName", "caption", "type", "description")
          )
        }
      })

      test_that("`refresh`", {
        refresh <- try(cavd$refresh(), silent = TRUE)

        expect_is(refresh, "logical")
        expect_true(refresh)
      })




    }
  }
}

test_study(
  study = "",
  datasets = c("BAMA", "ICS", "ELISPOT", "Demographics", "NAb")
)
test_study(
  study = "cvd408",
  datasets = c("BAMA", "ICS", "Demographics", "NAb")
)
test_study(
  study = "vtn505",
  datasets = c("BAMA", "Demographics", "ICS", "NAb"),
  niDatasets = c("ADCP", "DEM SUPP", "Fc Array")
)
test_study(
  study = "",
  datasets = c("BAMA", "ICS", "ELISPOT", "Demographics", "NAb"),
  groupId = 220,
  groupLabel = c("NYVAC_durability" = "NYVAC durability comparison")
)
test_study(
  study = "",
  datasets = c("BAMA", "Demographics", "ICS", "NAb"),
  groupId = ifelse(onStaging, 226, 228),
  groupLabel = c("HVTN 505 case control subjects" = "HVTN 505 case control subjects")
)

email <- DataSpaceR:::getUserEmail(DataSpaceR:::PRODUCTION, NULL)
if (identical(email, "jkim2345@scharp.org")) {
  test_study(
    study = "",
    datasets = c("Demographics", "NAb"),
    groupId = 216,
    groupLabel = c("mice" = "mice")
  )
  test_study(
    study = "",
    datasets = c("Demographics", "NAb"),
    groupId = 217,
    groupLabel = c("CAVD 242" = "CAVD 242")
  )
}
