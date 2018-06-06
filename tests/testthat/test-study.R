
test_study <- function(study, datasets, groupId = NULL, groupLabel = NULL) {
  datasets <- sort(datasets)
  target <- ifelse(study != "", study, ifelse(is.null(groupLabel), "CAVD", groupLabel))
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
    con_names <- c(".__enclos_env__",
                   "studyInfo",
                   "group",
                   "treatmentArm",
                   "cache",
                   "availableDatasets",
                   "config",
                   "study",
                   "clone",
                   "refresh",
                   "getVariableInfo",
                   "clearCache",
                   "getDataset",
                   "print",
                   "initialize")
    test_that("`DataSpaceStudy` contains correct fields and methods", {
      expect_equal(names(cavd), con_names)
    })

    if (identical(names(cavd), con_names)) {
      test_that("`print`", {
        path <- ifelse(study == "", study,  paste0("/", study))
        con_output <- c("<DataSpaceStudy>",
                        ifelse(is.null(groupLabel),
                               paste0("  Study: ", ifelse(study == "", "CAVD", study)),
                               paste0("  Group: ", groupLabel)),
                        paste0("  URL: https://dataspace.cavd.org/CAVD", path),
                        "  Available datasets:",
                        strwrap(datasets, prefix = "    - "))
        cap_output <- capture.output(cavd$print())
        expect_equal(cap_output, con_output)
      })

      test_that("`config`", {
        configs <- c("labkey.url.base", "labkey.user.email", "curlOptions",
                     "verbose", "labkey.url.path")

        expect_is(cavd$config, "list")
        expect_equal(names(cavd$config), configs)
      })

      test_that("`availableDatasets`", {
        expect_is(cavd$availableDatasets, "data.table")
        expect_equal(names(cavd$availableDatasets),
                     c("name", "label", "n"))
        expect_equal(cavd$availableDatasets$name,
                     datasets)
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
        expect_equal(names(cavd$treatmentArm),
                     c("arm_id", "arm_part", "arm_group", "arm_name",
                       "randomization", "coded_label", "last_day", "description"))
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

      test_that("`getDataset`", {
        for (datasetName in cavd$availableDatasets$name) {
          dataset <- try(cavd$getDataset(datasetName = datasetName), silent = TRUE)
          expect_is(dataset, "data.table", info = datasetName)
          expect_gt(nrow(dataset), 0)
        }
      })

      test_that("`getDataset` (access cache)", {
        for (datasetName in cavd$availableDatasets$name) {
          dataset <- try(cavd$getDataset(datasetName = datasetName), silent = TRUE)
          expect_is(dataset, "data.table", info = datasetName)
          expect_gt(nrow(dataset), 0)
        }
      })

      test_that("`clear_cache`", {
        expect_gt(length(cavd$cache), 0)

        before <- pryr::object_size(cavd)
        clearCache <- try(cavd$clearCache(), silent = TRUE)
        after <- pryr::object_size(cavd)

        expect_is(clearCache, "list")
        expect_length(cavd$cache, 0)
        expect_lte(after, before)
      })

      test_that("`getVariableInfo`", {
        for (datasetName in cavd$availableDatasets$name) {
          dataset <- try(cavd$getVariableInfo(datasetName = datasetName), silent = TRUE)
          expect_is(dataset, "data.table", info = datasetName)
          expect_gt(nrow(dataset), 0)
          expect_equal(names(dataset), c("fieldName", "caption", "type", "description"))
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

con <- connectDS()

test_study("", c("BAMA", "Demographics", "ELISPOT", "ICS", "NAb"))
test_study("cvd408", c("BAMA", "Demographics", "ICS", "NAb"))
test_study("", c("Demographics", "NAb"), groupId = 216, groupLabel = "mice")
test_study("", c("Demographics", "NAb"), groupId = 217, groupLabel = "CAVD 242")
