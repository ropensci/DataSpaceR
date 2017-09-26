context("DataSpaceStudy")

con <- connectDS()
cavd <- try(con$getStudy(""), silent = TRUE)

test_that("can connect to studies", {
  expect_is(cavd, "DataSpaceStudy")
  expect_is(cavd, "R6")
})

if ("DataSpaceStudy" %in% class(cavd)) {
  con_names <- c(".__enclos_env__",
                 "treatmentArm",
                 "cache",
                 "availableDatasets",
                 "config",
                 "study",
                 "clone",
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
      con_output <- c("<DataSpaceStudy>",
                      "  Study: CAVD",
                      "  URL: https://dataspace.cavd.org/CAVD",
                      "  Available datasets:",
                      "    - BAMA",
                      "    - Demographics",
                      "    - ELISPOT",
                      "    - ICS",
                      "    - NAb")
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
      expect_is(cavd$availableDatasets, "data.frame")
      expect_equal(names(cavd$availableDatasets),
                   c("name", "label", "n"))
      expect_equal(cavd$availableDatasets$name,
                   c("BAMA", "Demographics", "ELISPOT", "ICS", "NAb"))
    })

    test_that("`study`", {
      expect_equal(cavd$study, "")
    })

    test_that("`cache`", {
      expect_is(cavd$cache, "list")
      expect_length(cavd$cache, 0)
    })

    test_that("`treatmentArm`", {
      expect_is(cavd$treatmentArm, "data.frame")
      expect_equal(names(cavd$treatmentArm),
                   c("arm_id", "arm_part", "arm_group", "arm_name",
                     "randomization", "coded_label", "last_day", "description"))
      expect_gt(nrow(cavd$treatmentArm), 0)
    })

    test_that("`getDataset`", {
      for (datasetName in cavd$availableDatasets$name) {
        dataset <- try(cavd$getDataset(datasetName = datasetName), silent = TRUE)
        expect_is(dataset, "data.frame", info = datasetName)
        expect_gt(nrow(dataset), 0)
      }
    })

    test_that("`clear_cache`", {
      clearCache <- try(cavd$clearCache(), silent = TRUE)
      expect_is(clearCache, "list")
      expect_length(clearCache, 0)
    })

    test_that("`getVariableInfo`", {
      for (datasetName in cavd$availableDatasets$name) {
        dataset <- try(cavd$getVariableInfo(datasetName = datasetName), silent = TRUE)
        expect_is(dataset, "data.frame", info = datasetName)
        expect_gt(nrow(dataset), 0)
        expect_equal(names(dataset), c("fieldName", "caption", "type", "description"))
      }
    })
  }
}
