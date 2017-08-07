context("Framework")

con <- try(connectDS(""), silent = TRUE)

test_that("can connect to DataSpace", {
  expect_is(con, "DataSpaceConnection")
  expect_is(con, "R6")
})

if ("DataSpaceConnection" %in% class(con)) {
  con_names <- c(".__enclos_env__",
                 "cache",
                 "availableDatasets",
                 "config",
                 "study",
                 "clone",
                 "getVariableInfo",
                 "clearCache",
                 "getDataset",
                 "getAvailableDatasets",
                 "print",
                 "initialize")
  test_that("`DataSpaceConnection`` contains correct fields and methods", {
    expect_equal(names(con), con_names)
  })

  if (identical(names(con), con_names)) {
    test_that("`print`", {
      con_output <- c("DataSpace Connection to CAVD",
                      "URL: https://dataspace-staging.cavd.org/project/CAVD/",
                      # "User: unknown_user at not_a_domain.com",
                      "Available datasets",
                      "\tBAMA",
                      "\tDemographics",
                      "\tELISPOT",
                      "\tICS",
                      "\tNAb")
      cap_output <- capture.output(con$print())
      expect_equal(cap_output, con_output)
    })

    test_that("`config`", {
      configs <- c("labkey.url.base", "labkey.url.path", "labkey.user.email",
                   "curlOptions", "verbose")

      expect_is(con$config, "list")
      expect_equal(names(con$config), configs)
    })

    test_that("`availableDatasets`", {
      expect_is(con$availableDatasets, "data.frame")
      expect_equal(names(con$availableDatasets),
                   c("name", "label", "n"))
      expect_equal(con$availableDatasets$name,
                   c("BAMA", "Demographics", "ELISPOT", "ICS", "NAb"))
    })

    test_that("`study`", {
      expect_equal(con$study, "")
    })

    test_that("`cache`", {
      expect_is(con$cache, "list")
      expect_length(con$cache, 0)
    })

    test_that("`getAvailableDatasets`", {
      availableDatasets <- try(con$getAvailableDatasets(), silent = TRUE)
      expect_is(availableDatasets, "data.frame")
    })

    test_that("`getDataset`", {
      for (datasetName in con$availableDatasets$name) {
        dataset <- try(con$getDataset(datasetName = datasetName), silent = TRUE)
        expect_is(dataset, "data.frame", info = datasetName)
        expect_gt(nrow(dataset), 0)
      }
    })

    test_that("`clear_cache`", {
      clearCache <- try(con$clearCache(), silent = TRUE)
      expect_is(clearCache, "list")
      expect_length(clearCache, 0)
    })

    test_that("`getVariableInfo`", {
      for (datasetName in con$availableDatasets$Name) {
        dataset <- try(con$getVariableInfo(datasetName = datasetName), silent = TRUE)
        expect_is(dataset, "data.frame", info = datasetName)
        expect_gt(nrow(dataset), 0)
        expect_equal(names(dataset), c("fieldName", "caption", "type", "description"))
      }
    })
  }
}
