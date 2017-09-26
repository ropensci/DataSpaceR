context("DataSpaceConnection")

con <- try(connectDS(), silent = TRUE)

test_that("can connect to DataSpace", {
  expect_is(con, "DataSpaceConnection")
  expect_is(con, "R6")
})

if ("DataSpaceConnection" %in% class(con)) {
  con_names <- c(".__enclos_env__",
                 "availableStudies",
                 "config",
                 "clone",
                 "getStudy",
                 "print",
                 "initialize")
  test_that("`DataSpaceConnection`` contains correct fields and methods", {
    expect_equal(names(con), con_names)
  })

  if (identical(names(con), con_names)) {
    test_that("`print`", {
      con_output <- c("DataSpaceR Connection to CAVD",
                      "URL: https://dataspace.cavd.org/CAVD",
                      "User: jkim2345@scharp.org",
                      "Available datasets:",
                      "\tBAMA",
                      "\tDemographics",
                      "\tELISPOT",
                      "\tICS",
                      "\tNAb")
      # cap_output <- capture.output(con$print())
      # expect_equal(cap_output, con_output)
    })

    test_that("`config`", {
      configs <- c("labkey.url.base", "labkey.user.email", "curlOptions", "verbose")

      expect_is(con$config, "list")
      expect_equal(names(con$config), configs)
    })

    test_that("`availableStudies`", {
      expect_is(con$availableStudies, "data.frame")
      expect_equal(names(con$availableStudies),
                   c("study_name", "title"))
      expect_gt(nrow(con$availableStudies), 0)
    })

    test_that("`getStudy`", {
      cavd <- try(con$getStudy(""), silent = TRUE)

      expect_is(cavd, "DataSpaceStudy")
      expect_is(cavd, "R6")
    })
  }
}
