context("DataSpaceConnection")

con <- try(connectDS(), silent = TRUE)

test_that("can connect to DataSpace", {
  expect_is(con, "DataSpaceConnection")
  expect_is(con, "R6")
})

if ("DataSpaceConnection" %in% class(con)) {
  con_names <- c(
    ".__enclos_env__",
    "availableGroups",
    "availableStudies",
    "config",
    "clone",
    "refresh",
    "getGroup",
    "getStudy",
    "print",
    "initialize"
  )
  test_that("`DataSpaceConnection`` contains correct fields and methods", {
    expect_equal(names(con), con_names)
  })

  if (identical(names(con), con_names)) {
    test_that("`print`", {
      cap_output <- capture.output(con$print())
      expect_length(cap_output, 8)

      if (length(cap_output) == 8) {
        expect_equal(cap_output[1], "<DataSpaceConnection>")
        expect_equal(cap_output[2], "  URL: https://dataspace.cavd.org")
        expect_match(cap_output[3], "  User: \\S+@\\S+")
        expect_match(cap_output[4], "  Available studies: \\d+")
        expect_match(cap_output[5], "  - \\d+ studies with data")
        expect_match(cap_output[6], "  - \\d+ subjects")
        expect_match(cap_output[7], "  - \\d+ data points")
        expect_match(cap_output[8], "  Available groups: \\d+")
      }
    })

    test_that("`config`", {
      configs <- c(
        "labkeyUrlBase",
        "labkeyUserEmail",
        "curlOptions",
        "verbose",
        "packageVersion"
      )
      curlOptions <- c(
        "ssl_verifyhost",
        "ssl_verifypeer",
        "followlocation",
        "sslversion",
        "netrc_file",
        "useragent"
      )
      useragent <- paste0(
        "R/", R.version$major, ".", R.version$minor,
        " (", Sys.info()["sysname"], " ", Sys.info()["machine"], ")",
        " Rlabkey/", packageVersion("Rlabkey"),
        " DataSpaceR/", packageVersion("DataSpaceR")
      )

      expect_is(con$config, "list")
      expect_equal(names(con$config), configs)

      if (all.equal(names(con$config), configs)) {
        expect_equal(con$config$labkeyUrlBase, "https://dataspace.cavd.org")
        expect_match(con$config$labkeyUserEmail, "\\S+@\\S+")
        expect_false(con$config$verbose)
        expect_equal(con$config$packageVersion, packageVersion("DataSpaceR"))
        expect_is(con$config$curlOptions, "request")

        if (is(con$config$curlOptions, "request")) {
          expect_equal(names(con$config$curlOptions$options), curlOptions)

          if (all.equal(names(con$config$curlOptions$options), curlOptions)) {
            expect_equal(con$config$curlOptions$options$ssl_verifyhost, 2)
            expect_true(con$config$curlOptions$options$ssl_verifypeer)
            expect_true(con$config$curlOptions$options$followlocation)
            expect_equal(con$config$curlOptions$options$sslversion, 1)
            expect_equal(con$config$curlOptions$options$useragent, useragent)
            expect_match(con$config$curlOptions$options$netrc_file, "netrc")
          }
        }
      }
    })

    test_that("`availableStudies`", {
      expect_is(con$availableStudies, "data.table")
      expect_equal(
        names(con$availableStudies),
        c(
          "study_name", "short_name", "title", "type", "status",
          "stage", "species", "start_date", "strategy"
        )
      )
      expect_gt(nrow(con$availableStudies), 0)
    })

    test_that("`availableGroups`", {
      expect_is(con$availableGroups, "data.table")
      expect_equal(
        names(con$availableGroups),
        c(
          "id", "label", "originalLabel", "description", "createdBy",
          "shared", "n", "studies"
        )
      )
      expect_gt(nrow(con$availableGroups), 0)
    })

    test_that("`getStudy`", {
      cavd <- try(con$getStudy(""), silent = TRUE)

      expect_is(cavd, "DataSpaceStudy")
      expect_is(cavd, "R6")

      expect_error(con$getStudy("cvd0"))
    })

    test_that("`refresh`", {
      refresh <- try(con$refresh(), silent = TRUE)

      expect_is(refresh, "logical")
      expect_true(refresh)
    })
  }
}
