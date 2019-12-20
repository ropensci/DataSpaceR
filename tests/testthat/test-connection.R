context("DataSpaceConnection")

con <- try(connectDS(onStaging = onStaging), silent = TRUE)

test_that("can connect to DataSpace", {
  expect_is(con, "DataSpaceConnection")
  expect_is(con, "R6")
})

if ("DataSpaceConnection" %in% class(con)) {
  con_names <- c(
    ".__enclos_env__",
    "mabGrid",
    "mabGridSummary",
    "availableGroups",
    "availableStudies",
    "config",
    "clone",
    "refresh",
    "getMab",
    "resetMabGrid",
    "filterMabGrid",
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
        expect_equal(cap_output[2], paste0("  URL: ", baseUrl))
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
        expect_equal(con$config$labkeyUrlBase, baseUrl)
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
          "study_name", "short_name", "title", "type", "status", "stage",
          "species", "start_date", "strategy", "network", "data_availability"
        )
      )
      expect_gt(nrow(con$availableStudies), 0)
    })

    test_that("`availableGroups`", {
      expect_is(con$availableGroups, "data.table")
      expect_equal(
        names(con$availableGroups),
        c(
          "id", "label", "original_label", "description", "created_by",
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

    test_that("`resetMabGrid`", {
      oriCnt <- nrow(con$mabGridSummary)
      con$filterMabGrid("mab_mixture", c("mAb 96"))
      expect_true(oriCnt > nrow(con$mabGridSummary))
      con$resetMabGrid()
      expect_true(oriCnt == nrow(con$mabGridSummary))
    })

    test_that("Test `filterMab` errors, warnings, and subsetting.", {
      expect_error(
        con$filterMabGrid("mab_mixture", "NotAMab"),
        regexp = "NotAMab set to the `value` argument is/are not found in the column set in the `using` argument.\nOnly returning values found."
      )
      expect_error(
        con$filterMabGrid("mab_mixture", c("NotAMab1", "NotAMab2", "NotAMab3", "NotAMab4")),
        regexp = "NotAMab1, NotAMab2, NotAMab3, and others set to the `value` argument is/are not found in the column set in the `using` argument.\nOnly returning values found."
      )
      expect_warning(
        con$filterMabGrid("mab_mixture", c("PGT121", "NotAMab")),
        regexp = "NotAMab set to the `value` argument is/are not found in the column set in the `using` argument.\nOnly returning values found."
      )
      con$resetMabGrid()
      expect_warning(
        con$filterMabGrid("mab_mixture", c("PGT121", "NotAMab1", "NotAMab2", "NotAMab3")),
        regexp = "NotAMab1, NotAMab2, NotAMab3 set to the `value` argument is/are not found in the column set in the `using` argument.\nOnly returning values found."
      )
      con$resetMabGrid()
      expect_warning(
        con$filterMabGrid("mab_mixture", c("PGT121", "NotAMab1", "NotAMab2", "NotAMab3", "NotAMab4")),
        regexp = "NotAMab1, NotAMab2, NotAMab3, and others set to the `value` argument is/are not found in the column set in the `using` argument.\nOnly returning values found."
      )
      con$resetMabGrid()
      expect_warning(
        con$filterMabGrid("mab_mixture", c("PGT121", "PGT121", "PGT125", "PGT125", "PGT125", "NotAMab1", "NotAMab2", "NotAMab3", "NotAMab4")),
        regexp = "NotAMab1, NotAMab2, NotAMab3, and others set to the `value` argument is/are not found in the column set in the `using` argument.\nOnly returning values found."
      )
      expect_true(nrow(con$mabGridSummary) == 2)
      con$resetMabGrid()
    })

    test_that("Test `assertColumn`", {
      expect_error(
        con$filterMabGrid(c("This", "That"), "A Thing"),
        regexp = "May only pass one column at a time"
      )
      expect_error(
        con$filterMabGrid("This", "A Thing"),
        regexp = "\"This\" is not a valid column in the mabGrid."
      )
    })

    test_that("Test geomean calculation", {
      con$filterMabGrid("mab_mixture", "mAb 96")
      mab <- con$getMab()$nabMab
      expect_equal(all(mab$titer_curve_ic50 %in% c(-Inf, Inf)), is.na(con$mabGridSummary$geometric_mean_curve_ic50))
      con$resetMabGrid()

      con$filterMabGrid("mab_mixture", "PGDM1400")
      mab <- con$getMab()$nabMab
      expect_true(round(con$mabGridSummary$geometric_mean_curve_ic50, 2) == 0.05)
      con$resetMabGrid()
    })

    test_that("getMab pulls mAb data when filters are not set.", {
      con$resetMabGrid()
      mab <- con$getMab()
      expect_true(nrow(mab$nabMab) > 0)

      con <- try(connectDS(), silent = TRUE)
      mab <- con$getMab()
      expect_true(nrow(mab$nabMab) > 0)
    })

    test_that("Check for warnings.", {
      con$filterMabGrid(using = "hxb2_location", value = c("Env", "gp160"))
      expect_true(length(warnings()) == 0)
    })
  }
}
