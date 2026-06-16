context("Connection")

con <- connectDS(onStaging = onStaging)

test_that("can connect to DataSpace", {
  expect_is(con, "DataSpaceConnection")
  expect_is(con, "R6")
})

if ("DataSpaceConnection" %in% class(con)) {

  test_that("`DataSpaceConnection`` contains correct fields and methods", {

    cap_output <- capture.output(con$print())
    expect_length(cap_output, 26)
    if (length(cap_output) == 10) {
      expect_equal(cap_output[1], "<DataSpaceConnection>")
      expect_equal(cap_output[2], paste0("  URL: ", baseUrl))
      expect_match(cap_output[3], "  User: \\S+@\\S+$")
      expect_match(cap_output[4], "  Available studies: \\d+")
      expect_match(cap_output[5], "  - \\d+ studies with data")
      expect_match(cap_output[6], "  - \\d+ subjects")
      expect_match(cap_output[7], "  - \\d+ data points")
      expect_match(cap_output[8], "  Available groups: \\d+")
      expect_match(cap_output[9], "  Available publications: \\d+")
      expect_match(cap_output[10], "  - \\d+ publications")
    }

    expect_equal(
      cap_output[11:26],
      c(
        "  Available Connection objects:",
        "    - availableDonors"          ,
        "    - availableGroups"          ,
        "    - availableMabMixtures"     ,
        "    - availableMabs"            ,
        "    - availablePublications"    ,
        "    - availableStudies"         ,
        "    - availableViruses"         ,
        "    - virusNameMappingTables"   ,
        "  Available Connection methods:",
        "    - downloadPublicationData"  ,
        "    - getDaash"                 ,
        "    - getDonors"                ,
        "    - getGroups"                ,
        "    - getMabs"                  ,
        "    - getStudies"
      )
    )
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

  expect_error(
    con$getStudy(),
    "getStudies"
  )

  expect_error(
    con$getGroup(),
    "getGroups"
  )

  expect_error(
    con$getMab(),
    "getMabs"
  )

  expect_error(
    con$filterMabGrid(),
    "availableMabs"
  )

  expect_error(
    con$resetMabGrid(),
    "availableMabs"
  )

  expect_error(
    con$mabGridSummary,
    "availableMabs"
  )

  expect_error(
    con$mabGrid(),
    "availableMabs"
  )

  expect_error(
    con$virusMetadata,
    "virusNameMappingTables"
  )

  expect_message(
    downloadDocs <- con$availablePublications[
      publication_data_available == TRUE &
        !is.na(studies_with_data)
    ] |>
      con$downloadPublicationData(downloadDir = tempdir()),
    "Publications have been downloaded to"
  )

  expect_error(
    con$downloadPublicationData("not an id"),
    "No datasets to download from `availablePublications`."
  )

  expect_equal(
    length(downloadDocs %in% list.files(tempdir(), full.names = TRUE)), 5
  )

  Map(unlink, downloadDocs) |>
    invisible()

  test_that("`refresh`", {
    refresh <- con$refresh()
    expect_true(is.null(refresh))
  })
}

