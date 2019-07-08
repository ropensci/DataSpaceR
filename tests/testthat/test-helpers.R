context("helpers")

test_that("`getUrlBase`", {
  PRODUCTION <- "https://dataspace.cavd.org"
  STAGING <- "https://dataspace-staging.cavd.org"

  expect_equal(DataSpaceR:::getUrlBase(FALSE), PRODUCTION)
  expect_equal(DataSpaceR:::getUrlBase(TRUE), STAGING)

  assign("labkey.url.base", PRODUCTION, envir = .GlobalEnv)
  expect_equal(DataSpaceR:::getUrlBase(FALSE), PRODUCTION)
  expect_equal(DataSpaceR:::getUrlBase(TRUE), PRODUCTION)
  suppressWarnings(rm("labkey.url.base", envir = .GlobalEnv))
})

test_that("`getUserEmail`", {
  url <- "https://dataspace.cavd.org"
  email <- "jkim2345@scharp.org"

  expect_equal(DataSpaceR:::getUserEmail(url, email), email)
  expect_match(DataSpaceR:::getUserEmail(url, NULL), "\\w+@\\w+")

  assign("labkey.user.email", email, envir = .GlobalEnv)
  expect_equal(DataSpaceR:::getUserEmail(url, email), email)
  suppressWarnings(rm("labkey.user.email", envir = .GlobalEnv))
})

test_that("`getUrlPath`", {
  expect_equal(DataSpaceR:::getUrlPath(""), "/CAVD")
  expect_equal(DataSpaceR:::getUrlPath("cvd123"), "/CAVD/cvd123")
  expect_error(DataSpaceR:::getUrlPath(NULL), "'study' cannot be NULL.")

  assign("labkey.url.path", "/CAVD", envir = .GlobalEnv)
  expect_equal(DataSpaceR:::getUrlPath(NULL), "/CAVD")
  expect_equal(DataSpaceR:::getUrlPath("cvd123"), "/CAVD/cvd123")
  suppressWarnings(rm("labkey.url.path", envir = .GlobalEnv))
})

test_that("`checkStudy`", {
  url <- "https://dataspace.cavd.org"
  expect_null(DataSpaceR:::checkStudy("cvd232", url))
  expect_error(DataSpaceR:::checkStudy("hello", url))
  expect_error(DataSpaceR:::checkStudy("hello", url, verbose = TRUE))
})

test_that("`fixStudy`", {
  study <- "cvd232"
  url <- "https://dataspace.cavd.org"
  path <- "/CAVD/cvd232"

  expect_equal(DataSpaceR:::fixStudy(study, url, path), study)
  expect_equal(DataSpaceR:::fixStudy(NULL, url, path), study)
})

test_that("`getNetrc`", {
  expect_match(DataSpaceR:::getNetrc(NULL, NULL), "netrc")
  expect_is(DataSpaceR:::getNetrc("user", "password"), "character")

  assign("labkey.netrc.file", "/there/.netrc", envir = .GlobalEnv)
  expect_equal(DataSpaceR:::getNetrc(NULL, NULL), "/there/.netrc")
  suppressWarnings(rm("labkey.netrc.file", envir = .GlobalEnv))
})
