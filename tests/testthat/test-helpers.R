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
  expect_equal(DataSpaceR:::getUserEmail(url, NULL), email)

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
