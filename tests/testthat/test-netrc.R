context("netrc")

test_that("`writeNetrc`", {
  netrcFile <- try(writeNetrc(Sys.getenv("DS_login"), Sys.getenv("DS_pwd")), silent = TRUE)

  expect_is(netrcFile, "character")
  expect_true(file.exists(netrcFile))
  expect_match(readLines(netrcFile),
               "machine\\s+dataspace.cavd.org\\s+login\\s+\\S+\\s+password\\s+\\S+")
})

test_that("`checkNetrc`", {
  netrcFile <- try(checkNetrc(), silent = TRUE)

  expect_is(netrcFile, "character")
  expect_true(file.exists(netrcFile))
})
