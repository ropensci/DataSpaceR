context("netrc")

test_that("`writeNetrc`", {
  temp <- tempfile()
  netrcFile <- try(
    writeNetrc("fake@email.org", "fakePwd", netrcFile = temp),
    silent = TRUE
  )

  expect_is(netrcFile, "character")
  expect_equal(netrcFile, temp)
  expect_true(file.exists(netrcFile))
  expect_match(
    readLines(temp),
    "machine dataspace.cavd.org login fake@email.org password fakePwd"
  )
  expect_error(
    writeNetrc("fake@email.org", "fakePwd", netrcFile = temp),
    "Set `overwrite=TRUE` if you'd like to overwrite."
  )
})

test_that("`checkNetrc`", {
  netrcFile <- try(
    checkNetrc(),
    silent = TRUE
  )

  expect_is(netrcFile, "character")
  expect_true(file.exists(netrcFile))
  expect_message(checkNetrc(), "it looks valid")

  assign("labkey.netrc.file", "/there/.netrc", envir = .GlobalEnv)
  expect_error(DataSpaceR:::checkNetrc(), "There is no netrc file.")
  suppressWarnings(rm("labkey.netrc.file", envir = .GlobalEnv))
})
