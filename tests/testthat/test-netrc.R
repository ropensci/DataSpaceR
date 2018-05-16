context("netrc")

test_that("`writeNetrc`", {
  netrcFile <- try(writeNetrc("fake@email.org", "fakePwd"), silent = TRUE)

  expect_is(netrcFile, "character")
  expect_true(file.exists(netrcFile))
  expect_match(readLines(netrcFile),
               "machine dataspace.cavd.org login fake@email.org password fakePwd")
})

test_that("`checkNetrc`", {
  netrcFile <- try(checkNetrc(), silent = TRUE)

  expect_is(netrcFile, "character")
  expect_true(file.exists(netrcFile))
  expect_message(checkNetrc(), "it looks valid")
})
