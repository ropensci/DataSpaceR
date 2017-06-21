context("Connection")

test_that("can connect to DataSpace", {
  con <- try(connectDS(""), silent = TRUE)
  expect_is(con, "DataSpaceConnection")
})
