context("Connection")

test_that("can connect to DataSpace", {
  con <- try(CreateConnection(""), silent = TRUE)
  expect_is(con, "DataSpaceConnection")
})
