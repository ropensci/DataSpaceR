library(testthat)
library(DataSpaceR)

if (identical(tolower(Sys.getenv("NOT_CRAN")), "true")) {
  test_check("DataSpaceR")
}
