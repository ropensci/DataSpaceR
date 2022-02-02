context("DataSpaceMab")

con <- try(connectDS(), silent = TRUE)

test_that("can connect to DataSpace", {
  expect_is(con, "DataSpaceConnection")
  expect_is(con, "R6")
})

test_that("test mab object", {
  con$filterMabGrid("isotype", "IgG1")
  con$filterMabGrid("hxb2_location", "Env")
  con$filterMabGrid("clade", "B")
  con$filterMabGrid("mab_mixture", "PGT128")

  mab <- con$getMab()

  expect_equal(unique(mab$nabMab$mab_mix_label), "PGT128")
  expect_equal(unique(mab$nabMab$clade), "B")

  expect_true(max(nchar(mab$variableDefinitions$description), na.rm = T) > 50)

  studnames <- c(
    "network",
    "prot",
    "grant_pi_name",
    "investigator_name",
    "primary_poc_name",
    "primary_poc_email",
    "description",
    "type",
    "species",
    "access_level"
  )

  varinames <- c(
    "field_name",
    "caption",
    "description"
  )

  cklist <- list(
    mab$studyAndMabs,
    mab$mabs,
    mab$nabMab,
    mab$studies,
    mab$assays,
    mab$variableDefinitions
  )

  ## check that all tables have at least some values in them
  expect_true(all(sapply(cklist, function(i) !all(sapply(i, function(j) all(is.na(j)))))))

  ## check that all tables have at least a single record in them
  expect_true(all(sapply(cklist, function(i) nrow(i) > 0)))

  ## check columns that were set by the package after making making an API call
  expect_equal(names(cklist[[4]]), studnames)
  expect_equal(names(cklist[[6]]), varinames)

  ## test multiple mab mixtures
  con$resetMabGrid()
  con$filterMabGrid("mab_mixture", c("PGT128", "PGT121", "PGT125"))
  mab <- con$getMab()$mabs
  expect_equal(length(setdiff(mab$mab_mix_name_std, c("PGT128", "PGT121", "PGT125"))), 0)
})

test_that("test mab object results", {
  con$resetMabGrid()
  con$filterMabGrid("mab_mixture", "CH27")
  mab <- con$getMab()
  expect_true(all(sapply(mab$assays, function(x) !is.na(x))))
  expect_true(all(sapply(mab$mabs, function(x) !is.na(x))))
  expect_true(all(sapply(mab$studies, function(x) !is.na(x))))
  expect_true(all(sapply(mab$studyAndMabs, function(x) !is.na(x))))
  expect_true(all(mab$assays$prot %in% c("cvd409", "cvd425")))
  expect_true(all(mab$nabMab$virus %in% c("MN.3", "PVO.4", "TH023.6", "Ce0682_E4", "SHIV_C3", "SHIV_C4", "SHIV_C5")))
  expect_true(all(mab$studies$species %in% c("Non-Organism Study")))
  expect_true(all(mab$studyAndMabs$mab_mix_name_std %in% c("CH27")))
  expect_true(nrow(mab$variableDefinitions) == 51)
  expect_true(ncol(mab$variableDefinitions) == 3)
  expect_true(length(unique(mab$nabMab$prot)) == nrow(mab$studies))
  expect_true(length(unique(mab$studyAndMabs$prot)) == nrow(mab$studies))
})

test_that("test lanl metadata merge and duplicates in mab table", {
  con$resetMabGrid()
  con$filterMabGrid("mab_mixture", "PGT128")
  mab <- con$getMab()
  expect_true(nrow(mab$mabs) == 1)
  mab$getLanlMetadata()
  expect_true(class(mab$mabs$lanl_metadata) == "list")
  expect_true(length(mab$mabs$lanl_metadata[[1]]) == 4)
  expect_true(names(mab$mabs$lanl_metadata[[1]])[1] == "epitopes")
  expect_true("data.table" %in% class(mab$mabs$lanl_metadata[[1]]$epitopes$binding_type[[1]]))
})
