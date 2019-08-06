context("DataSpaceConnection")

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

    metanames <- c("important_info",
                   "export_date",
                   "data_summary_level",
                   "filters_applied")

    studnames <- c("network",
                   "prot",
                   "grant_pi_name",
                   "investigator_name",
                   "primary_poc_name",
                   "primary_poc_email",
                   "description",
                   "type",
                   "species",
                   "access_level")

    varinames <- c("field_name",
                   "caption",
                   "description")

    cklist <- list(mab$studyAndMabs,
                   mab$mabs,
                   mab$nabMab,
                   mab$metadata,
                   mab$studies,
                   mab$assays,
                   mab$variableDefinitions)

    ## check that all tables have at least some values in them
    expect_true(all(sapply(cklist, function(i) !all(sapply(i, function(j) all(is.na(j)))))))

    ## check that all tables have at least a single record in them
    expect_true(all(sapply(cklist, function(i) nrow(i) > 0)))

    ## check columns that were set by the package after making making an API call
    expect_equal(names(cklist[[4]]), metanames)
    expect_equal(names(cklist[[5]]), studnames)
    expect_equal(names(cklist[[7]]), varinames)

    ## test multiple mab mixtures
    con$resetMabGrid()
    con$filterMabGrid("mab_mixture", c("PGT128", "PGT121", "PGT125"))
    mab <- con$getMab()$mabs
    expect_equal(length(setdiff(mab$mab_mix_name_std, c("PGT128", "PGT121", "PGT125"))), 0)

})
