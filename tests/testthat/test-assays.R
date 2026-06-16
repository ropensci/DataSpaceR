context("Assays")

con <- connectDS(onStaging = onStaging)

test_that("Integrated Data", {

  studies <-
    con$availableStudies[
      study_id == "vtn505"
    ] |> con$getStudies()

  studies$loadAvailableDatasets()

  expect_true(
    all(names(studies$datasets) == c("BAMA", "Demographics", "ICS", "NAb", "ADCP", "DEM SUPP", "Fc Array"))
  )

  expect_true(
    nrow(studies$datasets$BAMA) != 0
  )

  expect_true(
    all(c("vtn505") %in% studies$availablePublications$related_studies)
  )

  paths <- studies$availablePublications |>
    studies$downloadPublicationData()

  expect_true(
    all(
      paths %in%
        list.files(tempdir(), full.names = TRUE)
    )
  )

  expect_equal(
    names(studies$datasets$BAMA), .BAMANAMES
  )

  expect_true(
    all(studies$variableDefinitions$BAMA$field_name %in% names(studies$datasets$BAMA))
  )

  expect_equal(
    names(studies$datasets$NAb), .NABNAMES
  )

  expect_true(
    all(studies$variableDefinitions$NAB$field_name %in% names(studies$datasets$NAB))
  )
  
  expect_equal(
    names(studies$datasets$ICS), .ICSNAMES
  )

  expect_true(
    all(studies$variableDefinitions$ICS$field_name %in% names(studies$datasets$ICS))
  )

})
