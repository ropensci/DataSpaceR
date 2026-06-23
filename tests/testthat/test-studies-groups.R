context("Studies and Groups")

con <- connectDS(onStaging = onStaging)

test_that("DataSpaceStudies", {

  expect_error(
    con$availablePublications |>
      con$getStudies(),
    "Argument must be `character` or `availableStudies`"
  )

  stu <- con$availableStudies[study_id %in% c("vtn505", "cvd123")] |>
    con$getStudies()

  stuout <- capture.output(stu$print())

  expect_true(
    all(
      c(
        "  Available integrated datasets:"      ,
        "    - Binding Ab multiplex assay"      ,
        "    - Demographics"                    ,
        "    - Intracellular Cytokine Staining" ,
        "    - Neutralizing antibody"           ,
        "  Available non-integrated datasets:"  ,
        "    - ADCP"                            ,
        "    - Demographics (Supplemental)"     ,
        "    - Fc Array"                        ,
        "  Available publication datasets:"     ,
        "    - Fong 2018 (J Infect Dis) Data"   ,
        "    - Hammer 2013 (N Engl J Med) Data" ,
        "    - Janes 2017 (J Infect Dis) Data"
      ) %in% stuout
    )
  )

  expect_true(
    nrow(stu$treatmentArm) == 2
  )

  expect_true(
    nrow(stu$studyInfo) == 1
  )

  pubs <- stu$availablePublications |>
    stu$downloadPublicationData()

  expect_true(
    all(pubs %in% list.files(tempdir(), full.names = T))
  )

  Map(unlink, pubs) |>
    invisible()

  expect_equal(
    nrow(stu$availableStudies), 1
  )

  expect_equal(
    nrow(stu$availableMabs), 0
  )

  c514 <- con$availableStudies[study_id == "cvd514"] |>
    con$getStudies()

  expect_true(
    nrow(c514$availableMabMixtures) == 1 &
      nrow(c514$availableMabs) == 1
  )

})

test_that("DataSpaceGroups", {

  expect_true(
    "NYVAC_durability" %in% con$availableGroups$original_label
  )

  grp <- con$getGroups(con$availableGroups[label %in% c("CAVD 239 integrated data", "NYVAC durability comparison")])

  expect_equal(
    names(grp$datasets), c("BAMA", "Demographics", "ELISPOT", "ICS", "NAb")
  )

  grp$availableDatasets

  expect_equal(
    names(grp$variableDefinitions$NAb), c("field_name", "caption", "description")
  )

  expect_true(
    "target_cell" %in% grp$variableDefinitions$NAb$field_name
  )

  expect_true(
    nrow(grp$availableMabMixtures) == 0 &
      nrow(grp$availableMabs) == 0
  )

})


