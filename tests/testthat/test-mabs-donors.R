context("MAbs and Donors")

con <- connectDS(onStaging = onStaging)

test_that("DataSpaceMabs", {

  expect_error(
    con$getMabs("mab123", includeMixtures = "only"),
    "No mAbs available"
  )

  expect_error(
    con$getMabs(con$availablePublications, includeMixtures = "only"),
    "Argument must be `character`, `availableMabs`, or `availableMabMixtures`."
  )

  mabs <- con$availableMabs[mab_name_std %in% c("VRC01", "PGT121")]

  mabyes  <- con$getMabs(mabs)
  mabno   <- con$getMabs(mabs, includeMixtures = "no")
  mabonly <- con$getMabs(mabs, includeMixtures = "only")

  expect_true(
    nrow(mabyes$mabMetadata) > 0 &
      nrow(mabyes$mabMetadata) == nrow(mabyes$availableMabs) &
      "availableMabs" %in% class(mabyes$mabMetadata)
  )

  cap_out <- capture.output(mabyes$print())
  expect_true(
    all(
      c(
        "URL"                         ,
        "User"                        ,
        "Summary"                     ,
        "Available MAb objects"       ,
        "Available MAb methods"       ,
        "Available Connection objects",
        "Available Connection methods",
        "8 studies"                   ,
        "2 mAb mixtures"              ,
        "2 mAbs"                      ,
        "9 nAb mAb assay neutralization tiers",
        "13 nAb mAb assay virus clades"
      ) |>
        sapply(
          \(.) any(grepl(., cap_out))
        ) |> unlist()
    )
  )

  expect_true(
    nrow(mabonly$mabMixMetadata) <
      nrow(mabno$mabMixMetadata)
    &
    nrow(mabonly$mabMixMetadata) <
      nrow(mabyes$mabMixMetadata)
  )

  mabmix <- con$availableMabMixtures[mab_mix_name_std %like% "PGDM1400"]

  mixyes <-
    expect_message(
      con$getMabs(mabmix),
      "All mabs in all mixtures passed with be returned"
    )

  expect_true(
    grepl(" - 5 mAbs.+ - 7 mAb mixtures", capture_output(mixyes$print()))
  )

  mixno <-
    expect_message(
      con$getMabs(mabmix, includeMixtures="no"),
      "All mabs in all mixtures passed with be returned"
    )

  expect_true(
    grepl(" - 5 mAbs.+ - 0 mAb mixtures", capture_output(mixno$print()))
  )

  mixonly <-
    expect_message(
      con$getMabs(mabmix, includeMixtures="only"),
      "All mabs in all mixtures passed with be returned"
    )

  expect_true(
    grepl(" - 5 mAbs.+ - 7 mAb mixtures", capture_output(mixonly$print()))
  )

  expect_equal(
    names(mixyes$datasets), c("NABMAb", "PKMAb")
  )

  mabyes$loadDaash()

  expect_equal(
    names(mabyes$datasets), c("NABMAb", "PKMAb", "daash")
  )

  expect_true(
    all(mabyes$variableDefinitions$NABMAb$field_name %in% names(mabyes$datasets$NABMAb))
  )
  
  expect_equal(
    names(mabyes$datasets$daash), c("topCalls", "alignments", "sequences", "alleleSequences", "runInformation", "pdbAccession")
  )

  expect_equal(
    names(mabyes$variableDefinitions$daash), c("topCalls", "alignments", "sequences", "alleleSequences", "runInformation", "pdbAccession")
  )

})

test_that("DataSpaceDonors", {

  expect_true(
    all(
      c("cds_donor_22", "cds_donor_44", "cds_donor_65") %in%
        con$availableDonors[lineage_sequences_available == TRUE, donor_id]
    )
  )

  don <- con$availableDonors[donor_code == "Donor CH505"] |>
    con$getDonors()

  expect_true(
    nrow(don$donorMetadata) > 0 &
      nrow(don$donorMetadata) == nrow(don$availableDonors) &
      "availableDonors" %in% class(don$donorMetadata)
  )

  expect_true(
    all(
      c(
        "URL"                         ,
        "User"                        ,
        "Summary"                     ,
        "Available Donors objects"    ,
        "Available Donors methods"    ,
        "Available Connection objects",
        "Available Connection methods"
      ) |>
        sapply(
          \(.) any(grepl(., capture.output(don$print())))
        ) |> unlist()
    )
  )

  expect_true(
    nrow(don$donorMetadata) != 0 &
      all(don$donorMetadata$donor_id == "cds_donor_44")
  )

  expect_true(
    nrow(don$mabMetadata) != 0 &
      all(grepl("^CH", don$mabMetadata$mab_name_std))
  )

  expect_true(
    grepl("<DataSpaceDonors>.+- 1 donors.+- 1 donors with lineage", capture_output(don$print()))
  )

  expect_message(
    don$loadDaash(), "Presently querying.+sequences"
  )

  expect_true(
    "daash" %in% names(don$datasets)
  )

  expect_true(
    don$datasets$daash$sequences[,.N, mab_id][,any(is.na(mab_id)) & any(!is.na(mab_id))]
  )

  expect_equal(
    names(don$datasets$daash), c("topCalls", "alignments", "sequences", "alleleSequences", "runInformation", "pdbAccession")
  )

  expect_equal(
    names(don$variableDefinitions$daash), c("topCalls", "alignments", "sequences", "alleleSequences", "runInformation", "pdbAccession")
  )

})
