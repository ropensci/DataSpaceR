context("DAASH")

con <- connectDS(onStaging = onStaging)

test_that("DataSpaceDaash", {

  expect_error(
    daash <- con$getDaash(con$availablePublications),
    "Must pass an `availableMabs` or `availableDonors` object"
  )

  daash <- con$getDaash(con$availableDonors[donor_id == "cds_donor_44",])

  cap_output <- capture.output(daash$print())
  expect_length(cap_output, 39)

  expect_equal(cap_output[1],  "<DataSpaceDaash>")
  expect_equal(cap_output[2],  paste0("  URL: ", baseUrl))
  expect_match(cap_output[3],  "  User: \\S+@\\S+$")
  expect_match(cap_output[4],  "  Summary")
  expect_match(cap_output[5],  "  - \\d+ mAbs")
  expect_match(cap_output[6],  "  - \\d+ donors")
  expect_match(cap_output[7],  "  - \\d+ predicted structures")
  expect_match(cap_output[8],  "  MAbs loaded to object:")
  expect_match(cap_output[10], "  Donors loaded to object:")
  expect_match(cap_output[12], "  MAb structures to download:")

  expect_equal(
    cap_output[14:39],
    c(
      "  Available DAASH objects:",
      "    - availableStructures",
      "    - daashMetadata",
      "    - datasets",
      "    - donorMetadata",
      "    - mabMetadata",
      "    - variableDefinitions",
      "  Available DAASH methods:",
      "    - downloadAntibodyStructures",
      "    - getFastaFromSequences",
      "  Available Connection objects:",
      "    - availableDonors",
      "    - availableGroups",
      "    - availableMabMixtures",
      "    - availableMabs",
      "    - availablePublications",
      "    - availableStudies",
      "    - availableViruses",
      "    - virusNameMappingTables",
      "  Available Connection methods:",
      "    - downloadPublicationData",
      "    - getDaash",
      "    - getDonors",
      "    - getGroups",
      "    - getMabs",
      "    - getStudies"
    )
  )

  expect_true(
    daash$daashMetadata[,any(is.na(mab_id)) & any(!is.na(mab_id))]
  )

  expect_true(
    nrow(daash$mabMetadata) > 0 &
      nrow(daash$donorMetadata) > 0 &
      nrow(daash$daashMetadata) > 0
  )

  fasta <- daash$getFastaFromSequences()

  expect_true(
    fasta |>
      grep("^>", x=_) |>
      length() == nrow(daash$daashMetadata)
  )

  fastaPath <- tempfile()
  daash$getFastaFromSequences(path = fastaPath)
  expect_equal(
    length(readLines(fastaPath)), length(fasta)
  )

  fastaAa <- daash$getFastaFromSequences(sequenceType = "aa")
  fastaPathAa <- tempfile()
  daash$getFastaFromSequences(path = fastaPathAa, sequenceType = "aa")
  expect_equal(
    length(readLines(fastaPathAa)), length(fastaAa)
  )

  expect_true(
    all(strsplit(fasta[2], "")[[1]] %in% c("G", "A", "T", "C"))
  )

  expect_false(
    all(strsplit(fastaAa[2], "")[[1]] %in% c("G", "A", "T", "C"))
  )
  
  expect_equal(
    length(daash$donorMetadata$donor_id), 1
  )
  
  expect_true(
    all(names(daash$datasets) %in% c("topCalls", "alignments", "sequences", "alleleSequences", "runInformation", "pdbAccession"))
  )

  daash <- con$getDaash(con$availableMabs[mab_name_std %like% "VRC01",])
  expect_true(
    all(names(daash$datasets) %in% c("topCalls", "alignments", "sequences", "alleleSequences", "runInformation", "pdbAccession"))
  )

  expect_true(
    all(grepl("VRC01", daash$mabMetadata$mab_name_std))
  )

  expect_true(
    any(grepl("NIH45", daash$donorMetadata$donor_code))
  )

  expect_true(
    all(grepl("VRC01", daash$daashMetadata$mab_name_std)) &
      any(grepl("NIH45", daash$daashMetadata$donor_code))
  )

  expect_true(
    nrow(daash$datasets$alignments) == nrow(unique(daash$datasets$alignments))
  )

  expect_true(
    all(names(daash$variableDefinitions) %in% names(daash$datasets))
  )

  dl <- tempdir()
  daash$downloadAntibodyStructures(dl)
  expect_true(
    length(list.files(dl, "\\.pdb$")) != 0
  )

})
