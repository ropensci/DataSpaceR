context("DataSpaceMabMetadata")

testMetadataOnFacet <- function(metadata, nMembers){
  seqIdLen <- metadata$sequences$mab_id |> unique() |> length()
  
  test_that(
    "Sequence table has the correct number of mab members",
    {
      expect_true(nMembers == metadata$sequences$mab_id |> unique() |> length())
    }
  )

  test_that(
    "All loadDaash objects have the correct number of members.",
    {
      expect_true( metadata$alignments$mab_id |> unique() |> length() == nMembers )
      expect_true( metadata$topCalls$mab_id |> unique() |> length() == nMembers )
      expect_true( seqIdLen <= nMembers * 2 & seqIdLen >= nMembers )      
    }
  )
  
  test_that(
    "Allele sequence table has no more members that the alignment data.",
    {
      expect_true(
        all(sort(unique(metadata$topCalls$allele)) == sort(unique(metadata$alleleSequences$allele)))
      )
    }
  )

  test_that(
    "All loadAlignment objects have the correct column names",
    {
      colList <- list(
        "alignments"      = c("sequence_id","mab_id","donor_id","mab_name_std","donor_code","stop_codon","vj_in_frame","productive","rev_comp","complete_vdj","v_call","d_call","j_call","sequence_alignment","germline_alignment","sequence_alignment_aa","germline_alignment_aa","v_alignment_start","v_alignment_end","d_alignment_start","d_alignment_end","j_alignment_start","j_alignment_end","v_sequence_alignment","v_sequence_alignment_aa","v_germline_alignment","v_germline_alignment_aa","d_sequence_alignment","d_sequence_alignment_aa","d_germline_alignment","d_germline_alignment_aa","j_sequence_alignment","j_sequence_alignment_aa","j_germline_alignment","j_germline_alignment_aa","fwr1","fwr1_aa","cdr1","cdr1_aa","fwr2","fwr2_aa","cdr2","cdr2_aa","fwr3","fwr3_aa","fwr4","fwr4_aa","cdr3","cdr3_aa","junction","junction_length","junction_aa","junction_aa_length","v_score","d_score","j_score","total_score","v_cigar","d_cigar","j_cigar","v_support","d_support","j_support","v_identity","d_identity","j_identity","v_sequence_start","v_sequence_end","v_germline_start","v_germline_end","d_sequence_start","d_sequence_end","d_germline_start","d_germline_end","j_sequence_start","j_sequence_end","j_germline_start","j_germline_end","fwr1_start","fwr1_end","cdr1_start","cdr1_end","fwr2_start","fwr2_end","cdr2_start","cdr2_end","fwr3_start","fwr3_end","fwr4_start","fwr4_end","cdr3_start","cdr3_end","np1","np1_length","np2","np2_length","run_application"),
        "topCalls"        = c("sequence_id","mab_id","donor_id","mab_name_std","donor_code","allele","percent_identity","matches","alignment_length","score","run_application"),
        "sequences"       = c("sequence_id","mab_id","donor_id","mab_name_std","donor_code","header","source","sequence_aa","sequence_nt"),
        "runInformation"  = c("run_application","run_information"),
        "allele_sequence" = c("allele", "allele_sequence_nt")
      )

      for(nm in names(colList))
        expect_true(all(colList[[nm]] == names(metadata[[nm]])), info = paste(nm, "does not match expected names."))
    }
  )

  test_that(
    "getFastaFromSequences call returns the correct number of sequences from a DataSpaceMabMetadata object and is the correct format.",
    {
      expect_true(
        all(
          list(
            metadata$getFastaFromSequences(),
            metadata$getFastaFromSequences(originalHeaders = TRUE)
          ) |>
            lapply( \(.) seqIdLen <= ( grep("^>.+", .) |> length()) ) |>
            unlist()
        )
      )
    }
  )
}

test_that(
  "Test DataSpaceMabMetadata object",
  {

    library(DataSpaceR)
    con <- connectDS(onStaging = onStaging)

    con$resetMabGrid()
    con$filterMabGrid("mab_mixture", "VRC01")
    mab <- con$getMab()
    met <- mab$mabMetadata
    met$loadDaash()

    testMetadataOnFacet(met, 1)

    ## "Test alignment data from mab metadata called from mab object with subset of mab_ids to loadDaash",
    con$resetMabGrid()
    con$filterMabGrid("mab_mixture", c("VRC01", "PGT121", "PGDM1400"))
    mab <- con$getMab()
    met <- mab$mabMetadata
    met$loadDaash(c("cds_mab_36", "cds_mab_26"))

    testMetadataOnFacet(met, 2)

    ## "Check that mab ids and still get the correct number of records in each object.",
    met$loadDaash("cds_mab_36")
    testMetadataOnFacet(met, 1)        
    met$loadDaash(c("cds_mab_36", "cds_mab_2"))

    testMetadataOnFacet(met, 2)

    ## "Check that mab metadata object pulled from connection is for all data and not what is filtered in the grid.",
    conMet <- con$getMabMetadata()
    expect_true(nrow(met$mabMetadata) < nrow(conMet$mabMetadata))

    ## "Check malformed mab ids passed to getMabMetadata"
    expect_error(con$getMabMetadata("123"), "All `mabIds` must be in the format")
    expect_error(con$getMabMetadata("cds_mab_-1"))
    expect_error(con$getMabMetadata("abc"))
    expect_error(con$getMabMetadata("cds_mab_10000000000"))

    ## test query of loadDaash on mab metadata from connection object
    met$loadDaash("cds_mab_36")
    testMetadataOnFacet(met, 1)

    met$loadDaash(c("cds_mab_2", "cds_mab_36"))
    testMetadataOnFacet(met, 2)

    ## check when a incorrect mab_id is passed to getMabMetadata
    expect_error(met <- con$getMabMetadata(c("cds_mab_36", "123")))
    testMetadataOnFacet(met, 2)

    ## test that all messsages and stops are triggered when expected
    expect_error(met$loadDaash("test"))
    expect_error(met$loadDaash("cds_mab_"))
    expect_error(met$loadDaash("cds_mab_abc"))
    expect_error(met$loadDaash(c("cds_mab_100000000", "cds_mab_100000001")), "None of the `mabIds` elements provided are found in this object.")
    expect_message(
      met$loadDaash(c("cds_mab_36", "cds_mab_100000000")),
      "Note: At least one element of `mabIds` is not available."
    )

    met <- con$getMabMetadata()

    tabs <- c(
      "alignments",    
      "topCalls",    
      "sequences",     
      "runInformation",
      "alleleSequences"
    ) |>
      lapply(
        \(tab) expect_message(met[[tab]], "^Please run \\`loadDaash\\(\\)\\` to access.+$")
      )

    expect_true(tabs |> unlist() |> is.null() |> all())

    ## "Check variable definitions loaded correctly"
    expect_true(all(unique(met$variableDefinitions$name) %in% c("mabMetadata","topCalls","alignments","sequences","alleleSequences","runInformation")))
})   

## disabled while CDS database is being updated
## test_that(
##   "Test DataSpaceDonorMetadata object",
##   {

##     library(DataSpaceR)
##     con <- connectDS(onStaging = onStaging)
      
##     ## "Check malformed mab ids passed to getDonorMetadata"
##     expect_error(con$getDonorMetadata("123"), "All `donorIds` must be in the format")
##     expect_error(con$getDonorMetadata("cds_donor_-1"))
##     expect_error(con$getDonorMetadata("abc"))
##     expect_error(con$getDonorMetadata("cds_donor_10000000000"))

##     ## test query of loadDaash on mab metadata from connection object
##     met$loadDaash("cds_donor_1")
##     testDonorMetaDataOnFacet(met, 1)

##     met$loadDaash(c("cds_donor_2", "cds_donor_3"))
##     testDonorMetaDataOnFacet(met, 2)

##     ## check when a incorrect donor_id is passed to getDonorMetadata
##     expect_error(met <- con$getDonorMetadata(c("cds_donor_4", "123")))
##     testDonorMetaDataOnFacet(met, 2)

##     ## test that all messsages and stops are triggered when expected
##     expect_error(met$loadDaash("test"))
##     expect_error(met$loadDaash("cds_donor_"))
##     expect_error(met$loadDaash("cds_donor_abc"))
##     expect_error(met$loadDaash(c("cds_donor_100000000", "cds_donor_100000001")), "None of the `donorIds` elements provided are found in this object.")
##     expect_message(
##       met$loadDaash(c("cds_donor_36", "cds_donor_100000000")),
##       "Note: At least one element of `donorIds` is not available."
##     )

##     met <- con$getDonorMetadata()

##     tabs <- c(
##       "alignments",    
##       "topCalls",    
##       "sequences",     
##       "runInformation",
##       "alleleSequences"
##     ) |>
##       lapply(
##         \(tab) expect_message(met[[tab]], "^Please run \\`loadDaash\\(\\)\\` to access.+$")
##       )

##     expect_true(tabs |> unlist() |> is.null() |> all())

##     ## "Check variable definitions loaded correctly"
##     expect_true(all(unique(met$variableDefinitions$name) %in% c("donorMetadata","topCalls","alignments","sequences","alleleSequences","runInformation")))
## })   
