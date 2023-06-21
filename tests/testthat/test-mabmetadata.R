context("DataSpaceMabMetadata")

testMabMetaDataOnFacet <- function(met, nMembers){
  seqIdLen <- met$sequences$mab_id |> unique() |> length()
  
  test_that(
    "Sequence table has the correct number of mab members",
    {
      expect_true(nMembers == met$sequences$mab_id |> unique() |> length())
    }
  )

  test_that(
    "All loadAlignment objects have the correct number of members.",
    {
      expect_true( met$alignments$mab_id |> unique() |> length() == nMembers )
      expect_true( met$topMatches$mab_id |> unique() |> length() == nMembers )
      expect_true( seqIdLen <= nMembers * 2 & seqIdLen >= nMembers )      
    }
  )
  
  test_that(
    "Allele sequence table has no more members that the alignment data.",
    {
      expect_true(
        all(sort(unique(met$topMatches$allele)) == sort(unique(met$alleleSequences$allele)))
      )
    }
  )

  test_that(
    "All loadAlignement objects have the correct column names",
    {
      colList <- list(
        "alignments"      = c("mab_id","mab_name_std","mab_lanlid","sequence_id","locus","stop_codon","vj_in_frame","productive","rev_comp","complete_vdj","v_call","d_call","j_call","sequence_alignment","germline_alignment","sequence_alignment_aa","germline_alignment_aa","v_alignment_start","v_alignment_end","d_alignment_start","d_alignment_end","j_alignment_start","j_alignment_end","v_sequence_alignment","v_sequence_alignment_aa","v_germline_alignment","v_germline_alignment_aa","d_sequence_alignment","d_sequence_alignment_aa","d_germline_alignment","d_germline_alignment_aa","j_sequence_alignment","j_sequence_alignment_aa","j_germline_alignment","j_germline_alignment_aa","fwr1","fwr1_aa","cdr1","cdr1_aa","fwr2","fwr2_aa","cdr2","cdr2_aa","fwr3","fwr3_aa","fwr4","fwr4_aa","cdr3","cdr3_aa","junction","junction_length","junction_aa","junction_aa_length","v_score","d_score","j_score","v_cigar","d_cigar","j_cigar","v_support","d_support","j_support","v_identity","d_identity","j_identity","v_sequence_start","v_sequence_end","v_germline_start","v_germline_end","d_sequence_start","d_sequence_end","d_germline_start","d_germline_end","j_sequence_start","j_sequence_end","j_germline_start","j_germline_end","fwr1_start","fwr1_end","cdr1_start","cdr1_end","fwr2_start","fwr2_end","cdr2_start","cdr2_end","fwr3_start","fwr3_end","fwr4_start","fwr4_end","cdr3_start","cdr3_end","np1","np1_length","np2","np2_length","run_application"),
        "topMatches"      = c("mab_id","mab_name_std","mab_lanlid","mab_hxb2_location","mab_ab_binding_type","mab_isotype","mab_donorid","mab_donor_species","mab_donor_clade","allele","sequence_id","percent_identity","matches","alignment_length","score","run_application"),
        "sequences"       = c("mab_id","sequence_id","locus","header","source","sequence_nt"),
        "runInformation"  = c("run_application","run_information"),
        "allele_sequence" = c("allele", "allele_sequence_nt")
      )

      for(nm in names(colList))
        expect_true(all(colList[[nm]] == names(met[[nm]])), info = paste(nm, "does not match expected names."))
    }
  )
  
  test_that(
    "getFastaFromSequences call returns the correct number of sequences from a DataSpaceMabMetadata object and is the correct format.",
    {
      expect_true(
        all(
          list(
            met$getFastaFromSequences(),
            met$getFastaFromSequences(originalHeaders = TRUE)
          ) |>
            lapply( \(.) seqIdLen <= ( grep("^>.+", .) |> length()) ) |>
            unlist()
        )
      )
    }
  )
}

## test alignment data from mab metadata called from mab object without subset of mab_ids to loadAlignments

library(DataSpaceR)
con <- connectDS(onStaging = T)

con$resetMabGrid()
con$filterMabGrid("mab_mixture", "VRC01")
mab <- con$getMab()
met <- mab$mabMetadata
met$loadAlignments()

testMabMetaDataOnFacet(met, 1)

## test alignment data from mab metadata called from mab object with subset of mab_ids to loadAlignments

con$resetMabGrid()
con$filterMabGrid("mab_mixture", c("VRC01", "PGT121", "PGDM1400"))
mab <- con$getMab()
met <- mab$mabMetadata
met$loadAlignments(c("cds_mab_36", "cds_mab_26"))
testMabMetaDataOnFacet(met, 2)

## check that this can be chaged and still get the correct number of records in each object
met$loadAlignments("cds_mab_36")
testMabMetaDataOnFacet(met, 1)

met$loadAlignments(c("cds_mab_36", "cds_mab_2"))
testMabMetaDataOnFacet(met, 2)

## test alignment data from mab metadata called from connection object
met <- con$getMabMetadata()
mabMet <- mab$mabMetadata
test_that(
  "Check that mab metadata object pulled from connection is for all data and not what is filtered in the grid.",
  {
    expect_true(nrow(met$mabMetadata) > nrow(mabMet$mabMetadata))
  }
)

## test query of loadAlignments on mab metadata from connection object

met$loadAlignments("cds_mab_36")
testMabMetaDataOnFacet(met, 1)

met$loadAlignments(c("cds_mab_2", "cds_mab_36"))
testMabMetaDataOnFacet(met, 2)

## test that all messsages and stops are triggered when expected

test_that(
  "Check that when passing bad values to loadAlignment, the proper messages and errors are thrown.",
  {
    expect_error(met$loadAlignments("test"))
    expect_error(met$loadAlignments("cds_mab_"))
    expect_error(met$loadAlignments("cds_mab_abc"))
    expect_error(met$loadAlignments("cds_mab_abc"))
    expect_error(met$loadAlignments(c("cds_mab_100000000", "cds_mab_100000001")), "None of the `mabAlignmentIds` elements provided exist in database.")
    expect_message(
      met$loadAlignments(c("cds_mab_36", "cds_mab_100000000")),
      "Note: At least one element of `mabAlignmentIds` is not available."
    )

    met <- con$getMabMetadata()

    tabs <- c(
      "alignments",    
      "topMatches",    
      "sequences",     
      "runInformation",
      "alleleSequences"
    ) |>
      lapply(
        \(tab) expect_message(met[[tab]], "^Please run \\`loadAlignments\\(\\)\\` to access.+$")
      )

    expect_true(tabs |> unlist() |> is.null() |> all())

    expect_message(
      con$getMabMetadata(lineage = T),
      "^Note: When `lineage` is TRUE.+"
    )
    
  }
)

## tests on mab metadata called from connection object
