isWindows <- function() {
  .Platform$OS.type == "windows"
}

getUrlBase <- function(onStaging) {
  production <- paste0("https://", PRODUCTION)
  staging <- paste0("https://", STAGING)

  if (exists("labkey.url.base", .GlobalEnv)) {
    labkeyUrlBase <- get("labkey.url.base", .GlobalEnv)
    labkeyUrlBase <- gsub("/$", "", labkeyUrlBase)
    assert_that(
      labkeyUrlBase == production || labkeyUrlBase == staging,
      msg = paste(
        "labkey.url.base should be either",
        production, "or", staging
      )
    )
  } else {
    if (onStaging) {
      labkeyUrlBase <- staging
    } else {
      labkeyUrlBase <- production
    }
  }

  labkeyUrlBase <- gsub("http:", "https:", labkeyUrlBase)
  if (length(grep("^https://", labkeyUrlBase)) == 0) {
    labkeyUrlBase <- paste0("https://", labkeyUrlBase)
  }

  labkeyUrlBase
}

getUserEmail <- function(labkeyUrlBase, login) {
  if (exists("labkey.user.email", .GlobalEnv)) {
    labkeyUserEmail <- get("labkey.user.email", .GlobalEnv)
  } else if (!is.null(login)) {
    labkeyUserEmail <- login
  } else if (file.exists(getNetrcPath())) {
    netrcFile <- getNetrcPath()
    netrc <- readChar(netrcFile, file.info(netrcFile)$size)
    netrc <- strsplit(netrc, split = "\\s+")[[1]]

    if (length(netrc) %% 6 == 0) {
      url.base <- gsub("https://", "", labkeyUrlBase)
      labkeyUserEmail <- netrc[which(url.base == netrc) + 2]
    } else {
      labkeyUserEmail <- ""
    }
  } else {
    labkeyUserEmail <- ""
  }

  labkeyUserEmail
}

getUrlPath <- function(study) {
  if (exists("labkey.url.path", .GlobalEnv)) {
    if (is.null(study)) {
      labkeyUrlPath <- get("labkey.url.path", .GlobalEnv)
    } else {
      labkeyUrlPath <- file.path("", "CAVD", tolower(study))
    }
  } else {
    if (is.null(study)) {
      stop("'study' cannot be NULL.", call. = FALSE)
    } else if (study == "") {
      labkeyUrlPath <- file.path("", "CAVD")
    } else {
      labkeyUrlPath <- file.path("", "CAVD", tolower(study))
    }
  }

  labkeyUrlPath
}

#' @importFrom Rlabkey getSession lsFolders
getValidStudies <- function(labkeyUrlBase) {
  folders <- lsFolders(getSession(labkeyUrlBase, folderPath = "CAVD"))
  validStudies <- grep("\\w+\\d+", basename(folders), value = TRUE)

  validStudies
}

checkStudy <- function(study, labkeyUrlBase, verbose = FALSE) {
  validStudies <- getValidStudies(labkeyUrlBase)
  reqStudy <- study

  if (!reqStudy %in% c("", validStudies)) {
    if (!verbose) {
      stop(paste0("'", reqStudy, "' is not a valid study."), call. = FALSE)
    } else {
      stop(paste0(
        "'", reqStudy, " is not a valid study.\nValid studies: ",
        paste(validStudies, collapse = ", ")
      ), call. = FALSE)
    }
  }

  invisible(NULL)
}

fixStudy <- function(study, labkeyUrlBase, labkeyUrlPath) {
  if (is.null(study)) study <- basename(labkeyUrlPath)

  # check if `study` is an actual study
  checkStudy(study, labkeyUrlBase)

  study
}

getNetrc <- function(login, password, onStaging = FALSE) {
  if (!is.null(login) && !is.null(password)) {
    netrc <- writeNetrc(
      login, password,
      onStaging = onStaging,
      netrcFile = tempfile()
    )
  } else if (exists("labkey.netrc.file", .GlobalEnv)) {
    netrc <- get("labkey.netrc.file", .GlobalEnv)
  } else {
    netrc <- getNetrcPath()
  }

  netrc
}

#' @importFrom utils packageVersion
#' @importFrom Rlabkey labkey.setCurlOptions
setCurlOptions <- function(netrcFile) {
  useragent <- paste0(
    "R/", R.version$major, ".", R.version$minor,
    " (", Sys.info()["sysname"], " ", Sys.info()["machine"], ")",
    " Rlabkey/", packageVersion("Rlabkey"),
    " DataSpaceR/", packageVersion("DataSpaceR")
  )

  curlOptions <- labkey.setCurlOptions(
    netrc_file = netrcFile,
    useragent = useragent
  )

  curlOptions
}


#' @importFrom httr GET content
checkCredential <- function(labkeyUrlBase, verbose) {
  url <- file.path(labkeyUrlBase, "/login-whoami.view")

  if (verbose) message("Checking credential at ", labkeyUrlBase, " ...")

  res <- GET(url, labkey.getRequestOptions())

  if (res$status_code == 200) {
    if (grepl("json", res$headers$`content-type`)) {
      parsed <- content(res)

      if (parsed$displayName == "guest") {
        stop(
          "Invalid credential or deactivated account. ",
          "Check your account in the portal.",
          call. = FALSE
        )
      } else {
        return(TRUE)
      }
    } else {
      stop(
        "Something went wrong. Check the portal and try again.",
        call. = FALSE
      )
    }
  } else if (res$status_code == 401) {
    stop(
      "Invalid credential or deactivated account. ",
      "Check your account in the portal.",
      call. = FALSE
    )
  } else if (res$status_code == 403) {
    stop(
      "The portal is in admin-only mode. ",
      "Please try again later.",
      call. = FALSE
    )
  } else {
    stop(
      "Something went wrong. ",
      "Check the portal and try again.",
      call. = FALSE
    )
  }
}

makeCountQuery <- function(dataset, group) {
  query <-
    paste(
      "SELECT",
      "COUNT(participantid) AS n,",
      paste0("'", dataset, "' AS Name"),
      "FROM",
      dataset
    )

  if (!is.null(group)) {
    query <- paste(
      query,
      "WHERE",
      paste0("participantid.\"", names(group), "\" = '", group, "'")
    )
  }

  query
}

assertColumn <- function(using, self) {
  assert_that(
    length(using) == 1,
    msg = "May only pass one column at a time"
  )
  assert_that(
    using %in% names(self$mabGrid),
    msg = paste0("\"", using, "\" is not a valid column in the mabGrid.")
  )
}

switchColumn <- function(using) {
  switch(using,
    "mab_mixture" = "mab_mix_name_std",
    "donor_species" = "mab_donor_species",
    "isotype" = "mab_isotype",
    "hxb2_location" = "mab_hxb2_location",
    "virus" = "virus",
    "clade" = "clade",
    "tier" = "neutralization_tier",
    "curve_ic50" = "titer_curve_ic50",
    "study" = "study"
  )
}

isFromMabGrid <- function(column) {
  column %in% c("mab_mix_name_std", "virus", "clade", "neutralization_tier", "titer_curve_ic50", "study")
}

truncatePrintable <- function(text, len = 70) {
  if( nchar(text) > len ){
    paste0( substr(text, 1, len), "..." )
  } else {
    text
  }
}

generateFasta <- function(sequences, seqIds=NULL, sequenceType="nt", originalHeaders=FALSE){
  if(length(sequences) == 0) stop("Please run `loadDaash()` to access fasta file for sequences available.")

  if(is.null(seqIds))
      seqs <- copy(sequences)
  else
      seqs <- sequences[sequence_id %in% seqIds]

  if(sequenceType == "nt")
      seqs[, sequence := sequence_nt]
  else if(sequenceType == "aa")
      seqs[, sequence := sequence_aa]
  else
      stop("`sequenceType` provided is not valid. Use `aa` for amino acid or `nt` for nucleotide.")

  if(originalHeaders){
    seqs <- seqs[,.(header, sequence)]
  } else {
    seqs <- seqs[,.(header = cds_header, sequence)]
  }

  fasta <- apply(seqs, 1, \(l) {
    header <- l[1]
    sets <- nchar(l[2]) %/% 60
    c(
      ifelse(!grepl("^>", header), paste0(">", header), header), 
      sapply( 1:sets, \(set) substr(l[2], (set - 1) * 60 + 1, set * 60) ),
      substr(l[2], sets * 60 + 1, nchar(l[2]))
    )
  }) |> unlist()
  names(fasta) <- NULL
  
  return(fasta)
}

fetchLanlMetadata <- function(lanl_id){

  checkList <- function(x){
    if(any(c("list", "data.frame") %in% class(x))){
      lapply(x, function(y){
        if("data.frame" %in% class(y)) {
          setDT(y)
          checkList(y)
        } else {
          checkList(y)
        }
      })
    }
  }
  
  url <- paste0("https://www.hiv.lanl.gov/mojo/immunology/api/v1/epitope/ab?id=", lanl_id)
  if(is.na(lanl_id)) return(NA)
  res <- httr::GET(url)
  if(res$status == 200){
    json <- fromJSON(content(res, as="text")[[1]])
    json[["epitopes"]] <- data.table(json[["epitopes"]])
    json$source <- url
    lapply(json$epitopes, checkList)
    return(json)
  } else {
    return(paste0("No LANL metadata found at '", url, "'."))
  }

}

fetchDaash <- function(filter, config){

  daash <- list( 
    topCalls = data.table(),
    alignments = data.table(),
    sequences = data.table(),
    alleleSequences = data.table(),
    runInformation = data.table()
  )
        
  daash$sequences <- 
    labkey.selectRows(
      baseUrl = config$labkeyUrlBase,
      folderPath = "/CAVD",
      schemaName = "CDS",
      queryName = "donor_mab_sequence_header_source",
      colNameOpt = "fieldname",
      colFilter = filter,
      method = "GET"
    ) |> setDT()

  limit <- 200
  sids <- unique(daash$sequences$sequence_id)
  srts <- seq(1, length(sids), limit)
  ends <- shift(seq(0, length(sids), limit), fill = length(sids), type="lead")

  seqFilters <- Map(\(srt, end){
    c("sequence_id", "IN", paste(sids[srt:end], collapse = ";")) |> makeFilter()
  }, srts, ends)

  if(length(seqFilters) > 1)
    message(paste0("Presently querying ", length(sids), " sequences. This may take some time.."))
  
  seqs <- daash$sequences[,.(mab_id,donor_id,sequence_id,mab_name_std,donor_code)] |> unique()

  daash$topCalls <-
    merge( 
      seqs,     
      labkey.selectRows(
        baseUrl = config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "CDS",
        queryName = "donor_mab_sequence_germline",
        colNameOpt = "fieldname",
        colSelect = "sequence_id,allele,percent_identity,matches,alignment_length,score,run_application",
        colFilter = filter,
        method = "GET"
      ) |> setDT(),
      by = "sequence_id"
    )

  daash$alignments <- Map(\(seqFilter) {
    merge(
      seqs,
      labkey.selectRows(
        baseUrl = config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "CDS",
        queryName = "alignment",
        colNameOpt = "fieldname",
        colSelect = "sequence_id,stop_codon,vj_in_frame,productive,rev_comp,complete_vdj,v_call,d_call,j_call,sequence_alignment,germline_alignment,sequence_alignment_aa,germline_alignment_aa,v_alignment_start,v_alignment_end,d_alignment_start,d_alignment_end,j_alignment_start,j_alignment_end,v_sequence_alignment,v_sequence_alignment_aa,v_germline_alignment,v_germline_alignment_aa,d_sequence_alignment,d_sequence_alignment_aa,d_germline_alignment,d_germline_alignment_aa,j_sequence_alignment,j_sequence_alignment_aa,j_germline_alignment,j_germline_alignment_aa,fwr1,fwr1_aa,cdr1,cdr1_aa,fwr2,fwr2_aa,cdr2,cdr2_aa,fwr3,fwr3_aa,fwr4,fwr4_aa,cdr3,cdr3_aa,junction,junction_length,junction_aa,junction_aa_length,v_score,d_score,j_score,v_cigar,d_cigar,j_cigar,v_support,d_support,j_support,v_identity,d_identity,j_identity,v_sequence_start,v_sequence_end,v_germline_start,v_germline_end,d_sequence_start,d_sequence_end,d_germline_start,d_germline_end,j_sequence_start,j_sequence_end,j_germline_start,j_germline_end,fwr1_start,fwr1_end,cdr1_start,cdr1_end,fwr2_start,fwr2_end,cdr2_start,cdr2_end,fwr3_start,fwr3_end,fwr4_start,fwr4_end,cdr3_start,cdr3_end,np1,np1_length,np2,np2_length,run_application",
        colFilter = seqFilter,
        method = "GET"
      ) |> setDT(),
      by = "sequence_id"
    )
  }, seqFilters) |>
    rbindlist() 

  alleleFilter <- makeFilter(
    c(
      "allele", "IN", paste(unique(daash$topCalls$allele), collapse = ";")
    )
  )
  
  daash$alleleSequences <- labkey.selectRows(
    baseUrl = config$labkeyUrlBase,
    folderPath = "/CAVD",
    schemaName = "CDS",
    queryName = "allele_sequence",
    colNameOpt = "fieldname",
    colSelect = "allele,allele_sequence_nt",
    colFilter = alleleFilter,
    method = "GET"
  ) |>
    setDT()
  
  daash$runInformation <- labkey.selectRows(
    baseUrl = config$labkeyUrlBase,
    folderPath = "/CAVD",
    schemaName = "cds",
    queryName = "alignment_run",
    colSelect = "run_application,run_information",
    colNameOpt = "fieldname",
    method = "GET"
  ) |>
    setDT()
  
  daash$runInformation[,run_information:=lapply(gsub("\"\"", "\"", run_information), fromJSON)]

  return(daash)

}

nt2aa <- function(nt){
  nt <- toupper(nt)
  codon <- fread(system.file("aa_lookup.csv", package="DataSpaceR"))
  aa <- Map(\(sequence){
    end <- seq(3, nchar(sequence), 3)
    sta <- seq(1, nchar(sequence), 3)[seq_len(length(end))]
    nts <- Map(substr, sequence, sta, end) |>
      unlist() |>
      (\(.) data.table(nt=., ord=1:length(.)))() |>
      merge(codon, all.x = T) |>
      (\(.) .[is.na(aa), aa:="X"])()
    return(nts[order(ord),]$aa |> paste(collapse=""))
  }, nt) |> unlist()
  return(aa)
}

checkMabIdFormat <- function(mabIds){
  if(!all(grepl("^cds_mab_[0-9]+$", mabIds)))
    stop("All `mabIds` must be in the format `^cds_mab_[0-9]+$`")
}

#' @importFrom Rlabkey makeFilter
#' @export
Rlabkey::makeFilter

#' @importFrom utils getFromNamespace
labkey.getRequestOptions <- getFromNamespace("labkey.getRequestOptions", "Rlabkey")
labkey.get <- getFromNamespace("labkey.get", "Rlabkey")
