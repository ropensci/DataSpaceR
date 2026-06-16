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
    netrc <- readNetrc(getNetrcPath())
    url.base <- gsub("https://", "", labkeyUrlBase)
    netrc[, machine := gsub("https://", "", machine) ]
    labkeyUserEmail <-
      netrc[which(machine == url.base), login] |>
      rev() |>
      _[1]
  } else {
    labkeyUserEmail <- ""
  }
  labkeyUserEmail
}

getStudyUrlPaths <- function(studies) {
    if (is.null(studies)) {
      if (exists("labkey.url.path", .GlobalEnv)) {
        return(get("labkey.url.path", .GlobalEnv))
      } else {
        stop("'studies' cannot be NULL.", call. = FALSE)
      }
    } else {
      paths <- file.path("", "CAVD", tolower(studies))
      names(paths) <- tolower(studies)
      return(paths)
    }
}

getValidStudies <- function(labkeyUrlBase) {
  folderPath <- "CAVD"
  folders <- basename(lsFolders(getSession(labkeyUrlBase, folderPath = "CAVD")))
  return(folders[folders != folderPath])
}

checkStudies <- function(studies, labkeyUrlBase) {
  validStudies <- getValidStudies(labkeyUrlBase)

  missingStudies <- setdiff(studies, validStudies)
  if( length(missingStudies) != 0 )
    warning("Studies queried are not valid: ", paste(missingStudies, collapse = ", "))

  return( setdiff(studies, missingStudies) )
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

readNetrc <- function(netrcPath = getNetrc()){

  netvals <-
    readLines(netrcPath) |>
    grep("^[^#]", x = _, value = T) |>
    gsub(" +", " ", x = _) |>
    paste(collapse = " ") |>
    strsplit(" ") |>
    unlist()

  netnms <- c("machine", "login", "password")
  netvals <- netnms |>
    lapply( \(.) netvals[which(netvals == .) + 1] )

  names(netvals) <- netnms

  if(!Reduce(all, Map(\(nr, len) length(nr) == len, netvals, length(netvals[[1]]))))
    stop("Something wrong with the `.netrc` file. See  and `?writeNetrc` and `https://everything.curl.dev/usingcurl/netrc.html`.")

  netvals <- netvals |>
    setDT()

  return(netvals)
}

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

truncatePrintable <- function(text, len = 70) {
  if( nchar(text) > len ){
    paste0( substr(text, 1, len), "..." )
  } else {
    text
  }
}

generateFasta <- function(sequences, seqIds=NULL, sequenceType="nt"){
  if(length(sequences) == 0) stop("Please run `loadDaash()` to access fasta file for sequences available.")

  if(sequenceType == "nt")
    sequences[, sequence := sequence_nt]
  else if(sequenceType == "aa")
    sequences[, sequence := sequence_aa]
  else
    stop("`sequenceType` provided is not valid. Use `aa` for amino acid or `nt` for nucleotide.")

  sequences <- sequences[,.(header = header, sequence)]

  fasta <- apply(sequences, 1, \(l) {
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

limitFilters <- function(tab, field, limit = 250){
  sids <- unique(tab[[field]])
  srts <- seq(1, length(sids), limit)
  ends <- shift(seq(0, length(sids), limit), fill = length(sids), type="lead")
  if(length(sids) %% limit == 0) # glitch when the modulus is zero, full filter is produced as last item.
    ends <- unique(ends)
  Map(\(srt, end){
    c(field, "IN", paste(sids[srt:end], collapse = ";")) |> makeFilter()
  }, srts, ends)
}

splitSeqFilter <- function(filter){
  seqFilters <-
      data.table(
        sequence_id = strsplit(gsub("sequence_id~in=", "", filter[grepl("^sequence_id", filter)]), "%3B")[[1]]
      ) |>
    limitFilters(field = "sequence_id")

  return(seqFilters)
}

fetchDaash <- function(filter, config){

  daash <- list(
    topCalls        = data.table(),
    alignments      = data.table(),
    sequences       = data.table(),
    alleleSequences = data.table(),
    runInformation  = data.table(),
    pdbAccession    = data.table()
  )

  getSeqs <- function(filter)
    labkey.selectRows(
      baseUrl = config$labkeyUrlBase,
      folderPath = "/CAVD",
      schemaName = "CDS",
      queryName = "donor_mab_sequence_header_source",
      colNameOpt = "fieldname",
      colFilter = filter,
      method = "GET"
    ) |>
      setDT() |>
      suppressWarnings()

  getTopCalls <- function(filter)
    labkey.selectRows(
      baseUrl = config$labkeyUrlBase,
      folderPath = "/CAVD",
      schemaName = "CDS",
      queryName = "donor_mab_sequence_germline",
      colNameOpt = "fieldname",
      colSelect = "sequence_id,allele,percent_identity,matches,alignment_length,score,run_application",
      colFilter = filter,
      method = "GET"
    ) |>
      setDT() |>
      suppressWarnings()

  getPdb <- function(filter)
    labkey.selectRows(
      baseUrl = config$labkeyUrlBase,
      folderPath = "/CAVD",
      schemaName = "CDS",
      queryName = "donor_mab_sequence_pdb_accession",
      colNameOpt = "fieldname",
      colSelect = "mab_id,donor_id,sequence_id,pdb_accession_id,pident,length,mismatch,gapopen,qstart,qend,sstart,send,evalue,bitscore",
      colFilter = filter,
      method = "GET"
    ) |>
      setDT() |>
      suppressWarnings()

  if(any(grepl("sequence_id~in=", filter))){

    seqFilters <- splitSeqFilter(filter)
    if(Sys.getenv("DSR_TESTING") == "TRUE")
      seqFilters <- seqFilters[1:min(length(seqFilters), 2)]

    filters <- lapply(
      seqFilters,
      \(sf) c(sf, filter[!grepl("^sequence_id", filter)]) |> as.matrix()
    )

    daash$sequences <- filters |>
      lapply(\(filter){
        getSeqs(filter)
      }) |> rbindlist()

    daash$topCalls <- filters |>
      lapply(\(filter){
        getTopCalls(filter)
      }) |> rbindlist()

    daash$pdbAccession <- filters |>
      lapply(\(filter){
        getPdb(filter)
      }) |> rbindlist()

  } else {

    daash$sequences <- getSeqs(filter)
    daash$topCalls <- getTopCalls(filter)
    daash$pdbAccession <- getPdb(filter)

  }

  seqs <- daash$sequences[,.(sequence_id, mab_id, donor_id, mab_name_std, donor_code)]

  daash$topCalls <-
    merge(
      seqs,
      daash$topCalls,
      by = "sequence_id",
      allow.cartesian = TRUE # a sequence can be found in multiple mabs due to bispecificity
    )

  daash$alleleSequences <-
    labkey.selectRows(
      baseUrl = config$labkeyUrlBase,
      folderPath = "/CAVD",
      schemaName = "CDS",
      queryName = "allele_sequence",
      colNameOpt = "fieldname",
      method = "GET"
    ) |>
    setDT() |>
    removeContainerId() |>
    _[allele %in% daash$topCalls$allele,]

  daash$topCalls[
    order(-score, allele),
    rank:=seq(1, nrow(.SD)),
    .(sequence_id, substr(allele, 1, 4))
  ]

  seqFilters <- limitFilters(daash$sequences, "sequence_id")
  if(Sys.getenv("DSR_TESTING") == "TRUE") seqFilters <- seqFilters[1:min(length(seqFilters), 2)]

  if(length(seqFilters) > 1)
    message(
      paste0(
        "Presently querying ", length(unique(daash$sequence$sequence_id)), " sequences."
      )
    )

  daash$alignments <- Map(\(seqFilter) {
      labkey.selectRows(
        baseUrl = config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "CDS",
        queryName = "alignment",
        colNameOpt = "fieldname",
        colSelect = "sequence_id,stop_codon,vj_in_frame,productive,rev_comp,complete_vdj,v_call,d_call,j_call,sequence_alignment,germline_alignment,sequence_alignment_aa,germline_alignment_aa,v_alignment_start,v_alignment_end,d_alignment_start,d_alignment_end,j_alignment_start,j_alignment_end,v_sequence_alignment,v_sequence_alignment_aa,v_germline_alignment,v_germline_alignment_aa,d_sequence_alignment,d_sequence_alignment_aa,d_germline_alignment,d_germline_alignment_aa,j_sequence_alignment,j_sequence_alignment_aa,j_germline_alignment,j_germline_alignment_aa,fwr1,fwr1_aa,cdr1,cdr1_aa,fwr2,fwr2_aa,cdr2,cdr2_aa,fwr3,fwr3_aa,fwr4,fwr4_aa,cdr3,cdr3_aa,junction,junction_length,junction_aa,junction_aa_length,v_score,d_score,j_score,v_cigar,d_cigar,j_cigar,v_support,d_support,j_support,v_identity,d_identity,j_identity,v_sequence_start,v_sequence_end,v_germline_start,v_germline_end,d_sequence_start,d_sequence_end,d_germline_start,d_germline_end,j_sequence_start,j_sequence_end,j_germline_start,j_germline_end,fwr1_start,fwr1_end,cdr1_start,cdr1_end,fwr2_start,fwr2_end,cdr2_start,cdr2_end,fwr3_start,fwr3_end,fwr4_start,fwr4_end,cdr3_start,cdr3_end,np1,np1_length,np2,np2_length,run_application",
        colFilter = seqFilter,
        method = "GET"
      ) |> setDT()
  }, seqFilters) |>
    rbindlist() |>
    unique() |>
    merge(seqs, by = "sequence_id", allow.cartesian = T)

  if(all(daash$sequences$sequence_id %in% daash$alignments$alignment_id))
    stop("DAASH sequences not found in alignments. Report this to support@dataspace.cavd.org.")

  daash$runInformation <- labkey.selectRows(
    baseUrl = config$labkeyUrlBase,
    folderPath = "/CAVD",
    schemaName = "cds",
    queryName = "alignment_run",
    colSelect = "run_application,run_information",
    colNameOpt = "fieldname",
    method = "GET"
  ) |>
    setDT() |>
    _[run_application %in% unique(daash$topCalls$run_application)]

  daash$alignments[,total_score:=unlist(Map(sum, v_score, d_score, j_score, MoreArgs=list(na.rm=T)))]
  jScoreOrder <- which(names(daash$alignments) == "j_score")
  data.table::setcolorder(
    daash$alignments,
    c(
      names(daash$alignments)[1:jScoreOrder],
      "total_score",
      names(daash$alignments)[(jScoreOrder+1):(ncol(daash$alignments)-1)]
    )
  )

  daash$runInformation[,run_information:=lapply(gsub("\"\"", "\"", run_information), fromJSON)]

  daash$alignments <- merge(
    daash$alignments,
    daash$sequences[,.(sequence_id, chain)] |> unique(),
    by = "sequence_id", all.x = T
  )

  daash$topCalls <- merge(
    daash$topCalls,
    daash$sequences[,.(sequence_id, chain)] |> unique(),
    by = "sequence_id", all.x = T
  )

  keycols <- c("sequence_id","mab_id","donor_id","mab_name_std","donor_code","chain")

  data.table::setcolorder(daash$alignments,     c(keycols,"stop_codon","vj_in_frame","productive","rev_comp","complete_vdj","v_call","d_call","j_call","sequence_alignment","germline_alignment","sequence_alignment_aa","germline_alignment_aa","v_alignment_start","v_alignment_end","d_alignment_start","d_alignment_end","j_alignment_start","j_alignment_end","v_sequence_alignment","v_sequence_alignment_aa","v_germline_alignment","v_germline_alignment_aa","d_sequence_alignment","d_sequence_alignment_aa","d_germline_alignment","d_germline_alignment_aa","j_sequence_alignment","j_sequence_alignment_aa","j_germline_alignment","j_germline_alignment_aa","fwr1","fwr1_aa","cdr1","cdr1_aa","fwr2","fwr2_aa","cdr2","cdr2_aa","fwr3","fwr3_aa","fwr4","fwr4_aa","cdr3","cdr3_aa","junction","junction_length","junction_aa","junction_aa_length","v_score","d_score","j_score","total_score","v_cigar","d_cigar","j_cigar","v_support","d_support","j_support","v_identity","d_identity","j_identity","v_sequence_start","v_sequence_end","v_germline_start","v_germline_end","d_sequence_start","d_sequence_end","d_germline_start","d_germline_end","j_sequence_start","j_sequence_end","j_germline_start","j_germline_end","fwr1_start","fwr1_end","cdr1_start","cdr1_end","fwr2_start","fwr2_end","cdr2_start","cdr2_end","fwr3_start","fwr3_end","fwr4_start","fwr4_end","cdr3_start","cdr3_end","np1","np1_length","np2","np2_length","run_application"))
  data.table::setcolorder(daash$topCalls,       c(keycols,"allele","percent_identity","matches","alignment_length","score","rank","run_application"))
  data.table::setcolorder(daash$sequences,      c(keycols,"sequence_aa","sequence_nt"))
  data.table::setcolorder(daash$runInformation, c("run_application","run_information"))

  data.table::setorderv(daash$alignments, keycols)
  data.table::setorderv(daash$topCalls,   c(keycols, "rank"))
  data.table::setorderv(daash$sequences,  keycols)

  return(daash)
}

fetchDaashVariableDefinitions <- function(daash, config) {

  queries <- list(
    "topCalls"        = "sequence_germline"               ,
    "alignments"      = "alignment"                       ,
    "sequences"       = "donor_mab_sequence_header_source",
    "alleleSequences" = "allele_sequence"                 ,
    "runInformation"  = "alignment_run"                   ,
    "pdbAccession"    = "donor_mab_sequence_pdb_accession"
  )

  varInfo <- Map(f = \(query, query_name){
    getVarInfo(query, names(daash[[query_name]]), config$labkeyUrlBase, "CDS")
  }, queries, names(queries))

  names(varInfo) <- names(queries)

  return(varInfo)
}

publicationSqlCall <- function(){
"
SELECT publication.id as publication_id, label, author_first as first_author, author_all as all_authors, title, journal_short journal, date publication_date,
link, pmid as pubmed_id, related_studies, studies_with_data, filename IS NOT NULL as publication_data_available, document_id,
filename as remote_path
FROM publication
LEFT OUTER JOIN
  (
    SELECT GROUP_CONCAT(DISTINCT prot, ', ') AS related_studies, publication_id
    FROM studypublication
    GROUP BY publication_id
  ) sp
  ON sp.publication_id = publication.id
LEFT OUTER JOIN
  (
  SELECT GROUP_CONCAT(DISTINCT prot, ', ') AS studies_with_data, publication_id
  FROM (
    studypublication
    LEFT OUTER JOIN
      (
        SELECT study_name, data_availability FROM study
      ) sdy
      ON sdy.study_name = prot
    )
  WHERE data_availability IS NOT NULL
  GROUP BY publication_id
  ) da
  ON da.publication_id = publication.id
LEFT OUTER JOIN
  (
    SELECT filename, document_id, label, publication_id FROM learn_publicationdata
  ) pd
  ON pd.publication_id = publication.id
"
}

datasetCountQuery <- function(group = NULL, group_label = NULL) {
  paste(
    "SELECT",
    "ds.Name AS assay_identifier,",
    "ds.Label AS assay_label,",
    "ds_n.n",
    "FROM",
    "(",
    makeCountQuery("ICS", group, group_label),
    "UNION",
    makeCountQuery("BAMA", group, group_label),
    "UNION",
    makeCountQuery("ELISPOT", group, group_label),
    "UNION",
    makeCountQuery("NAb", group, group_label),
    "UNION",
    makeCountQuery("Demographics", group, group_label),
    "UNION",
    makeCountQuery("PKMAb", group, group_label),
    "UNION",
    makeCountQuery("BCR_Sequence", group, group_label),
    ") AS ds_n",
    "INNER JOIN DataSets AS ds ON ds.Name = ds_n.Name",
    "WHERE ds_n.n > 0"
  )
}

makeCountQuery <- function(dataset, group = NULL, group_label = NULL) {
  paste(
    "SELECT",
    "COUNT(participantid) AS n,",
    paste0("'", dataset, "' AS Name"),
    "FROM",
    dataset,
    ifelse(
      !is.null(group),
      paste("WHERE", paste(paste0("participantid.\"", group, "\" = '", group_label, "'"), collapse = ' OR ')),
      ""
    )
  )
}

removeContainerId <- function(lk){
  suppressWarnings(
    lk[,-c("container","Container")]
  )
}

getVarInfo <- function(assay_identifier, fields, labkeyUrlBase, schema = "study"){
  labkey.getQueryDetails(
    baseUrl = labkeyUrlBase,
    folderPath = "CAVD",
    schemaName = schema,
    queryName = assay_identifier
  ) |>
    setDT() |>
    _[, .(field_name = fieldName, caption, description)] |>
    _[field_name %in% fields]
}

downloadDocuments <- function(availableDocuments, downloadDir = tempdir()) {

  availableDocuments <-
    availableDocuments[,
    {

      src <- remote_path
      dst <- file.path(downloadDir, basename(src))

      message("downloading ", basename(src), " to `", downloadDir, "`...")

      ret <-
        suppressMessages(
          tryCatch(
            Rlabkey:::labkey.webdav.getByUrl( url, localFilePath = dst, overwrite = TRUE ),
            error = \(e) FALSE
          )
        )

      success <- ifelse(
        !is.null(ret) && !is.na(ret) && ret == FALSE,
        FALSE,
        file.exists(dst)
      )

      .(source = src, destination = dst, success = success)

    }, url][ availableDocuments, on = "url" ]

  if(any(availableDocuments[,!success]))
    warning("Failed to download: ", paste0(availableDocuments[success == FALSE, basename(source)], collapse = ", "))

  class(availableDocuments) <- c(class(availableDocuments), "downloadDocuments")

  return(availableDocuments)
}

unzipDocuments <- function(downloadDocuments){

  downloadDocuments[
    success & destination %like% ".+\\.zip",
    {
      suppressWarnings(
        dir.create(tools::file_path_sans_ext(destination))
      )
      unzip(destination, exdir = tools::file_path_sans_ext(destination), overwrite = TRUE)
      .(unzipDir = tools::file_path_sans_ext(destination))
    }, destination
  ][downloadDocuments, on = "destination"]

}

loadDocuments <- function(downloadDocuments){

  downloadDocuments[,.(
    datasets = unzipDir |>
      list.files("\\.csv$", full.names = T) |>
      lapply(fread)
  ), unzipDir
  ][downloadDocuments, on = "unzipDir"]

}

setDatasetNames <- function(datasets){
  for(i in datasets){
    setnames(
      i,
      c("prot", "study_prot", "SubjectId", "SubjectVisit/Visit"),
      c("study_id", "study_id", "participant_id", "participant_visit"),
      skip_absent=TRUE
    )
    setnames(
      i,
      names(i),
      tolower(names(i))
    )
  }
}

cleanReservedDataSpaceR6 <- function(nm){
  nm <- nm[
    !nm %in% c(
      ".__enclos_env__", "clone", "refresh", "print", "initialize", "config",
      "getStudy", "getGroup", "getMab", "filterMabGrid", "resetMabGrid",
      "mabGridSummary", "mabGrid", "virusMetadata"
    )
  ] |>
    sort()
}

labkey.getRequestOptions <- getFromNamespace("labkey.getRequestOptions", "Rlabkey")
labkey.get <- getFromNamespace("labkey.get", "Rlabkey")

