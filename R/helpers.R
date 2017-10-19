getUrlBase <- function(onStaging) {
  if (exists("labkey.url.base", .GlobalEnv)) {
    labkey.url.base <- get("labkey.url.base", .GlobalEnv)
  } else {
    if (onStaging) {
      labkey.url.base <- "https://dataspace-staging.cavd.org"
    } else {
      labkey.url.base <- "https://dataspace.cavd.org"
    }
  }

  labkey.url.base <- gsub("http:", "https:", labkey.url.base)
  if (length(grep("^https://", labkey.url.base)) == 0) {
    labkey.url.base <- paste0("https://", labkey.url.base)
  }

  labkey.url.base
}

getUserEmail <- function(labkey.url.base, login) {
  if (exists("labkey.user.email", .GlobalEnv)) {
    labkey.user.email <- get("labkey.user.email", .GlobalEnv)
  } else if (!is.null(login)) {
    labkey.user.email <- login
  } else if (file.exists("~/.netrc") || file.exists("~/_netrc")) {
    netrcFile <- ifelse(.Platform$OS.type == "windows", "~/_netrc", "~/.netrc")
    netrc <- readChar(netrcFile, file.info(netrcFile)$size)
    netrc <- strsplit(netrc, split = "\\s+")[[1]]

    if (length(netrc) %% 6 == 0) {
      url.base <- gsub("https://", "", labkey.url.base)
      labkey.user.email <- netrc[which(url.base == netrc) + 2]
    } else {
      labkey.user.email <- ""
    }
  } else {
    labkey.user.email <- ""
  }

  labkey.user.email
}

getUrlPath <- function(study) {
  if (exists("labkey.url.path", .GlobalEnv)) {
    if (is.null(study)) {
      labkey.url.path <- get("labkey.url.path", .GlobalEnv)
    } else {
      labkey.url.path <- file.path("", "CAVD", tolower(study))
    }
  } else {
    if (is.null(study)) {
      stop("study cannot be NULL")
    } else if (study == "") {
      labkey.url.path <- file.path("", "CAVD")
    } else {
      labkey.url.path <- file.path("", "CAVD", tolower(study))
    }
  }

  labkey.url.path
}

getValidStudies <- function(labkey.url.base) {
  folders <- lsFolders(getSession(labkey.url.base, folderPath = "CAVD"))
  validStudies <- grep("\\w+\\d+", basename(folders), value = TRUE)

  validStudies
}

checkStudy <- function(study, labkey.url.base, verbose = FALSE) {
  validStudies <- getValidStudies(labkey.url.base)
  reqStudy <- tolower(study)

  if (!reqStudy %in% c("", validStudies)) {
    if (!verbose) {
      stop(paste0(reqStudy, " is not a valid study"))
    } else {
      stop(paste0(reqStudy, " is not a valid study\nValid studies: ",
                  paste(validStudies, collapse = ", ")))
    }
  }

  invisible(NULL)
}

fixStudy <- function(study, labkey.url.base, labkey.url.path) {
  if (is.null(study)) {
    study <- basename(labkey.url.path)
  }

  # check if `study` is an actual study
  checkStudy(study, labkey.url.base)

  study
}

getNetrc <- function(login, password, onStaging = FALSE) {
  if (onStaging) {
    machine <- "dataspace-staging.cavd.org"
  } else {
    machine <- "dataspace.cavd.org"
  }

  if (!is.null(login) && !is.null(password)) {
    netrc <- writeNetrc(login, password, machine)
  } else if (exists("labkey.netrc.file", .GlobalEnv)) {
    netrc <- get("labkey.netrc.file", .GlobalEnv)
  } else {
    if (.Platform$OS.type == "windows") {
      netrc <- paste0(Sys.getenv("HOME"), "\\_netrc")
    } else {
      netrc <- paste0(Sys.getenv("HOME"), "/.netrc")
    }
  }

  netrc
}

#' @importFrom utils packageVersion
setCurlOptions <- function(netrcFile) {
  useragent <- paste("DataSpaceR", packageVersion("DataSpaceR"))

  curlOptions <- labkey.setCurlOptions(netrc.file = netrcFile,
                                       useragent = useragent)

  curlOptions
}

makeCountQuery <- function(dataset, group) {
  query <-
    paste("SELECT",
            "COUNT(participantid) AS n,",
            paste0("'", dataset, "' AS Name"),
          "FROM",
            dataset)

  if (!is.null(group)) {
    query <- paste(
      query,
      "WHERE",
        paste0("participantid.\"", group, "\" = '", group, "'")
    )
  }

  query
}
