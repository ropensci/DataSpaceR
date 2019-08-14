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
  reqStudy <- tolower(study)

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
checkCredential <- function(onStaging, verbose) {
  if (verbose) message("Checking credential...")

  url <- paste0(
    "https://",
    ifelse(onStaging, STAGING, PRODUCTION),
    "/login-whoami.view"
  )

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
  switch(
    using,
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

#' @importFrom Rlabkey makeFilter
#' @export
Rlabkey::makeFilter

#' @importFrom utils getFromNamespace
labkey.getRequestOptions <- getFromNamespace("labkey.getRequestOptions", "Rlabkey")
labkey.get <- getFromNamespace("labkey.get", "Rlabkey")
