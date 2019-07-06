isWindows <- function() {
  .Platform$OS.type == "windows"
}

getUrlBase <- function(onStaging) {
  production <- paste0("https://", PRODUCTION)
  staging <- paste0("https://", STAGING)

  if (exists("labkey.url.base", .GlobalEnv)) {
    labkey.url.base <- get("labkey.url.base", .GlobalEnv)
    labkey.url.base <- gsub("/$", "", labkey.url.base)
    assert_that(labkey.url.base == production || labkey.url.base == staging,
                msg = paste("labkey.url.base should be either",
                            production, "or", staging))
  } else {
    if (onStaging) {
      labkey.url.base <- staging
    } else {
      labkey.url.base <- production
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
    netrcFile <- ifelse(isWindows(), "~/_netrc", "~/.netrc")
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
      stop("'study' cannot be NULL.", call. = FALSE)
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
      stop(paste0("'", reqStudy, "' is not a valid study."), call. = FALSE)
    } else {
      stop(paste0("'", reqStudy, " is not a valid study.\nValid studies: ",
                  paste(validStudies, collapse = ", ")), call. = FALSE)
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
  if (!is.null(login) && !is.null(password)) {
    netrc <- writeNetrc(login, password, onStaging = onStaging)
  } else if (exists("labkey.netrc.file", .GlobalEnv)) {
    netrc <- get("labkey.netrc.file", .GlobalEnv)
  } else {
    netrc <- paste0(
      Sys.getenv("HOME"),
      ifelse(isWindows(), "\\_netrc", "/.netrc")
    )
  }

  netrc
}

#' @importFrom utils packageVersion
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

  res <- GET(
    url = url,
    config = Rlabkey:::labkey.getRequestOptions()
  )

  if (res$status_code == 200) {
    if (grepl("json", res$headers$`content-type`)) {
      parsed <- content(res)

      if (parsed$displayName == "guest") {
        stop("Invalid credential or deactivated account. Check your account in the portal.", call. = FALSE)
      } else {
        return(TRUE)
      }
    } else {
      stop("Something went wrong. Check the portal and try again.", call. = FALSE)
    }
  } else if (res$status_code == 401) {
    stop("Invalid credential or deactivated account. Check your account in the portal.", call. = FALSE)
  } else if (res$status_code == 403) {
    stop("The portal is in admin-only mode. Please try again later.", call. = FALSE)
  } else {
    stop("Something went wrong. Check the portal and try again.", call. = FALSE)
  }
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
        paste0("participantid.\"", names(group), "\" = '", group, "'")
    )
  }

  query
}

assertColumn <- function(using) {
  assert_that(
    using %in% c("mab_mixture", "donor_species", "isotype", "hxb2_location", "viruses", "clades", "tiers", "curve_ic50", "studies"),
    msg = paste("\"", using, "\" is not a valid column.")
  )
}

switchColumn <- function(using) {
  switch(
    using,
    "mab_mixture" = "mab_mix_name_std",
    "donor_species" = "mab_donor_species",
    "isotype" = "mab_isotype",
    "hxb2_location" = "mab_hxb2_location",
    "viruses" = "virus",
    "clades" = "clade",
    "tiers" = "neutralization_tier",
    "curve_ic50" = "titer_curve_ic50",
    "studies" = "study"
  )
}

isFromMabGrid <- function(column) {
  column %in% c("mab_mix_name_std", "virus", "clade", "neutralization_tier", "titer_curve_ic50", "study")
}
