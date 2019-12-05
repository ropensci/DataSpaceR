#' @title Write a netrc file
#'
#' @description Write a netrc file that is valid for accessing DataSpace.
#'
#' @details
#' The database is accessed with the user's credentials.
#' A netrc file storing login and password information is required.
#' See \href{https://docs.ropensci.org/DataSpaceR/}{here}
#' for instruction on how to register and set DataSpace credential.
#' By default \code{curl} will look for the file in your home directory.
#'
#' @param login A character. Email address used for logging in on DataSpace.
#' @param password A character. Password associated with the login.
#' @param netrcFile A character. Credentials will be written into that file.
#' If left NULL, netrc will be written into a temporary file.
#' @param onStaging A logical. Whether to connect to the staging server instead
#' of the production server.
#' @param overwrite A logical. Whether to overwrite the existing netrc file.
#'
#' @return A character vector containing the netrc file path
#' @seealso \code{\link{connectDS}} \code{\link{checkNetrc}}
#' @examples
#' # First, create an account in the DataSpace App and read the terms of use
#' # Next, create a netrc file using writeNetrc()
#' writeNetrc(
#'   login = "dataspaceuser@email.com",
#'   password = "yourSecretPassword"
#' )
#' # Specify `netrcFile = getNetrcPath()` to write netrc in the default path
#' @export
writeNetrc <- function(login,
                       password,
                       netrcFile = NULL,
                       onStaging = FALSE,
                       overwrite = FALSE) {
  if (is.null(netrcFile)) netrcFile <- tempfile()
  if (file.exists(netrcFile) && !overwrite) {
    stop(
      "'", netrcFile, "' already exists. ",
      "Set `overwrite=TRUE` if you'd like to overwrite.",
      call. = FALSE
    )
  }

  string <- paste(
    "machine", ifelse(onStaging, STAGING, PRODUCTION),
    "login", login,
    "password", password
  )

  # create a netrc file
  write(string, netrcFile)

  # set the owner-only permission
  if (!isWindows()) {
    Sys.chmod(netrcFile, mode = "600")
  }

  invisible(netrcFile)
}

#' @title Check netrc file
#'
#' @description Check that there is a netrc file with a valid entry for the
#' CAVD DataSpace.
#'
#' @param netrcFile A character. File path to netrc file to check.
#' @param onStaging A logical. Whether to check the staging server instead
#' of the production server.
#' @param verbose A logical. Whether to print the extra details for
#' troubleshooting.
#'
#' @return The name of the netrc file
#' @seealso \code{\link{connectDS}} \code{\link{writeNetrc}}
#' @examples
#' try(checkNetrc())
#' @export
checkNetrc <- function(netrcFile = getNetrcPath(),
                       onStaging = FALSE,
                       verbose = TRUE) {
  if (exists("labkey.netrc.file", .GlobalEnv)) {
    netrcFile <- get("labkey.netrc.file", .GlobalEnv)
  }

  if (!file.exists(netrcFile)) {
    stop(
      "There is no netrc file. Use `writeNetrc()` to create one.",
      call. = FALSE
    )
  }

  lines <- readLines(netrcFile)
  lines <- gsub("http.*//", "", lines)
  machine <- ifelse(onStaging, STAGING, PRODUCTION)
  if (length(grep(paste0("machine\\s", machine), lines)) == 0) {
    stop(
      "No entry found for '", machine, "' in '", netrcFile, "'.",
      call. = FALSE
    )
  }

  if (verbose) {
    message("netrc file found at '", netrcFile, "', and it looks valid.")
  }

  netrcFile
}

#' @title Get a default netrc file path
#'
#' @description Get a default netrc file path
#'
#' @return A character vector containing the default netrc file path
#'
#' @examples
#' getNetrcPath()
#' @export
getNetrcPath <- function() {
  home <- Sys.getenv("HOME")
  if (isWindows()) {
    file.path(home, "_netrc")
  } else {
    file.path(home, ".netrc")
  }
}
