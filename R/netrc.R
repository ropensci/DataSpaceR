#' @title Write a netrc file
#'
#' @description Write a netrc file that is valid for accessing DataSpace.
#'
#' @param login A character. Email address used for logging in on DataSpace.
#' @param password A character. Password associated with the login.
#' @param onStaging A logical. Whether to connect to the staging server instead
#' of the production server.
#' @param netrcFile A character. Credentials will be written into that file.
#' If left NULL, netrc will be written into a temporary file.
#'
#' @return A character vector containing the file paths for netrc
#' @seealso \code{\link{connectDS}} \code{\link{checkNetrc}}
#' @examples
#' \dontrun{
#' writeNetrc("dataspaceuser@email.com", "mypassword")
#' }
#' @export
writeNetrc <- function(login,
                       password,
                       onStaging = FALSE,
                       netrcFile = getNetrcPath()) {
  if (file.exists(netrcFile)) {
    stop(
      "'", netrcFile, "' already exists. ",
      "Remove it manually if you'd like to overwrite.",
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
#' @description Check that there is a netrc file with a valid entry for DataSpace.
#'
#' @param onStaging A logical. Whether to check the staging server instead
#' of the production server.
#'
#' @details
#' In order to connect to DataSpace, you will need a \code{.netrc} file in your
#' contains a \code{machine} name (hostname of DataSpace), and \code{login} and
#' \code{password}.
#' See \href{https://www.labkey.org/wiki/home/Documentation/page.view?name=netrc}{here}
#' for more information. By default \code{RCurl} will look for the file in your
#' home directoty.
#'
#' If no netrc is available or it is not formatted properly,
#' \code{\link{writeNetrc}}  can be used to write one.
#' Otherwise, when specifying login and password in \code{connectDS},
#' a temporary file will be created for that connection.
#'
#' @return The name of the netrc file
#' @seealso \code{\link{connectDS}} \code{\link{writeNetrc}}
#' @examples
#' \dontrun{
#' checkNetrc()
#' }
#' @export
checkNetrc <- function(onStaging = FALSE, verbose = TRUE) {
  if (exists("labkey.netrc.file", .GlobalEnv)) {
    netrcFile <- get("labkey.netrc.file", .GlobalEnv)
  } else {
    netrcFile <- getNetrcPath()
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

getNetrcPath <- function() {
  home <- Sys.getenv("HOME")
  if (isWindows()) {
    file.path(home, "_netrc")
  } else {
    file.path(home, ".netrc")
  }
}
