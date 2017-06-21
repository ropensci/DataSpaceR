#' @title Write a netrc file
#'
#' @description Write a netrc file that is valid for accessing DataSpace.
#'
#' @param login A character. Email address used for logging in on DataSpace.
#' @param password A character. Password associated with the login.
#' @param machine A character. Host of DataSpace.
#' @param netrcFile A character. Credentials will be written into that file.
#' If left NULL, netrc will be written into a temporary file.
#'
#' @return A character vector containing the file paths for netrc
#' @seealso \code{\link{connectDS}} \code{\link{check_netrc}}
#' @examples
#' \dontrun{
#' write_netrc("dataspaceuser@email.com", "mypassword")
#' }
#' @export
write_netrc <- function(login, password,
                        machine = "dataspace-staging.cavd.org",
                        netrcFile = NULL) {
  string <- paste("machine", machine,
                  "login", login,
                  "password", password)
  if(is.null(netrcFile)) {
    netrcFile <- tempfile()
  } else if(file.exists(netrcFile)) {
    stop("The file you are trying to write to already exists.\n
         Remove manually if you wish to overwrite.")
  }
  write(string, netrcFile)

  invisible(netrcFile)
}

#' @title Check netrc file
#'
#' @description Check that there is a netrc file with a valid entry for DataSpace.
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
#' \code{\link{write_netrc}}  can be used to write one.
#' Otherwise, when specifying login and password in \code{connectDS},
#' a temporary file will be created for that connection.
#'
#' @return The name of the netrc file
#' @seealso \code{\link{connectDS}} \code{\link{write_netrc}}
#' @examples
#' \dontrun{
#' check_netrc()
#' }
#' @export
check_netrc <- function() {
  if(exists("labkey.netrc.file", .GlobalEnv)) {
    netrcFile <- get("labkey.netrc.file", .GlobalEnv)
  } else {
    netrcFile <- "~/.netrc"
  }
  if(!file.exists(netrcFile)) {
    stop("There is no netrc file. Use `write_netrc()`")
  } else {
    cat("netrc file found at", netrcFile)
  }
  lines <- readLines(netrcFile)
  lines <- gsub("http.*//", "", lines)
  if(length(grep("machine\\sdataspace-staging.cavd.org", lines)) == 0) {
    stop("No entry found for dataspace-staging.cavd.org in the netrc file.")
  }
  cat(", and it looks valid.\n")

  invisible(netrcFile)
}
