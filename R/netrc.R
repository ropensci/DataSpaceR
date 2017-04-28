#' @title Write a netrc file
#'
#' @description Write a netrc file that is valid for accessing DataSpace
#'
#' @param login A \code{character}. The email address used for loging in on
#' DataSpace.
#' @param password A \code{character}. The password associated with the login.
#' @param file A \code{character}. The credentials will be written into that
#' file. If left NULL, the netrc will be written into a temporary file.
#'
#' @return A character vector containing the file paths for netrc
#' @seealso \code{\link{CreateConnection}} \code{\link{check_netrc}}
#' @examples
#' \dontrun{
#' write_netrc("dataspaceuser@email.com", "mypassword")
#' }
#' @export
write_netrc <- function(login, password, file = NULL) {
  string <- paste("machine dataspace-staging.cavd.org login", login, "password", password)
  if(is.null(file)) {
    file <- tempfile()
  } else if(file.exists(file)) {
    stop("The file you are trying to write to already exists. Remove manually if you wish to overwrite.")
  }
  write(string, file)
  return(file)
}

#' @title Check netrc file
#'
#' @description Check that there is a netrc file with a valid entry for DataSpace.
#'
#' @details
#' In order to connect to DataSpace, you will need a `.netrc` file in your
#' contains a `machine` name (hostname of DataSpace), and `login` and
#' `password`. See [here](https://www.labkey.org/wiki/home/Documentation/page.view?name=netrc)
#' for more information. By default \code{RCurl} will look for the file in your
#' home directoty.
#'
#' If no netrc is available or it is not formatted properly, \code{\link{write_netrc}}
#' can be used to write one. Otherwise, when specifying login and password in
#' \code{CreateConnection}, a temporary file will be created for that connection.
#'
#' @return The name of the netrc file
#' @seealso \code{\link{CreateConnection}} \code{\link{write_netrc}}
#' @examples
#' \dontrun{
#' check_netrc()
#' }
#' @export
check_netrc <- function() {
  if(exists("labkey.netrc.file", .GlobalEnv)) {
    netrc_file <- get("labkey.netrc.file", .GlobalEnv)
  } else {
    netrc_file <- "~/.netrc"
  }
  if(!file.exists(netrc_file)) {
    stop("There is no netrc file. Use `write_netrc()`")
  } else {
    cat("netrc file found at", netrc_file)
  }
  lines <- readLines(netrc_file)
  lines <- gsub("http.*//", "", lines)
  if(length(grep("machine\\sdataspace-staging.cavd.org", lines)) == 0) {
    stop("No entry found for dataspace-staging.cavd.org in the netrc file.")
  }
  cat(", and it looks valid.\n")
  return(netrc_file)
}
