.onAttach <- function(libname, pkgname) {
  def_netrc <- ifelse(.Platform$OS.type == "windows", "~/_netrc", "~/.netrc")

  if (!file.exists(def_netrc) && !exists("labkey.sessionCookieName") && Sys.getenv("DS_login") == "") {
    packageStartupMessage("A netrc file is required to connect to the DataSpace.")
  }
}

.onLoad <- function(libname, pkgname) {
  # set ca bundle file path for windows
  if (.Platform$OS.type == "windows") {
    options(RCurlOptions = list(cainfo = system.file("ssl_certs/cacert.pem",
                                                     package = pkgname)))
  }
}
