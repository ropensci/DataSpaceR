.onAttach <- function(libname, pkgname) {
  packageStartupMessage("By exporting data from the CAVD DataSpace, you agree to be bound by the Terms of Use available on the CAVD DataSpace sign-in page at https://dataspace.cavd.org/cds/CAVD/app.view?")

  netrc <- ifelse(.Platform$OS.type == "windows", "~/_netrc", "~/.netrc")

  if (!file.exists(netrc) &&
      !exists("labkey.sessionCookieName") &&
      Sys.getenv("DS_login") == "") {
    packageStartupMessage("A netrc file is required to connect to the DataSpace.")
  }
}

.onLoad <- function(libname, pkgname) {
  # set ca bundle file path for windows
  if (.Platform$OS.type == "windows") {
    options(
      RCurlOptions = list(
        cainfo = system.file(
          "ssl_certs/cacert.pem",
          package = pkgname
        )
      )
    )
  }
}

.onUnload <- function(libname, pkgname) {
  # remove ca bundle file path for windows
  if (.Platform$OS.type == "windows") {
    options(RCurlOptions = NULL)
  }
}
