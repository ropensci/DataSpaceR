.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "By exporting data from the CAVD DataSpace, ",
    "you agree to be bound by the Terms of Use available on ",
    "the CAVD DataSpace sign-in page at ",
    "https://dataspace.cavd.org/cds/CAVD/app.view?"
  )

  netrc <- getNetrcPath()

  if (!file.exists(netrc) &&
    !exists("labkey.sessionCookieName") &&
    Sys.getenv("DS_login") == "") {
    packageStartupMessage(
      "A netrc file is required to connect to the DataSpace."
    )
  }
}
