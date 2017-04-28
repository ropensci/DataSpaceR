#' CreateConnection
#'
#' @param study A \code{"character"} vector naming the study.
#' @param login A \code{"character"}. Optional argument. If there is no netrc
#' file a temporary one can be written by passing login and password of an
#' active DataSpace account.
#' @param password A \code{"character"}. Optional. The password for the selected
#' login.
#' @param verbose A \code{"logical"} whether to print the extra details for
#' troubleshooting.
#'
#' @return an instance of an \code{DataSpaceConnection}
#'
#' @examples
#' \dontrun{
#' # Single study
#' con <- CreateConnection("cvd408")
#' # Cross study
#' con <- CreateConnection("")
#' }
#' @export
#' @importFrom utils packageVersion
CreateConnection <- function(study = NULL,
                             login = NULL,
                             password = NULL,
                             verbose = FALSE) {
  .DSCon$new(study, login, password, verbose)
}
