#' @title Create a connection to DataSpace
#'
#' @description Constructor for \code{\link{DataSpaceConnection-class}}
#'
#' @details Instantiates an \code{DataSpaceConnection} for \code{study}
#' The constructor will try to take the values of the various `\code{labkey.*}`
#' parameters from the global environment. If they don't exist, it will use
#' default values. These are assigned to `options`, which are then used by the
#' \code{DataSpaceConnection} class.
#'
#' @param study A character. Name of the study to connect.
#' @param login A character. Optional argument. If there is no netrc
#' file a temporary one can be written by passing login and password of an
#' active DataSpace account.
#' @param password A character. Optional. The password for the selected
#' login.
#' @param verbose A logical. Whether to print the extra details for
#' troubleshooting.
#'
#' @return an instance of \code{DataSpaceConnection}
#' @seealso \code{\link{DataSpaceConnection-class}}
#' @examples
#' \dontrun{
#' # Single study
#' con <- CreateConnection("cvd408")
#'
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
