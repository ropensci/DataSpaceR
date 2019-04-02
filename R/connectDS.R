#' @title Create a connection to DataSpace
#'
#' @description Constructor for \code{\link{DataSpaceConnection}}
#'
#' @details Instantiates an \code{DataSpaceConnection}.
#' The constructor will try to take the values of the various \code{labkey.*}
#' parameters from the global environment. If they don't exist, it will use
#' default values. These are assigned to `options`, which are then used by the
#' \code{DataSpaceConnection} class.
#'
#' @param login A character. Optional argument. If there is no netrc
#' file a temporary one can be written by passing login and password of an
#' active DataSpace account.
#' @param password A character. Optional. The password for the selected
#' login.
#' @param verbose A logical. Whether to print the extra details for
#' troubleshooting.
#' @param onStaging A logical. Whether to connect to the staging server instead
#' of the production server.
#'
#' @return an instance of \code{DataSpaceConnection}
#' @seealso \code{\link{DataSpaceConnection}}
#' @examples
#' \dontrun{
#' con <- connectDS()
#' }
#'
#' con <- try(connectDS())
#' if (inherits(con, "try-error")) {
#'   warning("Read README for more information on how to set up a .netrc file.")
#' }
#' @export
connectDS <- function(login = NULL,
                      password = NULL,
                      verbose = FALSE,
                      onStaging = FALSE) {
  DataSpaceConnection$new(login, password, verbose, onStaging)
}
