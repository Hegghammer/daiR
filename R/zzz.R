#' Run when daiR is attached
#'
#' @param libname name of library
#' @param pkgname name of package

.onLoad <- function(libname, pkgname) {

  .auth <<- gargle::init_AuthState(
    package     = "daiR",
    auth_active = TRUE
  )

  invisible()

  dai_auth()

}
