#' Run when daiR is attached
#'
#' @param libname name of library
#' @param pkgname name of package

.onAttach <- function(libname, pkgname) {

  packageStartupMessage("Welcome to daiR 0.9.5, your gateway to Google Document AI v1.")

  dai_auth()

}

