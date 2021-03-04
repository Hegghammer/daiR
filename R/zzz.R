#' Run when daiR is attached
#'
#' @param libname name of library
#' @param pkgname name of package

.onAttach <- function(libname, pkgname) {

  packageStartupMessage("Welcome to daiR 0.0.0.9900, your gateway to Google Document AI v1beta2.")

}

