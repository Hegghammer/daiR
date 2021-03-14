#' Run when daiR loads
#'
#' @param libname name of library
#' @param pkgname name of package

.onLoad <- function(libname, pkgname) {

  .auth <<- gargle::init_AuthState(
    package     = "daiR",
    auth_active = TRUE
  )

  invisible()

}

#' Run when daiR is attached
#'
#' @param libname name of library
#' @param pkgname name of package

.onAttach <- function(libname, pkgname) {

  packageStartupMessage("Welcome to daiR 0.4.0, your gateway to Google Document AI v1beta2.")

  if (grepl("json$", Sys.getenv("GCS_AUTH_FILE"))) {
    dai_auth()
  }

}

