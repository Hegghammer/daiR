#' Run when daiR is attached
#' @importFrom fs dir_ls
#' @importFrom googleCloudStorageR gcs_upload
#' @param libname name of library
#' @param pkgname name of package

.onAttach <- function(libname, pkgname) {

  packageStartupMessage("Welcome to daiR 0.9.9, your gateway to Google Document AI v1.")

  dai_auth()

}
