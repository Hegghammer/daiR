#' Run when daiR is attached
#' @importFrom fs dir_ls
#' @importFrom googleCloudStorageR gcs_upload
#' @param libname name of library
#' @param pkgname name of package
#' @return no return value, called for side effects
#' @export

.onAttach <- function(libname, pkgname) {

  packageStartupMessage(glue::glue("Welcome to daiR {utils::packageVersion('daiR')}, your gateway to Google Document AI v1."))

  dai_auth()

}
