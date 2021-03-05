#' Get service account token to use daiR
#'
#' @param scopes scopes wanted for the token.
#' @param env_var name of the .Renviron variable containing the path to the json file with your service account key.
#'
#' @return access token
#' @export
#'
#' @examples
#' \dontrun{
#' # A simple call with default parameters:
#' dai_auth()
#'
#' # A tailored call:
#' dai_auth(scopes = c("https://www.googleapis.com/auth/cloud-platform",
#'                     "https://www.googleapis.com/auth/bigquery"),
#'          env_var = "ANOTHER_AUTH_FILE")
#'}

dai_auth <- function(scopes = "https://www.googleapis.com/auth/cloud-platform",
                     env_var = "GCS_AUTH_FILE"
                     ) {

  if (!(length(env_var) == 1)){
    stop("Error: Environment variable must be length 1.")
  }

  # get token
  google_token <- gargle::credentials_service_account(scopes = scopes,
                                                      path = Sys.getenv(env_var)
                                                      )

  glue::glue("Setting scopes to {scopes}.")

  glue::glue("Successfully auto-authenticated via '{basename(Sys.getenv(env_var))}'.")

  return(google_token)
}

#' Get project id from service account file
#'
#' @param env_var name of the .Renviron variable containing the path to the json file with your service account key.
#'
#' @return string with your project id
#' @export
#'
#' @examples
#' \dontrun{
#'project_id <- get_project_id()
#'}

get_project_id <- function(env_var = "GCS_AUTH_FILE") {

  json <- jsonlite::fromJSON(Sys.getenv(env_var))

  project_id <- json[[2]]

  return(project_id)
}

#' Convert images to PDF
#'
#' This function converts a vector of images to a single pdf.
#'
#' @param files a vector of image files
#' @param pdf_name name of the new pdf
#'
#' @details Combines any number of image files of almost any type
#' to a single PDF. The vector can consist of different image file types.
#' See the \code{magick} package documentation for details on
#' supported file types.
#'
#' @export
#' @examples
#' \dontrun{
#' # Single file
#' image_to_pdf("document.jpg", "document.pdf")
#'
#' # A vector of files:
#' image_to_pdf(images)
#' }

image_to_pdf <- function(files, pdf_name) {

  if (!(is.vector(files))) {
    stop("Error: 'files' input not a vector.")
  }

  if (!(is.character(files))) {
    stop("Error: 'files' input not a character vector.")
  }

  if (!(grepl(".pdf$", pdf_name))) {
    stop("Error: destination filename not .pdf")
  }

  magick::image_write(magick::image_read(files),
                      format = "pdf",
                      pdf_name
                      )
}

#' Create folder in Google Storage
#'
#' @param fname name of new folder
#' @param bucket name of bucket. Defaults to global bucket if set.
#'
#' @details Creates a "folder" in your Google Storage bucket by uploading an empty file.
#' @export
#'
#' @examples
#' \dontrun{
#' create_folder("results")
#' create_folder("pdfs/documents/json_output")
#' }

create_folder <- function(fname,
                          bucket = googleCloudStorageR::gcs_get_global_bucket()
                          ) {

  if (!(length(fname) == 1 && is.character(fname))) {
    stop("Error: invalid folder name format.")
  }

  if (!(length(bucket) == 1 && is.character(bucket))) {
    stop("Error: invalid bucket name format.")
  }

  if (!(grep("/$", fname))){
    fname <- glue::glue("{fname}/")
  }

  fs::file_create("emptyfile")

  googleCloudStorageR::gcs_upload("emptyfile",
                                  bucket = bucket,
                                  name = fname
                                  )

  glue::glue("Created folder '{bucket}/{fname}'.")

}
