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
#' supported file types. Note that on Linux, ImageMagick may not allow
#' conversion to pdf for security reasons. The setting can be turned off;
#' see: https://stackoverflow.com/a/52863413.
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

  # checks
  if (!(is.vector(files))) {
    stop("'files' input not a vector.")
  }

  if (!(is.character(files))) {
    stop("'files' input not a character vector.")
  }

  if (!(grepl("\\.pdf$", pdf_name))) {
    stop("Destination filename not .pdf")
  }

  # convert
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
                          bucket = Sys.getenv("GCS_DEFAULT_BUCKET")
                          ) {

  # check
  if (!(length(fname) == 1 && is.character(fname))) {
    stop("Invalid folder name format.")
  }

  if (!(length(bucket) == 1 && is.character(bucket)) | bucket == "") {
    stop("Invalid bucket name format.")
  }

  if (!(grepl("/$", fname))){
    fname <- glue::glue("{fname}/")
  }

  # create empty file
  empty <- tempfile()

  fs::file_create(empty)

  # upload to Google Storage
  out <- googleCloudStorageR::gcs_upload(empty,
                                  bucket = bucket,
                                  name = fname
                                  )
  return(out)
}

#' Check that a file is pdf
#'
#' @param file a filepath
#' @return boolean
#' @export
#' @examples
#' \dontrun{
#' is_pdf("document.pdf")
#' }

is_pdf <- function(file){

  result <- suppressMessages(try(pdftools::pdf_info(file), silent = TRUE))

  if (class(result) != "try-error") return(TRUE)

  return(FALSE)
}



#' Check that a file is json
#'
#' @param file a filepath
#'
#' @return boolean
#' @export
#'
#' @examples
#' \dontrun{
#' is_json("file.json")
#' }

is_json <- function(file){

  result <- suppressMessages(try(jsonlite::fromJSON(file), silent = TRUE))

  if (class(result) != "try-error") return(TRUE)

  return(FALSE)
}

#' Convert pdf to base64-encoded binary tiff
#' @param file path to a single-page pdf file
#' @export
#' @return string containing base64-encoded binary tiff

pdf_to_binbase <- function(file) {

  if (!(is_pdf(file))){
    stop("Input file not a pdf.")
  }

  img <- magick::image_read_pdf(file)

  img_gray <- magick::image_convert(img, colorspace = "Gray")

  filepath <- file.path(tempdir(), "dai_temp.tiff")

  magick::image_write(img_gray, filepath, format = "tiff", compression = "JPEG")

  enc <- base64enc::base64encode(filepath)

  return(enc)
}

#' Convert image file to base64-encoded binary tiff
#' @param file path to an image file
#' @export
#' @return string containing base64-encoded binary tiff

img_to_binbase <- function(file) {

  if (is_pdf(file)){
    stop("Input file is .pdf.")
  }

  img <- magick::image_read(file)

  img_gray <- magick::image_convert(img, colorspace = "Gray")

  filepath <- file.path(tempdir(), "dai_temp.tiff")

  magick::image_write(img_gray, filepath, format = "tiff", compression = "JPEG")

  enc <- base64enc::base64encode(filepath)

  return(enc)
}
