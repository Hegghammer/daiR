#' Convert images to PDF
#'
#' @description This helper function converts a vector of images to a
#' single PDF.
#' @param files a vector of image files
#' @param pdf_name a string with the name of the new PDF
#' @return no return value, called for side effects
#' @details Combines any number of image files of almost any type
#' to a single PDF. The vector can consist of different image file types.
#' See the 'Magick' package documentation <https://cran.r-project.org/package=magick>
#' for details on supported file types. Note that on Linux, ImageMagick may
#' not allow conversion to pdf for security reasons.
#' @export
#' @examples
#' \dontrun{
#' # Single file
#' new_pdf <- file.path(tempdir(), "document.pdf")
#' image_to_pdf("document.jpg", new_pdf)
#'
#' # A vector of image files:
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

#' Check that a file is PDF
#'
#' @description Checks whether a file is a PDF file.
#' @param file a filepath
#' @return a boolean
#' @export
#'
#' @examples
#' \dontrun{
#' is_pdf("document.pdf")
#' }

is_pdf <- function(file) {

  result <- suppressMessages(try(pdftools::pdf_info(file), silent = TRUE))

  return(!(inherits(result, "try-error")))
}

#' Check that a file is JSON
#'
#' @description Checks whether a file is a JSON file.
#' @param file a filepath
#' @return a boolean
#' @export
#'
#' @examples
#' \dontrun{
#' is_json("file.json")
#' }

is_json <- function(file) {

  result <- suppressMessages(try(jsonlite::fromJSON(file), silent = TRUE))

  return(!(inherits(result, "try-error")))

}

#' Check that a string is a valid colour representation
#'
#' @description Checks whether a string is a valid colour representation.
#' @param x a string
#' @return a boolean
#' @export
#'
#' @examples
#' \dontrun{
#' is_colour("red")
#' is_colour("#12345")
#' }

is_colour <- function(x) {

  result <- suppressMessages(try(grDevices::col2rgb(x), silent = TRUE))

  return(!(inherits(result, "try-error")))

}

#' PDF to base64 tiff
#'
#' @description Converts a PDF file to a base64-encoded binary .tiff file.
#' @param file path to a single-page pdf file
#' @return a base64-encoded string
#' @export
#' @examples
#' \dontrun{
#' doc_encoded <- pdf_to_binbase("document.pdf")
#' }

pdf_to_binbase <- function(file) {

  if (!(is_pdf(file))) {
    stop("Input file not a pdf.")
    }

  img <- magick::image_read_pdf(file)

  img_gray <- magick::image_convert(img, colorspace = "Gray")

  filepath <- file.path(tempdir(), "dai_temp.tiff")

  magick::image_write(img_gray, filepath, format = "tiff", compression = "JPEG")

  enc <- base64enc::base64encode(filepath)

  return(enc)

}

#' Image to base64 tiff
#'
#' @description Converts an image file to a base64-encoded binary .tiff file.
#' @param file path to an image file
#' @return a base64-encoded string
#' @export
#' @examples
#' \dontrun{
#' img_encoded <- img_to_binbase("image.png")
#' }

img_to_binbase <- function(file) {

  if (is_pdf(file)) {
    stop("Input file is .pdf.")
    }

  img <- magick::image_read(file)

  img_gray <- magick::image_convert(img, colorspace = "Gray")

  filepath <- file.path(tempdir(), "dai_temp.tiff")

  magick::image_write(img_gray, 
                      filepath, 
                      format = "tiff", 
                      compression = "JPEG"
                      )

  enc <- base64enc::base64encode(filepath)

  return(enc)

}
