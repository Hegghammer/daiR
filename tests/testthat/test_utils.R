## DAI_AUTH --------------------------------------------------------------------


## GET_PROJECT_ID --------------------------------------------------------------


## IMAGE_TO_PDF ----------------------------------------------------------------

test_that("image_to_pdf() warns of input errors", {
  expect_error(image_to_pdf(mtcars)) # if dataframe
  expect_error(image_to_pdf(as.matrix(mtcars))) # if matrix
  expect_error(image_to_pdf(TRUE)) # if logical
  expect_error(image_to_pdf(1)) # if numeric
  expect_error(image_to_pdf(as.integer(1))) # if integer
  expect_error(image_to_pdf("foo.png", "foopdf")) # if pdf_name not .pdf
  expect_error(image_to_pdf("foo.png", "foo.png")) # if pdf_name not .pdf
} )

test_that("image_to_pdf() returns a pdf file", {
  output <- "output.pdf" # sample output filename
  image <- testthat::test_path("examples", "image.jpg")
  image_to_pdf(image, output)
  expect_true(daiR::is_pdf(output))
  file.remove(output)
} )

test_that("image_to_pdf() handles different formats and multiple files", {

  output <- "output.pdf" # sample output filename

  # create function to check that a file renders
  renders <- function(file) {
    image_to_pdf(file, output)
    if(file.exists(output)) {
      result <- suppressMessages(try(pdftools::pdf_info(output), silent = TRUE))
      if(class(result) != "try-error") return(TRUE)
      return(FALSE)
    }
    return(FALSE)
  }

  # create function to convert images
    convert <- function(path_in, type_out) {
    img <- magick::image_read(path_in)
    no_ext <- stringr::str_extract(basename(path_in), ".*(?=\\.\\w{3,4}$)")
    magick::image_write(img, glue::glue('{no_ext}.{type_out}'), format = type_out)
  }

  # convert example file to different formats
  jpg <- testthat::test_path("examples", "image.jpg")
  types <- c("jpeg", "png", "bmp", "gif", "tiff")
  purrr::map(types, ~ convert(jpg, .x))
  files <- list.files(pattern = stringr::str_extract(basename(jpg), ".*(?=\\.\\w{3,4}$)"))

  # test on each image type
  expect_true(all(purrr::map_lgl(files, renders)))

  # test on vector of image files
  expect_true(renders(files))

  # cleanup
  tmp_files <- c(files, output)
  file.remove(tmp_files)
})

## CREATE_FOLDER ---------------------------------------------------------------

## IS_PDF ----------------------------------------------------------------------

test_that("is_pdf() calls out non-pdfs", {
  image <- testthat::test_path("examples", "image.jpg")
  fs::file_create("fake.pdf")
  fake <- "fake.pdf"
  nonexist <- "nonexist.pdf"
  expect_false(is_pdf(image))
  expect_false(is_pdf(fake))
  expect_false(is_pdf(nonexist))
  file.remove(fake)
} )
