
## IMAGE_TO_PDF ----------------------------------------------------------------

test_that("image_to_pdf() warns of input errors", {
  expect_error(image_to_pdf(TRUE)) # if logical
  expect_error(image_to_pdf(1)) # if numeric
  expect_error(image_to_pdf(mtcars)) # if dataframe
  expect_error(image_to_pdf(as.matrix(mtcars))) # if matrix
  expect_error(image_to_pdf("foo.png", "foopdf")) # if pdf_name not .pdf
  expect_error(image_to_pdf("foo.png", "foo.png")) # if pdf_name not .pdf
} )

test_that("image_to_pdf() returns a pdf file", {
  output <- file.path(tempdir(), "output.pdf")
  image <- testthat::test_path("examples", "image.jpg")
  image_to_pdf(image, output)
  expect_true(daiR::is_pdf(output))
  unlink(output, force = TRUE)
} )

test_that("image_to_pdf() handles different formats and multiple files", {

  output <- file.path(tempdir(), "output.pdf")

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
    filepath <- file.path(tempdir(), glue::glue('{no_ext}.{type_out}'))
    magick::image_write(img, filepath, format = type_out)
  }

  # convert example file to different formats
  jpg <- testthat::test_path("examples", "image.jpg")
  types <- c("jpeg", "png", "bmp", "gif", "tiff")
  purrr::map(types, ~ convert(jpg, .x))
  files <- list.files(tempdir(),
                      pattern = stringr::str_extract(basename(jpg), ".*(?=\\.\\w{3,4}$)"),
                      full.names = TRUE)

  # test on each image type
  expect_true(all(purrr::map_lgl(files, renders)))

  # test on vector of image files
  expect_true(renders(files))

  # cleanup
  tmp_files <- c(files, output)
  unlink(tmp_files, force = TRUE)
})

## CREATE_FOLDER ---------------------------------------------------------------

test_that("create_folder() warns of input errors", {
  expect_error(create_folder(fname = c("folder1", "folder2")), "Invalid folder name format.")
  expect_error(create_folder(fname = 2021), "Invalid folder name format.")
  expect_error(create_folder(fname = "folder", bucket = c("two_buckets", "at_once")), "Invalid bucket name format.")
  expect_error(create_folder(fname = "folder", bucket = 2021), "Invalid bucket name format.")
} )

test_that("create_folder() works", {
  skip_on_ci()
  skip_if_offline()

  folder1 <- "foo"
  folder2 <- "foo/"
  folder3 <- "many/sub/folders/"
  folder4 <- "2021_01/"

  out1 <- create_folder(folder1)
  expect_identical(out1$name, paste0(folder1, "/"))
  out2 <- create_folder(folder2)
  expect_identical(out2$name, folder2)
  out3 <- create_folder(folder3)
  expect_identical(out3$name, folder3)
  out4 <- create_folder(folder4)
  expect_identical(out4$name, folder4)

  bucket <- Sys.getenv("GCS_DEFAULT_BUCKET")
  out5 <- create_folder(folder1, bucket)
  expect_identical(out5$bucket, bucket)
  dai_auth()
})

## IS_PDF ----------------------------------------------------------------------

test_that("is_pdf() calls out non-pdfs", {
  image <- testthat::test_path("examples", "image.jpg")
  fake <- tempfile(fileext = ".pdf")
  fs::file_create(fake)
  nonexist <- "nonexist.pdf"
  expect_false(is_pdf(image))
  expect_false(is_pdf(fake))
  expect_false(is_pdf(nonexist))
  unlink(fake, force = TRUE)
} )

## IS_JSON ----------------------------------------------------------------------

# TODO

## PDF_TO_BINBASE --------------------------------------------------------------

test_that("pdf_to_binbase() rejects non-pdfs", {
  image <- testthat::test_path("examples", "image.jpg")
  expect_error(pdf_to_binbase(image), "Input file not a pdf.")
})

test_that("pdf_to_binbase() produces a base64 string", {
  image <- testthat::test_path("examples", "image.jpg")
  output <- file.path(tempdir(), "output.pdf")
  image_to_pdf(image, output)
  base <- pdf_to_binbase(output)
  expect_type(base, "character")
  expect_match(base, "^[a-zA-Z0-9+/]+={,2}$")
  unlink(output, force = TRUE)
})

## IMG_TO_BINBASE --------------------------------------------------------------

test_that("img_to_binbase() rejects pdfs", {
  image <- testthat::test_path("examples", "image.jpg")
  output <- file.path(tempdir(), "output.pdf")
  image_to_pdf(image, output)
  expect_error(img_to_binbase(output), "Input file is .pdf.")
  unlink(output, force = TRUE)
})

test_that("img_to_binbase() produces a base64 string", {
  image <- testthat::test_path("examples", "image.jpg")
  base <- img_to_binbase(image)
  expect_type(base, "character")
  expect_match(base, "^[a-zA-Z0-9+/]+={,2}$")
})
