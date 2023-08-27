## OBJECT TYPES ----------------------------------------------------------------

null <- NULL
na <- NA
boolean <- TRUE
number_random <- sample(1:1000, 1)
string_random <- paste0(sample(letters, 5), collapse = "")
vector_strings <- c("foo", "bar")
list_strings <- list("foo", "bar")
df <- mtcars
matrix <- as.matrix(mtcars)

# correct but irrelevant JSON file
fill <- list("a" = 1, "b" = 2) 
json <- jsonlite::toJSON(fill)
madeup_json_file <- tempfile(fileext = ".json")
write(json, madeup_json_file)

## IMAGE_TO_PDF ----------------------------------------------------------------

test_that("image_to_pdf() warns of input errors", {
  expect_error(image_to_pdf(null))
  expect_error(image_to_pdf(na))
  expect_error(image_to_pdf(boolean))
  expect_error(image_to_pdf(number_random))
  expect_error(image_to_pdf(df))
  expect_error(image_to_pdf(matrix))
  expect_error(image_to_pdf(vector_strings))
  expect_error(image_to_pdf(list_strings))
} )

test_that("image_to_pdf() returns a pdf file", {
  skip_on_cran()
  skip_on_ci()
  output <- file.path(tempdir(), "output.pdf")
  image <- testthat::test_path("examples", "image.jpg")
  image_to_pdf(image, output) # NB sometimes magick problem on Linux
  expect_true(daiR::is_pdf(output))
  unlink(output, force = TRUE)
} )

test_that("image_to_pdf() handles different formats and multiple files", {
  #skip() # NB sometimes magick problem on Linux
  skip_on_cran()
  skip_on_ci()
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
})

## IS_JSON ----------------------------------------------------------------------

test_that("is_json() calls out non-jsons", {
  image <- testthat::test_path("examples", "image.jpg")
  pdf <- testthat::test_path("examples", "sample.pdf")
  txt <- file.path(tempdir(), "foo.txt")
  write("Lorem ipsum", txt)
  csv <- file.path(tempdir(), "bar.csv")
  write("Lorem,ipsum", csv)
  empty <- tempfile(fileext = ".json")
  fs::file_create(empty)
  nonexist <- "nonexist.json"
  #expect_false(is_json(image))
  #expect_false(is_json(pdf))
  expect_false(is_json(txt))
  expect_false(is_json(csv))
  expect_false(is_json(empty))
  expect_false(is_json(nonexist))
  unlink(c(txt, csv, empty), force = TRUE)
})

test_that("is_json() recognizes jsons", {
  json1 <- testthat::test_path("examples", "output.json")
  json2 <- testthat::test_path("examples", "output_blank.json")
  json3 <- testthat::test_path("examples", "sample_v1.json")
  json4 <- testthat::test_path("examples", "sample3pg.json")
  expect_true(is_json(json1))
  expect_true(is_json(json2))
  expect_true(is_json(json3))
  expect_true(is_json(json4))
} )

## PDF_TO_BINBASE --------------------------------------------------------------

test_that("pdf_to_binbase() rejects non-pdfs", {
  skip_on_cran()
  skip_on_ci()
  image <- testthat::test_path("examples", "image.jpg")
  expect_error(pdf_to_binbase(image), "Input file not a pdf.")
})

test_that("pdf_to_binbase() produces a base64 string", {
  #skip() # NB sometimes magick problem on Linux
  skip_on_cran()
  skip_on_ci()
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
  #skip() # NB sometimes magick problem on Linux
  skip_on_cran()
  skip_on_ci()
  image <- testthat::test_path("examples", "image.jpg")
  output <- file.path(tempdir(), "output.pdf")
  image_to_pdf(image, output)
  expect_error(img_to_binbase(output), "Input file is .pdf.")
  unlink(output, force = TRUE)
})

test_that("img_to_binbase() produces a base64 string", {
  #skip() # NB sometimes magick problem on Linux
  skip_on_cran()
  skip_on_ci()
  image <- testthat::test_path("examples", "image.jpg")
  base <- img_to_binbase(image)
  expect_type(base, "character")
  expect_match(base, "^[a-zA-Z0-9+/]+={,2}$")
})
