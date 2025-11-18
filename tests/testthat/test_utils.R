## SETUP AND TEST FIXTURES -------------------------------------------------

# Basic invalid object types
null <- NULL
na <- NA
boolean <- TRUE
number_random <- sample(1:1000, 1)
string_random <- paste0(sample(letters, 5), collapse = "")
vector_strings <- c("foo", "bar")
list_strings <- list("foo", "bar")
df <- mtcars
matrix <- as.matrix(mtcars)

# Invalid JSON file (not a valid format)
fill <- list("a" = 1, "b" = 2)
json <- jsonlite::toJSON(fill)
madeup_json_file <- tempfile(fileext = ".json")
write(json, madeup_json_file)

## HELPER FUNCTIONS FOR TESTING --------------------------------------------

#' Test invalid inputs for image_to_pdf function
test_invalid_image_inputs <- function() {
  output_pdf <- file.path(tempdir(), "test.pdf")

  test_that("image_to_pdf() errors with non-vector 'files' parameter", {
    # These are not vectors, so fail the first check
    expect_error(image_to_pdf(df, output_pdf), "'files' input not a vector.")
    expect_error(image_to_pdf(matrix, output_pdf), "'files' input not a vector.")
  })

  test_that("image_to_pdf() errors with non-character vector 'files' parameter", {
    # These are vectors but not character vectors, so fail the second check
    expect_error(image_to_pdf(na, output_pdf), "'files' input not a character vector.")
    expect_error(image_to_pdf(boolean, output_pdf), "'files' input not a character vector.")
    expect_error(image_to_pdf(number_random, output_pdf), "'files' input not a character vector.")
    expect_error(image_to_pdf(list_strings, output_pdf), "'files' input not a character vector.")
  })

  test_that("image_to_pdf() errors with NULL 'files' parameter", {
    # NULL is a special case - it may behave differently
    expect_error(image_to_pdf(null, output_pdf))
  })

  test_that("image_to_pdf() errors with invalid 'pdf_name' parameter", {
    image <- testthat::test_path("examples", "image.jpg")

    # Non-.pdf extensions
    expect_error(image_to_pdf(image, "output.txt"), "Destination filename not .pdf")
    expect_error(image_to_pdf(image, "output.png"), "Destination filename not .pdf")
    expect_error(image_to_pdf(image, "output.jpg"), "Destination filename not .pdf")
    expect_error(image_to_pdf(image, "output"), "Destination filename not .pdf")
    expect_error(image_to_pdf(image, "output.PDF"), "Destination filename not .pdf")
  })
}

#' Test invalid inputs for validation functions (is_pdf, is_json, is_colour)
test_invalid_validation_inputs <- function(func_name, test_func) {
  test_that(paste0(func_name, " handles invalid inputs"), {
    # These shouldn't crash but return FALSE or handle gracefully
    expect_no_error(test_func(null))
    expect_no_error(test_func(na))
    expect_no_error(test_func(number_random))
    expect_no_error(test_func(string_random))
  })
}

## IMAGE_TO_PDF ----------------------------------------------------------------

test_invalid_image_inputs()

test_that("image_to_pdf() creates a PDF file from single image", {
  skip_on_cran()
  skip_on_ci()

  output <- file.path(tempdir(), "output.pdf")
  image <- testthat::test_path("examples", "image.jpg")

  # Clean up any existing file
  if (file.exists(output)) unlink(output, force = TRUE)

  image_to_pdf(image, output)

  expect_true(file.exists(output))
  expect_true(daiR::is_pdf(output))

  unlink(output, force = TRUE)
})

test_that("image_to_pdf() handles different image formats", {
  skip_on_cran()
  skip_on_ci()

  output <- file.path(tempdir(), "output.pdf")

  # Helper function to check if conversion works
  renders <- function(file) {
    if (file.exists(output)) unlink(output, force = TRUE)
    image_to_pdf(file, output)

    if (file.exists(output)) {
      result <- suppressMessages(try(pdftools::pdf_info(output), silent = TRUE))
      return(!inherits(result, "try-error"))
    }
    return(FALSE)
  }

  # Helper function to convert images to different formats
  convert <- function(path_in, type_out) {
    img <- magick::image_read(path_in)
    no_ext <- stringr::str_extract(basename(path_in), ".*(?=\\.\\w{3,4}$)")
    filepath <- file.path(tempdir(), glue::glue("{no_ext}.{type_out}"))
    magick::image_write(img, filepath, format = type_out)
    return(filepath)
  }

  # Convert example file to different formats
  jpg <- testthat::test_path("examples", "image.jpg")
  types <- c("jpeg", "png", "bmp", "gif", "tiff")
  files <- purrr::map_chr(types, ~ convert(jpg, .x))

  # Test on each image type
  expect_true(all(purrr::map_lgl(files, renders)))

  # Cleanup
  tmp_files <- c(files, output)
  unlink(tmp_files, force = TRUE)
})

test_that("image_to_pdf() handles vector of multiple image files", {
  skip_on_cran()
  skip_on_ci()

  output <- file.path(tempdir(), "output_multi.pdf")

  # Helper to convert images
  convert <- function(path_in, type_out) {
    img <- magick::image_read(path_in)
    no_ext <- stringr::str_extract(basename(path_in), ".*(?=\\.\\w{3,4}$)")
    filepath <- file.path(tempdir(), glue::glue("{no_ext}.{type_out}"))
    magick::image_write(img, filepath, format = type_out)
    return(filepath)
  }

  # Create multiple image files
  jpg <- testthat::test_path("examples", "image.jpg")
  types <- c("png", "jpeg", "bmp")
  files <- purrr::map_chr(types, ~ convert(jpg, .x))

  # Clean up any existing output
  if (file.exists(output)) unlink(output, force = TRUE)

  # Convert vector of files
  image_to_pdf(files, output)

  expect_true(file.exists(output))
  expect_true(daiR::is_pdf(output))

  # Cleanup
  tmp_files <- c(files, output)
  unlink(tmp_files, force = TRUE)
})

## IS_PDF ----------------------------------------------------------------------

test_invalid_validation_inputs("is_pdf()", is_pdf)

test_that("is_pdf() correctly identifies non-PDF files", {
  image <- testthat::test_path("examples", "image.jpg")
  fake <- tempfile(fileext = ".pdf")
  fs::file_create(fake)
  nonexist <- "nonexistent_file.pdf"

  expect_false(is_pdf(image))
  expect_false(is_pdf(fake))
  expect_false(is_pdf(nonexist))
  expect_false(is_pdf(madeup_json_file))

  unlink(fake, force = TRUE)
})

test_that("is_pdf() correctly identifies valid PDF files", {
  pdf <- testthat::test_path("examples", "sample.pdf")
  expect_true(is_pdf(pdf))
})

test_that("is_pdf() handles edge cases", {
  # Empty string
  expect_false(is_pdf(""))

  # File with .pdf extension but wrong content
  fake_pdf <- tempfile(fileext = ".pdf")
  writeLines("This is not a PDF", fake_pdf)
  expect_false(is_pdf(fake_pdf))
  unlink(fake_pdf, force = TRUE)
})

## IS_JSON ----------------------------------------------------------------------

test_invalid_validation_inputs("is_json()", is_json)

test_that("is_json() correctly identifies non-JSON files", {
  image <- testthat::test_path("examples", "image.jpg")
  pdf <- testthat::test_path("examples", "sample.pdf")

  # Create various non-JSON files
  txt <- file.path(tempdir(), "foo.txt")
  write("Lorem ipsum", txt)

  csv <- file.path(tempdir(), "bar.csv")
  write("Lorem,ipsum", csv)

  empty <- tempfile(fileext = ".json")
  fs::file_create(empty)

  nonexist <- "nonexistent_file.json"

  expect_false(is_json(image))
  expect_false(is_json(pdf))

  # Text-based files
  expect_false(is_json(txt))
  expect_false(is_json(csv))
  expect_false(is_json(empty))
  expect_false(is_json(nonexist))

  unlink(c(txt, csv, empty), force = TRUE)
})

test_that("is_json() correctly identifies valid JSON files", {
  json1 <- testthat::test_path("examples", "output.json")
  json2 <- testthat::test_path("examples", "output_blank.json")
  json3 <- testthat::test_path("examples", "sample_v1.json")
  json4 <- testthat::test_path("examples", "sample3pg.json")

  expect_true(is_json(json1))
  expect_true(is_json(json2))
  expect_true(is_json(json3))
  expect_true(is_json(json4))
  expect_true(is_json(madeup_json_file))
})

test_that("is_json() handles edge cases", {
  # Empty string
  expect_false(is_json(""))

  # File with .json extension but invalid JSON content
  fake_json <- tempfile(fileext = ".json")
  writeLines("This is not JSON {invalid}", fake_json)
  expect_false(is_json(fake_json))
  unlink(fake_json, force = TRUE)

  # File with valid JSON but wrong extension
  valid_json_wrong_ext <- tempfile(fileext = ".txt")
  write(jsonlite::toJSON(list(a = 1, b = 2)), valid_json_wrong_ext)
  expect_true(is_json(valid_json_wrong_ext))
  unlink(valid_json_wrong_ext, force = TRUE)
})

## IS_COLOUR -------------------------------------------------------------------

test_invalid_validation_inputs("is_colour()", is_colour)

test_that("is_colour() correctly identifies valid color names", {
  # Standard R color names
  expect_true(is_colour("red"))
  expect_true(is_colour("blue"))
  expect_true(is_colour("green"))
  expect_true(is_colour("white"))
  expect_true(is_colour("black"))
  expect_true(is_colour("yellow"))
  expect_true(is_colour("purple"))
  expect_true(is_colour("orange"))
  expect_true(is_colour("pink"))
  expect_true(is_colour("brown"))

  # More obscure but valid R colors
  expect_true(is_colour("cornflowerblue"))
  expect_true(is_colour("darkseagreen"))
  expect_true(is_colour("lightgoldenrodyellow"))
})

test_that("is_colour() correctly identifies valid hex colors", {
  # 6-digit hex codes
  expect_true(is_colour("#FF0000"))
  expect_true(is_colour("#00FF00"))
  expect_true(is_colour("#0000FF"))
  expect_true(is_colour("#FFFFFF"))
  expect_true(is_colour("#000000"))
  expect_true(is_colour("#123456"))
  expect_true(is_colour("#ABCDEF"))

  # Lowercase hex codes
  expect_true(is_colour("#ff0000"))
  expect_true(is_colour("#abcdef"))

  # 3-digit hex codes
  expect_true(is_colour("#FFF"))
  expect_true(is_colour("#000"))
  expect_true(is_colour("#F00"))
  expect_true(is_colour("#0F0"))
})

test_that("is_colour() correctly identifies invalid colors", {
  # Invalid color names
  expect_false(is_colour("notacolor"))
  expect_false(is_colour("redd"))
  expect_false(is_colour("colour"))
  expect_false(is_colour(string_random))

  # Invalid hex codes
  expect_false(is_colour("#12345")) # 5 digits
  expect_false(is_colour("#1234567")) # 7 digits
  expect_false(is_colour("#GGGGGG")) # Invalid hex characters
  expect_false(is_colour("FF0000")) # Missing #
  expect_false(is_colour("#")) # Just #
  expect_false(is_colour("##FF0000")) # Double #

  # Empty and special cases
  expect_false(is_colour(""))
  expect_false(is_colour(" "))
})

test_that("is_colour() handles vectors appropriately", {
  # Single valid color
  expect_true(is_colour("red"))

  # Note: col2rgb can handle vectors, so this might return TRUE
  # depending on implementation - testing actual behavior
  multi_colors <- c("red", "blue")
  result <- is_colour(multi_colors)
  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("is_colour() recognizes numeric palette indices", {
  expect_true(is_colour("1"))
  expect_true(is_colour("123"))
  expect_true(is_colour("8"))

  # But very large numbers might not work
  # (palette has limited colors, but col2rgb handles overflow)
  expect_true(is_colour("999"))
})

## PDF_TO_BINBASE --------------------------------------------------------------

test_that("pdf_to_binbase() rejects non-PDF files", {
  skip_on_cran()
  skip_on_ci()

  image <- testthat::test_path("examples", "image.jpg")
  expect_error(pdf_to_binbase(image), "Input file not a pdf.")

  # Test with JSON file
  expect_error(pdf_to_binbase(madeup_json_file), "Input file not a pdf.")
})

test_that("pdf_to_binbase() produces a valid base64 string", {
  skip_on_cran()
  skip_on_ci()

  image <- testthat::test_path("examples", "image.jpg")
  output <- file.path(tempdir(), "output.pdf")

  image_to_pdf(image, output)
  base <- pdf_to_binbase(output)

  expect_type(base, "character")
  expect_length(base, 1)
  expect_match(base, "^[a-zA-Z0-9+/]+={0,2}$")
  expect_true(nchar(base) > 0)

  unlink(output, force = TRUE)
})

test_that("pdf_to_binbase() handles actual PDF files", {
  skip_on_cran()
  skip_on_ci()

  pdf <- testthat::test_path("examples", "sample.pdf")
  base <- pdf_to_binbase(pdf)

  expect_type(base, "character")
  expect_length(base, 1)
  expect_match(base, "^[a-zA-Z0-9+/]+={0,2}$")
})

test_that("pdf_to_binbase() handles edge cases", {
  skip_on_cran()
  skip_on_ci()

  # Nonexistent file
  expect_error(pdf_to_binbase("nonexistent.pdf"))

  # Empty file with .pdf extension
  fake_pdf <- tempfile(fileext = ".pdf")
  fs::file_create(fake_pdf)
  expect_error(pdf_to_binbase(fake_pdf), "Input file not a pdf.")
  unlink(fake_pdf, force = TRUE)
})

## IMG_TO_BINBASE --------------------------------------------------------------

test_that("img_to_binbase() rejects PDF files", {
  skip_on_cran()
  skip_on_ci()

  image <- testthat::test_path("examples", "image.jpg")
  output <- file.path(tempdir(), "output.pdf")

  image_to_pdf(image, output)
  expect_error(img_to_binbase(output), "Input file is .pdf.")

  unlink(output, force = TRUE)
})

test_that("img_to_binbase() rejects actual PDF files", {
  skip_on_cran()
  skip_on_ci()

  pdf <- testthat::test_path("examples", "sample.pdf")
  expect_error(img_to_binbase(pdf), "Input file is .pdf.")
})

test_that("img_to_binbase() produces a valid base64 string from images", {
  skip_on_cran()
  skip_on_ci()

  image <- testthat::test_path("examples", "image.jpg")
  base <- img_to_binbase(image)

  expect_type(base, "character")
  expect_length(base, 1)
  expect_match(base, "^[a-zA-Z0-9+/]+={0,2}$")
  expect_true(nchar(base) > 0)
})

test_that("img_to_binbase() handles different image formats", {
  skip_on_cran()
  skip_on_ci()

  # Helper to convert images
  convert <- function(path_in, type_out) {
    img <- magick::image_read(path_in)
    no_ext <- stringr::str_extract(basename(path_in), ".*(?=\\.\\w{3,4}$)")
    filepath <- file.path(tempdir(), glue::glue("{no_ext}.{type_out}"))
    magick::image_write(img, filepath, format = type_out)
    return(filepath)
  }

  # Create images in different formats
  jpg <- testthat::test_path("examples", "image.jpg")
  types <- c("png", "bmp", "tiff", "gif")
  files <- purrr::map_chr(types, ~ convert(jpg, .x))

  # Test each format
  for (file in files) {
    base <- img_to_binbase(file)
    expect_type(base, "character")
    expect_match(base, "^[a-zA-Z0-9+/]+={0,2}$")
  }

  # Cleanup
  unlink(files, force = TRUE)
})

test_that("img_to_binbase() handles edge cases", {
  skip_on_cran()
  skip_on_ci()

  # Nonexistent file
  expect_error(img_to_binbase("nonexistent.jpg"))
})

## INTEGRATION TESTS -----------------------------------------------------------

test_that("conversion functions work together properly", {
  skip_on_cran()
  skip_on_ci()

  # Create a PDF from image
  image <- testthat::test_path("examples", "image.jpg")
  pdf_output <- file.path(tempdir(), "integration_test.pdf")

  image_to_pdf(image, pdf_output)

  # Verify it's a valid PDF
  expect_true(is_pdf(pdf_output))
  expect_false(is_json(pdf_output))

  # Convert to base64
  base64_result <- pdf_to_binbase(pdf_output)
  expect_type(base64_result, "character")
  expect_true(nchar(base64_result) > 0)

  # Original image should also convert to base64
  img_base64 <- img_to_binbase(image)
  expect_type(img_base64, "character")
  expect_true(nchar(img_base64) > 0)

  # Cleanup
  unlink(pdf_output, force = TRUE)
})

test_that("validation functions are consistent", {
  pdf <- testthat::test_path("examples", "sample.pdf")
  image <- testthat::test_path("examples", "image.jpg")
  json <- testthat::test_path("examples", "output.json")

  # PDF file
  expect_true(is_pdf(pdf))
  expect_false(is_json(pdf))

  # Image file
  expect_false(is_pdf(image))
  expect_false(is_json(image))

  # JSON file
  expect_false(is_pdf(json))
  expect_true(is_json(json))
})

## CLEANUP ---------------------------------------------------------------------

unlink(madeup_json_file, force = TRUE)

# Clean up any remaining test files in tempdir
test_files <- list.files(tempdir(),
  pattern = "^(output|dai_temp|integration_test)",
  full.names = TRUE
)
unlink(test_files, force = TRUE)

