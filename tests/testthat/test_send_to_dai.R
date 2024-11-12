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

## DAI_SYNC --------------------------------------------------------------------

test_that("dai_sync calls out input errors", {
  skip_on_cran()
  skip_on_ci()
  expect_error(dai_sync(file = null), "Invalid file input.")
  expect_error(dai_sync(file = na), "Invalid file input.")
  expect_error(dai_sync(file = boolean), "Invalid file input.")
  expect_error(dai_sync(file = number_random), "Invalid file input.")
  expect_error(dai_sync(file = vector_strings), "Invalid file input.")
  expect_error(dai_sync(file = list_strings), "Invalid file input.")
  expect_error(dai_sync(file = df), "Invalid file input.")
  expect_error(dai_sync(file = matrix), "Invalid file input.")
  expect_error(dai_sync(file = "foo.txt"), "Unsupported file format. DAI accepts only bmp, gif, jpeg, jpg, pdf, png, tif, tiff, and webp.")
  expect_error(dai_sync(file = "foo.docx"), "Unsupported file format. DAI accepts only bmp, gif, jpeg, jpg, pdf, png, tif, tiff, and webp.")
  expect_error(dai_sync(file = "foo.mp4"), "Unsupported file format. DAI accepts only bmp, gif, jpeg, jpg, pdf, png, tif, tiff, and webp.")
  expect_error(dai_sync(file = "foo"), "Unsupported file format. DAI accepts only bmp, gif, jpeg, jpg, pdf, png, tif, tiff, and webp.")
  expect_error(dai_sync(file = "foo.pdf"), "Input file not a real pdf. Is the file in your working directory?")
  expect_error(dai_sync(file = "foo.png", proj_id = number_random), "Invalid proj_id.")
  expect_error(dai_sync(file = "foo.png", proj_id = vector_strings), "Invalid proj_id.")
  expect_error(dai_sync(file = "foo.png", proj_id = "abc", proc_id = number_random), "Invalid proc_id.")
  expect_error(dai_sync(file = "foo.png", proj_id = "abc", proc_id = "abc", loc = "USA"), "Invalid location parameter.")
})

test_that("dai_sync informs about unsuccessful requests", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  file <- testthat::test_path("examples", "image.jpg")
  response <- dai_sync(file, token = NULL)
  expect_equal(response[["status_code"]], 401)
})

test_that("dai_sync gets text from an example file", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  image <- testthat::test_path("examples", "image.jpg")
  response <- dai_sync(image)
  expect_equal(response[["status_code"]], 200)
  parsed <- httr::content(response)
  expect_type(parsed[["document"]][["text"]], "character")
})

## DAI_ASYNC --------------------------------------------------------------------

test_that("dai_async calls out input errors", {
  skip_on_cran()
  skip_on_ci()
  expect_error(dai_async(files = null), "Invalid files parameter.")
  expect_error(dai_async(files = na), "Invalid files parameter.")
  expect_error(dai_async(files = boolean), "Invalid files parameter.")
  expect_error(dai_async(files = number_random), "Invalid files parameter.")
  expect_error(dai_async(files = df), "Invalid files parameter.")
  expect_error(dai_async(files = matrix), "Invalid files parameter.")
  expect_error(dai_async(files = "foo.pdf", dest_folder = vector_strings), "Invalid dest_folder parameter.")
  expect_error(dai_async(files = "foo.pdf", dest_folder = number_random), "Invalid dest_folder parameter.")
  expect_error(dai_async(files = "foo.pdf", bucket = vector_strings), "Invalid bucket parameter.")
  expect_error(dai_async(files = "foo.pdf", bucket = number_random), "Invalid bucket parameter.")
  expect_error(dai_async(files = "foo.pdf", bucket = "abc", proj_id = vector_strings), "Invalid proj_id parameter.")
  expect_error(dai_async(files = "foo.pdf", bucket = "abc", proj_id = number_random), "Invalid proj_id parameter.")
  expect_error(dai_async(files = "foo.pdf", bucket = "abc", proj_id = "abc", proc_id = "def", skip_rev = boolean), "Invalid skip_rev parameter.")
  expect_error(dai_async(files = "foo.pdf", bucket = "abc", proj_id = "abc", proc_id = "def", loc = "USA"), "Invalid loc parameter.")
})

test_that("dai_async informs about unsuccessful requests", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  response <- dai_async("foo.pdf", token = NULL)
  expect_equal(response[["status_code"]], 401)
})

test_that("dai_async sends succesful requests with input in different formats", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  response <- dai_async("foo.pdf")
  Sys.sleep(0.5)
  expect_equal(response[["status_code"]], 200)
  response <- dai_async("foo.gif")
  Sys.sleep(0.5)
  expect_equal(response[["status_code"]], 200)
  response <- dai_async("foo.tiff")
  Sys.sleep(0.5)
  expect_equal(response[["status_code"]], 200)
  # response <- dai_async("foo.pdf", dest_folder = "folder/")
  # Sys.sleep(0.5)
  # expect_equal(response[["status_code"]], 200)
  # response <- dai_async("foo.pdf", bucket = "gs://bucket")
  # Sys.sleep(0.5)
  # expect_equal(response[["status_code"]], 200)
  # response <- dai_async("foo.pdf", bucket = "bucket/")
  # Sys.sleep(0.5)
  # expect_equal(response[["status_code"]], 200)
  response <- dai_async(c("foo.pdf", "bar.pdf"))
  Sys.sleep(0.5)
  expect_equal(response[["status_code"]], 200)
})

## DAI_STATUS-------------------------------------------------------------------

test_that("dai_status calls out input errors", {
  skip_on_cran()
  skip_on_ci()
  expect_error(dai_status(null), "Input is not a valid HTTP response.")
  expect_error(dai_status(na), "Input is not a valid HTTP response.")
  expect_error(dai_status(boolean), "Input is not a valid HTTP response.")
  expect_error(dai_status(number_random), "Input is not a valid HTTP response.")
  expect_error(dai_status(string_random), "Input is not a valid HTTP response.")
  expect_error(dai_status(vector_strings), "Input is not a valid HTTP response.")
  expect_error(dai_status(list_strings), "Input is not a valid HTTP response.?")
  expect_error(dai_status(df), "Input is not a valid HTTP response.")
  expect_error(dai_status(matrix), "Input is not a valid HTTP response.")
})

test_that("dai_status calls out input errors", {
  skip_on_cran()
  skip_on_ci()
  resp <- dai_async("foo.pdf")
  expect_error(dai_status(response = resp, loc = number_random), "Invalid location parameter.")
  expect_error(dai_status(response = resp, loc = "USA"), "Invalid location parameter.")
})

## DAI_NOTIFY ----------------------------------------
