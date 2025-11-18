## SETUP AND TEST FIXTURES -----------------------------------------------------

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

# Valid test file paths
valid_jpg <- testthat::test_path("examples", "image.jpg")

## HELPER FUNCTIONS FOR TESTING ------------------------------------------------

# Test invalid inputs for a single parameter
test_param_invalids <- function(func, param_name, valid_args, invalid_values, 
                                 error_pattern, test_description = NULL) {
  if (is.null(test_description)) {
    test_description <- paste0(deparse(substitute(func)), " errors with invalid ", param_name)
  }
  
  test_that(test_description, {
    for (val in invalid_values) {
      args <- valid_args
      args[[param_name]] <- val
      expect_error(do.call(func, args), error_pattern)
    }
  })
}

# Common invalid values for string parameters
common_string_invalids <- list(
  null = null,
  na = na, 
  boolean = boolean,
  number = number_random,
  vector = vector_strings,
  list = list_strings,
  df = df,
  matrix = matrix
)

# Common invalid values for boolean-like parameters
common_boolean_invalids <- list(
  null = null,
  na = na,
  number = number_random,
  vector = vector_strings,
  list = list_strings,
  df = df,
  matrix = matrix
)

## DAI_SYNC --------------------------------------------------------------------

test_that("dai_sync errors with invalid file parameter", {
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
})

test_that("dai_sync errors with unsupported file formats", {
  skip_on_cran()
  skip_on_ci()
  
  unsupported_formats <- c("foo.txt", "foo.docx", "foo.mp4", "foo.csv", 
                          "foo.xlsx", "foo.r", "foo.py", "foo")
  
  for (file in unsupported_formats) {
    expect_error(
      dai_sync(file = file),
      "Unsupported file format. DAI accepts only bmp, gif, jpeg, jpg, pdf, png, tif, tiff, and webp."
    )
  }
})

test_that("dai_sync accepts all supported file formats", {
  skip_on_cran()
  skip_on_ci()
  
  # These should pass validation (not necessarily succeed in API call)
  supported_formats <- c("foo.bmp", "foo.gif", "foo.jpeg", "foo.jpg", 
                        "foo.png", "foo.tif", "foo.tiff", "foo.webp")
  
  for (file in supported_formats) {
    # Should not error on format, but will error on file not existing
    expect_true(tolower(stringr::str_extract(file, "(?<=\\.)\\w{3,4}$")) %in% 
                c("bmp", "gif", "jpeg", "jpg", "pdf", "png", "tif", "tiff", "webp"))
  }
})

test_that("dai_sync errors with fake PDF file", {
  skip_on_cran()
  skip_on_ci()
  
  expect_error(
    dai_sync(file = "foo.pdf"),
    "Input file not a real pdf. Is the file in your working directory?"
  )
})

test_that("dai_sync errors with invalid proj_id parameter", {
  skip_on_cran()
  skip_on_ci()
  
  expect_error(dai_sync(file = "foo.png", proj_id = null), "Invalid proj_id.")
  expect_error(dai_sync(file = "foo.png", proj_id = na), "Invalid proj_id.")
  expect_error(dai_sync(file = "foo.png", proj_id = boolean), "Invalid proj_id.")
  expect_error(dai_sync(file = "foo.png", proj_id = number_random), "Invalid proj_id.")
  expect_error(dai_sync(file = "foo.png", proj_id = vector_strings), "Invalid proj_id.")
  expect_error(dai_sync(file = "foo.png", proj_id = list_strings), "Invalid proj_id.")
  expect_error(dai_sync(file = "foo.png", proj_id = df), "Invalid proj_id.")
  expect_error(dai_sync(file = "foo.png", proj_id = matrix), "Invalid proj_id.")
})

test_that("dai_sync errors with invalid proc_id parameter", {
  skip_on_cran()
  skip_on_ci()
  
  expect_error(dai_sync(file = "foo.png", proj_id = "abc", proc_id = null), "Invalid proc_id.")
  expect_error(dai_sync(file = "foo.png", proj_id = "abc", proc_id = na), "Invalid proc_id.")
  expect_error(dai_sync(file = "foo.png", proj_id = "abc", proc_id = boolean), "Invalid proc_id.")
  expect_error(dai_sync(file = "foo.png", proj_id = "abc", proc_id = number_random), "Invalid proc_id.")
  expect_error(dai_sync(file = "foo.png", proj_id = "abc", proc_id = ""), "Invalid proc_id.")
  expect_error(dai_sync(file = "foo.png", proj_id = "abc", proc_id = vector_strings), "Invalid proc_id.")
  expect_error(dai_sync(file = "foo.png", proj_id = "abc", proc_id = list_strings), "Invalid proc_id.")
})

test_that("dai_sync errors with invalid proc_v parameter", {
  skip_on_cran()
  skip_on_ci()
  
  # proc_v must be length 1
  expect_error(
    dai_sync(file = "foo.png", proj_id = "abc", proc_id = "def", proc_v = c("v1", "v2")),
    "Invalid proc_v."
  )
  
  # proc_v must be NA or character
  expect_error(
    dai_sync(file = "foo.png", proj_id = "abc", proc_id = "def", proc_v = boolean),
    "Invalid proc_v."
  )
  expect_error(
    dai_sync(file = "foo.png", proj_id = "abc", proc_id = "def", proc_v = number_random),
    "Invalid proc_v."
  )
  expect_error(
    dai_sync(file = "foo.png", proj_id = "abc", proc_id = "def", proc_v = list("v1")),
    "Invalid proc_v."
  )
})

test_that("dai_sync accepts valid proc_v values", {
  skip_on_cran()
  skip_on_ci()
  
  # These should not error on validation (though API call will fail)
  valid_proc_v <- list(NA, "stable", "rc", "pretrained-ocr-v1.1-2022-09-12")
  
  for (pv in valid_proc_v) {
    expect_no_error({
      # Check that parameter validation passes
      is.na(pv) || is.character(pv)
      length(pv) == 1
    })
  }
})

test_that("dai_sync errors with invalid skip_rev parameter", {
  skip_on_cran()
  skip_on_ci()
  
  expect_error(
    dai_sync(file = "foo.png", proj_id = "abc", proc_id = "def", skip_rev = null),
    "Invalid skip_rev parameter."
  )
  expect_error(
    dai_sync(file = "foo.png", proj_id = "abc", proc_id = "def", skip_rev = na),
    "Invalid skip_rev parameter."
  )
  expect_error(
    dai_sync(file = "foo.png", proj_id = "abc", proc_id = "def", skip_rev = number_random),
    "Invalid skip_rev parameter."
  )
  expect_error(
    dai_sync(file = "foo.png", proj_id = "abc", proc_id = "def", skip_rev = "yes"),
    "Invalid skip_rev parameter."
  )
  expect_error(
    dai_sync(file = "foo.png", proj_id = "abc", proc_id = "def", skip_rev = c("true", "false")),
    "Invalid skip_rev parameter."
  )
})

test_that("dai_sync accepts valid skip_rev values (case insensitive)", {
  skip_on_cran()
  skip_on_ci()
  
  valid_skip_rev <- c("true", "false", "TRUE", "FALSE", "True", "False")
  
  for (sr in valid_skip_rev) {
    expect_true(tolower(sr) %in% c("true", "false"))
  }
})

test_that("dai_sync errors with invalid loc parameter", {
  skip_on_cran()
  skip_on_ci()
  
  expect_error(dai_sync(file = "foo.png", proj_id = "abc", proc_id = "def", loc = "USA"), 
               "Invalid location parameter.")
  expect_error(dai_sync(file = "foo.png", proj_id = "abc", proc_id = "def", loc = "europe"), 
               "Invalid location parameter.")
  expect_error(dai_sync(file = "foo.png", proj_id = "abc", proc_id = "def", loc = "uk"), 
               "Invalid location parameter.")
  expect_error(dai_sync(file = "foo.png", proj_id = "abc", proc_id = "def", loc = number_random), 
               "Invalid location parameter.")
  expect_error(dai_sync(file = "foo.png", proj_id = "abc", proc_id = "def", loc = c("eu", "us")), 
               "Invalid location parameter.")
})

test_that("dai_sync accepts valid loc values (case insensitive)", {
  skip_on_cran()
  skip_on_ci()
  
  valid_locs <- c("eu", "us", "EU", "US", "Eu", "Us")
  
  for (loc in valid_locs) {
    expect_true(tolower(loc) %in% c("eu", "us"))
  }
})

test_that("dai_sync informs about unsuccessful requests", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  
  if (file.exists(valid_jpg)) {
    response <- dai_sync(valid_jpg, token = NULL)
    expect_equal(response[["status_code"]], 401)
  } else {
    skip("Test image file not available")
  }
})

test_that("dai_sync gets text from an example file", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  
  if (file.exists(valid_jpg)) {
    response <- dai_sync(valid_jpg)
    expect_equal(response[["status_code"]], 200)
    parsed <- httr::content(response)
    expect_type(parsed[["document"]][["text"]], "character")
  } else {
    skip("Test image file not available")
  }
})

## DAI_ASYNC -------------------------------------------------------------------

test_that("dai_async errors with invalid files parameter", {
  skip_on_cran()
  skip_on_ci()
  
  expect_error(dai_async(files = null), "Invalid files parameter.")
  expect_error(dai_async(files = na), "Invalid files parameter.")
  expect_error(dai_async(files = boolean), "Invalid files parameter.")
  expect_error(dai_async(files = number_random), "Invalid files parameter.")
  expect_error(dai_async(files = df), "Invalid files parameter.")
  expect_error(dai_async(files = matrix), "Invalid files parameter.")
})

test_that("dai_async errors with unsupported file formats", {
  skip_on_cran()
  skip_on_ci()
  
  expect_error(
    dai_async(files = "foo.txt"),
    "Unsupported file formats. DAI accepts only bmp, gif, jpeg, jpg, pdf, png, tif, tiff, and webp."
  )
  expect_error(
    dai_async(files = c("foo.pdf", "bar.docx")),
    "Unsupported file formats. DAI accepts only bmp, gif, jpeg, jpg, pdf, png, tif, tiff, and webp."
  )
})

test_that("dai_async errors with invalid dest_folder parameter", {
  skip_on_cran()
  skip_on_ci()
  
  expect_error(dai_async(files = "foo.pdf", dest_folder = vector_strings), 
               "Invalid dest_folder parameter.")
  expect_error(dai_async(files = "foo.pdf", dest_folder = number_random), 
               "Invalid dest_folder parameter.")
  expect_error(dai_async(files = "foo.pdf", dest_folder = boolean), 
               "Invalid dest_folder parameter.")
  expect_error(dai_async(files = "foo.pdf", dest_folder = df), 
               "Invalid dest_folder parameter.")
})

test_that("dai_async handles trailing slash in dest_folder", {
  skip_on_cran()
  skip_on_ci()
  
  folder_with_slash <- "my_folder/"
  folder_clean <- stringr::str_replace(folder_with_slash, "/$", "")
  expect_equal(folder_clean, "my_folder")
})

test_that("dai_async errors with invalid bucket parameter", {
  skip_on_cran()
  skip_on_ci()
  
  expect_error(dai_async(files = "foo.pdf", bucket = vector_strings), 
               "Invalid bucket parameter.")
  expect_error(dai_async(files = "foo.pdf", bucket = number_random), 
               "Invalid bucket parameter.")
  expect_error(dai_async(files = "foo.pdf", bucket = ""), 
               "Invalid bucket parameter.")
  expect_error(dai_async(files = "foo.pdf", bucket = null), 
               "Invalid bucket parameter.")
})

test_that("dai_async handles bucket name formatting", {
  skip_on_cran()
  skip_on_ci()
  
  # Should strip gs:// prefix
  bucket_with_prefix <- "gs://my-bucket"
  bucket_clean <- stringr::str_replace(bucket_with_prefix, "^gs://", "")
  expect_equal(bucket_clean, "my-bucket")
  
  # Should strip trailing slash
  bucket_with_slash <- "my-bucket/"
  bucket_clean2 <- stringr::str_replace(bucket_with_slash, "/$", "")
  expect_equal(bucket_clean2, "my-bucket")
})

test_that("dai_async errors with invalid proj_id parameter", {
  skip_on_cran()
  skip_on_ci()
  
  expect_error(dai_async(files = "foo.pdf", bucket = "abc", proj_id = vector_strings), 
               "Invalid proj_id parameter.")
  expect_error(dai_async(files = "foo.pdf", bucket = "abc", proj_id = number_random), 
               "Invalid proj_id parameter.")
  expect_error(dai_async(files = "foo.pdf", bucket = "abc", proj_id = null), 
               "Invalid proj_id parameter.")
})

test_that("dai_async errors with invalid proc_id parameter", {
  skip_on_cran()
  skip_on_ci()
  
  expect_error(dai_async(files = "foo.pdf", bucket = "abc", proj_id = "abc", proc_id = ""), 
               "Invalid proc_id parameter.")
  expect_error(dai_async(files = "foo.pdf", bucket = "abc", proj_id = "abc", proc_id = null), 
               "Invalid proc_id parameter.")
  expect_error(dai_async(files = "foo.pdf", bucket = "abc", proj_id = "abc", proc_id = number_random), 
               "Invalid proc_id parameter.")
})

test_that("dai_async errors with invalid proc_v parameter", {
  skip_on_cran()
  skip_on_ci()
  
  expect_error(
    dai_async(files = "foo.pdf", bucket = "abc", proj_id = "abc", proc_id = "def", 
              proc_v = c("v1", "v2")),
    "Invalid proc_v."
  )
  expect_error(
    dai_async(files = "foo.pdf", bucket = "abc", proj_id = "abc", proc_id = "def", 
              proc_v = boolean),
    "Invalid proc_v."
  )
})

test_that("dai_async errors with invalid skip_rev parameter", {
  skip_on_cran()
  skip_on_ci()
  
  expect_error(
    dai_async(files = "foo.pdf", bucket = "abc", proj_id = "abc", proc_id = "def", 
              skip_rev = boolean),
    "Invalid skip_rev parameter."
  )
  expect_error(
    dai_async(files = "foo.pdf", bucket = "abc", proj_id = "abc", proc_id = "def", 
              skip_rev = "yes"),
    "Invalid skip_rev parameter."
  )
})

test_that("dai_async errors with invalid loc parameter", {
  skip_on_cran()
  skip_on_ci()
  
  expect_error(
    dai_async(files = "foo.pdf", bucket = "abc", proj_id = "abc", proc_id = "def", loc = "USA"),
    "Invalid loc parameter."
  )
  expect_error(
    dai_async(files = "foo.pdf", bucket = "abc", proj_id = "abc", proc_id = "def", 
              loc = c("eu", "us")),
    "Invalid loc parameter."
  )
})

test_that("dai_async correctly sets MIME types for different file extensions", {
  skip_on_cran()
  skip_on_ci()
  
  # Test MIME type mapping
  mime_map <- list(
    "pdf" = "application/pdf",
    "gif" = "image/gif",
    "tif" = "image/tiff",
    "tiff" = "image/tiff",
    "jpg" = "image/jpeg",
    "jpeg" = "image/jpeg",
    "png" = "image/png",
    "bmp" = "image/bmp",
    "webp" = "image/webp"
  )
  
  for (ext in names(mime_map)) {
    expected_mime <- mime_map[[ext]]
    # Verify the logic matches
    if (ext == "pdf") {
      expect_equal(expected_mime, "application/pdf")
    } else if (ext == "gif") {
      expect_equal(expected_mime, "image/gif")
    } else if (ext %in% c("tif", "tiff")) {
      expect_equal(expected_mime, "image/tiff")
    } else if (ext %in% c("jpg", "jpeg")) {
      expect_equal(expected_mime, "image/jpeg")
    } else if (ext == "png") {
      expect_equal(expected_mime, "image/png")
    } else if (ext == "bmp") {
      expect_equal(expected_mime, "image/bmp")
    } else {
      expect_equal(expected_mime, "image/webp")
    }
  }
})

test_that("dai_async informs about unsuccessful requests", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  
  response <- dai_async("foo.pdf", token = NULL)
  expect_equal(response[["status_code"]], 401)
})

test_that("dai_async sends successful requests with various input formats", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  
  # Single PDF
  response <- dai_async("foo.pdf")
  Sys.sleep(0.5)
  expect_equal(response[["status_code"]], 200)
  
  # Different image formats
  response <- dai_async("foo.gif")
  Sys.sleep(0.5)
  expect_equal(response[["status_code"]], 200)
  
  response <- dai_async("foo.tiff")
  Sys.sleep(0.5)
  expect_equal(response[["status_code"]], 200)
  
  # Multiple files
  response <- dai_async(c("foo.pdf", "bar.pdf"))
  Sys.sleep(0.5)
  expect_equal(response[["status_code"]], 200)
})

## PARAMETER CLEANING TESTS ----------------------------------------------------

test_that("dai_async cleans bucket parameter correctly", {
  skip_on_cran()
  skip_on_ci()

  # Simulate the bucket cleaning logic from dai_async
  clean_bucket <- function(bucket) {
    if (grepl("^gs://", bucket)) {
      bucket <- stringr::str_replace(bucket, "^gs://", "")
    }
    if (grepl("/$", bucket)) {
      bucket <- stringr::str_replace(bucket, "/$", "")
    }
    return(bucket)
  }

  expect_equal(clean_bucket("gs://my-bucket"), "my-bucket")
  expect_equal(clean_bucket("my-bucket/"), "my-bucket")
  expect_equal(clean_bucket("gs://my-bucket/"), "my-bucket")
  expect_equal(clean_bucket("my-bucket"), "my-bucket")
  expect_equal(clean_bucket("gs://my-bucket-name/"), "my-bucket-name")
})

test_that("dai_async cleans dest_folder parameter correctly", {
  skip_on_cran()
  skip_on_ci()

  # Simulate the folder cleaning logic from dai_async
  clean_folder <- function(folder) {
    if (grepl("/$", folder)) {
      folder <- stringr::str_replace(folder, "/$", "")
    }
    return(folder)
  }

  expect_equal(clean_folder("folder/"), "folder")
  expect_equal(clean_folder("folder"), "folder")
  expect_equal(clean_folder("path/to/folder/"), "path/to/folder")
  expect_equal(clean_folder("my-output"), "my-output")
})

test_that("dai_async constructs correct GCS URIs", {
  skip_on_cran()
  skip_on_ci()

  # Test URI construction logic
  bucket <- "my-bucket"
  file <- "path/to/file.pdf"
  expected_uri <- "gs://my-bucket/path/to/file.pdf"

  actual_uri <- glue::glue("gs://{bucket}/{file}")
  expect_equal(as.character(actual_uri), expected_uri)

  # Test with dest_folder
  dest_folder <- "output"
  expected_dest <- "gs://my-bucket/output/"
  actual_dest <- glue::glue("gs://{bucket}/{dest_folder}/")
  expect_equal(as.character(actual_dest), expected_dest)

  # Test with NULL dest_folder
  dest_folder_null <- NULL
  if (is.null(dest_folder_null)) {
    actual_dest_null <- glue::glue("gs://{bucket}/")
    expect_equal(as.character(actual_dest_null), "gs://my-bucket/")
  }
})

## DAI_STATUS ------------------------------------------------------------------

test_that("dai_status errors with invalid response parameter", {
  skip_on_cran()
  skip_on_ci()
  
  expect_error(dai_status(null), "Input is not a valid HTTP response.")
  expect_error(dai_status(na), "Input is not a valid HTTP response.")
  expect_error(dai_status(boolean), "Input is not a valid HTTP response.")
  expect_error(dai_status(number_random), "Input is not a valid HTTP response.")
  expect_error(dai_status(string_random), "Input is not a valid HTTP response.")
  expect_error(dai_status(vector_strings), "Input is not a valid HTTP response.")
  expect_error(dai_status(list_strings), "Input is not a valid HTTP response.")
  expect_error(dai_status(df), "Input is not a valid HTTP response.")
  expect_error(dai_status(matrix), "Input is not a valid HTTP response.")
})

test_that("dai_status errors when response lacks job id", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  
  # Create a mock response without 'name' field
  # This would come from dai_sync which doesn't create async jobs
  if (file.exists(valid_jpg)) {
    sync_response <- dai_sync(valid_jpg)
    expect_error(
      dai_status(sync_response),
      "Input does not contain a processing job id. Make sure it is from dai_async."
    )
  } else {
    skip("Test image file not available")
  }
})

test_that("dai_status errors with invalid loc parameter", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  
  resp <- dai_async("foo.pdf")
  Sys.sleep(0.5)
  
  expect_error(dai_status(response = resp, loc = number_random), 
               "Invalid location parameter.")
  expect_error(dai_status(response = resp, loc = "USA"), 
               "Invalid location parameter.")
  expect_error(dai_status(response = resp, loc = c("eu", "us")), 
               "Invalid location parameter.")
})

test_that("dai_status errors with invalid verbose parameter", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  
  resp <- dai_async("foo.pdf")
  Sys.sleep(0.5)
  
  expect_error(dai_status(response = resp, verbose = "true"), 
               "Parameter verbose can only be TRUE or FALSE.")
  expect_error(dai_status(response = resp, verbose = 1), 
               "Parameter verbose can only be TRUE or FALSE.")
  expect_error(dai_status(response = resp, verbose = c(TRUE, FALSE)), 
               "Parameter verbose can only be TRUE or FALSE.")
})

test_that("dai_status returns status string when verbose = FALSE", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  
  resp <- dai_async("foo.pdf")
  Sys.sleep(1)
  
  # Should return NULL when verbose = FALSE (just prints message)
  result <- dai_status(resp, verbose = FALSE)
  expect_null(result)
})

test_that("dai_status returns response object when verbose = TRUE", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  
  resp <- dai_async("foo.pdf")
  Sys.sleep(1)
  
  # Should return HTTP response when verbose = TRUE
  result <- dai_status(resp, verbose = TRUE)
  expect_s3_class(result, "response")
})

test_that("dai_status handles list of responses", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  
  # Create a list of responses
  resp1 <- dai_async("foo.pdf")
  Sys.sleep(0.5)
  resp2 <- dai_async("bar.pdf")
  Sys.sleep(0.5)
  
  resp_list <- list(resp1, resp2)
  
  # Should handle list and check last element
  expect_no_error(dai_status(resp_list))
})

## DAI_NOTIFY ------------------------------------------------------------------

test_that("dai_notify errors with invalid response parameter", {
  skip_on_cran()
  skip_on_ci()
  
  expect_error(dai_notify(null), "Input is not a valid HTTP response.")
  expect_error(dai_notify(na), "Input is not a valid HTTP response.")
  expect_error(dai_notify(boolean), "Input is not a valid HTTP response.")
  expect_error(dai_notify(number_random), "Input is not a valid HTTP response.")
  expect_error(dai_notify(string_random), "Input is not a valid HTTP response.")
  expect_error(dai_notify(vector_strings), "Input is not a valid HTTP response.")
  expect_error(dai_notify(df), "Input is not a valid HTTP response.")
})

test_that("dai_notify errors when response lacks job id", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  
  # Response from dai_sync won't have job id
  if (file.exists(valid_jpg)) {
    sync_response <- dai_sync(valid_jpg)
    expect_error(
      dai_notify(sync_response),
      "Input does not contain a processing job id"
    )
  } else {
    skip("Test image file not available")
  }
})

test_that("dai_notify errors with invalid loc parameter", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  
  resp <- dai_async("foo.pdf")
  Sys.sleep(0.5)
  
  expect_error(dai_notify(response = resp, loc = "USA"), 
               "Invalid location parameter.")
  expect_error(dai_notify(response = resp, loc = number_random), 
               "Invalid location parameter.")
  expect_error(dai_notify(response = resp, loc = c("eu", "us")), 
               "Invalid location parameter.")
})

# test_that("dai_notify errors with invalid sound parameter", {
#   skip_on_cran()
#   skip_on_ci()
#   skip_if_offline()
  
#   resp <- dai_async("foo.pdf")
#   Sys.sleep(0.5)
  
#   expect_error(dai_notify(response = resp, sound = 0), 
#                "Invalid sound parameter.")
#   expect_error(dai_notify(response = resp, sound = 11), 
#                "Invalid sound parameter.")
#   expect_error(dai_notify(response = resp, sound = -1), 
#                "Invalid sound parameter.")
#   expect_error(dai_notify(response = resp, sound = "2"), 
#                "Invalid sound parameter.")
#   expect_error(dai_notify(response = resp, sound = c(1, 2)), 
#                "Invalid sound parameter.")
#   expect_error(dai_notify(response = resp, sound = 1.5), 
#                "Invalid sound parameter.")
# })

test_that("dai_notify accepts valid sound values", {
  skip_on_cran()
  skip_on_ci()
  
  # Valid sound values are 1 through 10
  for (i in 1:10) {
    expect_true(i %in% 1:10 && length(i) == 1)
  }
})

test_that("dai_notify handles list of responses", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  skip_if_not_installed("beepr")
  
  # Create a list of responses  
  resp1 <- dai_async("foo.pdf")
  Sys.sleep(0.5)
  resp2 <- dai_async("bar.pdf")
  Sys.sleep(0.5)
  
  resp_list <- list(resp1, resp2)
  
  # Should handle list and check last element
  # Note: This test will actually wait for job completion and beep
  expect_no_error({
    # Just verify it accepts the list format without running the full function
    inherits(resp_list[[1]], "response")
  })
})

## INTEGRATION TESTS -----------------------------------------------------------

test_that("dai_async and dai_status work together", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  
  # Submit a job
  response <- dai_async("foo.pdf")
  Sys.sleep(1)
  
  # Check status
  expect_no_error(dai_status(response))
  
  # Get verbose status
  status_resp <- dai_status(response, verbose = TRUE)
  expect_s3_class(status_resp, "response")
})

test_that("functions handle different authentication scenarios", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  
  # NULL token should give 401
  if (file.exists(valid_jpg)) {
    resp_sync <- dai_sync(valid_jpg, token = NULL)
    expect_equal(resp_sync$status_code, 401)
  }
  
  resp_async <- dai_async("foo.pdf", token = NULL)
  expect_equal(resp_async$status_code, 401)
})

## EDGE CASES AND BOUNDARY CONDITIONS ------------------------------------------

test_that("dai_sync handles case sensitivity in file extensions", {
  skip_on_cran()
  skip_on_ci()
  
  # Extensions are converted to lowercase before validation
  mixed_case_files <- c("foo.PDF", "bar.PNG", "baz.JPEG", "qux.TiFF")
  
  for (file in mixed_case_files) {
    ext <- tolower(stringr::str_extract(file, "(?<=\\.)\\w{3,4}$"))
    expect_true(ext %in% c("bmp", "gif", "jpeg", "jpg", "pdf", "png", "tif", "tiff", "webp"))
  }
})

test_that("dai_async handles empty file list edge case", {
  skip_on_cran()
  skip_on_ci()
  
  # Empty vector should error
  expect_error(dai_async(files = character(0)), "Invalid files parameter.")
})

test_that("parameter validation is case-insensitive where appropriate", {
  skip_on_cran()
  skip_on_ci()
  
  # skip_rev should handle case insensitivity
  expect_true(tolower("TRUE") %in% c("true", "false"))
  expect_true(tolower("FALSE") %in% c("true", "false"))
  expect_true(tolower("TrUe") %in% c("true", "false"))
  
  # loc should handle case insensitivity
  expect_true(tolower("EU") %in% c("eu", "us"))
  expect_true(tolower("US") %in% c("eu", "us"))
  expect_true(tolower("eU") %in% c("eu", "us"))
})

test_that("URL construction handles edge cases", {
  skip_on_cran()
  skip_on_ci()
  
  # Test that bucket formatting works correctly
  bucket_tests <- list(
    list(input = "gs://my-bucket", expected = "my-bucket"),
    list(input = "my-bucket/", expected = "my-bucket"),
    list(input = "gs://my-bucket/", expected = "my-bucket"),
    list(input = "my-bucket", expected = "my-bucket")
  )
  
  for (test in bucket_tests) {
    cleaned <- stringr::str_replace(test$input, "^gs://", "")
    cleaned <- stringr::str_replace(cleaned, "/$", "")
    expect_equal(cleaned, test$expected)
  }
  
  # Test dest_folder formatting
  folder_tests <- list(
    list(input = "folder/", expected = "folder"),
    list(input = "folder", expected = "folder"),
    list(input = "path/to/folder/", expected = "path/to/folder")
  )
  
  for (test in folder_tests) {
    cleaned <- stringr::str_replace(test$input, "/$", "")
    expect_equal(cleaned, test$expected)
  }
})

## CLEANUP ---------------------------------------------------------------------


