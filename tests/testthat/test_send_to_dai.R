
## DAI_SYNC --------------------------------------------------------------------

test_that("dai_sync calls out input errors", {
  expect_error(dai_sync(file = mtcars), "Invalid file input.")
  expect_error(dai_sync(file = as.matrix(mtcars)), "Invalid file input.")
  expect_error(dai_sync(file = TRUE), "Invalid file input.")
  expect_error(dai_sync(file = 1), "Invalid file input.")
  expect_error(dai_sync(file = as.integer(1)), "Invalid file input.")
  expect_error(dai_sync(file = c("foo.pdf", "bar.pdf")), "Invalid file input.")
  expect_error(dai_sync(file = "foo.txt"),
               "Unsupported file format. See documentation for details.")
  expect_error(dai_sync(file = "foo.docx"),
               "Unsupported file format. See documentation for details.")
  expect_error(dai_sync(file = "foo.mp4"),
               "Unsupported file format. See documentation for details.")
  expect_error(dai_sync(file = "foo"),
               "Unsupported file format. See documentation for details.")
  expect_error(dai_sync(file = "foo.pdf"),
               "Input file not a real pdf. Is the file in your working directory?")
  expect_error(dai_sync(file = "foo.png", proj_id = 012345),
               "Invalid proj_id.")
  expect_error(dai_sync(file = "foo.png", proj_id = c("Project1", "Project2")),
               "Invalid proj_id.")
} )

test_that("dai_sync calls out input errors (CONT)", {
  skip_if_no_token() # bc from here down it calls get_project_id()
  skip_if_offline()
  samplepdf <- testthat::test_path("examples", "sample.pdf")
  expect_error(dai_sync(file = samplepdf, loc = "USA"),
               "Invalid location parameter.")
  expect_error(dai_sync(file = samplepdf, proc_id = ""),
               "Invalid proc_id parameter.")
} )

test_that("dai_sync informs about unsuccessful requests", {
  skip_if_no_token()
  skip_if_offline()
  file <- testthat::test_path("examples", "image.jpg")
  response <- dai_sync(file, token = NULL)
  expect_equal(response[["status_code"]], 401)
  test_auth()
})

test_that("dai_sync gets text from an example file", {
  skip_if_no_token()
  skip_if_offline()

  image <- testthat::test_path("examples", "image.jpg")
  response <- dai_sync(image)
  expect_equal(response[["status_code"]], 200)
  parsed <- httr::content(response)
  expect_type(parsed[["document"]][["text"]], "character")

  pdf <- image_to_pdf(image, "foo.pdf")
  response <- dai_sync(pdf)
  expect_equal(response[["status_code"]], 200)
  parsed <- httr::content(response)
  expect_type(parsed[["document"]][["text"]], "character")
  unlink("foo.pdf", force = TRUE)
})

test_that("dai_sync gets text from a random, real pdf", {
  skip_on_ci()
  skip_if_offline()
  filepath <- get_random_pdf()
  response <- dai_sync(filepath)
  expect_equal(response[["status_code"]], 200)
  parsed <- httr::content(response)
  expect_type(parsed[["document"]][["text"]], "character")
  unlink(filepath, force = TRUE)
})

## DAI_ASYNC --------------------------------------------------------------------

test_that("dai_async calls out input errors", {
  expect_error(dai_async(files = mtcars), "Invalid files parameter.")
  expect_error(dai_async(files = as.matrix(mtcars)), "Invalid files parameter.")
  expect_error(dai_async(files = TRUE), "Invalid files parameter.")
  expect_error(dai_async(files = NULL), "Invalid files parameter.")
  expect_error(dai_async(files = 1), "Invalid files parameter.")
  expect_error(dai_async(files = as.integer(1)), "Invalid files parameter.")
  expect_error(dai_async(files = "foo.png"), "Files contain unsupported file types. Only .pdf, .gif, and .tiff accepted.")
  expect_error(dai_async(files = "foo.pdf", dest_folder = c("folder1", "folder2")),
               "Invalid dest_folder parameter.")
  expect_error(dai_async(files = "foo.pdf", dest_folder = 12345),
               "Invalid dest_folder parameter.")
  #expect_error(dai_async(files = "foo.pdf", bucket = c("bucket1", "bucket2")),
  #             "Invalid bucket parameter.")
  expect_error(dai_async(files = "foo.pdf", bucket = 12345),
               "Invalid bucket parameter.")
  expect_error(dai_async(files = "foo.pdf", bucket = NULL),
               "Invalid bucket parameter.")
  #expect_error(dai_async(files = "foo.pdf", proj_id = c("project1", "project2")),
  #             "Invalid proj_id parameter.")
  #expect_error(dai_async(files = "foo.pdf", proj_id = 12345),
  #             "Invalid proj_id parameter.")
  #expect_error(dai_async(files = "foo.pdf", proj_id = NULL),
  #             "Invalid proj_id parameter.")
} )

test_that("dai_async calls out input errors (CONT)", {
  skip_if_no_token() # bc from here down it calls get_project_id()
  skip_if_offline()
  expect_error(dai_async(files = "foo.pdf", loc = "USA"),
               "Invalid loc parameter.")
  expect_error(dai_async(files = "foo.pdf", loc = NULL),
               "Invalid loc parameter.")
  expect_error(dai_async(files = "foo.pdf", bucket = ""),
               "Invalid bucket parameter.")
  expect_error(dai_async(files = "foo.pdf", proc_id = ""),
               "Invalid proc_id parameter.")
} )

test_that("dai_async informs about unsuccessful requests", {
  skip_if_no_token()
  skip_if_offline()
  response <- dai_async("foo.pdf", token = NULL)
  expect_equal(response[["status_code"]], 401)
  test_auth()
})

test_that("dai_async sends succesful requests with input in different formats", {
  skip_if_no_token()
  skip_if_offline()
  response <- dai_async("foo.pdf")
  expect_equal(response[["status_code"]], 200)
  response <- dai_async("foo.gif")
  expect_equal(response[["status_code"]], 200)
  response <- dai_async("foo.tiff")
  expect_equal(response[["status_code"]], 200)
  response <- dai_async("foo.pdf", dest_folder = "folder/")
  expect_equal(response[["status_code"]], 200)
  response <- dai_async("foo.pdf", bucket = "gs://bucket")
  expect_equal(response[["status_code"]], 200)
  response <- dai_async("foo.pdf", bucket = "bucket/")
  expect_equal(response[["status_code"]], 200)
  response <- dai_async(c("foo.pdf", "bar.pdf"))
  expect_equal(response[["status_code"]], 200)
  expect_equal(response[["status_code"]], 200)
})

test_that("dai_async produces actual jsons from random, real pdfs", {
  skip_on_ci()
  skip_if_offline()

  # get random file
  filepath <- get_random_pdf()

  # upload to bucket
  filename <- basename(filepath)
  googleCloudStorageR::gcs_upload(file = filepath,
                                  bucket = Sys.getenv("GCS_DEFAULT_BUCKET"),
                                  name = filename)

  # process
  response <- dai_async(filename)
  expect_equal(response[["status_code"]], 200)

  # wait
  message("Waiting for DAI to generate json...")
  processed <- FALSE
  count <- 0
  while (count < 100 && isFALSE(processed)){
    Sys.sleep(2)
    content <- googleCloudStorageR::gcs_list_objects(bucket = Sys.getenv("GCS_DEFAULT_BUCKET"))
    filename_no_ext <- stringr::str_sub(filename, end=-5)
    search_term <- glue::glue("{filename_no_ext}-0.json$")
    if (any(grepl(search_term, content$name))) {
      processed <- TRUE
    }
    count <- count + 1
  }

  # get json
  message("Retrieving json...")
  full_json_name <- grep(search_term, content$name, value = TRUE)
  json_path <- file.path(tempdir(), basename(full_json_name))
  googleCloudStorageR::gcs_get_object(full_json_name,
                                      bucket = Sys.getenv("GCS_DEFAULT_BUCKET"),
                                      saveToDisk = json_path)
  # inspect
  expect_true(is_json(json_path))
  parsed <- jsonlite::fromJSON(json_path)
  expect_type(parsed[["text"]], "character")

  # clean
  unlink(json_path, force = TRUE)
})
