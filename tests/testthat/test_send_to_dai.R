
## DAI_SYNC --------------------------------------------------------------------

test_that("dai_sync calls out input errors", {
  expect_error(dai_sync(file = mtcars), "Invalid file input.")
  expect_error(dai_sync(file = as.matrix(mtcars)), "Invalid file input.")
  expect_error(dai_sync(file = TRUE), "Invalid file input.")
  expect_error(dai_sync(file = 123), "Invalid file input.")
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

## DAI_ASYNC --------------------------------------------------------------------

test_that("dai_async calls out input errors", {
  expect_error(dai_async(files = mtcars), "Invalid files parameter.")
  expect_error(dai_async(files = as.matrix(mtcars)), "Invalid files parameter.")
  expect_error(dai_async(files = TRUE), "Invalid files parameter.")
  expect_error(dai_async(files = NULL), "Invalid files parameter.")
  expect_error(dai_async(files = 123), "Invalid files parameter.")
  expect_error(dai_async(files = "foo.png"), "Files contain unsupported file types. Only .pdf, .gif, and .tiff accepted.")
  expect_error(dai_async(files = "foo.pdf", dest_folder = c("folder1", "folder2")),
               "Invalid dest_folder parameter.")
  expect_error(dai_async(files = "foo.pdf", dest_folder = 12345),
               "Invalid dest_folder parameter.")
  expect_error(dai_async(files = "foo.pdf", bucket = c("bucket1", "bucket2")),
               "Invalid bucket parameter.")
  expect_error(dai_async(files = "foo.pdf", bucket = 12345),
               "Invalid bucket parameter.")
  expect_error(dai_async(files = "foo.pdf", bucket = NULL),
               "Invalid bucket parameter.")
  expect_error(dai_async(files = "foo.pdf", proj_id = c("project1", "project2")),
               "Invalid proj_id parameter.")
  expect_error(dai_async(files = "foo.pdf", proj_id = 12345),
               "Invalid proj_id parameter.")
  expect_error(dai_async(files = "foo.pdf", proj_id = NULL),
               "Invalid proj_id parameter.")
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
})

## DAI_STATUS-------------------------------------------------------------------

test_that("dai_status calls out input errors", {
  expect_error(dai_status(NULL), "Input is not a valid HTTP response.")
  expect_error(dai_status(123), "Input is not a valid HTTP response.")
  expect_error(dai_status(mtcars), "Input is not a valid HTTP response.")
  expect_error(dai_status(as.matrix(mtcars)), "Input is not a valid HTTP response.")
  expect_error(dai_status("string"), "Input is not a valid HTTP response.")
  expect_error(dai_status(c("string", "vector")), "Input is not a valid HTTP response.")
  expect_error(dai_status(list("a", "list")), "Input is not a valid HTTP response.?")
  wrong <- dai_user()
  expect_error(dai_status(response = wrong), "Input does not contain a processing job id. Make sure it is from dai_async.")
  resp <- dai_async("foo.pdf")
  expect_error(dai_status(response = resp, loc = 123), "Invalid location parameter.")
  expect_error(dai_status(response = resp, loc = "USA"), "Invalid location parameter.")
  expect_error(dai_status(response = resp, verbose = "yes"), "Parameter verbose can only be TRUE or FALSE.")
} )

## DAI_SYNC_TAB-----------------------------------------------------------------

test_that("dai_sync_tab calls out input errors", {
  expect_error(dai_sync_tab(file = mtcars), "Invalid file input.")
  expect_error(dai_sync_tab(file = as.matrix(mtcars)), "Invalid file input.")
  expect_error(dai_sync_tab(file = TRUE), "Invalid file input.")
  expect_error(dai_sync_tab(file = NULL), "Invalid file input.")
  expect_error(dai_sync_tab(file = 123), "Invalid file input.")
  expect_error(dai_sync_tab(file = c("foo.png", "bar.png"), "Invalid file input."))
  expect_error(dai_sync_tab(file = list("foo.png", "bar.png"), "Invalid file input."))
  expect_error(dai_sync_tab(file = "foo.csv"), "Unsupported file format. See documentation for details.")
  expect_error(dai_sync_tab(file = "bar.doc"), "Unsupported file format. See documentation for details.")
  expect_error(dai_sync_tab(file = "foobar.txt"), "Unsupported file format. See documentation for details.")
  expect_error(dai_sync_tab(file = "barfoo.avi"), "Unsupported file format. See documentation for details.")
  expect_error(dai_sync_tab(file = "fake.pdf"), "Input file not a real pdf. Is the file in your working directory?")
  expect_error(dai_sync_tab(file = "foo.png", proj_id = mtcars), "Invalid proj_id.")
  expect_error(dai_sync_tab(file = "foo.png", proj_id = as.matrix(mtcars)), "Invalid proj_id.")
  expect_error(dai_sync_tab(file = "foo.png", proj_id = TRUE), "Invalid proj_id.")
  expect_error(dai_sync_tab(file = "foo.png", proj_id = NULL), "Invalid proj_id.")
  expect_error(dai_sync_tab(file = "foo.png", proj_id = 123), "Invalid proj_id.")
  expect_error(dai_sync_tab(file = "foo.png", proj_id = c("abc", "def")), "Invalid proj_id.")
  expect_error(dai_sync_tab(file = "foo.png", proj_id = list("abc", "def")), "Invalid proj_id.")
  expect_error(dai_sync_tab(file = "foo.png", proj_id = "abc", loc = 123), "Invalid location parameter.")
  expect_error(dai_sync_tab(file = "foo.png", proj_id = "abc", loc = "USA"), "Invalid location parameter.")
  expect_error(dai_sync_tab(file = "foo.png", proj_id = "abc", loc = c("eu", "us")), "Invalid location parameter.")
})

test_that("dai_sync_tab informs about unsuccessful requests", {
  skip_if_no_token()
  skip_if_offline()
  file <- testthat::test_path("examples", "image.jpg")
  response <- dai_sync_tab(file, token = NULL)
  expect_equal(response[["status_code"]], 403)
})

test_that("dai_sync_tab sends succesful requests with jpgs and pdfs", {
  skip_if_no_token()
  skip_if_offline()
  file1 <- testthat::test_path("examples", "image.jpg")
  file2 <- testthat::test_path("examples", "sample.pdf")
  response <- dai_sync_tab(file1)
  expect_equal(response[["status_code"]], 200)
  response <- dai_sync_tab(file2)
  expect_equal(response[["status_code"]], 200)
})

## DAI_ASYNC_TAB----------------------------------------------------------------

test_that("dai_async_tab calls out input errors", {
  expect_error(dai_async_tab(files = mtcars), "Invalid files parameter.")
  expect_error(dai_async_tab(files = as.matrix(mtcars)), "Invalid files parameter.")
  expect_error(dai_async_tab(files = TRUE), "Invalid files parameter.")
  expect_error(dai_async_tab(files = NULL), "Invalid files parameter.")
  expect_error(dai_async_tab(files = 123), "Invalid files parameter.")
  expect_error(dai_async_tab(files = "foo.png"), "Input file type not supported.")
  expect_error(dai_async_tab(files = "foo.pdf", filetype = mtcars, "Invalid filetype parameter.")) # wrong filetype format
  expect_error(dai_async_tab(files = "foo.pdf", filetype = as.matrix(mtcars), "Invalid filetype parameter."))
  expect_error(dai_async_tab(files = "foo.pdf", filetype = TRUE, "Invalid filetype parameter."))
  expect_error(dai_async_tab(files = "foo.pdf", filetype = NULL, "Invalid filetype parameter."))
  expect_error(dai_async_tab(files = "foo.pdf", filetype = 123, "Invalid filetype parameter."))
  expect_error(dai_async_tab(files = "foo.pdf", filetype = c("gif", "pdf"), "Invalid filetype parameter."))
  expect_error(dai_async_tab(files = "foo.pdf", filetype = list("gif", "pdf"), "Invalid filetype parameter."))
  expect_error(dai_async_tab(files = "foo.pdf", filetype = "gif", "Mismatch between filetype parameter and actual format of files.")) # mismatch
  expect_error(dai_async_tab(files = "foo.gif", filetype = "tiff", "Mismatch between filetype parameter and actual format of files.")) # mismatch
  expect_error(dai_async_tab(files = "foo.pdf", dest_folder = c("folder1", "folder2")), "Invalid dest_folder parameter.")
  expect_error(dai_async_tab(files = "foo.pdf", dest_folder = 12345), "Invalid dest_folder parameter.")
  expect_error(dai_async_tab(files = "foo.pdf", bucket = c("bucket1", "bucket2")), "Invalid bucket parameter.")
  expect_error(dai_async_tab(files = "foo.pdf", bucket = 12345), "Invalid bucket parameter.")
  expect_error(dai_async_tab(files = "foo.pdf", bucket = NULL), "Invalid bucket parameter.")
  expect_error(dai_async_tab(files = "foo.pdf", proj_id = c("project1", "project2")), "Invalid proj_id parameter.")
  expect_error(dai_async_tab(files = "foo.pdf", proj_id = 12345), "Invalid proj_id parameter.")
  expect_error(dai_async_tab(files = "foo.pdf", proj_id = NULL), "Invalid proj_id parameter.")
  expect_error(dai_async_tab(files = "foo.pdf", loc = 123), "Invalid loc parameter.")
  expect_error(dai_async_tab(files = "foo.pdf", loc = c("eu", "us")), "Invalid loc parameter.")
  expect_error(dai_async_tab(files = "foo.pdf", loc = "usa"), "Invalid loc parameter.")
  expect_error(dai_async_tab(files = "foo.pdf", pps = "five"), "Invalid pps parameter.")
  expect_error(dai_async_tab(files = "foo.pdf", pps = 200), "Invalid pps parameter.")
  expect_error(dai_async_tab(files = "foo.pdf", pps = 0), "Invalid pps parameter.")
  expect_error(dai_async_tab(files = "foo.pdf", pps = 10-50), "Invalid pps parameter.")
} )

test_that("dai_async_tab sends succesful requests with input in different formats", {
  skip_if_no_token()
  skip_if_offline()
  response <- dai_async_tab("foo.pdf")
  expect_equal(response[[1]][["status_code"]], 200)
  response <- dai_async_tab("foo.gif", filetype = "gif")
  expect_equal(response[[1]][["status_code"]], 200)
  response <- dai_async_tab("foo.tiff", filetype = "tiff")
  expect_equal(response[[1]][["status_code"]], 200)
  response <- dai_async_tab("foo.pdf", dest_folder = "folder/")
  expect_equal(response[[1]][["status_code"]], 200)
  response <- dai_async_tab("foo.pdf", bucket = "gs://bucket")
  expect_equal(response[[1]][["status_code"]], 200)
  response <- dai_async_tab("foo.pdf", bucket = "bucket/")
  expect_equal(response[[1]][["status_code"]], 200)
  response <- dai_async_tab(c("foo.pdf", "bar.pdf"))
  expect_equal(response[[1]][["status_code"]], 200)
})
