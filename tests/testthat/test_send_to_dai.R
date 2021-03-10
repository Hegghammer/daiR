
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

  expect_error(dai_sync(file = "foo.pdf", proj_id = 012345),
               "Invalid proj_id.")
  expect_error(dai_sync(file = "foo.pdf", proj_id = c("Project1", "Project2")),
               "Invalid proj_id.")

#  expect_error(dai_sync(file = "foo.pdf", loc = "USA"),
#               "Invalid location parameter.")
} )

test_that("dai_sync works", {
  skip_if_no_token()
  skip_if_offline()

  file1 <- testthat::test_path("examples", "image.jpg")
  response1 <- dai_sync(file1)
  expect_equal(response1[["status_code"]], 200)
  parsed1 <- httr::content(response1)
  expect_type(parsed1[["text"]], "character")

  file2 <- image_to_pdf(file1, "foo.pdf")
  response2 <- dai_sync(file2)
  expect_equal(response2[["status_code"]], 200)
  parsed2 <- httr::content(response2)
  expect_type(parsed2[["text"]], "character")

  file.remove("foo.pdf")
  dai_auth()
})

test_that("dai_sync informs about unsuccessful requests", {
  skip_if_no_token()
  skip_if_offline()

  file <- testthat::test_path("examples", "image.jpg")
  response <- dai_sync(file, token = NULL)
  expect_equal(response[["status_code"]], 403)
  dai_auth()
})


## DAI_ASYNC --------------------------------------------------------------------

test_that("dai_async calls out input errors", {

  expect_error(dai_async(files = mtcars), "Invalid files parameter.")
  expect_error(dai_async(files = as.matrix(mtcars)), "Invalid files parameter.")
  expect_error(dai_async(files = TRUE), "Invalid files parameter.")
  expect_error(dai_async(files = NULL), "Invalid files parameter.")
  expect_error(dai_async(files = 1), "Invalid files parameter.")
  expect_error(dai_async(files = as.integer(1)), "Invalid files parameter.")

  expect_error(dai_async(files = "foo.png"), "Input file type not supported.")
  expect_error(dai_async(files = c("foo.pdf", "bar.gif")), "Elements in files vector not all of the same type.")

  expect_error(dai_async(files = "foo.pdf", filetype = "bdf"),
               "Invalid filetype parameter.")
  expect_error(dai_async(files = "foo.pdf", filetype = NULL),
               "Invalid filetype parameter.")
  expect_error(dai_async(files = "foo.pdf", filetype = "tiff"),
               "Mismatch between filetype parameter and actual format of files.")

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

#  expect_error(dai_async(files = "foo.pdf", loc = "USA"),
#               "Invalid loc parameter.")
  expect_error(dai_async(files = "foo.pdf", loc = NULL),
               "Invalid loc parameter.")

  expect_error(dai_async(files = "foo.pdf", pps = 0),
               "Invalid pps parameter.")
  expect_error(dai_async(files = "foo.pdf", pps = 150),
               "Invalid pps parameter.")
  expect_error(dai_async(files = "foo.pdf", pps = 1.1),
               "Invalid pps parameter.")
  expect_error(dai_async(files = "foo.pdf", pps = "ten"),
               "Invalid pps parameter.")
  expect_error(dai_async(files = "foo.pdf", pps = NULL),
               "Invalid pps parameter.")
} )


test_that("dai_sync sends succesful requests to Document AI", {
  skip_if_no_token()
  skip_if_offline()

  response <- dai_async("foo.pdf")
  expect_equal(response[[1]][["status_code"]], 200)

  response <- dai_async("foo.gif", filetype = "gif")
  expect_equal(response[[1]][["status_code"]], 200)

  response <- dai_async("foo.tiff", filetype = "tiff")
  expect_equal(response[[1]][["status_code"]], 200)

  response <- dai_async("foo.pdf", dest_folder = "folder/")
  expect_equal(response[[1]][["status_code"]], 200)

  response <- dai_async("foo.pdf", bucket = "gs://bucket")
  expect_equal(response[[1]][["status_code"]], 200)

  response <- dai_async("foo.pdf", bucket = "bucket/")
  expect_equal(response[[1]][["status_code"]], 200)

  response <- dai_async(c("foo.pdf", "bar.pdf"))
  expect_equal(response[[1]][["status_code"]], 200)
  expect_equal(response[[2]][["status_code"]], 200)
  dai_auth()
})

test_that("dai_sync informs about unsuccessful requests", {
  skip_if_no_token()
  skip_if_offline()

  response <- dai_async("foo.pdf", token = NULL)
  expect_equal(response[[1]][["status_code"]], 403)
  dai_auth()
})
