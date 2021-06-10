
## DAI_AUTH --------------------------------------------------------------------

test_that("dai_auth warns of incorrect credentials", {
  expect_message(dai_auth(path = 123), "Access token not available. Have you provided a valid service account key file?")
  expect_message(dai_auth(path = c("file1.json", "file2.json")), "Access token not available. Have you provided a valid service account key file?")
  expect_message(dai_auth(path = NULL), "Access token not available. Have you provided a valid service account key file?")
  foo <- jsonlite::toJSON("a fake json file")
  expect_message(dai_auth(path = foo), "Access token not available. Have you provided a valid service account key file?")
})

test_that("dai_auth authenticates with correct credentials", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  expect_message(dai_auth(), "Valid access token available.")
})


## DAI_TOKEN -------------------------------------------------------------------

test_that("dai_token warns of incorrect credentials", {
  expect_message(dai_token(path = 123), "Invalid GCS credentials. No token produced.")
  expect_message(dai_token(path = c("file1.json", "file2.json")), "Invalid GCS credentials. No token produced.")
  expect_message(dai_token(path = NULL), "Invalid GCS credentials. No token produced.")
  foo <- jsonlite::toJSON("a fake json file")
  expect_message(dai_token(path = foo), "Invalid GCS credentials. No token produced.")
})

test_that("dai_token produces token given correct credentials", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  expect_type(dai_token(), "environment")
  expect_s3_class(dai_token(), "Token2.0")
})

## DAI_USER --------------------------------------------------------------------

test_that("dai_user works", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  response <- dai_user()
  expect_equal(response[[2]], 200)
})

## GET_PROJECT_ID --------------------------------------------------------------

test_that("get_project_id calls out input errors", {
  expect_error(get_project_id(path = 12345), "Error: invalid path parameter.")
  expect_error(get_project_id(path = c("file1.json", "file2.json")), "Error: invalid path parameter.")
  expect_error(get_project_id(path = NULL), "Error: invalid path parameter.")
})

