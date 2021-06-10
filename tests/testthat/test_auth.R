
## DAI_AUTH --------------------------------------------------------------------

test_that("dai_auth calls out input errors", {
  expect_error(dai_auth(scopes = NULL), "Error: invalid scopes parameter.")
  expect_error(dai_auth(scopes = 123), "Error: invalid scopes parameter.")
  expect_error(dai_auth(scopes = "www.google.com"), "Error: invalid scope URLs.")
  expect_error(dai_auth(path = 123), "Error: invalid path parameter.")
  expect_error(dai_auth(path = c("file1.json", "file2.json")), "Error: invalid path parameter.")
  expect_error(dai_auth(path = NULL), "Error: invalid path parameter.")
})

test_that("dai_auth does not authenticate with wrong credentials", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  json <- jsonlite::toJSON("a fake json file")
  expect_message(dai_auth(path = json), "Token not available. Have you provided a valid service account key file?")
})

## DAI_TOKEN -------------------------------------------------------------------

test_that("dai_token works", {
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

