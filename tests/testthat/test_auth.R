
## DAI_AUTH --------------------------------------------------------------------

test_that("dai_auth calls out input errors", {
  expect_error(dai_auth(scopes = NULL), "Error: invalid scopes parameter.")
  expect_error(dai_auth(scopes = 123), "Error: invalid scopes parameter.")
  expect_error(dai_auth(scopes = "www.google.com"), "Error: invalid scope URLs.")
  expect_error(dai_auth(path = 123), "Error: invalid path parameter.")
  expect_error(dai_auth(path = c("file1.json", "file2.json")), "Error: invalid path parameter.")
  expect_error(dai_auth(path = NULL), "Error: invalid path parameter.")
})

test_that("dai_auth authenticates with correct credentials", {
  skip_if_no_token()
  skip_if_offline()

  dai_deauth()
  test_auth()
  expect_true(dai_has_token())
})

test_that("dai_auth sets scopes as instructed", {
  skip_on_ci()
  skip_if_offline()

  dai_deauth()
  scope1 <- "https://www.googleapis.com/auth/cloud-platform"
  dai_auth(scopes = scope1)
  token <- dai_token()
  expect_match(token[["params"]][["scope"]], glue::glue("^{scope1}.*"))

  dai_deauth()
  scope2 <- "https://www.googleapis.com/auth/books"
  scope3 <- "https://www.googleapis.com/auth/documents"
  dai_auth(scopes = c(scope2, scope3))
  token <- dai_token()
  expect_match(token[["params"]][["scope"]], glue::glue("^{scope2} {scope3}.*"))
  test_auth()
})

test_that("dai_auth does not authenticate with wrong credentials", {
  skip_if_no_token()
  skip_if_offline()
  dai_deauth()
  json <- jsonlite::toJSON("a fake json file")
  dai_auth(path = json)
  expect_false(dai_has_token())
  test_auth()
})

## DAI_TOKEN -------------------------------------------------------------------

test_that("dai_token works", {
  skip_on
  skip_if_no_token()
  skip_if_offline()
  dai_deauth()
  expect_equal(dai_token(), NULL)

  .auth$set_auth_active(TRUE)
  expect_type(dai_token(), "environment")
  expect_s3_class(dai_token(), "Token2.0")

  test_auth()
  expect_type(dai_token(), "environment")
  expect_s3_class(dai_token(), "Token2.0")
})

## DAI_HAS_TOKEN ---------------------------------------------------------------

test_that("dai_has_token works", {
  skip_on_ci()
  skip_if_no_token()
  skip_if_offline()
  dai_deauth()
  expect_false(dai_has_token())
  test_auth()
  expect_true(dai_has_token())
})

## DAI_USER --------------------------------------------------------------------

test_that("dai_user works", {
  skip_on_ci()
  skip_if_no_token()
  skip_if_offline()
  test_auth()
  response <- dai_user()
  expect_equal(response[[2]], 200)
  dai_deauth()
  response <- dai_user()
  expect_equal(response[[2]], 401)
  test_auth()
})

## GET_PROJECT_ID --------------------------------------------------------------

test_that("get_project_id calls out input errors", {
  expect_error(get_project_id(path = 12345), "Error: invalid path parameter.")
  expect_error(get_project_id(path = c("file1.json", "file2.json")), "Error: invalid path parameter.")
  expect_error(get_project_id(path = NULL), "Error: invalid path parameter.")
})

## DAI_DEAUTH ------------------------------------------------------------------

test_that("dai_deauth works", {
  skip_on_ci()
  skip_if_no_token()
  skip_if_offline()
  dai_deauth()
  expect_false(dai_has_token())
  test_auth()
})


