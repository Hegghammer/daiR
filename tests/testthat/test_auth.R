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

## DAI_AUTH --------------------------------------------------------------------

test_that("dai_auth warns of incorrect credentials", {
  expect_message(dai_auth(path = null), "Access token not available. Have you provided a valid service account key file?")
  expect_message(dai_auth(path = na), "Access token not available. Have you provided a valid service account key file?")
  expect_message(dai_auth(path = boolean), "Access token not available. Have you provided a valid service account key file?")
  expect_message(dai_auth(path = number_random), "Access token not available. Have you provided a valid service account key file?")
  expect_message(dai_auth(path = vector_strings), "Access token not available. Have you provided a valid service account key file?")
  expect_message(dai_auth(path = list_strings), "Access token not available. Have you provided a valid service account key file?")
  expect_message(dai_auth(path = df), "Access token not available. Have you provided a valid service account key file?")
  expect_message(dai_auth(path = matrix), "Access token not available. Have you provided a valid service account key file?")
  expect_message(dai_auth(path = madeup_json_file), "Access token not available. Have you provided a valid service account key file?")
})

test_that("dai_auth authenticates with correct credentials", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  expect_message(dai_auth(), "Access token available.")
})

## DAI_TOKEN -------------------------------------------------------------------

test_that("dai_token warns of incorrect credentials", {
  expect_message(dai_token(path = null), "Invalid GCS credentials. No token produced.")
  expect_message(dai_token(path = na), "Invalid GCS credentials. No token produced.")
  expect_message(dai_token(path = boolean), "Invalid GCS credentials. No token produced.")
  expect_message(dai_token(path = number_random), "Invalid GCS credentials. No token produced.")
  expect_message(dai_token(path = vector_strings), "Invalid GCS credentials. No token produced.")
  expect_message(dai_token(path = list_strings), "Invalid GCS credentials. No token produced.")
  expect_message(dai_token(path = df), "Invalid GCS credentials. No token produced.")
  expect_message(dai_token(path = matrix), "Invalid GCS credentials. No token produced.")
  expect_message(dai_token(path = madeup_json_file), "Invalid GCS credentials. No token produced.")
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
  expect_error(get_project_id(path = null), "Error: invalid path parameter.")
  expect_error(get_project_id(path = na), "Error: invalid path parameter.")
  expect_error(get_project_id(path = boolean), "Error: invalid path parameter.")
  expect_error(get_project_id(path = number_random), "Error: invalid path parameter.")
  expect_error(get_project_id(path = vector_strings), "Error: invalid path parameter.")
  expect_error(get_project_id(path = list_strings), "Error: invalid path parameter.")
  expect_error(get_project_id(path = df), "Error: invalid path parameter.")
  expect_error(get_project_id(path = matrix), "Error: invalid path parameter.")
})

## CLEANUP ---------------------------------------------------------------------

unlink(madeup_json_file, force = TRUE)
