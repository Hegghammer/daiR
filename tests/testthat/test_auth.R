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

# Create a JSON file without project_id (for testing)
fill_no_project <- list("a" = 1, "b" = 2)
json_no_project <- jsonlite::toJSON(fill_no_project)
madeup_json_file <- tempfile(fileext = ".json")
write(json_no_project, madeup_json_file)

# Create a JSON file WITH project_id (for testing get_project_id)
fill_with_project <- list("project_id" = "test-project-123", "type" = "service_account")
json_with_project <- jsonlite::toJSON(fill_with_project)
valid_json_file <- tempfile(fileext = ".json")
write(json_with_project, valid_json_file)

## DAI_AUTH ----------------------------------------------------------------

test_that("dai_auth warns when path is invalid type", {
  expect_message(dai_auth(path = null), "Access token not available. Have you provided a valid service account key file?")
  expect_message(dai_auth(path = na), "Access token not available. Have you provided a valid service account key file?")
  expect_message(dai_auth(path = boolean), "Access token not available. Have you provided a valid service account key file?")
  expect_message(dai_auth(path = number_random), "Access token not available. Have you provided a valid service account key file?")
  expect_message(dai_auth(path = vector_strings), "Access token not available. Have you provided a valid service account key file?")
  expect_message(dai_auth(path = list_strings), "Access token not available. Have you provided a valid service account key file?")
  expect_message(dai_auth(path = df), "Access token not available. Have you provided a valid service account key file?")
  expect_message(dai_auth(path = matrix), "Access token not available. Have you provided a valid service account key file?")
})

test_that("dai_auth warns when JSON file is invalid for authentication", {
  expect_message(dai_auth(path = madeup_json_file), "Access token not available. Have you provided a valid service account key file?")
})

test_that("dai_auth warns when path is an empty string", {
  expect_message(dai_auth(path = ""), "Access token not available. Have you provided a valid service account key file?")
})

test_that("dai_auth warns when path is a non-existent file", {
  expect_message(dai_auth(path = "nonexistent_file.json"), "Access token not available. Have you provided a valid service account key file?")
})

test_that("dai_auth returns invisibly (no explicit return value)", {
  result <- suppressMessages(dai_auth(path = null))
  expect_null(result)
})

test_that("dai_auth accepts custom scopes parameter", {
  custom_scope <- "https://www.googleapis.com/auth/drive"
  expect_message(
    dai_auth(path = null, scopes = custom_scope),
    "Access token not available"
  )
})

test_that("dai_auth authenticates with correct credentials", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  expect_message(dai_auth(), "Access token available.")
})

test_that("dai_auth works with custom scopes and valid credentials", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  custom_scope <- "https://www.googleapis.com/auth/cloud-platform"
  expect_message(dai_auth(scopes = custom_scope), "Access token available.")
})

## DAI_TOKEN ---------------------------------------------------------------

test_that("dai_token warns and returns NULL with invalid path types", {
  expect_message(dai_token(path = null), "Invalid GCS credentials. No token produced.")
  expect_null(suppressMessages(dai_token(path = null)))

  expect_message(dai_token(path = na), "Invalid GCS credentials. No token produced.")
  expect_null(suppressMessages(dai_token(path = na)))

  expect_message(dai_token(path = boolean), "Invalid GCS credentials. No token produced.")
  expect_null(suppressMessages(dai_token(path = boolean)))

  expect_message(dai_token(path = number_random), "Invalid GCS credentials. No token produced.")
  expect_null(suppressMessages(dai_token(path = number_random)))

  expect_message(dai_token(path = vector_strings), "Invalid GCS credentials. No token produced.")
  expect_null(suppressMessages(dai_token(path = vector_strings)))

  expect_message(dai_token(path = list_strings), "Invalid GCS credentials. No token produced.")
  expect_null(suppressMessages(dai_token(path = list_strings)))

  expect_message(dai_token(path = df), "Invalid GCS credentials. No token produced.")
  expect_null(suppressMessages(dai_token(path = df)))

  expect_message(dai_token(path = matrix), "Invalid GCS credentials. No token produced.")
  expect_null(suppressMessages(dai_token(path = matrix)))
})

test_that("dai_token warns with invalid JSON file", {
  expect_message(dai_token(path = madeup_json_file), "Invalid GCS credentials. No token produced.")
  expect_null(suppressMessages(dai_token(path = madeup_json_file)))
})

test_that("dai_token warns with empty string path", {
  expect_message(dai_token(path = ""), "Invalid GCS credentials. No token produced.")
})

test_that("dai_token warns with non-existent file path", {
  expect_message(dai_token(path = "nonexistent_file.json"), "Invalid GCS credentials. No token produced.")
})

test_that("dai_token accepts custom scopes parameter", {
  custom_scope <- "https://www.googleapis.com/auth/drive"
  expect_message(
    dai_token(path = null, scopes = custom_scope),
    "Invalid GCS credentials"
  )
})

test_that("dai_token produces valid token with correct credentials", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  token <- dai_token()
  expect_type(token, "environment")
  expect_s3_class(token, "Token2.0")
  expect_true(inherits(token, "Token2.0"))
})

test_that("dai_token works with custom scopes and valid credentials", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  custom_scope <- "https://www.googleapis.com/auth/cloud-platform"
  token <- dai_token(scopes = custom_scope)
  expect_s3_class(token, "Token2.0")
})

## DAI_USER ----------------------------------------------------------------

test_that("dai_user returns a list with expected structure", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  info <- dai_user()

  # Check it's a list
  expect_true(is.list(info))

  # Check for expected fields
  expect_true("id" %in% names(info))

  # Check that id is not empty
  expect_true(nchar(info$id) > 0)
})

test_that("dai_user returns consistent results on multiple calls", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  info1 <- dai_user()
  info2 <- dai_user()

  # The same user should have the same id
  expect_equal(info1$id, info2$id)
})

## GET_PROJECT_ID ----------------------------------------------------------

test_that("get_project_id errors with invalid path types", {
  expect_error(get_project_id(path = null), "Error: invalid path parameter.")
  expect_error(get_project_id(path = na), "Error: invalid path parameter.")
  expect_error(get_project_id(path = boolean), "Error: invalid path parameter.")
  expect_error(get_project_id(path = number_random), "Error: invalid path parameter.")
  expect_error(get_project_id(path = vector_strings), "Error: invalid path parameter.")
  expect_error(get_project_id(path = list_strings), "Error: invalid path parameter.")
  expect_error(get_project_id(path = df), "Error: invalid path parameter.")
  expect_error(get_project_id(path = matrix), "Error: invalid path parameter.")
})

test_that("get_project_id errors with empty string path", {
  expect_error(get_project_id(path = ""), "Error: invalid path parameter.")
})

test_that("get_project_id extracts project_id from valid JSON file", {
  project_id <- get_project_id(path = valid_json_file)
  expect_type(project_id, "character")
  expect_equal(project_id, "test-project-123")
  expect_equal(length(project_id), 1)
})

test_that("get_project_id returns NULL when JSON lacks project_id field", {
  result <- get_project_id(path = madeup_json_file)
  expect_null(result)
})

test_that("get_project_id errors gracefully with non-existent file", {
  expect_error(get_project_id(path = "nonexistent_file.json"))
})

test_that("get_project_id errors gracefully with non-JSON file", {
  non_json_file <- tempfile(fileext = ".txt")
  writeLines("This is not JSON", non_json_file)

  expect_error(get_project_id(path = non_json_file))

  unlink(non_json_file, force = TRUE)
})

test_that("get_project_id uses GCS_AUTH_FILE env var by default", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  # Only test if GCS_AUTH_FILE is set
  if (Sys.getenv("GCS_AUTH_FILE") != "") {
    project_id <- get_project_id()
    expect_type(project_id, "character")
    expect_true(nchar(project_id) > 0)
  } else {
    skip("GCS_AUTH_FILE environment variable not set")
  }
})

## INTEGRATION TESTS -------------------------------------------------------

test_that("dai_token and dai_user work together", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  # Get token
  token <- dai_token()
  expect_s3_class(token, "Token2.0")

  # Use token to get user info
  info <- dai_user()
  expect_true(is.list(info))
  expect_true("id" %in% names(info))
})

test_that("authentication workflow is consistent", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  # Check auth
  expect_message(dai_auth(), "Access token available.")

  # Get token
  token <- dai_token()
  expect_s3_class(token, "Token2.0")

  # Get user info
  info <- dai_user()
  expect_true(is.list(info))

  # Get project id
  project_id <- get_project_id()
  expect_type(project_id, "character")
})

## CLEANUP -----------------------------------------------------------------

unlink(madeup_json_file, force = TRUE)
unlink(valid_json_file, force = TRUE)
