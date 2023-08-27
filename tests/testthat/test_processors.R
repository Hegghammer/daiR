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

# GET_PROCESSORS --------------------------------------------------------------------------------

test_that("get_processors calls out input errors", {
  skip_on_cran()
  skip_on_ci()
  expect_error(get_processors(proj_id = null), "Invalid proj_id parameter.")
  expect_error(get_processors(proj_id = na), "Invalid proj_id parameter.")
  expect_error(get_processors(proj_id = boolean), "Invalid proj_id parameter.")
  expect_error(get_processors(proj_id = number_random), "Invalid proj_id parameter.")
  expect_error(get_processors(proj_id = vector_strings), "Invalid proj_id parameter.")
  expect_error(get_processors(proj_id = list_strings), "Invalid proj_id parameter.")
  expect_error(get_processors(proj_id = df), "Invalid proj_id parameter.")
  expect_error(get_processors(proj_id = matrix), "Invalid proj_id parameter.")

  expect_error(get_processors(loc = null), "Invalid loc parameter.")
  expect_error(get_processors(loc = na), "Invalid loc parameter.")
  expect_error(get_processors(loc = boolean), "Invalid loc parameter.")
  expect_error(get_processors(loc = number_random), "Invalid loc parameter.")
  expect_error(get_processors(loc = string_random), "Invalid loc parameter.")
  expect_error(get_processors(loc = vector_strings), "Invalid loc parameter.")
  expect_error(get_processors(loc = list_strings), "Invalid loc parameter.")
  expect_error(get_processors(loc = df), "Invalid loc parameter.")
  expect_error(get_processors(loc = matrix), "Invalid loc parameter.")
  expect_error(get_processors(loc = "usa"), "Invalid loc parameter.")
})

test_that("get_processors gets processors", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  response <- get_processors()
  expect_equal(class(response), "data.frame")
})

# GET_PROCESSOR_INFO--------------------------------------------------------------------------------

test_that("get_processor_info calls out input errors", {
  skip_on_cran()
  skip_on_ci()
  expect_error(get_processor_info(proj_id = null), "Invalid proj_id parameter.")
  expect_error(get_processor_info(proj_id = na), "Invalid proj_id parameter.")
  expect_error(get_processor_info(proj_id = boolean), "Invalid proj_id parameter.")
  expect_error(get_processor_info(proj_id = number_random), "Invalid proj_id parameter.")
  expect_error(get_processor_info(proj_id = vector_strings), "Invalid proj_id parameter.")
  expect_error(get_processor_info(proj_id = list_strings), "Invalid proj_id parameter.")
  expect_error(get_processor_info(proj_id = df), "Invalid proj_id parameter.")
  expect_error(get_processor_info(proj_id = matrix), "Invalid proj_id parameter.")

  expect_error(get_processor_info(proc_id = null), "Invalid proc_id parameter.")
  expect_error(get_processor_info(proc_id = na), "Invalid proc_id parameter.")
  expect_error(get_processor_info(proc_id = boolean), "Invalid proc_id parameter.")
  expect_error(get_processor_info(proc_id = number_random), "Invalid proc_id parameter.")
  expect_error(get_processor_info(proc_id = vector_strings), "Invalid proc_id parameter.")
  expect_error(get_processor_info(proc_id = list_strings), "Invalid proc_id parameter.")
  expect_error(get_processor_info(proc_id = df), "Invalid proc_id parameter.")
  expect_error(get_processor_info(proc_id = matrix), "Invalid proc_id parameter.")

  expect_error(get_processor_info(loc = null), "Invalid loc parameter.")
  expect_error(get_processor_info(loc = na), "Invalid loc parameter.")
  expect_error(get_processor_info(loc = boolean), "Invalid loc parameter.")
  expect_error(get_processor_info(loc = number_random), "Invalid loc parameter.")
  expect_error(get_processor_info(loc = string_random), "Invalid loc parameter.")
  expect_error(get_processor_info(loc = vector_strings), "Invalid loc parameter.")
  expect_error(get_processor_info(loc = list_strings), "Invalid loc parameter.")
  expect_error(get_processor_info(loc = df), "Invalid loc parameter.")
  expect_error(get_processor_info(loc = matrix), "Invalid loc parameter.")
  expect_error(get_processor_info(loc = "usa"), "Invalid loc parameter.")
})

test_that("get_processor_info gets processor info", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  response <- get_processor_info()
  expect_equal(class(response), "list")
})

# GET_PROCESSOR_VERSIONS--------------------------------------------------------------------------------

test_that("get_processor_versions calls out input errors", {
  skip_on_cran()
  skip_on_ci()
  expect_error(get_processor_versions(proj_id = null), "Invalid proj_id parameter.")
  expect_error(get_processor_versions(proj_id = na), "Invalid proj_id parameter.")
  expect_error(get_processor_versions(proj_id = boolean), "Invalid proj_id parameter.")
  expect_error(get_processor_versions(proj_id = number_random), "Invalid proj_id parameter.")
  expect_error(get_processor_versions(proj_id = vector_strings), "Invalid proj_id parameter.")
  expect_error(get_processor_versions(proj_id = list_strings), "Invalid proj_id parameter.")
  expect_error(get_processor_versions(proj_id = df), "Invalid proj_id parameter.")
  expect_error(get_processor_versions(proj_id = matrix), "Invalid proj_id parameter.")

  expect_error(get_processor_versions(proc_id = null), "Invalid proc_id parameter.")
  expect_error(get_processor_versions(proc_id = na), "Invalid proc_id parameter.")
  expect_error(get_processor_versions(proc_id = boolean), "Invalid proc_id parameter.")
  expect_error(get_processor_versions(proc_id = number_random), "Invalid proc_id parameter.")
  expect_error(get_processor_versions(proc_id = vector_strings), "Invalid proc_id parameter.")
  expect_error(get_processor_versions(proc_id = list_strings), "Invalid proc_id parameter.")
  expect_error(get_processor_versions(proc_id = df), "Invalid proc_id parameter.")
  expect_error(get_processor_versions(proc_id = matrix), "Invalid proc_id parameter.")

  expect_error(get_processor_versions(loc = null), "Invalid loc parameter.")
  expect_error(get_processor_versions(loc = na), "Invalid loc parameter.")
  expect_error(get_processor_versions(loc = boolean), "Invalid loc parameter.")
  expect_error(get_processor_versions(loc = number_random), "Invalid loc parameter.")
  expect_error(get_processor_versions(loc = string_random), "Invalid loc parameter.")
  expect_error(get_processor_versions(loc = vector_strings), "Invalid loc parameter.")
  expect_error(get_processor_versions(loc = list_strings), "Invalid loc parameter.")
  expect_error(get_processor_versions(loc = df), "Invalid loc parameter.")
  expect_error(get_processor_versions(loc = matrix), "Invalid loc parameter.")
  expect_error(get_processor_versions(loc = "usa"), "Invalid loc parameter.")
})

test_that("get_processor_versions gets processor versions", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  response <- suppressWarnings(get_processor_versions())
  expect_equal(class(response), "data.frame")
})