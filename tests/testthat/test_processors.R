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

## LIST_PROCESSOR_TYPES ----------------------------------------

test_that("list_processor_types calls out input errors", {
  skip_on_cran()
  skip_on_ci()

  expect_error(list_processor_types(full_list = null), "Invalid full_list argument. Must be either TRUE or FALSE.")
  expect_error(list_processor_types(full_list = na), "Invalid full_list argument. Must be either TRUE or FALSE.")
  expect_error(list_processor_types(full_list = number_random), "Invalid full_list argument. Must be either TRUE or FALSE.")
  expect_error(list_processor_types(full_list = vector_strings), "Invalid full_list argument. Must be either TRUE or FALSE.")
  expect_error(list_processor_types(full_list = list_strings), "Invalid full_list argument. Must be either TRUE or FALSE.")
  expect_error(list_processor_types(full_list = df), "Invalid full_list argument. Must be either TRUE or FALSE.")
  expect_error(list_processor_types(full_list = matrix), "Invalid full_list argument. Must be either TRUE or FALSE.")

  expect_error(list_processor_types(proj_id = null), "Invalid proj_id parameter.")
  expect_error(list_processor_types(proj_id = na), "Invalid proj_id parameter.")
  expect_error(list_processor_types(proj_id = boolean), "Invalid proj_id parameter.")
  expect_error(list_processor_types(proj_id = number_random), "Invalid proj_id parameter.")
  expect_error(list_processor_types(proj_id = vector_strings), "Invalid proj_id parameter.")
  expect_error(list_processor_types(proj_id = list_strings), "Invalid proj_id parameter.")
  expect_error(list_processor_types(proj_id = df), "Invalid proj_id parameter.")
  expect_error(list_processor_types(proj_id = matrix), "Invalid proj_id parameter.")

  expect_error(list_processor_types(loc = null), "Invalid loc parameter.")
  expect_error(list_processor_types(loc = na), "Invalid loc parameter.")
  expect_error(list_processor_types(loc = boolean), "Invalid loc parameter.")
  expect_error(list_processor_types(loc = number_random), "Invalid loc parameter.")
  expect_error(list_processor_types(loc = string_random), "Invalid loc parameter.")
  expect_error(list_processor_types(loc = vector_strings), "Invalid loc parameter.")
  expect_error(list_processor_types(loc = list_strings), "Invalid loc parameter.")
  expect_error(list_processor_types(loc = df), "Invalid loc parameter.")
  expect_error(list_processor_types(loc = matrix), "Invalid loc parameter.")
  expect_error(list_processor_types(loc = "usa"), "Invalid loc parameter. Must be either 'eu' or 'us'.")
})

test_that("list_processor_types lists processor types", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  response <- list_processor_types()
  expect_equal(class(response), "character")
})

## CREATE_PROCESSOR

test_that("create_processor calls out input errors", {
  skip_on_cran()
  skip_on_ci()

  expect_error(create_processor(name = null), "Invalid name parameter.")
  expect_error(create_processor(name = na), "Invalid name parameter.")
  expect_error(create_processor(name = boolean), "Invalid name parameter.")
  expect_error(create_processor(name = number_random), "Invalid name parameter.")
  expect_error(create_processor(name = vector_strings), "Invalid name parameter.")
  expect_error(create_processor(name = list_strings), "Invalid name parameter.")
  expect_error(create_processor(name = df), "Invalid name parameter.")
  expect_error(create_processor(name = matrix), "Invalid name parameter.")

  expect_error(create_processor(name = "test", proj_id = null), "Invalid proj_id parameter.")
  expect_error(create_processor(name = "test", proj_id = na), "Invalid proj_id parameter.")
  expect_error(create_processor(name = "test", proj_id = boolean), "Invalid proj_id parameter.")
  expect_error(create_processor(name = "test", proj_id = number_random), "Invalid proj_id parameter.")
  expect_error(create_processor(name = "test", proj_id = vector_strings), "Invalid proj_id parameter.")
  expect_error(create_processor(name = "test", proj_id = list_strings), "Invalid proj_id parameter.")
  expect_error(create_processor(name = "test", proj_id = df), "Invalid proj_id parameter.")
  expect_error(create_processor(name = "test", proj_id = matrix), "Invalid proj_id parameter.")

  expect_error(create_processor(name = "test", loc = null), "Invalid loc parameter. Must be either 'eu' or 'us'.")
  expect_error(create_processor(name = "test", loc = na), "Invalid loc parameter. Must be either 'eu' or 'us'.")
  expect_error(create_processor(name = "test", loc = boolean), "Invalid loc parameter. Must be either 'eu' or 'us'.")
  expect_error(create_processor(name = "test", loc = number_random), "Invalid loc parameter. Must be either 'eu' or 'us'.")
  expect_error(create_processor(name = "test", loc = string_random), "Invalid loc parameter. Must be either 'eu' or 'us'.")
  expect_error(create_processor(name = "test", loc = vector_strings), "Invalid loc parameter. Must be either 'eu' or 'us'.")
  expect_error(create_processor(name = "test", loc = list_strings), "Invalid loc parameter. Must be either 'eu' or 'us'.")
  expect_error(create_processor(name = "test", loc = df), "Invalid loc parameter. Must be either 'eu' or 'us'.")
  expect_error(create_processor(name = "test", loc = matrix), "Invalid loc parameter. Must be either 'eu' or 'us'.")
  expect_error(create_processor(name = "test", loc = "usa"), "Invalid loc parameter. Must be either 'eu' or 'us'.")
})

test_that("create_processor creates processor", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  id <- create_processor("test")
  expect_equal(class(id), "character")
  delete_processor(id)
})

## GET_PROCESSORS ---------------------------------------------------------

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

## GET_PROCESSOR_INFO----------------------------------------------------

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

  expect_error(get_processor_info(proc_id = get_processors()$id[1], loc = null), "Invalid loc parameter.")
  expect_error(get_processor_info(proc_id = get_processors()$id[1], loc = na), "Invalid loc parameter.")
  expect_error(get_processor_info(proc_id = get_processors()$id[1], loc = boolean), "Invalid loc parameter.")
  expect_error(get_processor_info(proc_id = get_processors()$id[1], loc = number_random), "Invalid loc parameter.")
  expect_error(get_processor_info(proc_id = get_processors()$id[1], loc = string_random), "Invalid loc parameter.")
  expect_error(get_processor_info(proc_id = get_processors()$id[1], loc = vector_strings), "Invalid loc parameter.")
  expect_error(get_processor_info(proc_id = get_processors()$id[1], loc = list_strings), "Invalid loc parameter.")
  expect_error(get_processor_info(proc_id = get_processors()$id[1], loc = df), "Invalid loc parameter.")
  expect_error(get_processor_info(proc_id = get_processors()$id[1], loc = matrix), "Invalid loc parameter.")
  expect_error(get_processor_info(proc_id = get_processors()$id[1], loc = "usa"), "Invalid loc parameter.")
})

test_that("get_processor_info gets processor info", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  response <- get_processor_info(proc_id = get_processors()$id[1])
  expect_equal(class(response), "list")
})

## GET_PROCESSOR_VERSIONS ----------------------------------------

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

  expect_error(get_processor_versions(proc_id = get_processors()$id[1], loc = null), "Invalid loc parameter.")
  expect_error(get_processor_versions(proc_id = get_processors()$id[1], loc = na), "Invalid loc parameter.")
  expect_error(get_processor_versions(proc_id = get_processors()$id[1], loc = boolean), "Invalid loc parameter.")
  expect_error(get_processor_versions(proc_id = get_processors()$id[1], loc = number_random), "Invalid loc parameter.")
  expect_error(get_processor_versions(proc_id = get_processors()$id[1], loc = string_random), "Invalid loc parameter.")
  expect_error(get_processor_versions(proc_id = get_processors()$id[1], loc = vector_strings), "Invalid loc parameter.")
  expect_error(get_processor_versions(proc_id = get_processors()$id[1], loc = list_strings), "Invalid loc parameter.")
  expect_error(get_processor_versions(proc_id = get_processors()$id[1], loc = df), "Invalid loc parameter.")
  expect_error(get_processor_versions(proc_id = get_processors()$id[1], loc = matrix), "Invalid loc parameter.")
  expect_error(get_processor_versions(proc_id = get_processors()$id[1], loc = "usa"), "Invalid loc parameter.")
})

test_that("get_processor_versions gets processor versions", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  response <- suppressWarnings(get_processor_versions(proc_id = get_processors()$id[1]))
  expect_equal(class(response), "data.frame")
})

## ENABLE_PROCESSOR ----------------------------------------

test_that("enable_processor calls out input errors", {
  skip_on_cran()
  skip_on_ci()

  expect_error(enable_processor(proc_id = null), "Invalid proc_id parameter.")
  expect_error(enable_processor(proc_id = na), "Invalid proc_id parameter.")
  expect_error(enable_processor(proc_id = boolean), "Invalid proc_id parameter.")
  expect_error(enable_processor(proc_id = number_random), "Invalid proc_id parameter.")
  expect_error(enable_processor(proc_id = vector_strings), "Invalid proc_id parameter.")
  expect_error(enable_processor(proc_id = list_strings), "Invalid proc_id parameter.")
  expect_error(enable_processor(proc_id = df), "Invalid proc_id parameter.")
  expect_error(enable_processor(proc_id = matrix), "Invalid proc_id parameter.")

  procs <- get_processors()
  id <- procs$id[1]

  expect_error(enable_processor(proc_id = id, proj_id = null), "Invalid proj_id parameter.")
  expect_error(enable_processor(proc_id = id, proj_id = na), "Invalid proj_id parameter.")
  expect_error(enable_processor(proc_id = id, proj_id = boolean), "Invalid proj_id parameter.")
  expect_error(enable_processor(proc_id = id, proj_id = number_random), "Invalid proj_id parameter.")
  expect_error(enable_processor(proc_id = id, proj_id = vector_strings), "Invalid proj_id parameter.")
  expect_error(enable_processor(proc_id = id, proj_id = list_strings), "Invalid proj_id parameter.")
  expect_error(enable_processor(proc_id = id, proj_id = df), "Invalid proj_id parameter.")
  expect_error(enable_processor(proc_id = id, proj_id = matrix), "Invalid proj_id parameter.")

  expect_error(enable_processor(proc_id = id, loc = null), "Invalid loc parameter.")
  expect_error(enable_processor(proc_id = id, loc = na), "Invalid loc parameter.")
  expect_error(enable_processor(proc_id = id, loc = boolean), "Invalid loc parameter.")
  expect_error(enable_processor(proc_id = id, loc = number_random), "Invalid loc parameter.")
  expect_error(enable_processor(proc_id = id, loc = string_random), "Invalid loc parameter.")
  expect_error(enable_processor(proc_id = id, loc = vector_strings), "Invalid loc parameter.")
  expect_error(enable_processor(proc_id = id, loc = list_strings), "Invalid loc parameter.")
  expect_error(enable_processor(proc_id = id, loc = df), "Invalid loc parameter.")
  expect_error(enable_processor(proc_id = id, loc = matrix), "Invalid loc parameter.")
  expect_error(enable_processor(proc_id = id, loc = "usa"), "Invalid loc parameter.")
})

test_that("enable_processor enables processors", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  procs <- get_processors()
  id <- procs$id[1]
  response <- suppressWarnings(enable_processor(proc_id = id))
  expect_equal(class(response), "character")
})

## DISABLE_PROCESSOR ----------------------------------------

test_that("disable_processor calls out input errors", {
  skip_on_cran()
  skip_on_ci()
  expect_error(disable_processor(proc_id = null), "Invalid proc_id parameter.")
  expect_error(disable_processor(proc_id = na), "Invalid proc_id parameter.")
  expect_error(disable_processor(proc_id = boolean), "Invalid proc_id parameter.")
  expect_error(disable_processor(proc_id = number_random), "Invalid proc_id parameter.")
  expect_error(disable_processor(proc_id = vector_strings), "Invalid proc_id parameter.")
  expect_error(disable_processor(proc_id = list_strings), "Invalid proc_id parameter.")
  expect_error(disable_processor(proc_id = df), "Invalid proc_id parameter.")
  expect_error(disable_processor(proc_id = matrix), "Invalid proc_id parameter.")

  procs <- get_processors()
  id <- procs$id[1]

  expect_error(disable_processor(proc_id = id, proj_id = null), "Invalid proj_id parameter.")
  expect_error(disable_processor(proc_id = id, proj_id = na), "Invalid proj_id parameter.")
  expect_error(disable_processor(proc_id = id, proj_id = boolean), "Invalid proj_id parameter.")
  expect_error(disable_processor(proc_id = id, proj_id = number_random), "Invalid proj_id parameter.")
  expect_error(disable_processor(proc_id = id, proj_id = vector_strings), "Invalid proj_id parameter.")
  expect_error(disable_processor(proc_id = id, proj_id = list_strings), "Invalid proj_id parameter.")
  expect_error(disable_processor(proc_id = id, proj_id = df), "Invalid proj_id parameter.")
  expect_error(disable_processor(proc_id = id, proj_id = matrix), "Invalid proj_id parameter.")

  expect_error(disable_processor(proc_id = id, loc = null), "Invalid loc parameter.")
  expect_error(disable_processor(proc_id = id, loc = na), "Invalid loc parameter.")
  expect_error(disable_processor(proc_id = id, loc = boolean), "Invalid loc parameter.")
  expect_error(disable_processor(proc_id = id, loc = number_random), "Invalid loc parameter.")
  expect_error(disable_processor(proc_id = id, loc = string_random), "Invalid loc parameter.")
  expect_error(disable_processor(proc_id = id, loc = vector_strings), "Invalid loc parameter.")
  expect_error(disable_processor(proc_id = id, loc = list_strings), "Invalid loc parameter.")
  expect_error(disable_processor(proc_id = id, loc = df), "Invalid loc parameter.")
  expect_error(disable_processor(proc_id = id, loc = matrix), "Invalid loc parameter.")
  expect_error(disable_processor(proc_id = id, loc = "usa"), "Invalid loc parameter.")
})

test_that("disable_processor disables processors", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  procs <- get_processors()
  id <- procs$id[1]
  response <- suppressWarnings(disable_processor(proc_id = id))
  expect_equal(class(response), "character")
  enable_processor(proc_id = id)
})

## DELETE_PROCESSOR ----------------------------------------

test_that("delete_processor calls out input errors", {
  skip_on_cran()
  skip_on_ci()
  expect_error(delete_processor(proc_id = null), "Invalid proc_id parameter.")
  expect_error(delete_processor(proc_id = na), "Invalid proc_id parameter.")
  expect_error(delete_processor(proc_id = boolean), "Invalid proc_id parameter.")
  expect_error(delete_processor(proc_id = number_random), "Invalid proc_id parameter.")
  expect_error(delete_processor(proc_id = vector_strings), "Invalid proc_id parameter.")
  expect_error(delete_processor(proc_id = list_strings), "Invalid proc_id parameter.")
  expect_error(delete_processor(proc_id = df), "Invalid proc_id parameter.")
  expect_error(delete_processor(proc_id = matrix), "Invalid proc_id parameter.")

  procs <- get_processors()
  id <- procs$id[1]

  expect_error(delete_processor(proc_id = id, proj_id = null), "Invalid proj_id parameter.")
  expect_error(delete_processor(proc_id = id, proj_id = na), "Invalid proj_id parameter.")
  expect_error(delete_processor(proc_id = id, proj_id = boolean), "Invalid proj_id parameter.")
  expect_error(delete_processor(proc_id = id, proj_id = number_random), "Invalid proj_id parameter.")
  expect_error(delete_processor(proc_id = id, proj_id = vector_strings), "Invalid proj_id parameter.")
  expect_error(delete_processor(proc_id = id, proj_id = list_strings), "Invalid proj_id parameter.")
  expect_error(delete_processor(proc_id = id, proj_id = df), "Invalid proj_id parameter.")
  expect_error(delete_processor(proc_id = id, proj_id = matrix), "Invalid proj_id parameter.")

  expect_error(delete_processor(proc_id = id, loc = null), "Invalid loc parameter.")
  expect_error(delete_processor(proc_id = id, loc = na), "Invalid loc parameter.")
  expect_error(delete_processor(proc_id = id, loc = boolean), "Invalid loc parameter.")
  expect_error(delete_processor(proc_id = id, loc = number_random), "Invalid loc parameter.")
  expect_error(delete_processor(proc_id = id, loc = string_random), "Invalid loc parameter.")
  expect_error(delete_processor(proc_id = id, loc = vector_strings), "Invalid loc parameter.")
  expect_error(delete_processor(proc_id = id, loc = list_strings), "Invalid loc parameter.")
  expect_error(delete_processor(proc_id = id, loc = df), "Invalid loc parameter.")
  expect_error(delete_processor(proc_id = id, loc = matrix), "Invalid loc parameter.")
  expect_error(delete_processor(proc_id = id, loc = "usa"), "Invalid loc parameter.")
})

test_that("delete_processor deletes processors", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  # response <- suppressWarnings(delete_processor(proc_id = get_processors()$id[1]))
  # expect_equal(class(response), "data.frame")
})