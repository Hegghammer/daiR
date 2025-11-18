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

## HELPER FUNCTIONS FOR TESTING --------------------------------------------

# Helper function to test full_list parameter
test_full_list_param <- function(func, func_name) {
  test_that(paste0(func_name, " errors with invalid full_list parameter"), {
    skip_on_cran()
    skip_on_ci()

    expect_error(func(full_list = null), "Invalid full_list argument. Must be either TRUE or FALSE.")
    expect_error(func(full_list = na), "Invalid full_list argument. Must be either TRUE or FALSE.")
    expect_error(func(full_list = number_random), "Invalid full_list argument. Must be either TRUE or FALSE.")
    expect_error(func(full_list = vector_strings), "Invalid full_list argument. Must be either TRUE or FALSE.")
    expect_error(func(full_list = list_strings), "Invalid full_list argument. Must be either TRUE or FALSE.")
    expect_error(func(full_list = df), "Invalid full_list argument. Must be either TRUE or FALSE.")
    expect_error(func(full_list = matrix), "Invalid full_list argument. Must be either TRUE or FALSE.")
  })
}

# Helper function to test proj_id parameter
test_proj_id_param <- function(func, func_name, ...) {
  test_that(paste0(func_name, " errors with invalid proj_id parameter"), {
    skip_on_cran()
    skip_on_ci()

    expect_error(func(proj_id = null, ...), "Invalid proj_id parameter.")
    expect_error(func(proj_id = na, ...), "Invalid proj_id parameter.")
    expect_error(func(proj_id = boolean, ...), "Invalid proj_id parameter.")
    expect_error(func(proj_id = number_random, ...), "Invalid proj_id parameter.")
    expect_error(func(proj_id = vector_strings, ...), "Invalid proj_id parameter.")
    expect_error(func(proj_id = list_strings, ...), "Invalid proj_id parameter.")
    expect_error(func(proj_id = df, ...), "Invalid proj_id parameter.")
    expect_error(func(proj_id = matrix, ...), "Invalid proj_id parameter.")
  })
}

# Helper function to test loc parameter
test_loc_param <- function(func, func_name, ...) {
  test_that(paste0(func_name, " errors with invalid loc parameter"), {
    skip_on_cran()
    skip_on_ci()

    expect_error(func(loc = null, ...), "Invalid loc parameter")
    expect_error(func(loc = na, ...), "Invalid loc parameter")
    expect_error(func(loc = boolean, ...), "Invalid loc parameter")
    expect_error(func(loc = number_random, ...), "Invalid loc parameter")
    expect_error(func(loc = string_random, ...), "Invalid loc parameter")
    expect_error(func(loc = vector_strings, ...), "Invalid loc parameter")
    expect_error(func(loc = list_strings, ...), "Invalid loc parameter")
    expect_error(func(loc = df, ...), "Invalid loc parameter")
    expect_error(func(loc = matrix, ...), "Invalid loc parameter")
    expect_error(func(loc = "usa", ...), "Invalid loc parameter. Must be either 'eu' or 'us'.")
  })
}

# Helper function to test name parameter (for create_processor)
test_name_param <- function(func, func_name) {
  test_that(paste0(func_name, " errors with invalid name parameter"), {
    skip_on_cran()
    skip_on_ci()

    expect_error(func(name = null), "Invalid name parameter.")
    expect_error(func(name = na), "Invalid name parameter.")
    expect_error(func(name = boolean), "Invalid name parameter.")
    expect_error(func(name = number_random), "Invalid name parameter.")
    expect_error(func(name = vector_strings), "Invalid name parameter.")
    expect_error(func(name = list_strings), "Invalid name parameter.")
    expect_error(func(name = df), "Invalid name parameter.")
    expect_error(func(name = matrix), "Invalid name parameter.")
  })
}

# Helper function to test proc_id parameter
test_proc_id_param <- function(func, func_name, ...) {
  test_that(paste0(func_name, " errors with invalid proc_id parameter"), {
    skip_on_cran()
    skip_on_ci()

    expect_error(func(proc_id = null, ...), "Invalid proc_id parameter.")
    expect_error(func(proc_id = na, ...), "Invalid proc_id parameter.")
    expect_error(func(proc_id = boolean, ...), "Invalid proc_id parameter.")
    expect_error(func(proc_id = number_random, ...), "Invalid proc_id parameter.")
    expect_error(func(proc_id = vector_strings, ...), "Invalid proc_id parameter.")
    expect_error(func(proc_id = list_strings, ...), "Invalid proc_id parameter.")
    expect_error(func(proc_id = df, ...), "Invalid proc_id parameter.")
    expect_error(func(proc_id = matrix, ...), "Invalid proc_id parameter.")
  })
}

# Helper function to test type parameter (for get_ids_by_type, get_versions_by_type)
test_type_param <- function(func, func_name) {
  test_that(paste0(func_name, " errors with invalid type parameter"), {
    skip_on_cran()
    skip_on_ci()

    expect_error(func(type = null), "Invalid type parameter.")
    expect_error(func(type = na), "Invalid type parameter.")
    expect_error(func(type = boolean), "Invalid type parameter.")
    expect_error(func(type = number_random), "Invalid type parameter.")
    expect_error(func(type = vector_strings), "Invalid type parameter.")
    expect_error(func(type = list_strings), "Invalid type parameter.")
    expect_error(func(type = df), "Invalid type parameter.")
    expect_error(func(type = matrix), "Invalid type parameter.")
  })
}

# Helper function to test empty string edge case for proc_id
test_proc_id_empty_string <- function(func, func_name, ...) {
  test_that(paste0(func_name, " errors with empty string proc_id"), {
    skip_on_cran()
    skip_on_ci()

    expect_error(func(proc_id = "", ...), "Invalid proc_id parameter.")
  })
}

## LIST_PROCESSOR_TYPES ------------------------------------------------

test_full_list_param(list_processor_types, "list_processor_types()")
test_proj_id_param(list_processor_types, "list_processor_types()")
test_loc_param(list_processor_types, "list_processor_types()")

test_that("list_processor_types() returns character vector with full_list = FALSE", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  response <- list_processor_types(full_list = FALSE)
  expect_type(response, "character")
  expect_true(length(response) > 0)
})

test_that("list_processor_types() returns list with full_list = TRUE", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  response <- list_processor_types(full_list = TRUE)
  expect_type(response, "list")
  expect_true(length(response) > 0)
})

test_that("list_processor_types() handles loc parameter case-insensitively", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  # Should work with uppercase
  expect_no_error(list_processor_types(loc = "EU"))
  expect_no_error(list_processor_types(loc = "US"))

  # Should work with lowercase
  expect_no_error(list_processor_types(loc = "eu"))
  expect_no_error(list_processor_types(loc = "us"))
})

## CREATE_PROCESSOR ----------------------------------------------------

test_name_param(create_processor, "create_processor()")
test_proj_id_param(create_processor, "create_processor()", name = "test")
test_loc_param(create_processor, "create_processor()", name = "test")

test_that("create_processor() errors with invalid type parameter", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  # Should error if type is not in available types
  expect_error(
    create_processor(name = "test", type = "INVALID_TYPE"),
    "Invalid type parameter or requested type not available."
  )
})

test_that("create_processor() creates processor and returns character id", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  # Create with default OCR_PROCESSOR type
  id <- create_processor(paste0("test-", as.integer(Sys.time())))
  expect_type(id, "character")
  expect_true(nchar(id) > 0)

  # Clean up
  suppressMessages(delete_processor(id))
})

test_that("create_processor() works with different processor types", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  available_types <- list_processor_types()

  # Test with first available type
  if (length(available_types) > 0) {
    id <- create_processor(
      name = paste0("test-type-", as.integer(Sys.time())),
      type = available_types[1]
    )
    expect_type(id, "character")
    suppressMessages(delete_processor(id))
  }
})

## GET_PROCESSORS ------------------------------------------------------

test_proj_id_param(get_processors, "get_processors()")
test_loc_param(get_processors, "get_processors()")

test_that("get_processors() returns a data.frame", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  response <- get_processors()
  expect_s3_class(response, "data.frame")

  # Check for expected columns
  expect_true("id" %in% names(response))
  expect_true("name" %in% names(response))
  expect_true("type" %in% names(response))
})

test_that("get_processors() handles empty processor list", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  # This should not error even if no processors exist
  expect_no_error(get_processors())
})

## GET_PROCESSOR_INFO --------------------------------------------------

test_proj_id_param(get_processor_info, "get_processor_info()", proc_id = "test-id")
test_proc_id_param(get_processor_info, "get_processor_info()")
test_proc_id_empty_string(get_processor_info, "get_processor_info()")

test_that("get_processor_info() errors with invalid loc parameter", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  # Get a real processor ID first
  procs <- get_processors()
  skip_if(nrow(procs) == 0, "No processors available for testing")

  id <- procs$id[1]

  expect_error(get_processor_info(proc_id = id, loc = null), "Invalid loc parameter.")
  expect_error(get_processor_info(proc_id = id, loc = na), "Invalid loc parameter.")
  expect_error(get_processor_info(proc_id = id, loc = boolean), "Invalid loc parameter.")
  expect_error(get_processor_info(proc_id = id, loc = number_random), "Invalid loc parameter.")
  expect_error(get_processor_info(proc_id = id, loc = string_random), "Invalid loc parameter.")
  expect_error(get_processor_info(proc_id = id, loc = vector_strings), "Invalid loc parameter.")
  expect_error(get_processor_info(proc_id = id, loc = list_strings), "Invalid loc parameter.")
  expect_error(get_processor_info(proc_id = id, loc = df), "Invalid loc parameter.")
  expect_error(get_processor_info(proc_id = id, loc = matrix), "Invalid loc parameter.")
  expect_error(get_processor_info(proc_id = id, loc = "usa"), "Invalid loc parameter.")
})

test_that("get_processor_info() returns a list", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  procs <- get_processors()
  skip_if(nrow(procs) == 0, "No processors available for testing")

  response <- get_processor_info(proc_id = procs$id[1])
  expect_type(response, "list")
})

## GET_PROCESSOR_VERSIONS ----------------------------------------------

test_proj_id_param(get_processor_versions, "get_processor_versions()", proc_id = "test-id")
test_proc_id_param(get_processor_versions, "get_processor_versions()")
test_proc_id_empty_string(get_processor_versions, "get_processor_versions()")

test_that("get_processor_versions() errors with invalid loc parameter", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  procs <- get_processors()
  skip_if(nrow(procs) == 0, "No processors available for testing")

  id <- procs$id[1]

  expect_error(get_processor_versions(proc_id = id, loc = null), "Invalid loc parameter.")
  expect_error(get_processor_versions(proc_id = id, loc = na), "Invalid loc parameter.")
  expect_error(get_processor_versions(proc_id = id, loc = boolean), "Invalid loc parameter.")
  expect_error(get_processor_versions(proc_id = id, loc = number_random), "Invalid loc parameter.")
  expect_error(get_processor_versions(proc_id = id, loc = string_random), "Invalid loc parameter.")
  expect_error(get_processor_versions(proc_id = id, loc = vector_strings), "Invalid loc parameter.")
  expect_error(get_processor_versions(proc_id = id, loc = list_strings), "Invalid loc parameter.")
  expect_error(get_processor_versions(proc_id = id, loc = df), "Invalid loc parameter.")
  expect_error(get_processor_versions(proc_id = id, loc = matrix), "Invalid loc parameter.")
  expect_error(get_processor_versions(proc_id = id, loc = "usa"), "Invalid loc parameter.")
})

test_that("get_processor_versions() returns a data.frame", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  procs <- get_processors()
  skip_if(nrow(procs) == 0, "No processors available for testing")

  response <- suppressWarnings(get_processor_versions(proc_id = procs$id[1]))
  expect_s3_class(response, "data.frame")

  # The function should return a data.frame
  # If there are rows AND a name column exists, shortName should be created
  if (nrow(response) > 0) {
    # We can't guarantee what columns the API returns
    # Just verify it's a valid data.frame with some content
    expect_true(ncol(response) > 0)
  }
})

## ENABLE_PROCESSOR ----------------------------------------------------

test_proc_id_param(enable_processor, "enable_processor()")
test_proj_id_param(enable_processor, "enable_processor()", proc_id = "test-id")
test_loc_param(enable_processor, "enable_processor()", proc_id = "test-id")

test_that("enable_processor() runs without error on valid processor", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  procs <- get_processors()
  skip_if(nrow(procs) == 0, "No processors available for testing")

  id <- procs$id[1]

  # Should not error (may already be enabled, but that's ok)
  expect_no_error(suppressMessages(enable_processor(proc_id = id)))
})

## DISABLE_PROCESSOR ---------------------------------------------------

test_proc_id_param(disable_processor, "disable_processor()")
test_proj_id_param(disable_processor, "disable_processor()", proc_id = "test-id")
test_loc_param(disable_processor, "disable_processor()", proc_id = "test-id")

test_that("disable_processor() runs without error on valid processor", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  procs <- get_processors()
  skip_if(nrow(procs) == 0, "No processors available for testing")

  id <- procs$id[1]

  # Disable then re-enable to restore state
  expect_no_error(suppressMessages(disable_processor(proc_id = id)))
  suppressMessages(enable_processor(proc_id = id))
})

test_that("enable/disable processor workflow works correctly", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  procs <- get_processors()
  skip_if(nrow(procs) == 0, "No processors available for testing")

  id <- procs$id[1]

  # Test the full enable -> disable -> enable cycle
  suppressMessages({
    enable_processor(proc_id = id)
    disable_processor(proc_id = id)
    enable_processor(proc_id = id)
  })

  # Should complete without error
  expect_true(TRUE)
})

## DELETE_PROCESSOR ----------------------------------------------------

test_proc_id_param(delete_processor, "delete_processor()")
test_proj_id_param(delete_processor, "delete_processor()", proc_id = "test-id")
test_loc_param(delete_processor, "delete_processor()", proc_id = "test-id")

test_that("delete_processor() returns a response object", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  # Create a processor specifically for deletion
  id <- create_processor(paste0("test-delete-", as.integer(Sys.time())))

  response <- suppressMessages(delete_processor(proc_id = id))

  # delete_processor returns the httr response object
  expect_s3_class(response, "response")
})

## GET_IDS_BY_TYPE -----------------------------------------------------

test_type_param(get_ids_by_type, "get_ids_by_type()")
test_proj_id_param(get_ids_by_type, "get_ids_by_type()", type = "OCR_PROCESSOR")
test_loc_param(get_ids_by_type, "get_ids_by_type()", type = "OCR_PROCESSOR")

test_that("get_ids_by_type() returns character vector for existing type", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  procs <- get_processors()
  skip_if(nrow(procs) == 0, "No processors available for testing")

  # Get a type that exists
  existing_type <- procs$type[1]

  result <- get_ids_by_type(type = existing_type)
  expect_type(result, "character")
  expect_true(length(result) > 0)
})

test_that("get_ids_by_type() handles non-existent type gracefully", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  # Should return a message, not error
  expect_message(
    get_ids_by_type(type = "NONEXISTENT_PROCESSOR_TYPE"),
    "No processor of type.*found"
  )
})

test_that("get_ids_by_type() returns unique IDs", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  procs <- get_processors()
  skip_if(nrow(procs) == 0, "No processors available for testing")

  existing_type <- procs$type[1]
  result <- get_ids_by_type(type = existing_type)

  # IDs should be unique
  expect_equal(length(result), length(unique(result)))
})

## GET_VERSIONS_BY_TYPE ------------------------------------------------

test_type_param(get_versions_by_type, "get_versions_by_type()")
test_proj_id_param(get_versions_by_type, "get_versions_by_type()", type = "OCR_PROCESSOR")
test_loc_param(get_versions_by_type, "get_versions_by_type()", type = "OCR_PROCESSOR")

test_that("get_versions_by_type() produces message output for existing type", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  procs <- get_processors()
  skip_if(nrow(procs) == 0, "No processors available for testing")

  existing_type <- procs$type[1]

  # Should produce messages about aliases and versions
  expect_message(get_versions_by_type(type = existing_type), "Aliases:")
  expect_message(get_versions_by_type(type = existing_type), "Full names:")
})

test_that("get_versions_by_type() handles non-existent type gracefully", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  expect_message(
    get_versions_by_type(type = "NONEXISTENT_PROCESSOR_TYPE"),
    "No processor of type.*found"
  )
})

## EDGE CASES AND INTEGRATION TESTS ------------------------------------

test_that("Functions handle case-insensitive loc parameter correctly", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  # Test various functions with different case loc parameters
  expect_no_error(list_processor_types(loc = "EU"))
  expect_no_error(list_processor_types(loc = "Us"))
  expect_no_error(get_processors(loc = "eU"))
})

test_that("All functions validate loc parameter consistently", {
  skip_on_cran()
  skip_on_ci()

  # All these should give the same error message
  error_msg_pattern <- "Invalid loc parameter"

  expect_error(list_processor_types(loc = "uk"), error_msg_pattern)
  expect_error(get_processors(loc = "ca"), error_msg_pattern)
  expect_error(create_processor(name = "test", loc = "au"), error_msg_pattern)
})

test_that("create_processor validates type against available types", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  # Get list of valid types
  valid_types <- list_processor_types()

  # Invalid type should error
  expect_error(
    create_processor(name = "test", type = "TOTALLY_FAKE_TYPE"),
    "Invalid type parameter or requested type not available"
  )

  # Valid type should work (if available)
  if (length(valid_types) > 0 && "OCR_PROCESSOR" %in% valid_types) {
    expect_no_error({
      id <- create_processor(paste0("test-valid-", as.integer(Sys.time())), type = "OCR_PROCESSOR")
      suppressMessages(delete_processor(id))
    })
  }
})

test_that("Processor lifecycle works end-to-end", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  # Create -> Get Info -> Disable -> Enable -> Delete
  proc_name <- paste0("test-lifecycle-", as.integer(Sys.time()))

  # Create
  id <- create_processor(proc_name)
  expect_type(id, "character")

  # Get info
  info <- get_processor_info(proc_id = id)
  expect_type(info, "list")

  # Should appear in list
  all_procs <- get_processors()
  expect_true(id %in% all_procs$id)

  # Disable
  suppressMessages(disable_processor(proc_id = id))

  # Enable
  suppressMessages(enable_processor(proc_id = id))

  # Delete
  response <- suppressMessages(delete_processor(proc_id = id))
  expect_s3_class(response, "response")
})

test_that("get_ids_by_type filters correctly", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  procs <- get_processors()
  skip_if(nrow(procs) == 0, "No processors available for testing")

  # Get processors of a specific type
  test_type <- procs$type[1]
  ids <- get_ids_by_type(type = test_type)

  # All returned IDs should be of the requested type
  for (id in ids) {
    proc_info <- procs[procs$id == id, ]
    expect_equal(proc_info$type[1], test_type)
  }
})

## CLEANUP -------------------------------------------------------------

# Most cleanup is done within individual tests
# Any test processors created should be deleted within their test

