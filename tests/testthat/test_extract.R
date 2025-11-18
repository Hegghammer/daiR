## SETUP AND TEST FIXTURES -----------------------------------------------------

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

# Invalid JSON file (not a valid Document AI response)
fill <- list("a" = 1, "b" = 2)
json <- jsonlite::toJSON(fill)
madeup_json_file <- tempfile(fileext = ".json")
write(json, madeup_json_file)

# Real JSON paths (to be used in tests)
sample_json <- testthat::test_path("examples", "output.json")
sample_json_tables <- testthat::test_path("examples", "table_form_parsed.json")
sample_json_blank <- testthat::test_path("examples", "output_blank.json")

## HELPER FUNCTIONS FOR TESTING ------------------------------------------------

# Test invalid inputs for sync type
test_sync_invalid_inputs <- function(extract_function, function_name) {
  test_that(paste0(function_name, " errors with invalid object for sync type"), {
    expect_error(extract_function(null), "Invalid object: not a valid HTTP response")
    expect_error(extract_function(na), "Invalid object: not a valid HTTP response")
    expect_error(extract_function(boolean), "Invalid object: not a valid HTTP response")
    expect_error(extract_function(number_random), "Invalid object: not a valid HTTP response")
    expect_error(extract_function(string_random), "Invalid object: not a valid HTTP response")
    expect_error(extract_function(vector_strings), "Invalid object: not a valid HTTP response")
    expect_error(extract_function(list_strings), "Invalid object: not a valid HTTP response")
    expect_error(extract_function(df), "Invalid object: not a valid HTTP response")
    expect_error(extract_function(matrix), "Invalid object: not a valid HTTP response")
  })
}

# Test invalid inputs for async type - file path errors
test_async_filepath_errors <- function(extract_function, function_name) {
  test_that(paste0(function_name, " errors with invalid filepaths for async type"), {
    expect_error(extract_function(string_random, type = "async"), "Invalid object: file is not a \\.json file or does not exist")
    expect_error(extract_function("wrong.txt", type = "async"), "Invalid object: file is not a \\.json file or does not exist")
    expect_error(extract_function("fake.json", type = "async"), "Invalid object: file is not a \\.json file or does not exist")
  })
}

# Test invalid inputs for async type - object type errors
test_async_object_type_errors <- function(extract_function, function_name) {
  test_that(paste0(function_name, " errors with invalid object types for async type"), {
    expect_error(extract_function(number_random, type = "async"), "Invalid object: must be a single character string filepath")
    expect_error(extract_function(vector_strings, type = "async"), "Invalid object: must be a single character string filepath")
    expect_error(extract_function(null, type = "async"), "Invalid object: must be a single character string filepath")
    expect_error(extract_function(boolean, type = "async"), "Invalid object: must be a single character string filepath")
  })
}

# Test type parameter validation
test_type_parameter <- function(extract_function, function_name) {
  test_that(paste0(function_name, " validates type parameter"), {
    expect_error(extract_function(sample_json, type = "invalid"), "Invalid type parameter.")
    expect_error(extract_function(sample_json, type = c("sync", "async")), "Invalid type parameter.")
    expect_error(extract_function(sample_json, type = NULL), "Invalid type parameter.")
    expect_error(extract_function(sample_json, type = 123), "Invalid type parameter.")
  })
}

# Test handling of invalid JSON format
test_invalid_json_format <- function(extract_function, function_name) {
  test_that(paste0(function_name, " handles JSON not from DAI"), {
    expect_error(extract_function(madeup_json_file, type = "async"), "JSON not in right format. Is it from DAI?")
  })
}

## GET_TEXT --------------------------------------------------------------------

test_sync_invalid_inputs(get_text, "get_text()")
test_async_filepath_errors(get_text, "get_text()")
test_async_object_type_errors(get_text, "get_text()")
test_type_parameter(get_text, "get_text()")
test_invalid_json_format(get_text, "get_text()")

test_that("get_text() validates save_to_file parameter", {
  # String value
  expect_error(
    get_text(sample_json, type = "async", save_to_file = "yes"),
    "Invalid save_to_file argument. Must be either TRUE or FALSE."
  )

  # Numeric value (now properly rejected)
  expect_error(
    get_text(sample_json, type = "async", save_to_file = 1),
    "Invalid save_to_file argument. Must be either TRUE or FALSE."
  )

  # Vector (now properly rejected with clear message)
  expect_error(
    get_text(sample_json, type = "async", save_to_file = c(TRUE, FALSE)),
    "Invalid save_to_file argument. Must be either TRUE or FALSE."
  )

  # NA (now properly rejected)
  expect_error(
    get_text(sample_json, type = "async", save_to_file = NA),
    "Invalid save_to_file argument. Must be either TRUE or FALSE."
  )
})

test_that("get_text() validates dest_dir parameter", {
  expect_error(
    get_text(sample_json, type = "async", save_to_file = TRUE, dest_dir = 123),
    "Invalid dest_dir argument. Must be a valid folder path."
  )
  expect_error(
    get_text(sample_json, type = "async", save_to_file = TRUE, dest_dir = c("path1", "path2")),
    "Invalid dest_dir argument. Must be a valid folder path."
  )
  expect_error(
    get_text(sample_json, type = "async", save_to_file = TRUE, dest_dir = TRUE),
    "Invalid dest_dir argument. Must be a valid folder path."
  )
})

test_that("get_text() validates outfile_stem parameter", {
  expect_error(
    get_text(sample_json, type = "async", save_to_file = TRUE, outfile_stem = c("name1", "name2")),
    "Invalid outfile_stem argument. Must be NULL or a string."
  )
  expect_error(
    get_text(sample_json, type = "async", save_to_file = TRUE, outfile_stem = 123),
    "Invalid outfile_stem argument. Must be NULL or a string."
  )
  expect_error(
    get_text(sample_json, type = "async", save_to_file = TRUE, outfile_stem = TRUE),
    "Invalid outfile_stem argument. Must be NULL or a string."
  )
})

test_that("get_text() gets text from DAI response from example file", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  file <- testthat::test_path("examples", "image.jpg")
  response <- dai_sync(file)
  text <- get_text(response)
  expect_type(text, "character")
  expect_true(nchar(text) > 0)
})

test_that("get_text() gets text from example json file", {
  text <- get_text(sample_json, type = "async")
  expect_type(text, "character")
  expect_true(nchar(text) > 0)
})

test_that("get_text() saves to file with custom outfile_stem", {
  temp_dir <- tempdir()
  custom_stem <- paste0("test_", format(Sys.time(), "%Y%m%d_%H%M%S"))

  text <- get_text(sample_json,
    type = "async", save_to_file = TRUE,
    dest_dir = temp_dir, outfile_stem = custom_stem
  )

  expected_file <- file.path(temp_dir, paste0(custom_stem, ".txt"))
  expect_true(file.exists(expected_file))
  expect_type(text, "character")

  # Clean up
  unlink(expected_file)
})

test_that("get_text() saves to file with default outfile_stem for async", {
  temp_dir <- tempdir()

  text <- get_text(sample_json, type = "async", save_to_file = TRUE, dest_dir = temp_dir)

  expected_file <- file.path(temp_dir, "output.txt")
  expect_true(file.exists(expected_file))

  # Clean up
  unlink(expected_file)
})

test_that("get_text() saves to file with default outfile_stem for sync", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  file <- testthat::test_path("examples", "image.jpg")
  response <- dai_sync(file)
  temp_dir <- tempdir()

  text <- get_text(response, save_to_file = TRUE, dest_dir = temp_dir)

  expected_file <- file.path(temp_dir, "output.txt")
  expect_true(file.exists(expected_file))

  # Clean up
  unlink(expected_file)
})

test_that("get_text() handles blank documents with warning", {
  expect_warning(
    text <- get_text(sample_json_blank, type = "async"),
    "DAI found no text. The document may be blank."
  )
  expect_equal(text, "")
})

test_that("get_text() returns text without saving when save_to_file = FALSE", {
  temp_dir <- tempdir()

  text <- get_text(sample_json, type = "async", save_to_file = FALSE, dest_dir = temp_dir)

  # Check that no file was created
  files <- list.files(temp_dir, pattern = "output\\.txt")
  expect_equal(length(files), 0)
  expect_type(text, "character")
})

test_that("get_text() handles dest_dir with trailing slash", {
  temp_dir <- paste0(tempdir(), "/")

  text <- get_text(sample_json,
    type = "async", save_to_file = TRUE,
    dest_dir = temp_dir, outfile_stem = "edge_case"
  )

  # Should still work despite trailing slash
  expect_type(text, "character")

  # Clean up
  unlink(file.path(temp_dir, "edge_case.txt"))
})

test_that("get_text() handles special characters in outfile_stem", {
  temp_dir <- tempdir()
  special_stem <- "test_file_2024"

  text <- get_text(sample_json,
    type = "async", save_to_file = TRUE,
    dest_dir = temp_dir, outfile_stem = special_stem
  )

  expected_file <- file.path(temp_dir, paste0(special_stem, ".txt"))
  expect_true(file.exists(expected_file))

  # Clean up
  unlink(expected_file)
})

## GET_TABLES ------------------------------------------------------------------

test_sync_invalid_inputs(get_tables, "get_tables()")
test_async_filepath_errors(get_tables, "get_tables()")
test_async_object_type_errors(get_tables, "get_tables()")
test_type_parameter(get_tables, "get_tables()")
test_invalid_json_format(get_tables, "get_tables()")

test_that("get_tables() warns of response not containing text", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  wrong <- dai_async("random.pdf")
  expect_error(get_tables(wrong), "The supplied object is not from a successful HTTP request.")
  blank <- dai_sync(testthat::test_path("examples", "blank.tiff"))
  expect_error(get_tables(blank), "DAI found no text. Was the page blank?")
})

test_that("get_tables() warns of file not containing text or proper format", {
  blank <- testthat::test_path("examples", "output_blank.json")
  expect_error(get_tables(blank, type = "async"), "DAI found no text. Was the document blank?")
})

test_that("get_tables() gets tables from example json file", {
  tbls <- get_tables(sample_json_tables, type = "async")
  expect_type(tbls, "list")
  expect_true(length(tbls) > 0)
  expect_true(is.data.frame(tbls[[1]]))
  expect_true(ncol(tbls[[1]]) > 0)
  expect_true(nrow(tbls[[1]]) >= 0)
})

test_that("get_tables() returns all tables from multi-table document", {
  tbls <- get_tables(sample_json_tables, type = "async")
  # Check that each element in the list is a data frame
  expect_true(all(sapply(tbls, is.data.frame)))
})

test_that("get_tables() handles empty table cells", {
  # This tests the helper function's ability to handle NULL or empty cells
  tbls <- get_tables(sample_json_tables, type = "async")

  # Tables should still be valid data frames even with empty cells
  expect_true(all(sapply(tbls, is.data.frame)))
})

## GET_ENTITIES ----------------------------------------------------------------

test_sync_invalid_inputs(get_entities, "get_entities()")
test_async_filepath_errors(get_entities, "get_entities()")
test_async_object_type_errors(get_entities, "get_entities()")
test_type_parameter(get_entities, "get_entities()")
test_invalid_json_format(get_entities, "get_entities()")

test_that("get_entities() warns of response not containing text", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  wrong <- dai_async("random.pdf")
  expect_error(get_entities(wrong), "The supplied object is not from a successful HTTP request.")
  blank <- dai_sync(testthat::test_path("examples", "blank.tiff"))
  expect_error(get_entities(blank), "DAI found no text. Was the page blank?")
})

test_that("get_entities() warns of file not containing text", {
  blank <- testthat::test_path("examples", "output_blank.json")
  expect_error(get_entities(blank, type = "async"), "DAI found no text. Was the document blank?")
})

test_that("get_entities() gets entities from example file", {
  ents <- get_entities(sample_json_tables, type = "async")
  expect_type(ents, "list")
  expect_true(length(ents) > 0)
  expect_true(is.data.frame(ents[[1]]))

  # Check that expected columns exist
  expected_cols <- c(
    "id", "mentionText", "type", "confidence",
    "start_ind", "end_ind", "left", "right", "top", "bottom"
  )
  expect_true(all(expected_cols %in% names(ents[[1]])))
})

test_that("get_entities() handles multiple entity pages", {
  ents <- get_entities(sample_json_tables, type = "async")
  # Each element should be a data frame
  expect_true(all(sapply(ents, is.data.frame)))
})

test_that("get_entities() entity data frames have correct structure", {
  ents <- get_entities(sample_json_tables, type = "async")

  for (ent_df in ents) {
    # Check column types
    expect_true(is.numeric(ent_df$id))
    expect_true(is.character(ent_df$mentionText))
    expect_true(is.character(ent_df$type))
    expect_true(is.numeric(ent_df$confidence))
    expect_true(is.numeric(ent_df$start_ind))
    expect_true(is.numeric(ent_df$end_ind))
    expect_true(is.numeric(ent_df$left))
    expect_true(is.numeric(ent_df$right))
    expect_true(is.numeric(ent_df$top))
    expect_true(is.numeric(ent_df$bottom))

    # Check that confidence is between 0 and 1
    expect_true(all(ent_df$confidence >= 0 & ent_df$confidence <= 1))

    # Check that coordinates make sense
    expect_true(all(ent_df$left <= ent_df$right))
    expect_true(all(ent_df$top <= ent_df$bottom))
  }
})

## INTEGRATION TESTS -----------------------------------------------------------

test_that("get_text() and get_tables() work together on same file", {
  text <- get_text(sample_json_tables, type = "async")
  tbls <- get_tables(sample_json_tables, type = "async")

  expect_type(text, "character")
  expect_type(tbls, "list")
  expect_true(length(tbls) > 0)
})

test_that("get_text() and get_entities() work together on same file", {
  text <- get_text(sample_json_tables, type = "async")
  ents <- get_entities(sample_json_tables, type = "async")

  expect_type(text, "character")
  expect_type(ents, "list")
  expect_true(length(ents) > 0)
})

test_that("All three extraction functions work on same file", {
  text <- get_text(sample_json_tables, type = "async")
  tbls <- get_tables(sample_json_tables, type = "async")
  ents <- get_entities(sample_json_tables, type = "async")

  expect_type(text, "character")
  expect_type(tbls, "list")
  expect_type(ents, "list")
})

## CLEANUP ---------------------------------------------------------------------

unlink(madeup_json_file, force = TRUE)
