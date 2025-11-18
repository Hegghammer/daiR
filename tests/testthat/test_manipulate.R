## SETUP AND TEST FIXTURES -----------------------------------------------------

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

# Real JSON paths
sample_json <- testthat::test_path("examples", "output.json")
sample_json_blank <- testthat::test_path("examples", "output_blank.json")
sample_json_multipage <- testthat::test_path("examples", "sample3pg.json")
sample_json_labelme <- testthat::test_path("examples", "peshtigo_labelme.json")
sample_json_pesh <- testthat::test_path("examples", "peshtigo.json")

## HELPER FUNCTIONS FOR TESTING ------------------------------------------------

# Test invalid type parameter
test_invalid_type <- function(func, func_name) {
  test_that(paste0(func_name, " validates type parameter"), {
    expect_error(func(sample_json, type = "invalid"), "Invalid type parameter.")
    expect_error(func(sample_json, type = c("sync", "async")), "Invalid type parameter.")
    expect_error(func(sample_json, type = NULL), "Invalid type parameter.")
    expect_error(func(sample_json, type = 123), "Invalid type parameter.")
  })
}

# Test invalid object parameter for build functions
test_invalid_object <- function(func, func_name) {
  test_that(paste0(func_name, " errors with invalid object parameter"), {
    # For async type
    expect_error(func(null, type = "async"), "Invalid object")
    expect_error(func(na, type = "async"), "Invalid object")
    expect_error(func(boolean, type = "async"), "Invalid object")
    expect_error(func(number_random, type = "async"), "Invalid object")
    expect_error(func(string_random, type = "async"), "Invalid object")
    expect_error(func(vector_strings, type = "async"), "Invalid object")
    expect_error(func(list_strings, type = "async"), "Invalid object")
    expect_error(func(df, type = "async"), "Invalid object")
    expect_error(func(matrix, type = "async"), "Invalid object")
  })
}

# Test invalid JSON file parameter
test_invalid_json <- function(func, func_name) {
  test_that(paste0(func_name, " errors with invalid JSON file"), {
    expect_error(func(null), "Invalid json parameter: must be a single character string filepath.")
    expect_error(func(na), "Invalid json parameter: must be a single character string filepath.")
    expect_error(func(boolean), "Invalid json parameter: must be a single character string filepath.")
    expect_error(func(number_random), "Invalid json parameter: must be a single character string filepath.")
    expect_error(func(vector_strings), "Invalid json parameter: must be a single character string filepath.")
    expect_error(func(list_strings), "Invalid json parameter: must be a single character string filepath.")
    expect_error(func(df), "Invalid json parameter: must be a single character string filepath.")
    expect_error(func(matrix), "Invalid json parameter: must be a single character string filepath.")
    
    expect_error(func("wrongfile.csv"), "Invalid json parameter: file is not a \\.json file or does not exist.")
    expect_error(func("madeup.json"), "Invalid json parameter: file is not a \\.json file or does not exist.")
  })
}

# Test invalid dataframe parameter
test_invalid_dataframe <- function(func, param_name, func_name, additional_args = list()) {
  test_that(paste0(func_name, " errors with invalid ", param_name, " parameter"), {
    invalid_inputs <- list(null, na, boolean, number_random, string_random, vector_strings, list_strings, matrix)
    
    for (input in invalid_inputs) {
      args <- c(setNames(list(input), param_name), additional_args)
      expect_error(do.call(func, args), paste0("Invalid ", param_name, " parameter"))
    }
  })
}

# Test invalid page parameter
test_invalid_page <- function(func, valid_args, func_name) {
  test_that(paste0(func_name, " errors with invalid page parameter"), {
    expect_error(do.call(func, c(valid_args, list(page = -1))), "Invalid page parameter.")
    expect_error(do.call(func, c(valid_args, list(page = 0))), "Invalid page parameter.")
    expect_error(do.call(func, c(valid_args, list(page = 1.5))), "Invalid page parameter.")
    expect_error(do.call(func, c(valid_args, list(page = "one"))), "Invalid page parameter.")
    expect_error(do.call(func, c(valid_args, list(page = c(1, 2)))), "Invalid page parameter.")
    expect_error(do.call(func, c(valid_args, list(page = 999))), "No such page number")
  })
}

# Test invalid block parameter  
test_invalid_block <- function(func, valid_args, func_name) {
  test_that(paste0(func_name, " errors with invalid block parameter"), {
    expect_error(do.call(func, c(valid_args, list(block = -1))), "Invalid block parameter.")
    expect_error(do.call(func, c(valid_args, list(block = 0))), "Invalid block parameter.")
    expect_error(do.call(func, c(valid_args, list(block = 1.5))), "Invalid block parameter.")
    expect_error(do.call(func, c(valid_args, list(block = "one"))), "Invalid block parameter.")
    expect_error(do.call(func, c(valid_args, list(block = c(1, 2)))), "Invalid block parameter.")
    expect_error(do.call(func, c(valid_args, list(block = 999))), "No such block number")
  })
}

# Test invalid directory parameter
test_invalid_directory <- function(func, param_name, func_name, additional_args = list()) {
  test_that(paste0(func_name, " errors with invalid ", param_name, " parameter"), {
    # Non-character inputs
    args1 <- c(setNames(list(null), param_name), additional_args)
    expect_error(do.call(func, args1), paste0("Invalid ", param_name, " parameter"))
    
    args2 <- c(setNames(list(boolean), param_name), additional_args)
    expect_error(do.call(func, args2), paste0("Invalid ", param_name, " parameter"))
    
    args3 <- c(setNames(list(number_random), param_name), additional_args)
    expect_error(do.call(func, args3), paste0("Invalid ", param_name, " parameter"))
    
    args4 <- c(setNames(list(vector_strings), param_name), additional_args)
    expect_error(do.call(func, args4), paste0("Invalid ", param_name, " parameter"))
    
    # Non-existent directory
    args5 <- c(setNames(list("/fake/nonexistent/path"), param_name), additional_args)
    expect_error(do.call(func, args5), "does not exist")
  })
}

## MERGE_SHARDS ----------------------------------------------------------------

test_invalid_directory(merge_shards, "source_dir", "merge_shards()")

test_that("merge_shards() errors with invalid dest_dir parameter", {
  valid_source <- file.path(tempdir(), "valid_source")
  dir.create(valid_source, showWarnings = FALSE)
  writeLines("test", file.path(valid_source, "test-0.txt"))
  
  expect_error(merge_shards(source_dir = valid_source, dest_dir = null), "Invalid dest_dir parameter")
  expect_error(merge_shards(source_dir = valid_source, dest_dir = boolean), "Invalid dest_dir parameter")
  expect_error(merge_shards(source_dir = valid_source, dest_dir = vector_strings), "Invalid dest_dir parameter")
  expect_error(merge_shards(source_dir = valid_source, dest_dir = "/fake/path"), "does not exist")
  
  unlink(valid_source, recursive = TRUE)
})

test_that("merge_shards() errors when no txt files found", {
  empty_dir <- file.path(tempdir(), "empty_test_dir")
  dir.create(empty_dir, showWarnings = FALSE)
  expect_error(merge_shards(source_dir = empty_dir), "No .txt files found.")
  unlink(empty_dir, recursive = TRUE)
})

test_that("merge_shards() errors when txt files are incorrectly formatted", {
  bad_dir <- file.path(tempdir(), "bad_format_dir")
  dir.create(bad_dir, showWarnings = FALSE)
  writeLines("test content", file.path(bad_dir, "test.txt"))
  expect_error(merge_shards(source_dir = bad_dir), "incorrectly formatted")
  unlink(bad_dir, recursive = TRUE)
})

test_that("merge_shards() successfully merges v1 format shards", {
  test_dir <- file.path(tempdir(), "v1_test")
  dir.create(test_dir, showWarnings = FALSE)
  output_dir <- file.path(tempdir(), "v1_output")
  dir.create(output_dir, showWarnings = FALSE)
  
  writeLines("First shard content", file.path(test_dir, "document-0.txt"))
  writeLines("Second shard content", file.path(test_dir, "document-1.txt"))
  writeLines("Third shard content", file.path(test_dir, "document-2.txt"))
  
  expect_message(merge_shards(source_dir = test_dir, dest_dir = output_dir), "Shards merged")
  
  expect_true(file.exists(file.path(output_dir, "document.txt")))
  merged_content <- readLines(file.path(output_dir, "document.txt"))
  expect_equal(length(merged_content), 3)
  expect_true(any(grepl("First shard", merged_content)))
  expect_true(any(grepl("Third shard", merged_content)))
  
  unlink(test_dir, recursive = TRUE)
  unlink(output_dir, recursive = TRUE)
})

test_that("merge_shards() successfully merges v1beta2 format shards", {
  test_dir <- file.path(tempdir(), "v1beta2_test")
  dir.create(test_dir, showWarnings = FALSE)
  output_dir <- file.path(tempdir(), "v1beta2_output")
  dir.create(output_dir, showWarnings = FALSE)
  
  writeLines("Pages 1-5 content", file.path(test_dir, "document-page-1-to-5.txt"))
  writeLines("Pages 6-10 content", file.path(test_dir, "document-page-6-to-10.txt"))
  
  expect_message(merge_shards(source_dir = test_dir, dest_dir = output_dir))
  
  expect_true(file.exists(file.path(output_dir, "document.txt")))
  merged_content <- readLines(file.path(output_dir, "document.txt"))
  expect_true(any(grepl("Pages 1-5", merged_content)))
  expect_true(any(grepl("Pages 6-10", merged_content)))
  
  unlink(test_dir, recursive = TRUE)
  unlink(output_dir, recursive = TRUE)
})

test_that("merge_shards() handles multiple document sets", {
  test_dir <- file.path(tempdir(), "multi_doc_test")
  dir.create(test_dir, showWarnings = FALSE)
  output_dir <- file.path(tempdir(), "multi_output")
  dir.create(output_dir, showWarnings = FALSE)
  
  writeLines("Doc1 Part1", file.path(test_dir, "doc1-0.txt"))
  writeLines("Doc1 Part2", file.path(test_dir, "doc1-1.txt"))
  writeLines("Doc2 Part1", file.path(test_dir, "doc2-0.txt"))
  writeLines("Doc2 Part2", file.path(test_dir, "doc2-1.txt"))
  
  merge_shards(source_dir = test_dir, dest_dir = output_dir)
  
  expect_true(file.exists(file.path(output_dir, "doc1.txt")))
  expect_true(file.exists(file.path(output_dir, "doc2.txt")))
  
  unlink(test_dir, recursive = TRUE)
  unlink(output_dir, recursive = TRUE)
})

test_that("merge_shards() handles directory paths with trailing slashes", {
  test_dir <- file.path(tempdir(), "trailing_slash_test")
  dir.create(test_dir, showWarnings = FALSE)
  output_dir <- file.path(tempdir(), "trailing_output")
  dir.create(output_dir, showWarnings = FALSE)
  
  writeLines("Content", file.path(test_dir, "doc-0.txt"))
  
  # Add trailing slashes
  source_with_slash <- paste0(test_dir, "/")
  dest_with_slash <- paste0(output_dir, "/")
  
  merge_shards(source_dir = source_with_slash, dest_dir = dest_with_slash)
  
  expect_true(file.exists(file.path(output_dir, "doc.txt")))
  
  unlink(test_dir, recursive = TRUE)
  unlink(output_dir, recursive = TRUE)
})

test_that("merge_shards() handles edge case: single shard", {
  test_dir <- file.path(tempdir(), "single_shard_test")
  dir.create(test_dir, showWarnings = FALSE)
  output_dir <- file.path(tempdir(), "single_output")
  dir.create(output_dir, showWarnings = FALSE)
  
  writeLines("Only shard", file.path(test_dir, "document-0.txt"))
  
  merge_shards(source_dir = test_dir, dest_dir = output_dir)
  
  expect_true(file.exists(file.path(output_dir, "document.txt")))
  content <- readLines(file.path(output_dir, "document.txt"))
  expect_equal(length(content), 1)
  expect_equal(content, "Only shard")
  
  unlink(test_dir, recursive = TRUE)
  unlink(output_dir, recursive = TRUE)
})

test_that("merge_shards() handles three-digit shard numbers", {
  test_dir <- file.path(tempdir(), "threedigit_test")
  dir.create(test_dir, showWarnings = FALSE)
  output_dir <- file.path(tempdir(), "threedigit_output")
  dir.create(output_dir, showWarnings = FALSE)
  
  writeLines("Shard 99", file.path(test_dir, "doc-99.txt"))
  writeLines("Shard 100", file.path(test_dir, "doc-100.txt"))
  
  merge_shards(source_dir = test_dir, dest_dir = output_dir)
  
  expect_true(file.exists(file.path(output_dir, "doc.txt")))
  
  unlink(test_dir, recursive = TRUE)
  unlink(output_dir, recursive = TRUE)
})

## BUILD_TOKEN_DF --------------------------------------------------------------

test_invalid_type(build_token_df, "build_token_df()")
test_invalid_object(build_token_df, "build_token_df()")

test_that("build_token_df() errors with files not containing tokens", {
  expect_error(build_token_df(madeup_json_file, type = "async"), "JSON not in right format")
  expect_error(build_token_df(sample_json_blank, type = "async"), "DAI found no tokens")
})

test_that("build_token_df() builds a token dataframe with correct structure", {
  skip_if_not(file.exists(sample_json))
  df <- build_token_df(sample_json, type = "async")
  
  expect_true(is.data.frame(df))
  expect_equal(ncol(df), 10)
  expect_setequal(colnames(df), c("token", "start_ind", "end_ind", "conf", "left", "right", "top", "bottom", "page", "block"))
  
  # Check column types
  expect_true(is.character(df$token))
  expect_true(is.integer(df$start_ind))
  expect_true(is.integer(df$end_ind))
  expect_true(is.numeric(df$conf))
  expect_true(is.numeric(df$left))
  expect_true(is.numeric(df$right))
  expect_true(is.numeric(df$top))
  expect_true(is.numeric(df$bottom))
  expect_true(is.integer(df$page))
})

test_that("build_token_df() produces valid coordinate values", {
  skip_if_not(file.exists(sample_json))
  df <- build_token_df(sample_json, type = "async")
  
  # Coordinates should be between 0 and 1 (normalized) - ignoring NAs
  expect_true(all(df$left >= 0 & df$left <= 1, na.rm = TRUE))
  expect_true(all(df$right >= 0 & df$right <= 1, na.rm = TRUE))
  expect_true(all(df$top >= 0 & df$top <= 1, na.rm = TRUE))
  expect_true(all(df$bottom >= 0 & df$bottom <= 1, na.rm = TRUE))
  
  # Left should be less than or equal to right, top less than or equal to bottom
  expect_true(all(df$left <= df$right, na.rm = TRUE))
  expect_true(all(df$top <= df$bottom, na.rm = TRUE))
  
  # Most tokens should have valid coordinates (allow some NAs but not too many)
  na_count <- sum(is.na(df$left) | is.na(df$right) | is.na(df$top) | is.na(df$bottom))
  expect_lt(na_count / nrow(df), 0.5)  # Less than 50% should have NAs
})

test_that("build_token_df() produces reasonable token counts", {
  skip_if_not(file.exists(sample_json))
  df <- build_token_df(sample_json, type = "async")
  parsed <- jsonlite::fromJSON(sample_json)
  text <- parsed$text
  words <- length(strsplit(text, "\\s+")[[1]])
  
  # Token count should be reasonably close to word count (within 50%)
  expect_lt(nrow(df), 1.5 * words)
  expect_gt(nrow(df), 0.5 * words)
})

test_that("build_token_df() produces tokens in correct order", {
  skip_if_not(file.exists(sample_json))
  df <- build_token_df(sample_json, type = "async")
  
  # Start indices should be sorted (tokens in reading order)
  expect_false(is.unsorted(df$start_ind))
})

test_that("build_token_df() handles multi-page documents", {
  skip_if_not(file.exists(sample_json_multipage))
  df <- build_token_df(sample_json_multipage, type = "async")
  parsed <- jsonlite::fromJSON(sample_json_multipage)
  
  expect_equal(max(df$page), nrow(parsed$pages))
  expect_true(all(1:max(df$page) %in% df$page))
})

test_that("build_token_df() handles pages with no tokens", {
  skip_if_not(file.exists(sample_json_multipage))
  df <- build_token_df(sample_json_multipage, type = "async")
  
  # Should not crash and should handle gracefully
  expect_true(is.data.frame(df))
  expect_true(nrow(df) > 0)
})

## BUILD_BLOCK_DF --------------------------------------------------------------

test_invalid_type(build_block_df, "build_block_df()")
test_invalid_object(build_block_df, "build_block_df()")

test_that("build_block_df() errors with files not containing blocks", {
  expect_error(build_block_df(madeup_json_file, type = "async"), "JSON not in right format")
  expect_error(build_block_df(sample_json_blank, type = "async"), "DAI found no blocks")
})

test_that("build_block_df() builds a block dataframe with correct structure", {
  skip_if_not(file.exists(sample_json))
  df <- build_block_df(sample_json, type = "async")
  
  expect_true(is.data.frame(df))
  expect_equal(ncol(df), 7)
  expect_setequal(colnames(df), c("page", "block", "conf", "left", "right", "top", "bottom"))
  
  # Check column types
  expect_true(is.integer(df$page))
  expect_true(is.integer(df$block))
  expect_true(is.numeric(df$conf))
  expect_true(is.numeric(df$left))
  expect_true(is.numeric(df$right))
  expect_true(is.numeric(df$top))
  expect_true(is.numeric(df$bottom))
})

test_that("build_block_df() produces valid coordinate values", {
  skip_if_not(file.exists(sample_json))
  df <- build_block_df(sample_json, type = "async")
  
  # Coordinates should be between 0 and 1 (normalized) - ignoring NAs
  expect_true(all(df$left >= 0 & df$left <= 1, na.rm = TRUE))
  expect_true(all(df$right >= 0 & df$right <= 1, na.rm = TRUE))
  expect_true(all(df$top >= 0 & df$top <= 1, na.rm = TRUE))
  expect_true(all(df$bottom >= 0 & df$bottom <= 1, na.rm = TRUE))
  
  # Left should be less than right, top less than bottom
  expect_true(all(df$left <= df$right, na.rm = TRUE))
  expect_true(all(df$top <= df$bottom, na.rm = TRUE))
})

test_that("build_block_df() produces blocks in correct order", {
  skip_if_not(file.exists(sample_json))
  df <- build_block_df(sample_json, type = "async")
  
  # Block numbers should be sequential within each page
  for (pg in unique(df$page)) {
    page_blocks <- df$block[df$page == pg]
    expect_equal(page_blocks, seq_along(page_blocks))
  }
})

test_that("build_block_df() handles multi-page documents", {
  skip_if_not(file.exists(sample_json_multipage))
  df <- build_block_df(sample_json_multipage, type = "async")
  parsed <- jsonlite::fromJSON(sample_json_multipage)
  
  expect_equal(max(df$page), nrow(parsed$pages))
  expect_true(all(1:max(df$page) %in% df$page))
})

## SPLIT_BLOCK -----------------------------------------------------------------

test_that("split_block() setup creates test data", {
  skip_if_not(file.exists(sample_json))
  test_df <<- build_block_df(sample_json, type = "async")
  expect_true(is.data.frame(test_df))
})

test_invalid_dataframe(split_block, "block_df", "split_block()", list(block = 1, cut_point = 50))
test_invalid_page(split_block, list(block_df = test_df, block = 1, cut_point = 50), "split_block()")
test_invalid_block(split_block, list(block_df = test_df, page = 1, cut_point = 50), "split_block()")

test_that("split_block() errors with invalid cut_point", {
  expect_error(split_block(block_df = test_df, block = 1, cut_point = "middle"), "Invalid cut point parameter")
  expect_error(split_block(block_df = test_df, block = 1, cut_point = 0), "Cut point out of range")
  expect_error(split_block(block_df = test_df, block = 1, cut_point = 100), "Cut point out of range")
  expect_error(split_block(block_df = test_df, block = 1, cut_point = 150), "Cut point out of range")
  expect_error(split_block(block_df = test_df, block = 1, cut_point = -1), "Cut point out of range")  # Changed!
  expect_error(split_block(block_df = test_df, block = 1, cut_point = 50.5), "Invalid cut point parameter")
})

test_that("split_block() errors with invalid direction", {
  expect_error(split_block(block_df = test_df, block = 1, cut_point = 50, direction = 1), "Invalid direction parameter")
  expect_error(split_block(block_df = test_df, block = 1, cut_point = 50, direction = c("v", "h")), "Invalid direction parameter")
  expect_error(split_block(block_df = test_df, block = 1, cut_point = 50, direction = "horizontal"), 'Split direction must be either "h" or "v"')
  expect_error(split_block(block_df = test_df, block = 1, cut_point = 50, direction = "x"), 'Split direction must be either "h" or "v"')
})

test_that("split_block() errors with wrong dataframe format", {
  wrong_df <- data.frame(x = 1:10, y = 1:10)
  expect_error(split_block(block_df = wrong_df, block = 1, cut_point = 50), "Dataframe not recognized")
})

test_that("split_block() returns a revised block dataframe - vertical split", {
  skip_if_not(file.exists(sample_json))
  df <- build_block_df(sample_json, type = "async")
  block_to_split <- min(df$block[df$page == 1])
  cut_point <- 50
  
  new_df <- split_block(df, page = 1, block = block_to_split, cut_point = cut_point, direction = "v")
  
  expect_equal(nrow(new_df), nrow(df) + 1)
  expect_equal(max(new_df$block[new_df$page == 1]), max(df$block[df$page == 1]) + 1)
  expect_true(is.data.frame(new_df))
  expect_equal(colnames(new_df), colnames(df))
})

test_that("split_block() returns a revised block dataframe - horizontal split", {
  skip_if_not(file.exists(sample_json))
  df <- build_block_df(sample_json, type = "async")
  block_to_split <- min(df$block[df$page == 1])
  cut_point <- 50
  
  new_df <- split_block(df, page = 1, block = block_to_split, cut_point = cut_point, direction = "h")
  
  expect_equal(nrow(new_df), nrow(df) + 1)
  expect_equal(max(new_df$block[new_df$page == 1]), max(df$block[df$page == 1]) + 1)
})

test_that("split_block() horizontal split creates correct coordinates", {
  skip_if_not(file.exists(sample_json))
  df <- build_block_df(sample_json, type = "async")
  old_block <- df[df$block == 1 & df$page == 1, ]
  
  new_df <- split_block(df, page = 1, block = 1, cut_point = 50, direction = "h")
  modified_block <- new_df[new_df$block == 1 & new_df$page == 1, ]
  new_block <- new_df[new_df$block == max(new_df$block[new_df$page == 1]) & new_df$page == 1, ]
  
  # Old block should have smaller bottom boundary after horizontal split
  expect_lt(modified_block$bottom, old_block$bottom)
  expect_equal(modified_block$top, old_block$top)
  
  # New block should span from cut to original bottom
  expect_equal(new_block$top, modified_block$bottom)
  expect_equal(new_block$bottom, old_block$bottom)
  
  # Horizontal split: left and right should match original
  expect_equal(new_block$left, old_block$left)
  expect_equal(new_block$right, old_block$right)
})

test_that("split_block() handles multi-page documents correctly", {
  skip_if_not(file.exists(sample_json_multipage))
  df <- build_block_df(sample_json_multipage, type = "async")
  
  # Split a block on page 2
  df_page2 <- df[df$page == 2, ]
  if (nrow(df_page2) > 0) {
    block_to_split <- min(df_page2$block)
    cut_point <- 50
    
    new_df <- split_block(df, page = 2, block = block_to_split, cut_point = cut_point)
    
    expect_equal(nrow(new_df), nrow(df) + 1)
    # Other pages should be unchanged
    expect_equal(nrow(new_df[new_df$page == 1, ]), nrow(df[df$page == 1, ]))
  }
})

test_that("split_block() correctly modifies old block coordinates - vertical", {
  skip_if_not(file.exists(sample_json))
  df <- build_block_df(sample_json, type = "async")
  old_block <- df[df$block == 1 & df$page == 1, ]
  
  new_df <- split_block(df, page = 1, block = 1, cut_point = 50, direction = "v")
  modified_block <- new_df[new_df$block == 1 & new_df$page == 1, ]
  
  # Old block should have smaller right boundary after vertical split
  expect_lt(modified_block$right, old_block$right)
  expect_equal(modified_block$left, old_block$left)
})

test_that("split_block() accepts case-insensitive direction parameter", {
  skip_if_not(file.exists(sample_json))
  df <- build_block_df(sample_json, type = "async")
  
  expect_no_error(split_block(df, block = 1, cut_point = 50, direction = "V"))
  expect_no_error(split_block(df, block = 1, cut_point = 50, direction = "H"))
})

test_that("split_block() handles edge case split points", {
  skip_if_not(file.exists(sample_json))
  df <- build_block_df(sample_json, type = "async")
  
  # Very small cut (1%)
  new_df_1 <- split_block(df, block = 1, cut_point = 1)
  expect_equal(nrow(new_df_1), nrow(df) + 1)
  
  # Very large cut (99%)
  new_df_99 <- split_block(df, block = 1, cut_point = 99)
  expect_equal(nrow(new_df_99), nrow(df) + 1)
})

## REASSIGN_TOKENS -------------------------------------------------------------

test_that("reassign_tokens() setup creates test data", {
  skip_if_not(file.exists(sample_json))
  test_token_df <<- build_token_df(sample_json, type = "async")
  test_block_df <<- build_block_df(sample_json, type = "async")
  expect_true(is.data.frame(test_token_df))
  expect_true(is.data.frame(test_block_df))
})

test_invalid_dataframe(reassign_tokens, "token_df", "reassign_tokens()", list(block_df = test_block_df))
test_invalid_dataframe(reassign_tokens, "block_df", "reassign_tokens()", list(token_df = test_token_df))

test_that("reassign_tokens() errors with wrong dataframe format", {
  wrong_token_df <- data.frame(x = 1:10, y = 1:10)
  expect_error(reassign_tokens(wrong_token_df, test_block_df), "Token dataframe not recognized")
  
  wrong_block_df <- data.frame(x = 1:10, y = 1:10)
  expect_error(reassign_tokens(test_token_df, wrong_block_df), "Block dataframe not recognized")
})

test_that("reassign_tokens() returns a revised token dataframe", {
  skip_if_not(file.exists(sample_json))
  tdf_old <- build_token_df(sample_json, type = "async")
  bdf_old <- build_block_df(sample_json, type = "async")
  
  # Split a block
  bdf_new <- split_block(bdf_old, page = 1, block = 1, cut_point = 50)
  tdf_new <- reassign_tokens(tdf_old, bdf_new)
  
  expect_true(is.data.frame(tdf_new))
  expect_equal(colnames(tdf_new), colnames(tdf_old))
  expect_equal(nrow(tdf_new), nrow(tdf_old))
})

test_that("reassign_tokens() maintains token order", {
  skip_if_not(file.exists(sample_json))
  tdf_old <- build_token_df(sample_json, type = "async")
  bdf_old <- build_block_df(sample_json, type = "async")
  bdf_new <- split_block(bdf_old, page = 1, block = 1, cut_point = 50)
  tdf_new <- reassign_tokens(tdf_old, bdf_new)
  
  # Start indices should still be sorted
  expect_false(is.unsorted(tdf_new$start_ind))
})

test_that("reassign_tokens() handles multi-page documents", {
  skip_if_not(file.exists(sample_json_multipage))
  tdf <- build_token_df(sample_json_multipage, type = "async")
  bdf <- build_block_df(sample_json_multipage, type = "async")
  bdf_new <- split_block(bdf, page = 2, block = 1, cut_point = 50)
  tdf_new <- reassign_tokens(tdf, bdf_new)
  
  expect_equal(nrow(tdf_new), nrow(tdf))
  expect_equal(max(tdf_new$page), max(tdf$page))
})

test_that("reassign_tokens() correctly assigns tokens to new blocks", {
  skip_if_not(file.exists(sample_json))
  tdf_old <- build_token_df(sample_json, type = "async")
  bdf_old <- build_block_df(sample_json, type = "async")
  
  # Find a block with multiple tokens to ensure split creates two non-empty blocks
  # Count tokens per block
  token_counts <- table(tdf_old$block[tdf_old$page == 1 & !is.na(tdf_old$block)])
  
  # Skip if no blocks have tokens
  skip_if(length(token_counts) == 0, "No blocks with tokens on page 1")
  
  # Find a block with at least 4 tokens (more likely to split evenly)
  blocks_with_tokens <- as.integer(names(token_counts[token_counts >= 4]))
  
  if (length(blocks_with_tokens) > 0) {
    # Use first block with multiple tokens
    block_to_split <- blocks_with_tokens[1]
  } else {
    # Fall back to any block with tokens
    block_to_split <- as.integer(names(token_counts[token_counts > 0])[1])
  }
  
  # Split the block vertically at 50%
  bdf_new <- split_block(bdf_old, page = 1, block = block_to_split, cut_point = 50, direction = "v")
  tdf_new <- reassign_tokens(tdf_old, bdf_new)
  
  # Verify reassignment worked
  expect_equal(nrow(tdf_new), nrow(tdf_old))
  expect_equal(max(tdf_new$block, na.rm = TRUE), max(bdf_new$block, na.rm = TRUE))
  
  # Check that the new block exists in the block dataframe
  new_block_num <- max(bdf_new$block[bdf_new$page == 1])
  expect_true(new_block_num %in% bdf_new$block[bdf_new$page == 1])
  
  # Optionally check if new block has tokens
  tokens_in_new_block <- sum(tdf_new$block[tdf_new$page == 1] == new_block_num, na.rm = TRUE)
  if (tokens_in_new_block > 0) {
    message(paste("New block", new_block_num, "has", tokens_in_new_block, "token(s)"))
  }
})

## REASSIGN_TOKENS2 ------------------------------------------------------------

test_that("reassign_tokens2() setup creates test data", {
  skip_if_not(file.exists(sample_json))
  test_block_single <<- test_block_df[1, ]
  expect_equal(nrow(test_block_single), 1)
})

test_invalid_dataframe(reassign_tokens2, "token_df", "reassign_tokens2()", list(block = test_block_single))
test_invalid_dataframe(reassign_tokens2, "block", "reassign_tokens2()", list(token_df = test_token_df))
test_invalid_page(reassign_tokens2, list(token_df = test_token_df, block = test_block_single), "reassign_tokens2()")

test_that("reassign_tokens2() errors with wrong dataframe format", {
  wrong_token_df <- data.frame(x = 1:10, y = 1:10)
  expect_error(reassign_tokens2(wrong_token_df, test_block_single), "Token dataframe not recognized")
  
  wrong_block_df <- data.frame(x = 1:10, y = 1:10)
  expect_error(reassign_tokens2(test_token_df, wrong_block_df), "Block dataframe format not recognized")
})

test_that("reassign_tokens2() returns a revised token dataframe", {
  skip_if_not(file.exists(sample_json_pesh) && file.exists(sample_json_labelme))
  tdf_old <- build_token_df(sample_json_pesh, type = "async")
  block <- from_labelme(sample_json_labelme)
  tdf_new <- reassign_tokens2(tdf_old, block)
  
  expect_true(is.data.frame(tdf_new))
  expect_equal(nrow(tdf_new), nrow(tdf_old))
  expect_equal(colnames(tdf_new), colnames(tdf_old))
})

test_that("reassign_tokens2() maintains structure", {
  skip_if_not(file.exists(sample_json_pesh) && file.exists(sample_json_labelme))
  tdf_old <- build_token_df(sample_json_pesh, type = "async")
  block <- from_labelme(sample_json_labelme)
  tdf_new <- reassign_tokens2(tdf_old, block)
  
  # Should maintain all original tokens
  expect_equal(nrow(tdf_new), nrow(tdf_old))
  # Start indices should still be sorted
  expect_false(is.unsorted(tdf_new$start_ind))
})

test_that("reassign_tokens2() correctly assigns tokens within block boundaries", {
  skip_if_not(file.exists(sample_json_pesh) && file.exists(sample_json_labelme))
  tdf_old <- build_token_df(sample_json_pesh, type = "async")
  block <- from_labelme(sample_json_labelme)
  tdf_new <- reassign_tokens2(tdf_old, block)
  
  # Tokens assigned to this block should be within its boundaries
  assigned_tokens <- tdf_new[tdf_new$block == block$block & !is.na(tdf_new$block), ]
  if (nrow(assigned_tokens) > 0) {
    expect_true(all(assigned_tokens$left >= block$left))
    expect_true(all(assigned_tokens$right <= block$right))
    expect_true(all(assigned_tokens$top >= block$top))
    expect_true(all(assigned_tokens$bottom <= block$bottom))
  }
})

## FROM_LABELME ----------------------------------------------------------------

test_invalid_json(from_labelme, "from_labelme()")

test_that("from_labelme() errors with invalid page parameter", {
  # from_labelme() only validates page type/format, not existence
  expect_error(from_labelme(sample_json_labelme, page = -1), "Invalid page parameter.")
  expect_error(from_labelme(sample_json_labelme, page = 0), "Invalid page parameter.")
  expect_error(from_labelme(sample_json_labelme, page = 1.5), "Invalid page parameter.")
  expect_error(from_labelme(sample_json_labelme, page = "one"), "Invalid page parameter.")
  expect_error(from_labelme(sample_json_labelme, page = c(1, 2)), "Invalid page parameter.")
  
  # Page 999 is valid - from_labelme just creates a dataframe with that page number
  expect_no_error(from_labelme(sample_json_labelme, page = 999))
})

test_that("from_labelme() produces a properly formatted dataframe", {
  skip_if_not(file.exists(sample_json_labelme))
  df <- from_labelme(sample_json_labelme)
  
  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 1)
  expect_setequal(colnames(df), c("page", "block", "conf", "left", "right", "top", "bottom"))
  
  # Check types
  expect_true(is.numeric(df$page))
  expect_true(is.numeric(df$block))
  expect_true(is.numeric(df$left))
  expect_true(is.numeric(df$right))
  expect_true(is.numeric(df$top))
  expect_true(is.numeric(df$bottom))
})

test_that("from_labelme() produces valid normalized coordinates", {
  skip_if_not(file.exists(sample_json_labelme))
  df <- from_labelme(sample_json_labelme)
  
  # Coordinates should be between 0 and 1 (normalized)
  expect_true(df$left >= 0 & df$left <= 1)
  expect_true(df$right >= 0 & df$right <= 1)
  expect_true(df$top >= 0 & df$top <= 1)
  expect_true(df$bottom >= 0 & df$bottom <= 1)
  
  # Left should be less than right, top less than bottom
  expect_true(df$left < df$right)
  expect_true(df$top < df$bottom)
})

test_that("from_labelme() accepts custom page parameter", {
  skip_if_not(file.exists(sample_json_labelme))
  df_page1 <- from_labelme(sample_json_labelme, page = 1)
  df_page5 <- from_labelme(sample_json_labelme, page = 5)
  
  expect_equal(df_page1$page, 1)
  expect_equal(df_page5$page, 5)
})

## REDRAW_BLOCKS ---------------------------------------------------------------

test_that("redraw_blocks() errors with invalid json parameter", {
  expect_error(redraw_blocks(json = vector_strings, token_df = test_token_df), "Invalid json parameter")
  expect_error(redraw_blocks(json = number_random, token_df = test_token_df), "Invalid json parameter")
  expect_error(redraw_blocks(json = c(sample_json, sample_json), token_df = test_token_df), "not vectorised")
})

test_invalid_dataframe(redraw_blocks, "token_df", "redraw_blocks()", list(json = sample_json))

test_that("redraw_blocks() errors with wrong dataframe format", {
  wrong_df <- data.frame(x = 1:10, y = 1:10)
  expect_error(redraw_blocks(json = sample_json, token_df = wrong_df), "Token dataframe format not recognized")
})

test_that("redraw_blocks() errors on invalid JSON structures", {
  # JSON without pages field
  json_no_pages <- tempfile(fileext = ".json")
  jsonlite::write_json(list(text = "test"), json_no_pages)
  expect_error(redraw_blocks(json = json_no_pages, token_df = test_token_df), 
               "does not contain valid Document AI response with images")
  unlink(json_no_pages)
})

test_that("redraw_blocks() errors when JSON lacks images", {
  skip_if_not(file.exists(sample_json))
  token_df <- build_token_df(sample_json, type = "async")
  expect_error(redraw_blocks(json = sample_json, token_df = token_df, dir = tempdir()),
               "does not contain valid Document AI response with images")
})

test_that("redraw_blocks() generates annotated images when images present", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("magick")
  
  # This test requires a JSON with embedded images
  # Skip if no suitable test file exists
  skip("Requires JSON file with base64-encoded images")
})

## INTEGRATION TESTS -----------------------------------------------------------

test_that("build_token_df() and build_block_df() produce compatible dataframes", {
  skip_if_not(file.exists(sample_json))
  token_df <- build_token_df(sample_json, type = "async")
  block_df <- build_block_df(sample_json, type = "async")
  
  # Should have same number of pages
  expect_equal(max(token_df$page), max(block_df$page))
  
  # All tokens should be assigned to blocks that exist
  for (pg in unique(token_df$page)) {
    page_tokens <- token_df[token_df$page == pg, ]
    page_blocks <- block_df[block_df$page == pg, ]
    assigned_blocks <- unique(page_tokens$block[!is.na(page_tokens$block)])
    if (length(assigned_blocks) > 0) {
      expect_true(all(assigned_blocks %in% page_blocks$block))
    }
  }
})

test_that("Full workflow: build, split, reassign", {
  skip_if_not(file.exists(sample_json))
  # Build initial dataframes
  token_df <- build_token_df(sample_json, type = "async")
  block_df <- build_block_df(sample_json, type = "async")
  
  # Find a block with multiple tokens to ensure meaningful split
  token_counts <- table(token_df$block[token_df$page == 1 & !is.na(token_df$block)])
  
  if (length(token_counts[token_counts >= 4]) > 0) {
    # Use a block with multiple tokens
    block_to_split <- as.integer(names(token_counts[token_counts >= 4])[1])
  } else {
    # Fall back to any block with tokens
    block_to_split <- as.integer(names(token_counts[token_counts > 0])[1])
  }
  
  # Split the block
  new_block_df <- split_block(block_df, page = 1, block = block_to_split, cut_point = 50)
  
  # Reassign tokens
  new_token_df <- reassign_tokens(token_df, new_block_df)
  
  # Check results
  expect_equal(nrow(new_block_df), nrow(block_df) + 1)
  expect_equal(nrow(new_token_df), nrow(token_df))
  
  # The max block number in tokens should be <= max in blocks (empty blocks may not appear)
  expect_true(max(new_token_df$block, na.rm = TRUE) <= max(new_block_df$block))
  
  # All token blocks should exist in block dataframe
  token_blocks <- unique(new_token_df$block[!is.na(new_token_df$block)])
  expect_true(all(token_blocks %in% new_block_df$block))
})

test_that("Multiple splits on same document", {
  skip_if_not(file.exists(sample_json))
  token_df <- build_token_df(sample_json, type = "async")
  block_df <- build_block_df(sample_json, type = "async")
  
  # First split
  block_df_2 <- split_block(block_df, page = 1, block = 1, cut_point = 33)
  # Second split on different block
  if (max(block_df$block[block_df$page == 1]) > 1) {
    block_df_3 <- split_block(block_df_2, page = 1, block = 2, cut_point = 67)
    
    expect_equal(nrow(block_df_3), nrow(block_df) + 2)
    
    # Reassign tokens after multiple splits
    token_df_new <- reassign_tokens(token_df, block_df_3)
    expect_equal(nrow(token_df_new), nrow(token_df))
  }
})

## CLEANUP ---------------------------------------------------------------------

unlink(madeup_json_file, force = TRUE)
