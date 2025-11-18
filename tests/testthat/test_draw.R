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

# Invalid JSON file (not a valid Document AI response)
fill <- list("a" = 1, "b" = 2)
json <- jsonlite::toJSON(fill)
madeup_json_file <- tempfile(fileext = ".json")
write(json, madeup_json_file)

# Real JSON paths (to be used in tests)
realjson <- testthat::test_path("examples", "sample_v1.json")
realjson_entities <- testthat::test_path("examples", "table_form_parsed.json")

## HELPER FUNCTIONS FOR TESTING --------------------------------------------

# Function to test all invalid inputs for a draw function
test_invalid_inputs <- function(draw_function, function_name, test_json = realjson) {
  
  # Type parameter tests
  test_that(paste0(function_name, " errors with invalid type parameter"), {
    expect_error(draw_function(type = null), "Invalid type parameter.")
    expect_error(draw_function(type = na), "Invalid type parameter.")
    expect_error(draw_function(type = boolean), "Invalid type parameter.")
    expect_error(draw_function(type = number_random), "Invalid type parameter.")
    expect_error(draw_function(type = string_random), "Invalid type parameter.")
    expect_error(draw_function(type = df), "Invalid type parameter.")
    expect_error(draw_function(type = matrix), "Invalid type parameter.")
    expect_error(draw_function(type = vector_strings), "Invalid type parameter.")
    expect_error(draw_function(type = list_strings), "Invalid type parameter.")
  })
  
  # Object parameter tests (sync type)
  test_that(paste0(function_name, " errors with invalid object for sync type"), {
    expect_error(draw_function(type = "sync", object = null), "Object parameter not pointing to valid response object.")
    expect_error(draw_function(type = "sync", object = na), "Object parameter not pointing to valid response object.")
    expect_error(draw_function(type = "sync", object = boolean), "Object parameter not pointing to valid response object.")
    expect_error(draw_function(type = "sync", object = number_random), "Object parameter not pointing to valid response object.")
    expect_error(draw_function(type = "sync", object = df), "Object parameter not pointing to valid response object.")
    expect_error(draw_function(type = "sync", object = matrix), "Object parameter not pointing to valid response object.")
    expect_error(draw_function(type = "sync", object = vector_strings), "Object parameter not pointing to valid response object.")
    expect_error(draw_function(type = "sync", object = list_strings), "Object parameter not pointing to valid response object.")
  })
  
  # Object parameter tests (async type - invalid JSON)
  test_that(paste0(function_name, " errors with invalid JSON for async type"), {
    expect_error(draw_function(type = "async", object = null), "Object parameter not pointing to valid JSON file.")
    expect_error(draw_function(type = "async", object = na), "Object parameter not pointing to valid JSON file.")
    expect_error(draw_function(type = "async", object = boolean), "Object parameter not pointing to valid JSON file.")
    expect_error(draw_function(type = "async", object = number_random), "Object parameter not pointing to valid JSON file.")
    expect_error(draw_function(type = "async", object = string_random), "Object parameter not pointing to valid JSON file.")
  })
  
  # Prefix parameter tests
  test_that(paste0(function_name, " errors with invalid prefix parameter"), {
    expect_error(draw_function(type = "async", object = test_json, prefix = number_random), "Invalid prefix parameter.")
    expect_error(draw_function(type = "async", object = test_json, prefix = df), "Invalid prefix parameter.")
    expect_error(draw_function(type = "async", object = test_json, prefix = matrix), "Invalid prefix parameter.")
    expect_error(draw_function(type = "async", object = test_json, prefix = vector_strings), "Invalid prefix parameter.")
    expect_error(draw_function(type = "async", object = test_json, prefix = list_strings), "Invalid prefix parameter.")
  })
  
  # Dir parameter tests
  test_that(paste0(function_name, " errors with invalid dir parameter"), {
    expect_error(
      draw_function(type = "async", object = test_json, prefix = "test", dir = null),
      "Invalid dir parameter. Must be a valid folder path."
    )
    expect_error(
      draw_function(type = "async", object = test_json, prefix = "test", dir = na),
      "Invalid dir parameter. Must be a valid folder path."
    )
    expect_error(
      draw_function(type = "async", object = test_json, prefix = "test", dir = boolean),
      "Invalid dir parameter. Must be a valid folder path."
    )
    expect_error(
      draw_function(type = "async", object = test_json, prefix = "test", dir = number_random),
      "Invalid dir parameter. Must be a valid folder path."
    )
    expect_error(
      draw_function(type = "async", object = test_json, prefix = "test", dir = df),
      "Invalid dir parameter. Must be a valid folder path."
    )
    expect_error(
      draw_function(type = "async", object = test_json, prefix = "test", dir = matrix),
      "Invalid dir parameter. Must be a valid folder path."
    )
    expect_error(
      draw_function(type = "async", object = test_json, prefix = "test", dir = vector_strings),
      "Invalid dir parameter. Must be a valid folder path."
    )
    expect_error(
      draw_function(type = "async", object = test_json, prefix = "test", dir = list_strings),
      "Invalid dir parameter. Must be a valid folder path."
    )
  })
  
  # Linecol parameter tests
  test_that(paste0(function_name, " errors with invalid linecol parameter"), {
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", linecol = null),
      "Invalid linecol parameter. Must be a single valid colour representation."
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", linecol = na),
      "Invalid linecol parameter. Must be a single valid colour representation."
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", linecol = boolean),
      "Invalid linecol parameter. Must be a single valid colour representation."
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", linecol = string_random),
      "Invalid linecol parameter. Must be a single valid colour representation."
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", linecol = vector_strings),
      "Invalid linecol parameter. Must be a single valid colour representation."
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", linecol = list_strings),
      "Invalid linecol parameter. Must be a single valid colour representation."
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", linecol = df),
      "Invalid linecol parameter. Must be a single valid colour representation."
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", linecol = matrix),
      "Invalid linecol parameter. Must be a single valid colour representation."
    )
  })
  
  # Linewd parameter tests
  test_that(paste0(function_name, " errors with invalid linewd parameter"), {
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", linewd = null),
      "Invalid linewd parameter"
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", linewd = na),
      "Invalid linewd parameter"
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", linewd = boolean),
      "Invalid linewd parameter"
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", linewd = string_random),
      "Invalid linewd parameter"
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", linewd = vector_strings),
      "Invalid linewd parameter"
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", linewd = list_strings),
      "Invalid linewd parameter"
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", linewd = df),
      "Invalid linewd parameter"
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", linewd = matrix),
      "Invalid linewd parameter"
    )
  })
  
  # Fontcol parameter tests
  test_that(paste0(function_name, " errors with invalid fontcol parameter"), {
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", fontcol = null),
      "Invalid fontcol parameter. Must be a single valid colour representation."
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", fontcol = na),
      "Invalid fontcol parameter. Must be a single valid colour representation."
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", fontcol = boolean),
      "Invalid fontcol parameter. Must be a single valid colour representation."
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", fontcol = string_random),
      "Invalid fontcol parameter. Must be a single valid colour representation."
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", fontcol = vector_strings),
      "Invalid fontcol parameter. Must be a single valid colour representation."
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", fontcol = list_strings),
      "Invalid fontcol parameter. Must be a single valid colour representation."
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", fontcol = df),
      "Invalid fontcol parameter. Must be a single valid colour representation."
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", fontcol = matrix),
      "Invalid fontcol parameter. Must be a single valid colour representation."
    )
  })
  
  # Fontsize parameter tests
  test_that(paste0(function_name, " errors with invalid fontsize parameter"), {
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", fontsize = null),
      "Invalid fontsize parameter"
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", fontsize = na),
      "Invalid fontsize parameter"
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", fontsize = boolean),
      "Invalid fontsize parameter"
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", fontsize = string_random),
      "Invalid fontsize parameter"
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", fontsize = vector_strings),
      "Invalid fontsize parameter"
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", fontsize = list_strings),
      "Invalid fontsize parameter"
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", fontsize = df),
      "Invalid fontsize parameter"
    )
    expect_error(
      draw_function(type = "async", object = test_json, dir = ".", fontsize = matrix),
      "Invalid fontsize parameter"
    )
  })
}

# Function to test file generation
test_file_generation <- function(draw_function, function_name, boxtype, test_json = realjson) {
  test_that(paste0(function_name, " produces correctly named PNG files"), {
    skip_on_cran()
    skip_on_ci()
    skip_if_not_installed("grDevices")
    skip_if_not_installed("magick")
    
    # Create a temporary directory for this test
    test_dir <- file.path(tempdir(), paste0("test_", boxtype))
    dir.create(test_dir, showWarnings = FALSE)
    
    # Draw with default prefix
    suppressMessages(draw_function(type = "async", object = test_json, dir = test_dir))
    
    parsed <- jsonlite::fromJSON(test_json)
    pages <- parsed[["pages"]][["pageNumber"]]
    expected_filepaths <- character()
    stem <- substr(basename(test_json), 1, nchar(basename(test_json)) - 5)
    
    for (i in pages) {
      fname <- glue::glue("{stem}_page{i}_{boxtype}.png")
      fpath <- file.path(test_dir, fname)
      expected_filepaths <- c(expected_filepaths, fpath)
    }
    
    for (j in 1:length(expected_filepaths)) {
      expect_true(file.exists(expected_filepaths[j]))
    }
    
    # Clean up
    unlink(test_dir, recursive = TRUE, force = TRUE)
  })
}

## DRAW_BLOCKS -----------------------------------------------------------------

test_invalid_inputs(draw_blocks, "draw_blocks()")

test_that("draw_blocks() accepts valid color formats", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  test_dir <- tempdir()
  
  # Hex color
  expect_message(
    draw_blocks(type = "async", object = realjson, dir = test_dir, linecol = "#FF0000"),
    "Generated.*image\\(s\\) with block bounding boxes"
  )
  
  # Named color
  expect_message(
    draw_blocks(type = "async", object = realjson, dir = test_dir, linecol = "blue"),
    "Generated.*image\\(s\\) with block bounding boxes"
  )
})

test_that("draw_blocks() accepts different numeric parameters", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  test_dir <- tempdir()
  
  # Different line width
  expect_message(
    draw_blocks(type = "async", object = realjson, dir = test_dir, linewd = 1),
    "Generated.*image\\(s\\) with block bounding boxes"
  )
  
  # Different font size
  expect_message(
    draw_blocks(type = "async", object = realjson, dir = test_dir, fontsize = 2),
    "Generated.*image\\(s\\) with block bounding boxes"
  )
  
  # Decimal values
  expect_message(
    draw_blocks(type = "async", object = realjson, dir = test_dir, linewd = 2.5, fontsize = 3.5),
    "Generated.*image\\(s\\) with block bounding boxes"
  )
  
  # Very small positive values (boundary test - should work)
  expect_message(
    draw_blocks(type = "async", object = realjson, dir = test_dir, linewd = 0.1, fontsize = 0.1),
    "Generated.*image\\(s\\) with block bounding boxes"
  )
})

test_that("draw_blocks() works with custom prefix", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  test_dir <- tempdir()
  custom_prefix <- "mytest"
  
  suppressMessages(draw_blocks(type = "async", object = realjson, dir = test_dir, prefix = custom_prefix))
  
  parsed <- jsonlite::fromJSON(realjson)
  pages <- parsed[["pages"]][["pageNumber"]]
  
  for (i in pages) {
    fname <- glue::glue("{custom_prefix}_page{i}_blocks.png")
    fpath <- file.path(test_dir, fname)
    expect_true(file.exists(fpath))
    unlink(fpath, force = TRUE)
  }
})

test_that("draw_blocks() produces success message", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  expect_message(
    draw_blocks(type = "async", object = realjson, dir = tempdir()),
    "Generated.*image\\(s\\) with block bounding boxes"
  )
})

test_that("draw_blocks() completes without error", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  expect_no_error(
    suppressMessages(draw_blocks(type = "async", object = realjson, dir = tempdir()))
  )
})

test_file_generation(draw_blocks, "draw_blocks()", "blocks")

## DRAW_PARAGRAPHS -------------------------------------------------------------

test_invalid_inputs(draw_paragraphs, "draw_paragraphs()")

test_that("draw_paragraphs() works with custom parameters", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  test_dir <- tempdir()
  
  expect_message(
    draw_paragraphs(
      type = "async", 
      object = realjson, 
      dir = test_dir,
      linecol = "green",
      linewd = 2,
      fontcol = "purple",
      fontsize = 3
    ),
    "Generated.*image\\(s\\) with paragraph bounding boxes"
  )
})

test_that("draw_paragraphs() produces success message", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  expect_message(
    draw_paragraphs(type = "async", object = realjson, dir = tempdir()),
    "Generated.*image\\(s\\) with paragraph bounding boxes"
  )
})

test_that("draw_paragraphs() completes without error", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  expect_no_error(
    suppressMessages(draw_paragraphs(type = "async", object = realjson, dir = tempdir()))
  )
})

test_file_generation(draw_paragraphs, "draw_paragraphs()", "paragraphs")

## DRAW_LINES ------------------------------------------------------------------

test_invalid_inputs(draw_lines, "draw_lines()")

test_that("draw_lines() works with NULL prefix (default behavior)", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  test_dir <- tempdir()
  
  suppressMessages(draw_lines(type = "async", object = realjson, dir = test_dir, prefix = NULL))
  
  parsed <- jsonlite::fromJSON(realjson)
  pages <- parsed[["pages"]][["pageNumber"]]
  stem <- substr(basename(realjson), 1, nchar(basename(realjson)) - 5)
  
  for (i in pages) {
    fname <- glue::glue("{stem}_page{i}_lines.png")
    fpath <- file.path(test_dir, fname)
    expect_true(file.exists(fpath))
  }
})

test_that("draw_lines() produces success message", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  expect_message(
    draw_lines(type = "async", object = realjson, dir = tempdir()),
    "Generated.*image\\(s\\) with line bounding boxes"
  )
})

test_that("draw_lines() completes without error", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  expect_no_error(
    suppressMessages(draw_lines(type = "async", object = realjson, dir = tempdir()))
  )
})

test_file_generation(draw_lines, "draw_lines()", "lines")

## DRAW_TOKENS -----------------------------------------------------------------

test_invalid_inputs(draw_tokens, "draw_tokens()")

test_that("draw_tokens() works with different directory specifications", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  # Test with explicit tempdir path
  test_dir <- tempdir()
  
  suppressMessages(draw_tokens(type = "async", object = realjson, dir = test_dir))
  
  parsed <- jsonlite::fromJSON(realjson)
  pages <- parsed[["pages"]][["pageNumber"]]
  stem <- substr(basename(realjson), 1, nchar(basename(realjson)) - 5)
  
  for (i in pages) {
    fname <- glue::glue("{stem}_page{i}_tokens.png")
    expect_true(file.exists(file.path(test_dir, fname)))
  }
  
  # Test with getwd() as directory
  current_dir <- getwd()
  suppressMessages(draw_tokens(type = "async", object = realjson, dir = current_dir))
  
  for (i in pages) {
    fname <- glue::glue("{stem}_page{i}_tokens.png")
    expect_true(file.exists(file.path(current_dir, fname)))
    # Clean up
    unlink(file.path(current_dir, fname), force = TRUE)
  }
})

test_that("draw_tokens() produces success message", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  expect_message(
    draw_tokens(type = "async", object = realjson, dir = tempdir()),
    "Generated.*image\\(s\\) with token bounding boxes"
  )
})

test_that("draw_tokens() completes without error", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  expect_no_error(
    suppressMessages(draw_tokens(type = "async", object = realjson, dir = tempdir()))
  )
})

test_file_generation(draw_tokens, "draw_tokens()", "tokens")

## DRAW_ENTITIES ---------------------------------------------------------------

test_invalid_inputs(draw_entities, "draw_entities()", realjson_entities)

test_that("draw_entities() works with form parsed JSON", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  test_dir <- tempdir()
  
  expect_message(
    draw_entities(type = "async", object = realjson_entities, dir = test_dir),
    "Generated.*image\\(s\\) with entity bounding boxes"
  )
})

test_that("draw_entities() produces success message", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  expect_message(
    draw_entities(type = "async", object = realjson_entities, dir = tempdir()),
    "Generated.*image\\(s\\) with entity bounding boxes"
  )
})

test_that("draw_entities() completes without error", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  expect_no_error(
    suppressMessages(draw_entities(type = "async", object = realjson_entities, dir = tempdir()))
  )
})

test_file_generation(draw_entities, "draw_entities()", "entities", realjson_entities)

## MULTI-PAGE DOCUMENT TESTS ---------------------------------------------------

test_that("draw functions handle multi-page documents correctly", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  # Check if sample3pg.json exists
  multipage_json <- testthat::test_path("examples", "sample3pg.json")
  skip_if_not(file.exists(multipage_json), "Multi-page test file not available")
  
  # Check if the file has base64 image data
  parsed <- jsonlite::fromJSON(multipage_json)
  
  # If no images, test that proper error is raised
  if (is.null(parsed$pages$image$content) || 
      any(is.na(parsed$pages$image$content)) || 
      any(parsed$pages$image$content == "")) {
    
    expect_error(
      draw_blocks(type = "async", object = multipage_json, dir = tempdir()),
      "JSON file does not contain base64-encoded images"
    )
    
    skip("Multi-page test file verified to raise proper error for missing images")
  }
  
  pages <- parsed[["pages"]][["pageNumber"]]
  
  # Should be multi-page
  expect_true(length(pages) > 1)
  
  test_dir <- tempdir()
  
  # Test with blocks
  suppressMessages(draw_blocks(type = "async", object = multipage_json, dir = test_dir))
  
  stem <- substr(basename(multipage_json), 1, nchar(basename(multipage_json)) - 5)
  for (i in pages) {
    fname <- glue::glue("{stem}_page{i}_blocks.png")
    fpath <- file.path(test_dir, fname)
    expect_true(file.exists(fpath))
  }
})

## EDGE CASES ------------------------------------------------------------------

test_that("draw functions error appropriately when JSON lacks base64 images", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  # Create a JSON file without base64 image content
  json_without_images <- tempfile(fileext = ".json")
  
  # Simulate a Document AI response structure but with NULL image content
  test_data <- list(
    pages = list(
      list(
        pageNumber = 1,
        image = list(content = NA),
        blocks = list(
          list(
            layout = list(
              boundingPoly = list(
                normalizedVertices = list(
                  list(x = 0, y = 0),
                  list(x = 1, y = 0),
                  list(x = 1, y = 1),
                  list(x = 0, y = 1)
                )
              )
            )
          )
        )
      )
    )
  )
  
  jsonlite::write_json(test_data, json_without_images)
  
  # Test that all draw functions produce the appropriate error
  expect_error(
    draw_blocks(type = "async", object = json_without_images, dir = tempdir()),
    "JSON file does not contain base64-encoded images"
  )
  
  expect_error(
    draw_paragraphs(type = "async", object = json_without_images, dir = tempdir()),
    "JSON file does not contain base64-encoded images"
  )
  
  expect_error(
    draw_lines(type = "async", object = json_without_images, dir = tempdir()),
    "JSON file does not contain base64-encoded images"
  )
  
  expect_error(
    draw_tokens(type = "async", object = json_without_images, dir = tempdir()),
    "JSON file does not contain base64-encoded images"
  )
  
  # Clean up
  unlink(json_without_images, force = TRUE)
})

test_that("draw functions handle empty prefix string", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  test_dir <- tempdir()
  
  # Empty string should be treated as NULL (use default)
  suppressMessages(draw_blocks(type = "async", object = realjson, dir = test_dir, prefix = ""))
  
  parsed <- jsonlite::fromJSON(realjson)
  pages <- parsed[["pages"]][["pageNumber"]]
  
  # Should use the base name of the JSON file as prefix
  stem <- substr(basename(realjson), 1, nchar(basename(realjson)) - 5)
  for (i in pages) {
    fname <- glue::glue("{stem}_page{i}_blocks.png")
    fpath <- file.path(test_dir, fname)
    expect_true(file.exists(fpath))
  }
})

test_that("draw functions handle special characters in prefix", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  test_dir <- tempdir()
  
  # Prefix with underscores and numbers
  prefix_with_special <- "test_file_123"
  suppressMessages(draw_blocks(type = "async", object = realjson, dir = test_dir, prefix = prefix_with_special))
  
  parsed <- jsonlite::fromJSON(realjson)
  pages <- parsed[["pages"]][["pageNumber"]]
  
  for (i in pages) {
    fname <- glue::glue("{prefix_with_special}_page{i}_blocks.png")
    fpath <- file.path(test_dir, fname)
    expect_true(file.exists(fpath))
  }
})

test_that("draw functions reject negative numeric values", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  test_dir <- tempdir()
  
  # Negative linewd - now caught by validation with clear message
  expect_error(
    draw_blocks(type = "async", object = realjson, dir = test_dir, linewd = -1),
    "Invalid linewd parameter. Must be a single positive number greater than 0."
  )
  
  # Negative fontsize - now caught by validation with clear message
  expect_error(
    draw_blocks(type = "async", object = realjson, dir = test_dir, fontsize = -2),
    "Invalid fontsize parameter. Must be a single positive number greater than 0."
  )
  
  # Test with other draw functions to ensure consistency
  expect_error(
    draw_paragraphs(type = "async", object = realjson, dir = test_dir, linewd = -1),
    "Invalid linewd parameter"
  )
  
  expect_error(
    draw_lines(type = "async", object = realjson, dir = test_dir, fontsize = -1),
    "Invalid fontsize parameter"
  )
})

test_that("draw functions reject zero numeric values", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  test_dir <- tempdir()
  
  # Zero linewd
  expect_error(
    draw_blocks(type = "async", object = realjson, dir = test_dir, linewd = 0),
    "Invalid linewd parameter. Must be a single positive number greater than 0."
  )
  
  # Zero fontsize
  expect_error(
    draw_blocks(type = "async", object = realjson, dir = test_dir, fontsize = 0),
    "Invalid fontsize parameter. Must be a single positive number greater than 0."
  )
  
})

test_that("draw functions normalize directory paths correctly", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  test_dir <- tempdir()
  
  # Path with trailing slash
  dir_with_slash <- paste0(test_dir, "/")
  suppressMessages(draw_blocks(type = "async", object = realjson, dir = dir_with_slash))
  
  parsed <- jsonlite::fromJSON(realjson)
  pages <- parsed[["pages"]][["pageNumber"]]
  stem <- substr(basename(realjson), 1, nchar(basename(realjson)) - 5)
  
  for (i in pages) {
    fname <- glue::glue("{stem}_page{i}_blocks.png")
    fpath <- file.path(test_dir, fname)
    expect_true(file.exists(fpath))
  }
})

## INTEGRATION TESTS -----------------------------------------------------------

test_that("all draw functions work on the same document", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  test_dir <- tempdir()
  
  # Run all draw functions on the same JSON
  suppressMessages({
    draw_blocks(type = "async", object = realjson, dir = test_dir)
    draw_paragraphs(type = "async", object = realjson, dir = test_dir)
    draw_lines(type = "async", object = realjson, dir = test_dir)
    draw_tokens(type = "async", object = realjson, dir = test_dir)
  })
  
  parsed <- jsonlite::fromJSON(realjson)
  pages <- parsed[["pages"]][["pageNumber"]]
  stem <- substr(basename(realjson), 1, nchar(basename(realjson)) - 5)
  
  # Check that all file types were created
  for (i in pages) {
    for (boxtype in c("blocks", "paragraphs", "lines", "tokens")) {
      fname <- glue::glue("{stem}_page{i}_{boxtype}.png")
      fpath <- file.path(test_dir, fname)
      expect_true(file.exists(fpath))
    }
  }
})

test_that("draw functions create unique files with different prefixes", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  
  test_dir <- tempdir()
  
  # Create blocks with two different prefixes
  suppressMessages({
    draw_blocks(type = "async", object = realjson, dir = test_dir, prefix = "version1")
    draw_blocks(type = "async", object = realjson, dir = test_dir, prefix = "version2")
  })
  
  parsed <- jsonlite::fromJSON(realjson)
  pages <- parsed[["pages"]][["pageNumber"]]
  
  # Both sets of files should exist
  for (i in pages) {
    fname1 <- glue::glue("version1_page{i}_blocks.png")
    fname2 <- glue::glue("version2_page{i}_blocks.png")
    expect_true(file.exists(file.path(test_dir, fname1)))
    expect_true(file.exists(file.path(test_dir, fname2)))
  }
})

## CLEANUP ---------------------------------------------------------------------

unlink(madeup_json_file, force = TRUE)

# Clean up any remaining test files in tempdir
test_files <- list.files(tempdir(), pattern = ".*_(blocks|paragraphs|lines|tokens|entities)\\.png$", full.names = TRUE)
unlink(test_files, force = TRUE)
