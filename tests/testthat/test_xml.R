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

# Real JSON paths - using your actual test files
realjson <- testthat::test_path("examples", "output.json")
realjson_v1 <- testthat::test_path("examples", "sample_v1.json")
realjson_multipage <- testthat::test_path("examples", "sample3pg.json")

## MAKE_HOCR - BASIC VALIDATION TESTS --------------------------------------

test_that("make_hocr() errors with invalid type parameter", {
  expect_error(make_hocr(type = NULL, output = realjson), "Invalid type parameter")
  expect_error(make_hocr(type = NA, output = realjson), "Invalid type parameter")
  expect_error(make_hocr(type = TRUE, output = realjson), "Invalid type parameter")
  expect_error(make_hocr(type = 123, output = realjson), "Invalid type parameter")
  expect_error(make_hocr(type = "wrong", output = realjson), "Invalid type parameter")
  expect_error(make_hocr(type = c("async", "sync"), output = realjson), "Invalid type parameter")
})

test_that("make_hocr() errors with invalid output parameter", {
  expect_error(make_hocr(type = "async", output = NULL), "Invalid output parameter")
  expect_error(make_hocr(type = "async", output = NA), "Invalid output parameter")
  expect_error(make_hocr(type = "async", output = TRUE), "Invalid output parameter")
  expect_error(make_hocr(type = "async", output = 123), "Invalid output parameter")
  expect_error(make_hocr(type = "async", output = df), "Invalid output parameter")
})

test_that("make_hocr() errors with invalid dir parameter", {
  expect_error(
    make_hocr(type = "async", output = realjson, dir = NULL),
    "Invalid dir parameter"
  )
  expect_error(
    make_hocr(type = "async", output = realjson, dir = NA),
    "Invalid dir parameter"
  )
  expect_error(
    make_hocr(type = "async", output = realjson, dir = TRUE),
    "Invalid dir parameter"
  )
  expect_error(
    make_hocr(type = "async", output = realjson, dir = 123),
    "Invalid dir parameter"
  )
  expect_error(
    make_hocr(type = "async", output = realjson, dir = c("path1", "path2")),
    "Invalid dir parameter"
  )
})

test_that("make_hocr() errors with invalid outfile_name parameter", {
  expect_error(
    make_hocr(type = "async", output = realjson, outfile_name = NULL),
    "Invalid outfile_name parameter"
  )
  expect_error(
    make_hocr(type = "async", output = realjson, outfile_name = NA),
    "Invalid outfile_name parameter"
  )
  expect_error(
    make_hocr(type = "async", output = realjson, outfile_name = TRUE),
    "Invalid outfile_name parameter"
  )
  expect_error(
    make_hocr(type = "async", output = realjson, outfile_name = 123),
    "Invalid outfile_name parameter"
  )
  expect_error(
    make_hocr(type = "async", output = realjson, outfile_name = c("file1.hocr", "file2.hocr")),
    "Invalid outfile_name parameter"
  )
})

test_that("make_hocr() rejects invalid file extensions", {
  expect_error(
    make_hocr(type = "async", output = realjson, outfile_name = "test.pdf"),
    "outfile_name extension must be of type .hocr, .html, or .xml"
  )
  
  expect_error(
    make_hocr(type = "async", output = realjson, outfile_name = "test.txt"),
    "outfile_name extension must be of type .hocr, .html, or .xml"
  )
  
  expect_error(
    make_hocr(type = "async", output = realjson, outfile_name = "test.json"),
    "outfile_name extension must be of type .hocr, .html, or .xml"
  )
  
  expect_error(
    make_hocr(type = "async", output = realjson, outfile_name = "test"),
    "outfile_name extension must be of type .hocr, .html, or .xml"
  )
})

test_that("make_hocr() rejects filenames with slashes", {
  expect_error(
    make_hocr(type = "async", output = realjson, outfile_name = "path/to/test.hocr"),
    "outfile_name cannot contain slashes"
  )
  
  expect_error(
    make_hocr(type = "async", output = realjson, outfile_name = "path\\to\\test.hocr"),
    "outfile_name cannot contain slashes"
  )
})

## MAKE_HOCR - FILE GENERATION TESTS ---------------------------------------

test_that("make_hocr() generates .hocr files for async type", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("xml2")
  skip_if_not(file.exists(realjson), "Test JSON file not found")
  
  test_dir <- tempdir()
  outfile <- "test_output.hocr"
  
  suppressMessages(
    make_hocr(type = "async", output = realjson, outfile_name = outfile, dir = test_dir)
  )
  
  fpath <- file.path(test_dir, outfile)
  expect_true(file.exists(fpath))
  
  # Clean up
  unlink(fpath, force = TRUE)
})

test_that("make_hocr() generates .html files", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("xml2")
  skip_if_not(file.exists(realjson), "Test JSON file not found")
  
  test_dir <- tempdir()
  outfile <- "test_output.html"
  
  suppressMessages(
    make_hocr(type = "async", output = realjson, outfile_name = outfile, dir = test_dir)
  )
  
  fpath <- file.path(test_dir, outfile)
  expect_true(file.exists(fpath))
  
  # Clean up
  unlink(fpath, force = TRUE)
})

test_that("make_hocr() generates .xml files", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("xml2")
  skip_if_not(file.exists(realjson), "Test JSON file not found")
  
  test_dir <- tempdir()
  outfile <- "test_output.xml"
  
  suppressMessages(
    make_hocr(type = "async", output = realjson, outfile_name = outfile, dir = test_dir)
  )
  
  fpath <- file.path(test_dir, outfile)
  expect_true(file.exists(fpath))
  
  # Clean up
  unlink(fpath, force = TRUE)
})

test_that("make_hocr() uses default filename when not specified", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("xml2")
  skip_if_not(file.exists(realjson), "Test JSON file not found")
  
  test_dir <- tempdir()
  
  suppressMessages(
    make_hocr(type = "async", output = realjson, dir = test_dir)
  )
  
  fpath <- file.path(test_dir, "out.hocr")
  expect_true(file.exists(fpath))
  
  # Clean up
  unlink(fpath, force = TRUE)
})

test_that("make_hocr() produces success message", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("xml2")
  skip_if_not(file.exists(realjson), "Test JSON file not found")
  
  test_dir <- tempdir()
  outfile <- "test_success.hocr"
  
  expect_message(
    make_hocr(type = "async", output = realjson, outfile_name = outfile, dir = test_dir),
    "hOCR file named"
  )
  
  # Clean up
  unlink(file.path(test_dir, outfile), force = TRUE)
})

test_that("make_hocr() produces processing messages", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("xml2")
  skip_if_not(file.exists(realjson), "Test JSON file not found")
  
  test_dir <- tempdir()
  outfile <- "test_messages.hocr"
  
  expect_message(
    make_hocr(type = "async", output = realjson, outfile_name = outfile, dir = test_dir),
    "Generating hOCR file"
  )
  
  expect_message(
    make_hocr(type = "async", output = realjson, outfile_name = outfile, dir = test_dir),
    "Processing page"
  )
  
  # Clean up
  unlink(file.path(test_dir, outfile), force = TRUE)
})

test_that("make_hocr() completes without error", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("xml2")
  skip_if_not(file.exists(realjson), "Test JSON file not found")
  
  test_dir <- tempdir()
  outfile <- "test_noerror.hocr"
  
  expect_no_error(
    suppressMessages(
      make_hocr(type = "async", output = realjson, outfile_name = outfile, dir = test_dir)
    )
  )
  
  # Clean up
  unlink(file.path(test_dir, outfile), force = TRUE)
})

## MAKE_HOCR - XML STRUCTURE TESTS -----------------------------------------

test_that("make_hocr() generates valid XML", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("xml2")
  skip_if_not(file.exists(realjson), "Test JSON file not found")
  
  test_dir <- tempdir()
  outfile <- "test_valid.hocr"
  
  suppressMessages(
    make_hocr(type = "async", output = realjson, outfile_name = outfile, dir = test_dir)
  )
  
  fpath <- file.path(test_dir, outfile)
  
  # Should be able to read as XML without error
  expect_no_error(doc <- xml2::read_xml(fpath))
  
  doc <- xml2::read_xml(fpath)
  expect_s3_class(doc, "xml_document")
  
  # Clean up
  unlink(fpath, force = TRUE)
})

test_that("make_hocr() output contains required hOCR elements", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("xml2")
  skip_if_not(file.exists(realjson), "Test JSON file not found")
  
  test_dir <- tempdir()
  outfile <- "test_structure.hocr"
  
  suppressMessages(
    make_hocr(type = "async", output = realjson, outfile_name = outfile, dir = test_dir)
  )
  
  fpath <- file.path(test_dir, outfile)
  doc <- xml2::read_xml(fpath)
  
  # Check for key hOCR classes
  expect_true(length(xml2::xml_find_all(doc, "//*[@class='ocr_page']")) > 0)
  expect_true(length(xml2::xml_find_all(doc, "//*[@class='ocr_carea']")) > 0)
  expect_true(length(xml2::xml_find_all(doc, "//*[@class='ocr_par']")) > 0)
  expect_true(length(xml2::xml_find_all(doc, "//*[@class='ocr_line']")) > 0)
  expect_true(length(xml2::xml_find_all(doc, "//*[@class='ocrx_word']")) > 0)
  
  # Clean up
  unlink(fpath, force = TRUE)
})

test_that("make_hocr() output contains bbox information", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("xml2")
  skip_if_not(file.exists(realjson), "Test JSON file not found")
  
  test_dir <- tempdir()
  outfile <- "test_bbox.hocr"
  
  suppressMessages(
    make_hocr(type = "async", output = realjson, outfile_name = outfile, dir = test_dir)
  )
  
  fpath <- file.path(test_dir, outfile)
  doc <- xml2::read_xml(fpath)
  
  # Pages should have bbox
  pages <- xml2::xml_find_all(doc, "//*[@class='ocr_page']")
  if (length(pages) > 0) {
    page_titles <- xml2::xml_attr(pages, "title")
    expect_true(all(grepl("bbox", page_titles)))
  }
  
  # Words should have bbox and confidence
  words <- xml2::xml_find_all(doc, "//*[@class='ocrx_word']")
  if (length(words) > 0) {
    word_titles <- xml2::xml_attr(words, "title")
    expect_true(all(grepl("bbox", word_titles)))
    expect_true(all(grepl("x_wconf", word_titles)))
  }
  
  # Clean up
  unlink(fpath, force = TRUE)
})

test_that("make_hocr() output contains text content", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("xml2")
  skip_if_not(file.exists(realjson), "Test JSON file not found")
  
  test_dir <- tempdir()
  outfile <- "test_text.hocr"
  
  suppressMessages(
    make_hocr(type = "async", output = realjson, outfile_name = outfile, dir = test_dir)
  )
  
  fpath <- file.path(test_dir, outfile)
  doc <- xml2::read_xml(fpath)
  
  # Words should contain text
  words <- xml2::xml_find_all(doc, "//*[@class='ocrx_word']")
  if (length(words) > 0) {
    word_text <- xml2::xml_text(words)
    # At least some words should be non-empty
    expect_true(sum(nchar(word_text) > 0) > 0)
  }
  
  # Clean up
  unlink(fpath, force = TRUE)
})

## MAKE_HOCR - MULTI-PAGE TESTS --------------------------------------------

test_that("make_hocr() handles multi-page documents", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("xml2")
  skip_if_not(file.exists(realjson_multipage), "Multi-page test file not available")
  
  test_dir <- tempdir()
  outfile <- "test_multipage.hocr"
  
  suppressMessages(
    make_hocr(type = "async", output = realjson_multipage, outfile_name = outfile, dir = test_dir)
  )
  
  fpath <- file.path(test_dir, outfile)
  expect_true(file.exists(fpath))
  
  doc <- xml2::read_xml(fpath)
  pages <- xml2::xml_find_all(doc, "//*[@class='ocr_page']")
  
  # Should have multiple pages
  expect_true(length(pages) > 1)
  
  # Clean up
  unlink(fpath, force = TRUE)
})

test_that("make_hocr() processes all pages with messages", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("xml2")
  skip_if_not(file.exists(realjson_multipage), "Multi-page test file not available")
  
  test_dir <- tempdir()
  outfile <- "test_allpages.hocr"
  
  # Should show processing messages for each page
  expect_message(
    make_hocr(type = "async", output = realjson_multipage, outfile_name = outfile, dir = test_dir),
    "Processing page"
  )
  
  # Clean up
  unlink(file.path(test_dir, outfile), force = TRUE)
})

## MAKE_HOCR - EDGE CASES --------------------------------------------------

test_that("make_hocr() handles filenames with special characters", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("xml2")
  skip_if_not(file.exists(realjson), "Test JSON file not found")
  
  test_dir <- tempdir()
  
  # Filename with underscores and numbers
  outfile <- "test_file_123.hocr"
  suppressMessages(
    make_hocr(type = "async", output = realjson, outfile_name = outfile, dir = test_dir)
  )
  expect_true(file.exists(file.path(test_dir, outfile)))
  unlink(file.path(test_dir, outfile), force = TRUE)
  
  # Filename with hyphens
  outfile <- "test-file-name.hocr"
  suppressMessages(
    make_hocr(type = "async", output = realjson, outfile_name = outfile, dir = test_dir)
  )
  expect_true(file.exists(file.path(test_dir, outfile)))
  unlink(file.path(test_dir, outfile), force = TRUE)
})

test_that("make_hocr() handles case-insensitive extensions", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("xml2")
  skip_if_not(file.exists(realjson), "Test JSON file not found")
  
  test_dir <- tempdir()
  
  # Uppercase extension
  outfile_upper <- "test.HOCR"
  suppressMessages(
    make_hocr(type = "async", output = realjson, outfile_name = outfile_upper, dir = test_dir)
  )
  expect_true(file.exists(file.path(test_dir, outfile_upper)))
  unlink(file.path(test_dir, outfile_upper), force = TRUE)
  
  # Mixed case extension
  outfile_mixed <- "test.HoCr"
  suppressMessages(
    make_hocr(type = "async", output = realjson, outfile_name = outfile_mixed, dir = test_dir)
  )
  expect_true(file.exists(file.path(test_dir, outfile_mixed)))
  unlink(file.path(test_dir, outfile_mixed), force = TRUE)
})

test_that("make_hocr() works with different test files", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("xml2")
  
  test_dir <- tempdir()
  
  # Test with sample_v1.json if it exists
  if (file.exists(realjson_v1)) {
    outfile <- "test_v1.hocr"
    expect_no_error(
      suppressMessages(
        make_hocr(type = "async", output = realjson_v1, outfile_name = outfile, dir = test_dir)
      )
    )
    expect_true(file.exists(file.path(test_dir, outfile)))
    unlink(file.path(test_dir, outfile), force = TRUE)
  }
  
  # Test with output.json
  if (file.exists(realjson)) {
    outfile <- "test_output.hocr"
    expect_no_error(
      suppressMessages(
        make_hocr(type = "async", output = realjson, outfile_name = outfile, dir = test_dir)
      )
    )
    expect_true(file.exists(file.path(test_dir, outfile)))
    unlink(file.path(test_dir, outfile), force = TRUE)
  }
})

## MAKE_HOCR - INTEGRATION TESTS -------------------------------------------

test_that("make_hocr() produces consistent output across runs", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("xml2")
  skip_if_not(file.exists(realjson), "Test JSON file not found")
  
  test_dir <- tempdir()
  outfile1 <- "test_run1.hocr"
  outfile2 <- "test_run2.hocr"
  
  # Generate the same file twice
  suppressMessages({
    make_hocr(type = "async", output = realjson, outfile_name = outfile1, dir = test_dir)
    make_hocr(type = "async", output = realjson, outfile_name = outfile2, dir = test_dir)
  })
  
  fpath1 <- file.path(test_dir, outfile1)
  fpath2 <- file.path(test_dir, outfile2)
  
  # Read both files
  doc1 <- xml2::read_xml(fpath1)
  doc2 <- xml2::read_xml(fpath2)
  
  # Convert to strings for comparison
  str1 <- as.character(doc1)
  str2 <- as.character(doc2)
  
  # Should be identical
  expect_equal(str1, str2)
  
  # Clean up
  unlink(fpath1, force = TRUE)
  unlink(fpath2, force = TRUE)
})

test_that("make_hocr() generates unique IDs for all elements", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("xml2")
  skip_if_not(file.exists(realjson), "Test JSON file not found")
  
  test_dir <- tempdir()
  outfile <- "test_unique_ids.hocr"
  
  suppressMessages(
    make_hocr(type = "async", output = realjson, outfile_name = outfile, dir = test_dir)
  )
  
  fpath <- file.path(test_dir, outfile)
  doc <- xml2::read_xml(fpath)
  
  # Get all elements with IDs
  all_ids <- xml2::xml_attr(xml2::xml_find_all(doc, "//*[@id]"), "id")
  
  # All IDs should be unique
  expect_equal(length(all_ids), length(unique(all_ids)))
  
  # Clean up
  unlink(fpath, force = TRUE)
})

## CLEANUP -----------------------------------------------------------------

unlink(madeup_json_file, force = TRUE)

# Clean up any remaining test files in tempdir
test_files <- list.files(tempdir(), pattern = ".*\\.(hocr|html|xml)$", full.names = TRUE)
test_files <- test_files[grepl("test_", basename(test_files))]
unlink(test_files, force = TRUE)

