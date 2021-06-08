
## TEXT_FROM_DAI_RESPONSE ------------------------------------------------------

test_that("text_from_dai_response() warns of input errors", {
  expect_error(text_from_dai_response(NULL), "Input is not a valid HTTP response.")
  expect_error(text_from_dai_response(12345), "Input is not a valid HTTP response.")
  expect_error(text_from_dai_response(mtcars), "Input is not a valid HTTP response.")
  expect_error(text_from_dai_response(as.matrix(mtcars)), "Input is not a valid HTTP response.")
  expect_error(text_from_dai_response("string"), "Input is not a valid HTTP response.")
  expect_error(text_from_dai_response(c("string", "vector")), "Input is not a valid HTTP response.")
  expect_error(text_from_dai_response(list("a", "list")), "Input is not a valid HTTP response.")
})

test_that("text_from_dai_response() warns of response not containing text", {
  skip_on_cran()
  skip_on_ci()
  skip_if_no_token()
  skip_if_offline()
  wrong <- dai_async("random.pdf")
  expect_error(text_from_dai_response(wrong), "Input not recognized. Is it from dai_async?")
  wrong2 <- dai_user()
  expect_error(text_from_dai_response(wrong2), "Input not recognized. Is it from dai_async?")
  blank <- dai_sync(testthat::test_path("examples", "blank.tiff"))
  expect_error(text_from_dai_response(blank), "DAI found no text. Was the page blank?")
})

test_that("text_from_dai_response() gets text from DAI response from example file", {
  skip_on_cran()
  skip_on_ci()
  skip_if_no_token()
  skip_if_offline()
  file <- testthat::test_path("examples", "image.jpg")
  response <- dai_sync(file)
  text <- text_from_dai_response(response)
  expect_type(text, "character")
})

## TEXT_FROM_DAI_FILE ----------------------------------------------------------

test_that("text_from_dai_file() warns of input errors", {
  expect_error(text_from_dai_file(NULL), "Invalid file input.")
  expect_error(text_from_dai_file(12345), "Invalid file input.")
  expect_error(text_from_dai_file(mtcars), "Invalid file input.")
  expect_error(text_from_dai_file(as.matrix(mtcars)), "Invalid file input.")
  expect_error(text_from_dai_file(c("string", "vector")), "Invalid file input.")
  expect_error(text_from_dai_file(list("a", "list")), "Invalid file input.")
  expect_error(text_from_dai_file("wrong.txt"), "Input file not .json. Is the file in your working directory?")
  expect_error(text_from_dai_file("fake.json"), "Input file not .json. Is the file in your working directory?")
})

test_that("text_from_dai_file() warns of file not containing text", {
  random <- list("a" = 1, "b" = 2)
  json <- jsonlite::toJSON(random)
  madeup <- tempfile(fileext = ".json")
  write(json, madeup)
  expect_error(text_from_dai_file(madeup), "JSON not in right format. Is it from DAI?")
  blank <- testthat::test_path("examples", "output_blank.json")
  expect_error(text_from_dai_file(blank), "DAI found no text. Was the document blank?")
  unlink(madeup, force = TRUE)
})

test_that("text_from_dai_file() gets text from example json file", {
  sample <- testthat::test_path("examples", "output.json")
  text <- text_from_dai_file(sample)
  expect_type(text, "character")
})

## DRAW_BLOCKS -----------------------------------------------------------------

test_that("draw_blocks() warns of input errors", {
  realjson <- testthat::test_path("examples", "output.json")
  expect_error(draw_blocks(12345), "Invalid json input.")
  expect_error(draw_blocks(mtcars), "Invalid json input.")
  expect_error(draw_blocks(as.matrix(mtcars)), "Invalid json input.")
  expect_error(draw_blocks(c("string.json", "vector.json")), "Invalid json input. This function is not vectorised.")
  expect_error(draw_blocks(list("a.json", "list.json")), "Invalid json input. This function is not vectorised.")
  expect_error(draw_blocks("fake.json"), "Input 'json' not .json.")
})

test_that("draw_blocks() produces a correctly named png file", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  realjson <- testthat::test_path("examples", "sample_v1.json")
  draw_blocks(realjson, dir = tempdir())
  parsed <- jsonlite::fromJSON(realjson)
  pages <- parsed[["pages"]][["pageNumber"]]
  expected_filepaths <- character()
  for (i in pages) {
    fname <- glue::glue("page{i}_blocks.png")
    fpath <- file.path(tempdir(), fname)
    expected_filepaths <- c(expected_filepaths, fpath)
  }
  for (j in 1:length(expected_filepaths)) {
  expect_true(file.exists(expected_filepaths[j]))
  }
  unlink(expected_filepaths, force = TRUE)
})

## DRAW_PARAGRAPHS -------------------------------------------------------------

test_that("draw_paragraphs() warns of input errors", {
  realjson <- testthat::test_path("examples", "output.json")
  expect_error(draw_paragraphs(12345), "Invalid json input.")
  expect_error(draw_paragraphs(mtcars), "Invalid json input.")
  expect_error(draw_paragraphs(as.matrix(mtcars)), "Invalid json input.")
  expect_error(draw_paragraphs(c("string.json", "vector.json")), "Invalid json input. This function is not vectorised.")
  expect_error(draw_paragraphs(list("a.json", "list.json")), "Invalid json input. This function is not vectorised.")
  expect_error(draw_paragraphs("fake.json"), "Input 'json' not .json.")
})

test_that("draw_paragraphs() produces a correctly named new png file", {
  skip_on_cran()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  realjson <- testthat::test_path("examples", "sample_v1.json")
  draw_paragraphs(realjson, dir = tempdir())
  parsed <- jsonlite::fromJSON(realjson)
  pages <- parsed[["pages"]][["pageNumber"]]
  expected_filepaths <- character()
  for (i in pages) {
    fname <- glue::glue("page{i}_paragraphs.png")
    fpath <- file.path(tempdir(), fname)
    expected_filepaths <- c(expected_filepaths, fpath)
  }
  for (j in 1:length(expected_filepaths)) {
    expect_true(file.exists(expected_filepaths[j]))
  }
  unlink(expected_filepaths, force = TRUE)
})

## DRAW_LINES ------------------------------------------------------------------

test_that("draw_lines() warns of input errors", {
  realjson <- testthat::test_path("examples", "output.json")
  expect_error(draw_lines(12345), "Invalid json input.")
  expect_error(draw_lines(mtcars), "Invalid json input.")
  expect_error(draw_lines(as.matrix(mtcars)), "Invalid json input.")
  expect_error(draw_lines(c("string.json", "vector.json")), "Invalid json input. This function is not vectorised.")
  expect_error(draw_lines(list("a.json", "list.json")), "Invalid json input. This function is not vectorised.")
  expect_error(draw_lines("fake.json"), "Input 'json' not .json.")
})

test_that("draw_lines() produces a correctly named new png file", {
  skip_on_cran()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  realjson <- testthat::test_path("examples", "sample_v1.json")
  draw_lines(realjson, dir = tempdir())
  parsed <- jsonlite::fromJSON(realjson)
  pages <- parsed[["pages"]][["pageNumber"]]
  expected_filepaths <- character()
  for (i in pages) {
    fname <- glue::glue("page{i}_lines.png")
    fpath <- file.path(tempdir(), fname)
    expected_filepaths <- c(expected_filepaths, fpath)
  }
  for (j in 1:length(expected_filepaths)) {
    expect_true(file.exists(expected_filepaths[j]))
  }
  unlink(expected_filepaths, force = TRUE)
})

## DRAW_TOKENS -----------------------------------------------------------------

test_that("draw_tokens() warns of input errors", {
  realjson <- testthat::test_path("examples", "output.json")
  expect_error(draw_tokens(12345), "Invalid json input.")
  expect_error(draw_tokens(mtcars), "Invalid json input.")
  expect_error(draw_tokens(as.matrix(mtcars)), "Invalid json input.")
  expect_error(draw_tokens(c("string.json", "vector.json")), "Invalid json input. This function is not vectorised.")
  expect_error(draw_tokens(list("a.json", "list.json")), "Invalid json input. This function is not vectorised.")
  expect_error(draw_tokens("fake.json"), "Input 'json' not .json.")
})

test_that("draw_tokens() produces a correctly named new png file", {
  skip_on_cran()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  realjson <- testthat::test_path("examples", "sample_v1.json")
  draw_tokens(realjson, dir = tempdir())
  parsed <- jsonlite::fromJSON(realjson)
  pages <- parsed[["pages"]][["pageNumber"]]
  expected_filepaths <- character()
  for (i in pages) {
    fname <- glue::glue("page{i}_tokens.png")
    fpath <- file.path(tempdir(), fname)
    expected_filepaths <- c(expected_filepaths, fpath)
  }
  for (j in 1:length(expected_filepaths)) {
    expect_true(file.exists(expected_filepaths[j]))
  }
  unlink(expected_filepaths, force = TRUE)
})
