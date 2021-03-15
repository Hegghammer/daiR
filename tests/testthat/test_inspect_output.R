
## TEXT_FROM_DAI_RESPONSE ------------------------------------------------------

test_that("text_from_dai_response() warns of input errors", {
  expect_error(text_from_dai_response(NULL), "Object is not a valid HTTP response.")
  expect_error(text_from_dai_response(12345), "Object is not a valid HTTP response.")
  expect_error(text_from_dai_response(mtcars), "Object is not a valid HTTP response.")
  expect_error(text_from_dai_response(as.matrix(mtcars)), "Object is not a valid HTTP response.")
  expect_error(text_from_dai_response("string"), "Object is not a valid HTTP response.")
  expect_error(text_from_dai_response(c("string", "vector")), "Object is not a valid HTTP response.")
  expect_error(text_from_dai_response(list("a", "list")), "Object is not a valid HTTP response.")
})

test_that("text_from_dai_response() warns of response not containing text", {
  skip_if_no_token()
  skip_if_offline()

  wrong <- dai_async("random.pdf")
  expect_error(text_from_dai_response(wrong), "Input not recognized. Did you use dai_async instead of dai_sync?")

  wrong2 <- dai_user()
  expect_error(text_from_dai_response(wrong2), "Input not recognized. Is it from dai_async?")

  blank <- dai_sync(testthat::test_path("examples", "blank.tiff"))
  expect_error(text_from_dai_response(blank), "DAI found no text. Was the page blank?")
})

test_that("text_from_dai_response() reads real dai response with text", {
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

test_that("text_from_dai_file() reads real dai output file with text", {

  sample <- testthat::test_path("examples", "output.json")
  text <- text_from_dai_file(sample)
  expect_type(text, "character")

})

## DRAW_BLOCKS -----------------------------------------------------------------

test_that("draw_blocks() warns of input errors", {
  realpdf <- testthat::test_path("examples", "sample.pdf")
  realjson <- testthat::test_path("examples", "output.json")

  expect_error(draw_blocks(pdf = realpdf), 'argument "json" is missing, with no default')
  expect_error(draw_blocks(json = realjson), 'argument "pdf" is missing, with no default')
  expect_error(draw_blocks(12345, realjson), "Invalid pdf input.")
  expect_error(draw_blocks(realpdf, 12345), "Invalid json input.")
  expect_error(draw_blocks(mtcars, realjson), "Invalid pdf input.")
  expect_error(draw_blocks(realpdf, mtcars), "Invalid json input.")
  expect_error(draw_blocks(as.matrix(mtcars), realjson), "Invalid pdf input.")
  expect_error(draw_blocks(realpdf, as.matrix(mtcars)), "Invalid json input.")
  expect_error(draw_blocks(c("string.pdf", "vector.pdf"), realjson), "Invalid pdf input. This function is not vectorised.")
  expect_error(draw_blocks(realpdf, c("string.json", "vector.json")), "Invalid json input. This function is not vectorised.")
  expect_error(draw_blocks(list("a.pdf", "list.pdf"), realjson), "Invalid pdf input. This function is not vectorised.")
  expect_error(draw_blocks(realpdf, list("a.json", "list.json")), "Invalid json input. This function is not vectorised.")
  expect_error(draw_blocks(realpdf, "fake.json"), "Input 'json' not .json.")
  expect_error(draw_blocks("fake.pdf", realjson), "Input 'pdf' not a pdf.")
})

test_that("draw_blocks() produces a correctly named png file", {

  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")

  realpdf <- testthat::test_path("examples", "sample.pdf")
  realjson <- testthat::test_path("examples", "output.json")

  pages <- pdftools::pdf_info(realpdf)$pages
  expected_message <- glue::glue("Generated {pages} annotated image*")
  message <- capture.output(draw_blocks(realpdf, realjson), type = "message")
  expect_match(message, expected_message)

  expected_filepaths <- character()
  for (i in pages) {
    fname <- glue::glue("{stringr::str_sub(basename(realpdf), end=-5)}{i}_blocks.png")
    fpath <- file.path(tempdir(), fname)
    expected_filepaths <- c(expected_filepaths, fpath)
  }

  for (j in 1:length(expected_filepaths)) {
  expect_true(file.exists(expected_filepaths[j]))
  }

  unlink(expected_filepaths, force = TRUE)
})

test_that("draw_blocks() draws actual boxes on the png", {
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")

  realpdf <- testthat::test_path("examples", "sample.pdf")
  realjson <- testthat::test_path("examples", "output.json")
  draw_blocks(realpdf, realjson)

  filename <- glue::glue("{stringr::str_sub(basename(realpdf), end=-5)}1_blocks.png")
  filepath <- file.path(tempdir(), filename)

  expect_snapshot_file(filepath, basename(filepath))

  unlink(filepath, force = TRUE)
})

## DRAW_PARAGRAPHS -------------------------------------------------------------

test_that("draw_paragraphs() warns of input errors", {
  realpdf <- testthat::test_path("examples", "sample.pdf")
  realjson <- testthat::test_path("examples", "output.json")

  expect_error(draw_paragraphs(pdf = realpdf), 'argument "json" is missing, with no default')
  expect_error(draw_paragraphs(json = realjson), 'argument "pdf" is missing, with no default')
  expect_error(draw_paragraphs(12345, realjson), "Invalid pdf input.")
  expect_error(draw_paragraphs(realpdf, 12345), "Invalid json input.")
  expect_error(draw_paragraphs(mtcars, realjson), "Invalid pdf input.")
  expect_error(draw_paragraphs(realpdf, mtcars), "Invalid json input.")
  expect_error(draw_paragraphs(as.matrix(mtcars), realjson), "Invalid pdf input.")
  expect_error(draw_paragraphs(realpdf, as.matrix(mtcars)), "Invalid json input.")
  expect_error(draw_paragraphs(c("string.pdf", "vector.pdf"), realjson), "Invalid pdf input. This function is not vectorised.")
  expect_error(draw_paragraphs(realpdf, c("string.json", "vector.json")), "Invalid json input. This function is not vectorised.")
  expect_error(draw_paragraphs(list("a.pdf", "list.pdf"), realjson), "Invalid pdf input. This function is not vectorised.")
  expect_error(draw_paragraphs(realpdf, list("a.json", "list.json")), "Invalid json input. This function is not vectorised.")
  expect_error(draw_paragraphs(realpdf, "fake.json"), "Input 'json' not .json.")
  expect_error(draw_paragraphs("fake.pdf", realjson), "Input 'pdf' not a pdf.")
})

test_that("draw_paragraphs() produces a correctly named new png file", {

  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")

  realpdf <- testthat::test_path("examples", "sample.pdf")
  realjson <- testthat::test_path("examples", "output.json")

  pages <- pdftools::pdf_info(realpdf)$pages
  expected_message <- glue::glue("Generated {pages} annotated image*")
  message <- capture.output(draw_paragraphs(realpdf, realjson), type = "message")
  expect_match(message, expected_message)

  expected_filepaths <- character()
  for (i in pages) {
    fname <- glue::glue("{stringr::str_sub(basename(realpdf), end=-5)}{i}_paragraphs.png")
    fpath <- file.path(tempdir(), fname)
    expected_filepaths <- c(expected_filepaths, fpath)
  }

  for (j in 1:length(expected_filepaths)) {
    expect_true(file.exists(expected_filepaths[j]))
  }

  unlink(expected_filepaths, force = TRUE)
})

test_that("draw_paragraphs() draws actual boxes on the png", {
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")

  realpdf <- testthat::test_path("examples", "sample.pdf")
  realjson <- testthat::test_path("examples", "output.json")
  draw_paragraphs(realpdf, realjson)

  filename <- glue::glue("{stringr::str_sub(basename(realpdf), end=-5)}1_paragraphs.png")
  filepath <- file.path(tempdir(), filename)

  expect_snapshot_file(filepath, basename(filepath))

  unlink(filepath, force = TRUE)
})

## DRAW_LINES ------------------------------------------------------------------

test_that("draw_lines() warns of input errors", {
  realpdf <- testthat::test_path("examples", "sample.pdf")
  realjson <- testthat::test_path("examples", "output.json")

  expect_error(draw_lines(pdf = realpdf), 'argument "json" is missing, with no default')
  expect_error(draw_lines(json = realjson), 'argument "pdf" is missing, with no default')
  expect_error(draw_lines(12345, realjson), "Invalid pdf input.")
  expect_error(draw_lines(realpdf, 12345), "Invalid json input.")
  expect_error(draw_lines(mtcars, realjson), "Invalid pdf input.")
  expect_error(draw_lines(realpdf, mtcars), "Invalid json input.")
  expect_error(draw_lines(as.matrix(mtcars), realjson), "Invalid pdf input.")
  expect_error(draw_lines(realpdf, as.matrix(mtcars)), "Invalid json input.")
  expect_error(draw_lines(c("string.pdf", "vector.pdf"), realjson), "Invalid pdf input. This function is not vectorised.")
  expect_error(draw_lines(realpdf, c("string.json", "vector.json")), "Invalid json input. This function is not vectorised.")
  expect_error(draw_lines(list("a.pdf", "list.pdf"), realjson), "Invalid pdf input. This function is not vectorised.")
  expect_error(draw_lines(realpdf, list("a.json", "list.json")), "Invalid json input. This function is not vectorised.")
  expect_error(draw_lines(realpdf, "fake.json"), "Input 'json' not .json.")
  expect_error(draw_lines("fake.pdf", realjson), "Input 'pdf' not a pdf.")
})

test_that("draw_lines() produces a correctly named new png file", {

  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")

  realpdf <- testthat::test_path("examples", "sample.pdf")
  realjson <- testthat::test_path("examples", "output.json")

  pages <- pdftools::pdf_info(realpdf)$pages
  expected_message <- glue::glue("Generated {pages} annotated image*")
  message <- capture.output(draw_lines(realpdf, realjson), type = "message")
  expect_match(message, expected_message)

  expected_filepaths <- character()
  for (i in pages) {
    fname <- glue::glue("{stringr::str_sub(basename(realpdf), end=-5)}{i}_lines.png")
    fpath <- file.path(tempdir(), fname)
    expected_filepaths <- c(expected_filepaths, fpath)
  }

  for (j in 1:length(expected_filepaths)) {
    expect_true(file.exists(expected_filepaths[j]))
  }

  unlink(expected_filepaths, force = TRUE)
})

test_that("draw_lines() draws actual boxes on the png", {
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")

  realpdf <- testthat::test_path("examples", "sample.pdf")
  realjson <- testthat::test_path("examples", "output.json")
  draw_lines(realpdf, realjson)

  filename <- glue::glue("{stringr::str_sub(basename(realpdf), end=-5)}1_lines.png")
  filepath <- file.path(tempdir(), filename)

  expect_snapshot_file(filepath, basename(filepath))

  unlink(filepath, force = TRUE)
})

## DRAW_TOKENS -----------------------------------------------------------------

test_that("draw_tokens() warns of input errors", {
  realpdf <- testthat::test_path("examples", "sample.pdf")
  realjson <- testthat::test_path("examples", "output.json")

  expect_error(draw_tokens(pdf = realpdf), 'argument "json" is missing, with no default')
  expect_error(draw_tokens(json = realjson), 'argument "pdf" is missing, with no default')
  expect_error(draw_tokens(12345, realjson), "Invalid pdf input.")
  expect_error(draw_tokens(realpdf, 12345), "Invalid json input.")
  expect_error(draw_tokens(mtcars, realjson), "Invalid pdf input.")
  expect_error(draw_tokens(realpdf, mtcars), "Invalid json input.")
  expect_error(draw_tokens(as.matrix(mtcars), realjson), "Invalid pdf input.")
  expect_error(draw_tokens(realpdf, as.matrix(mtcars)), "Invalid json input.")
  expect_error(draw_tokens(c("string.pdf", "vector.pdf"), realjson), "Invalid pdf input. This function is not vectorised.")
  expect_error(draw_tokens(realpdf, c("string.json", "vector.json")), "Invalid json input. This function is not vectorised.")
  expect_error(draw_tokens(list("a.pdf", "list.pdf"), realjson), "Invalid pdf input. This function is not vectorised.")
  expect_error(draw_tokens(realpdf, list("a.json", "list.json")), "Invalid json input. This function is not vectorised.")
  expect_error(draw_tokens(realpdf, "fake.json"), "Input 'json' not .json.")
  expect_error(draw_tokens("fake.pdf", realjson), "Input 'pdf' not a pdf.")
})

test_that("draw_tokens() produces a correctly named new png file", {

  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")

  realpdf <- testthat::test_path("examples", "sample.pdf")
  realjson <- testthat::test_path("examples", "output.json")

  pages <- pdftools::pdf_info(realpdf)$pages
  expected_message <- glue::glue("Generated {pages} annotated image*")
  message <- capture.output(draw_tokens(realpdf, realjson), type = "message")
  expect_match(message, expected_message)

  expected_filepaths <- character()
  for (i in pages) {
    fname <- glue::glue("{stringr::str_sub(basename(realpdf), end=-5)}{i}_tokens.png")
    fpath <- file.path(tempdir(), fname)
    expected_filepaths <- c(expected_filepaths, fpath)
  }

  for (j in 1:length(expected_filepaths)) {
    expect_true(file.exists(expected_filepaths[j]))
  }

  unlink(expected_filepaths, force = TRUE)
})

test_that("draw_tokens() draws actual boxes on the png", {
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")

  realpdf <- testthat::test_path("examples", "sample.pdf")
  realjson <- testthat::test_path("examples", "output.json")
  draw_tokens(realpdf, realjson)

  filename <- glue::glue("{stringr::str_sub(basename(realpdf), end=-5)}1_tokens.png")
  filepath <- file.path(tempdir(), filename)

  expect_snapshot_file(filepath, basename(filepath))

  unlink(filepath, force = TRUE)
})
