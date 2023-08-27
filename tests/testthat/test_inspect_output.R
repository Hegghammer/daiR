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

# correct but irrelevant JSON file
fill <- list("a" = 1, "b" = 2) 
json <- jsonlite::toJSON(fill)
madeup_json_file <- tempfile(fileext = ".json")
write(json, madeup_json_file)

## TEXT_FROM_DAI_RESPONSE ------------------------------------------------------

test_that("text_from_dai_response() warns of input errors", {
  expect_error(text_from_dai_response(null), "Input is not a valid HTTP response.")
  expect_error(text_from_dai_response(na), "Input is not a valid HTTP response.")
  expect_error(text_from_dai_response(boolean), "Input is not a valid HTTP response.")
  expect_error(text_from_dai_response(number_random), "Input is not a valid HTTP response.")
  expect_error(text_from_dai_response(string_random), "Input is not a valid HTTP response.")
  expect_error(text_from_dai_response(vector_strings), "Input is not a valid HTTP response.")
  expect_error(text_from_dai_response(list_strings), "Input is not a valid HTTP response.")
  expect_error(text_from_dai_response(df), "Input is not a valid HTTP response.")
  expect_error(text_from_dai_response(matrix), "Input is not a valid HTTP response.")
})

test_that("text_from_dai_response() warns of response not containing text", {
  skip_on_cran()
  skip_on_ci()
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
  skip_if_offline()
  file <- testthat::test_path("examples", "image.jpg")
  response <- dai_sync(file)
  text <- text_from_dai_response(response)
  expect_type(text, "character")
})

## TEXT_FROM_DAI_FILE ----------------------------------------------------------

test_that("text_from_dai_file() warns of input errors", {
  expect_error(text_from_dai_file(null), "Invalid file input.")
  expect_error(text_from_dai_file(na), "Invalid file input.")
  expect_error(text_from_dai_file(boolean), "Invalid file input.")
  expect_error(text_from_dai_file(number_random), "Invalid file input.")
  expect_error(text_from_dai_file(vector_strings), "Invalid file input.")
  expect_error(text_from_dai_file(list_strings), "Invalid file input.")
  expect_error(text_from_dai_file(df), "Invalid file input.")
  expect_error(text_from_dai_file(matrix), "Invalid file input.")
  expect_error(text_from_dai_file(string_random), "Input file not .json. Is the file in your working directory?")
  expect_error(text_from_dai_file("wrong.txt"), "Input file not .json. Is the file in your working directory?")
  expect_error(text_from_dai_file("fake.json"), "Input file not .json. Is the file in your working directory?")
})

test_that("text_from_dai_file() warns of file not containing text", {
  expect_error(text_from_dai_file(madeup_json_file), "JSON not in right format. Is it from DAI?")
  blank <- testthat::test_path("examples", "output_blank.json")
  expect_error(text_from_dai_file(blank), "DAI found no text. Was the document blank?")
  unlink(madeup_json_file, force = TRUE)
})

test_that("text_from_dai_file() gets text from example json file", {
  sample <- testthat::test_path("examples", "output.json")
  text <- text_from_dai_file(sample)
  expect_type(text, "character")
})

## DRAW_BLOCKS -----------------------------------------------------------------

test_that("draw_blocks() warns of input errors", {
  expect_error(draw_blocks(type = null), "Invalid type parameter.")
  expect_error(draw_blocks(type = na), "Invalid type parameter.")
  expect_error(draw_blocks(type = boolean), "Invalid type parameter.")
  expect_error(draw_blocks(type = number_random), "Invalid type parameter.")
  expect_error(draw_blocks(type = string_random), "Invalid type parameter.")
  expect_error(draw_blocks(type = df), "Invalid type parameter.")
  expect_error(draw_blocks(type = matrix), "Invalid type parameter.")
  expect_error(draw_blocks(type = vector_strings), "Invalid type parameter.")
  expect_error(draw_blocks(type = list_strings), "Invalid type parameter.")

  expect_error(draw_blocks(type = "sync", output = null), "Invalid output parameter.")
  expect_error(draw_blocks(type = "sync", output = na), "Invalid output parameter.")
  expect_error(draw_blocks(type = "sync", output = boolean), "Invalid output parameter.")
  expect_error(draw_blocks(type = "sync", output = number_random), "Invalid output parameter.")
  expect_error(draw_blocks(type = "sync", output = df), "Invalid output parameter.")
  expect_error(draw_blocks(type = "sync", output = matrix), "Invalid output parameter.")
  expect_error(draw_blocks(type = "sync", output = vector_strings), "Invalid output parameter.")
  expect_error(draw_blocks(type = "sync", output = list_strings), "Invalid output parameter.")

  realjson <- testthat::test_path("examples", "sample_v1.json")  
  expect_error(draw_blocks(type = "async", output = realjson, doc = number_random), "Invalid doc parameter.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = df), "Invalid doc parameter.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = matrix), "Invalid doc parameter.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = vector_strings), "Invalid doc parameter.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = list_strings), "Invalid doc parameter.")
  
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = number_random), "Invalid prefix parameter.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = df), "Invalid prefix parameter.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = matrix), "Invalid prefix parameter.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = vector_strings), "Invalid prefix parameter.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = list_strings), "Invalid prefix parameter.")

  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "test", dir = null),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "test", dir = na),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "test", dir = boolean),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "test", dir = number_random),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = df),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = matrix),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = vector_strings),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = list_strings),
               "Invalid dir parameter. Must be a valid folder path.")

  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = null),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = na),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = boolean),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = string_random),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = vector_strings),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = list_strings),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = df),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = matrix),
               "Invalid linecol parameter. Must be a single valid colour representation.")

  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = null),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = na),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = boolean),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = string_random),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = vector_strings),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = list_strings),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = df),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = matrix),
               "Invalid linewd parameter. Must be a single number.")

  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = null),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = na),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = boolean),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = string_random),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = vector_strings),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = list_strings),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = df),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = matrix),
               "Invalid fontcol parameter. Must be a single valid colour representation.")

  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = null),
               "Invalid fontsize parameter. Must be a single number.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = na),
               "Invalid fontsize parameter. Must be a single number.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = boolean),
               "Invalid fontsize parameter. Must be a single number.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = string_random),
               "Invalid fontsize parameter. Must be a single number.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = vector_strings),
               "Invalid fontsize parameter. Must be a single number.")  
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = list_strings),
               "Invalid fontsize parameter. Must be a single number.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = df),
               "Invalid fontsize parameter. Must be a single number.")
  expect_error(draw_blocks(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = matrix),
               "Invalid fontsize parameter. Must be a single number.")
  })

test_that("draw_blocks() produces a correctly named png file", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  realjson <- testthat::test_path("examples", "sample_v1.json")
  draw_blocks(type = "async", output = realjson, dir = tempdir())
  parsed <- jsonlite::fromJSON(realjson)
  pages <- parsed[["pages"]][["pageNumber"]]
  expected_filepaths <- character()
  stem <- substr(basename(realjson), 1, nchar(basename(realjson)) -5)
  for (i in pages) {
    fname <- glue::glue("{stem}_page{i}_blocks.png")
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
  expect_error(draw_paragraphs(type = null), "Invalid type parameter.")
  expect_error(draw_paragraphs(type = na), "Invalid type parameter.")
  expect_error(draw_paragraphs(type = boolean), "Invalid type parameter.")
  expect_error(draw_paragraphs(type = number_random), "Invalid type parameter.")
  expect_error(draw_paragraphs(type = string_random), "Invalid type parameter.")
  expect_error(draw_paragraphs(type = df), "Invalid type parameter.")
  expect_error(draw_paragraphs(type = matrix), "Invalid type parameter.")
  expect_error(draw_paragraphs(type = vector_strings), "Invalid type parameter.")
  expect_error(draw_paragraphs(type = list_strings), "Invalid type parameter.")

  expect_error(draw_paragraphs(type = "sync", output = null), "Invalid output parameter.")
  expect_error(draw_paragraphs(type = "sync", output = na), "Invalid output parameter.")
  expect_error(draw_paragraphs(type = "sync", output = boolean), "Invalid output parameter.")
  expect_error(draw_paragraphs(type = "sync", output = number_random), "Invalid output parameter.")
  expect_error(draw_paragraphs(type = "sync", output = df), "Invalid output parameter.")
  expect_error(draw_paragraphs(type = "sync", output = matrix), "Invalid output parameter.")
  expect_error(draw_paragraphs(type = "sync", output = vector_strings), "Invalid output parameter.")
  expect_error(draw_paragraphs(type = "sync", output = list_strings), "Invalid output parameter.")

  realjson <- testthat::test_path("examples", "sample_v1.json")  
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = number_random), "Invalid doc parameter.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = df), "Invalid doc parameter.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = matrix), "Invalid doc parameter.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = vector_strings), "Invalid doc parameter.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = list_strings), "Invalid doc parameter.")
  
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = number_random), "Invalid prefix parameter.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = df), "Invalid prefix parameter.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = matrix), "Invalid prefix parameter.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = vector_strings), "Invalid prefix parameter.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = list_strings), "Invalid prefix parameter.")

  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "test", dir = null),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "test", dir = na),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "test", dir = boolean),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "test", dir = number_random),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = df),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = matrix),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = vector_strings),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = list_strings),
               "Invalid dir parameter. Must be a valid folder path.")

  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = null),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = na),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = boolean),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = string_random),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = vector_strings),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = list_strings),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = df),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = matrix),
               "Invalid linecol parameter. Must be a single valid colour representation.")

  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = null),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = na),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = boolean),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = string_random),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = vector_strings),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = list_strings),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = df),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = matrix),
               "Invalid linewd parameter. Must be a single number.")

  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = null),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = na),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = boolean),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = string_random),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = vector_strings),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = list_strings),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = df),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = matrix),
               "Invalid fontcol parameter. Must be a single valid colour representation.")

  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = null),
               "Invalid fontsize parameter. Must be a single number.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = na),
               "Invalid fontsize parameter. Must be a single number.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = boolean),
               "Invalid fontsize parameter. Must be a single number.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = string_random),
               "Invalid fontsize parameter. Must be a single number.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = vector_strings),
               "Invalid fontsize parameter. Must be a single number.")  
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = list_strings),
               "Invalid fontsize parameter. Must be a single number.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = df),
               "Invalid fontsize parameter. Must be a single number.")
  expect_error(draw_paragraphs(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = matrix),
               "Invalid fontsize parameter. Must be a single number.")
})

test_that("draw_paragraphs() produces a correctly named new png file", {
  skip_on_cran()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  realjson <- testthat::test_path("examples", "sample_v1.json")
  draw_paragraphs(type = "async", output = realjson, dir = tempdir())
  parsed <- jsonlite::fromJSON(realjson)
  pages <- parsed[["pages"]][["pageNumber"]]
  expected_filepaths <- character()
  stem <- substr(basename(realjson), 1, nchar(basename(realjson)) -5)
  for (i in pages) {
    fname <- glue::glue("{stem}_page{i}_paragraphs.png")
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
  expect_error(draw_lines(type = null), "Invalid type parameter.")
  expect_error(draw_lines(type = na), "Invalid type parameter.")
  expect_error(draw_lines(type = boolean), "Invalid type parameter.")
  expect_error(draw_lines(type = number_random), "Invalid type parameter.")
  expect_error(draw_lines(type = string_random), "Invalid type parameter.")
  expect_error(draw_lines(type = df), "Invalid type parameter.")
  expect_error(draw_lines(type = matrix), "Invalid type parameter.")
  expect_error(draw_lines(type = vector_strings), "Invalid type parameter.")
  expect_error(draw_lines(type = list_strings), "Invalid type parameter.")

  expect_error(draw_lines(type = "sync", output = null), "Invalid output parameter.")
  expect_error(draw_lines(type = "sync", output = na), "Invalid output parameter.")
  expect_error(draw_lines(type = "sync", output = boolean), "Invalid output parameter.")
  expect_error(draw_lines(type = "sync", output = number_random), "Invalid output parameter.")
  expect_error(draw_lines(type = "sync", output = df), "Invalid output parameter.")
  expect_error(draw_lines(type = "sync", output = matrix), "Invalid output parameter.")
  expect_error(draw_lines(type = "sync", output = vector_strings), "Invalid output parameter.")
  expect_error(draw_lines(type = "sync", output = list_strings), "Invalid output parameter.")

  realjson <- testthat::test_path("examples", "sample_v1.json")  
  expect_error(draw_lines(type = "async", output = realjson, doc = number_random), "Invalid doc parameter.")
  expect_error(draw_lines(type = "async", output = realjson, doc = df), "Invalid doc parameter.")
  expect_error(draw_lines(type = "async", output = realjson, doc = matrix), "Invalid doc parameter.")
  expect_error(draw_lines(type = "async", output = realjson, doc = vector_strings), "Invalid doc parameter.")
  expect_error(draw_lines(type = "async", output = realjson, doc = list_strings), "Invalid doc parameter.")
  
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = number_random), "Invalid prefix parameter.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = df), "Invalid prefix parameter.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = matrix), "Invalid prefix parameter.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = vector_strings), "Invalid prefix parameter.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = list_strings), "Invalid prefix parameter.")

  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "test", dir = null),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "test", dir = na),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "test", dir = boolean),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "test", dir = number_random),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = df),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = matrix),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = vector_strings),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = list_strings),
               "Invalid dir parameter. Must be a valid folder path.")

  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = null),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = na),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = boolean),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = string_random),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = vector_strings),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = list_strings),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = df),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = matrix),
               "Invalid linecol parameter. Must be a single valid colour representation.")

  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = null),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = na),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = boolean),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = string_random),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = vector_strings),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = list_strings),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = df),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = matrix),
               "Invalid linewd parameter. Must be a single number.")

  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = null),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = na),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = boolean),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = string_random),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = vector_strings),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = list_strings),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = df),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = matrix),
               "Invalid fontcol parameter. Must be a single valid colour representation.")

  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = null),
               "Invalid fontsize parameter. Must be a single number.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = na),
               "Invalid fontsize parameter. Must be a single number.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = boolean),
               "Invalid fontsize parameter. Must be a single number.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = string_random),
               "Invalid fontsize parameter. Must be a single number.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = vector_strings),
               "Invalid fontsize parameter. Must be a single number.")  
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = list_strings),
               "Invalid fontsize parameter. Must be a single number.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = df),
               "Invalid fontsize parameter. Must be a single number.")
  expect_error(draw_lines(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = matrix),
               "Invalid fontsize parameter. Must be a single number.")
})

test_that("draw_lines() produces a correctly named new png file", {
  skip_on_cran()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  realjson <- testthat::test_path("examples", "sample_v1.json")
  draw_lines(type = "async", output = realjson, dir = tempdir())
  parsed <- jsonlite::fromJSON(realjson)
  pages <- parsed[["pages"]][["pageNumber"]]
  expected_filepaths <- character()
  stem <- substr(basename(realjson), 1, nchar(basename(realjson)) -5)
  for (i in pages) {
    fname <- glue::glue("{stem}_page{i}_lines.png")
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
  expect_error(draw_tokens(type = null), "Invalid type parameter.")
  expect_error(draw_tokens(type = na), "Invalid type parameter.")
  expect_error(draw_tokens(type = boolean), "Invalid type parameter.")
  expect_error(draw_tokens(type = number_random), "Invalid type parameter.")
  expect_error(draw_tokens(type = string_random), "Invalid type parameter.")
  expect_error(draw_tokens(type = df), "Invalid type parameter.")
  expect_error(draw_tokens(type = matrix), "Invalid type parameter.")
  expect_error(draw_tokens(type = vector_strings), "Invalid type parameter.")
  expect_error(draw_tokens(type = list_strings), "Invalid type parameter.")

  expect_error(draw_tokens(type = "sync", output = null), "Invalid output parameter.")
  expect_error(draw_tokens(type = "sync", output = na), "Invalid output parameter.")
  expect_error(draw_tokens(type = "sync", output = boolean), "Invalid output parameter.")
  expect_error(draw_tokens(type = "sync", output = number_random), "Invalid output parameter.")
  expect_error(draw_tokens(type = "sync", output = df), "Invalid output parameter.")
  expect_error(draw_tokens(type = "sync", output = matrix), "Invalid output parameter.")
  expect_error(draw_tokens(type = "sync", output = vector_strings), "Invalid output parameter.")
  expect_error(draw_tokens(type = "sync", output = list_strings), "Invalid output parameter.")

  realjson <- testthat::test_path("examples", "sample_v1.json")  
  expect_error(draw_tokens(type = "async", output = realjson, doc = number_random), "Invalid doc parameter.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = df), "Invalid doc parameter.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = matrix), "Invalid doc parameter.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = vector_strings), "Invalid doc parameter.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = list_strings), "Invalid doc parameter.")
  
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = number_random), "Invalid prefix parameter.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = df), "Invalid prefix parameter.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = matrix), "Invalid prefix parameter.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = vector_strings), "Invalid prefix parameter.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = list_strings), "Invalid prefix parameter.")

  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "test", dir = null),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "test", dir = na),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "test", dir = boolean),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "test", dir = number_random),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = df),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = matrix),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = vector_strings),
               "Invalid dir parameter. Must be a valid folder path.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = list_strings),
               "Invalid dir parameter. Must be a valid folder path.")

  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = null),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = na),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = boolean),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = string_random),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = vector_strings),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = list_strings),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = df),
               "Invalid linecol parameter. Must be a single valid colour representation.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linecol = matrix),
               "Invalid linecol parameter. Must be a single valid colour representation.")

  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = null),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = na),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = boolean),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = string_random),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = vector_strings),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = list_strings),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = df),
               "Invalid linewd parameter. Must be a single number.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", linewd = matrix),
               "Invalid linewd parameter. Must be a single number.")

  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = null),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = na),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = boolean),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = string_random),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = vector_strings),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = list_strings),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = df),
               "Invalid fontcol parameter. Must be a single valid colour representation.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontcol = matrix),
               "Invalid fontcol parameter. Must be a single valid colour representation.")

  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = null),
               "Invalid fontsize parameter. Must be a single number.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = na),
               "Invalid fontsize parameter. Must be a single number.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = boolean),
               "Invalid fontsize parameter. Must be a single number.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = string_random),
               "Invalid fontsize parameter. Must be a single number.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = vector_strings),
               "Invalid fontsize parameter. Must be a single number.")  
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = list_strings),
               "Invalid fontsize parameter. Must be a single number.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = df),
               "Invalid fontsize parameter. Must be a single number.")
  expect_error(draw_tokens(type = "async", output = realjson, doc = "source.pdf", prefix = "myfile", dir = ".", fontsize = matrix),
               "Invalid fontsize parameter. Must be a single number.")
})

test_that("draw_tokens() produces a correctly named new png file", {
  skip_on_cran()
  skip_if_not_installed("grDevices")
  skip_if_not_installed("magick")
  realjson <- testthat::test_path("examples", "sample_v1.json")
  draw_tokens(type = "async", output = realjson, dir = tempdir())
  parsed <- jsonlite::fromJSON(realjson)
  pages <- parsed[["pages"]][["pageNumber"]]
  expected_filepaths <- character()
  stem <- substr(basename(realjson), 1, nchar(basename(realjson)) -5)
  for (i in pages) {
    fname <- glue::glue("{stem}_page{i}_tokens.png")
    fpath <- file.path(tempdir(), fname)
    expected_filepaths <- c(expected_filepaths, fpath)
  }
  for (j in 1:length(expected_filepaths)) {
    expect_true(file.exists(expected_filepaths[j]))
  }
  unlink(expected_filepaths, force = TRUE)
})
