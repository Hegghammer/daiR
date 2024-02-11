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

## GET_TEXT ----------------------------------------

test_that("get_text() warns of input errors", {
  expect_error(get_text(null), "Input is not a valid HTTP response.")
  expect_error(get_text(na), "Input is not a valid HTTP response.")
  expect_error(get_text(boolean), "Input is not a valid HTTP response.")
  expect_error(get_text(number_random), "Input is not a valid HTTP response.")
  expect_error(get_text(string_random), "Input is not a valid HTTP response.")
  expect_error(get_text(vector_strings), "Input is not a valid HTTP response.")
  expect_error(get_text(list_strings), "Input is not a valid HTTP response.")
  expect_error(get_text(df), "Input is not a valid HTTP response.")
  expect_error(get_text(matrix), "Input is not a valid HTTP response.")  
  expect_error(get_text(string_random, type = "async"), "Input file not .json. Is the file in your working directory?")
  expect_error(get_text("wrong.txt", type = "async"), "Input file not .json. Is the file in your working directory?")
  expect_error(get_text("fake.json", type = "async"), "Input file not .json. Is the file in your working directory?")
})

test_that("get_text() warns of response not containing text", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  wrong <- dai_async("random.pdf")
  expect_error(get_text(wrong), "Input not recognized. Is it from dai_async?")
  blank <- dai_sync(testthat::test_path("examples", "blank.tiff"))
  expect_error(get_text(blank), "DAI found no text. Was the page blank?")
})

test_that("get_text() gets text from DAI response from example file", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  file <- testthat::test_path("examples", "image.jpg")
  response <- dai_sync(file)
  text <- get_text(response)
  expect_type(text, "character")
})

test_that("get_text() warns of file not containing text", {
  expect_error(get_text(madeup_json_file, type = "async"), "JSON not in right format. Is it from DAI?")
  blank <- testthat::test_path("examples", "output_blank.json")
  expect_error(get_text(blank, type = "async"), "DAI found no text. Was the document blank?")
})

test_that("get_text() gets text from example json file", {
  sample <- testthat::test_path("examples", "output.json")
  text <- get_text(sample, type = "async")
  expect_type(text, "character")
})

## GET_TABLES ----------------------------------------

test_that("get_tables() warns of input errors", {
  expect_error(get_tables(null), "The supplied object is not a valid HTTP response. Did you supply a json filepath without type = 'async'?")
  expect_error(get_tables(na), "The supplied object is not a valid HTTP response. Did you supply a json filepath without type = 'async'?")
  expect_error(get_tables(boolean), "The supplied object is not a valid HTTP response. Did you supply a json filepath without type = 'async'?")
  expect_error(get_tables(number_random), "The supplied object is not a valid HTTP response. Did you supply a json filepath without type = 'async'?")
  expect_error(get_tables(string_random), "The supplied object is not a valid HTTP response. Did you supply a json filepath without type = 'async'?")
  expect_error(get_tables(vector_strings), "The supplied object is not a valid HTTP response. Did you supply a json filepath without type = 'async'?")
  expect_error(get_tables(list_strings), "The supplied object is not a valid HTTP response. Did you supply a json filepath without type = 'async'?")
  expect_error(get_tables(df), "The supplied object is not a valid HTTP response. Did you supply a json filepath without type = 'async'?")
  expect_error(get_tables(matrix), "The supplied object is not a valid HTTP response. Did you supply a json filepath without type = 'async'?") 
  expect_error(get_tables(string_random, type = "async"), "Input file not .json. Is the file in your working directory?")
  expect_error(get_tables("wrong.txt", type = "async"), "Input file not .json. Is the file in your working directory?")
  expect_error(get_tables("fake.json", type = "async"), "Input file not .json. Is the file in your working directory?")
})

test_that("get_tables() warns of response not containing text", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  wrong <- dai_async("random.pdf")
  expect_error(get_tables(wrong), "The supplied object is not from a successful HTTP request.")
  blank <- dai_sync(testthat::test_path("examples", "blank.tiff"))
  expect_error(get_tables(blank), "DAI found no text. Was the page blank?")
})

test_that("get_tables() warns of file not containing text", {
  expect_error(get_tables(madeup_json_file, type = "async"), "JSON not in right format. Is it from DAI?")
  blank <- testthat::test_path("examples", "output_blank.json")
  expect_error(get_tables(blank, type = "async"), "DAI found no text. Was the document blank?")
  unlink(madeup_json_file, force = TRUE)
})

test_that("get_tables() gets tables from example response", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  procs <- get_processors()
  form_proc_id <- procs$id[procs$type == "FORM_PARSER_PROCESSOR"][1]
  file <- testthat::test_path("examples", "table.pdf")
  response <- dai_sync(file, proc_id = form_proc_id)
  tbls <- get_tables(response)
  expect_true(is.data.frame(tbls[[1]]))
})

test_that("get_tables() gets text from example json file", {
  sample <- testthat::test_path("examples", "table_form_parsed.json")
  tbls <- get_tables(sample, type = "async")
  expect_true(is.data.frame(tbls[[1]]))
})

## GET_ENTITIES ----------------------------------------

test_that("get_entities() warns of input errors", {
  expect_error(get_entities(null), "Object parameter not pointing to valid response object.")
  expect_error(get_entities(na), "Object parameter not pointing to valid response object.")
  expect_error(get_entities(boolean), "Object parameter not pointing to valid response object.")
  expect_error(get_entities(number_random), "Object parameter not pointing to valid response object.")
  expect_error(get_entities(string_random), "Object parameter not pointing to valid response object.")
  expect_error(get_entities(vector_strings), "Object parameter not pointing to valid response object.")
  expect_error(get_entities(list_strings), "Object parameter not pointing to valid response object.")
  expect_error(get_entities(df), "Object parameter not pointing to valid response object.")
  expect_error(get_entities(matrix), "Object parameter not pointing to valid response object.")  
  expect_error(get_entities(string_random, type = "async"), "Object parameter not pointing to valid JSON file.")
  expect_error(get_entities("wrong.txt", type = "async"), "Object parameter not pointing to valid JSON file.")
  expect_error(get_entities("fake.json", type = "async"), "Object parameter not pointing to valid JSON file.")
})

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
  expect_error(get_entities(madeup_json_file, type = "async"), "Object parameter not pointing to valid JSON file.")
  blank <- testthat::test_path("examples", "output_blank.json")
  expect_error(get_entities(blank, type = "async"), "DAI found no text. Was the document blank?")
})

test_that("get_entities() gets entities from example response", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  procs <- get_processors()
  form_proc_id <- procs$id[procs$type == "FORM_PARSER_PROCESSOR"][1]
  file <- testthat::test_path("examples", "table.pdf")
  response <- dai_sync(file, proc_id = form_proc_id)
  ents <- get_entities(response)
  expect_true(is.data.frame(ents[[1]]))
})

test_that("get_entities() gets entities from example file", {
  sample <- testthat::test_path("examples", "table_form_parsed.json")
  ents <- get_entities(sample, type = "async")
  expect_true(is.data.frame(ents[[1]]))
})

## CLEANUP ----------------------------------------
unlink(madeup_json_file, force = TRUE)
