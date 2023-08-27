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

## TABLES_FROM_DAI_RESPONSE ----------------------------------------------------

test_that("tables_from_dai_response() warns of input errors", {
  expect_error(tables_from_dai_response(null), "Object is not a valid HTTP response.")
  expect_error(tables_from_dai_response(na), "Object is not a valid HTTP response.")
  expect_error(tables_from_dai_response(boolean), "Object is not a valid HTTP response.")
  expect_error(tables_from_dai_response(number_random), "Object is not a valid HTTP response.")
  expect_error(tables_from_dai_response(string_random), "Object is not a valid HTTP response.")
  expect_error(tables_from_dai_response(vector_strings), "Object is not a valid HTTP response.")
  expect_error(tables_from_dai_response(list_strings), "Object is not a valid HTTP response.")
  expect_error(tables_from_dai_response(df), "Object is not a valid HTTP response.")
  expect_error(tables_from_dai_response(matrix), "Object is not a valid HTTP response.")
})

test_that("tables_from_dai_response() warns of response not containing tables", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  wrong <- dai_user()
  expect_error(tables_from_dai_response(wrong), "Object not a positive dai_sync response.")
  blank <- dai_sync_tab(testthat::test_path("examples", "blank.tiff"))
  expect_error(tables_from_dai_response(blank), "DAI found no text. Was the page blank?")
  no_tables <- dai_sync_tab(testthat::test_path("examples", "sample.pdf"))
  expect_error(tables_from_dai_response(no_tables), "DAI found no tables in the document.")
})

test_that("tables_from_dai_response() returns dataframes from dai response containing tables", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  file <- testthat::test_path("examples", "table.pdf")
  response <- dai_sync_tab(file)
  tables <- tables_from_dai_response(response)
  expect_true(is.list(tables))
  expect_true(all(sapply(tables, is.data.frame)))
})

## TABLES_FROM_DAI_FILE --------------------------------------------------------

test_that("tables_from_dai_file() warns of input errors", {
  expect_error(tables_from_dai_file(null), "Invalid file input.")
  expect_error(tables_from_dai_file(na), "Invalid file input.")
  expect_error(tables_from_dai_file(boolean), "Invalid file input.")
  expect_error(tables_from_dai_file(number_random), "Invalid file input.")
  expect_error(tables_from_dai_file(df), "Invalid file input.")
  expect_error(tables_from_dai_file(matrix), "Invalid file input.")
  expect_error(tables_from_dai_file(vector_strings), "Invalid file input.")
  expect_error(tables_from_dai_file(list_strings), "Invalid file input.")
  expect_error(tables_from_dai_file("wrong.txt"), "Input file not .json. Is the file in your working directory?")
  expect_error(tables_from_dai_file("fake.json"), "Input file not .json. Is the file in your working directory?")
})

test_that("tables_from_dai_file() warns of file not containing tables", {
  expect_error(tables_from_dai_file(madeup_json_file), "JSON not in right format. Is it from DAI?")
  blank <- testthat::test_path("examples", "output_blank.json")
  expect_error(tables_from_dai_file(blank), "DAI found no text. Was the document blank?")
  no_tables <- testthat::test_path("examples", "output.json")
  expect_error(tables_from_dai_file(no_tables), "DAI found no tables in the document.")
})

test_that("tables_from_dai_file() reads real dai output file with tables", {
  sample <- testthat::test_path("examples", "table_output.json")
  tables <- tables_from_dai_file(sample)
  expect_true(is.list(tables))
  expect_true(all(sapply(tables, is.data.frame)))
})

unlink(madeup_json_file, force = TRUE)
