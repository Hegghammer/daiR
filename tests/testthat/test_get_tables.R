## TABLES_FROM_DAI_RESPONSE ----------------------------------------------------

test_that("tables_from_dai_response() warns of input errors", {
  expect_error(tables_from_dai_response(NULL), "Object is not a valid HTTP response.")
  expect_error(tables_from_dai_response(12345), "Object is not a valid HTTP response.")
  expect_error(tables_from_dai_response(mtcars), "Object is not a valid HTTP response.")
  expect_error(tables_from_dai_response(as.matrix(mtcars)), "Object is not a valid HTTP response.")
  expect_error(tables_from_dai_response("string"), "Object is not a valid HTTP response.")
  expect_error(tables_from_dai_response(c("string", "vector")), "Object is not a valid HTTP response.")
  expect_error(tables_from_dai_response(list("a", "list")), "Object is not a valid HTTP response.")
})

test_that("tables_from_dai_response() warns of response not containing tables", {
  skip_if_no_token()
  skip_if_offline()

  wrong <- dai_user()
  expect_error(tables_from_dai_response(wrong), "Object not a positive dai_sync response.")

  blank <- dai_sync(testthat::test_path("examples", "blank.tiff"))
  expect_error(tables_from_dai_response(blank), "DAI found no text. Was the page blank?")

  no_tables <- dai_sync(testthat::test_path("examples", "sample.pdf"))
  expect_error(tables_from_dai_response(no_tables), "DAI found no tables in the document.")
})

test_that("tables_from_dai_response() returns dataframes from dai response containing tables", {
  skip_if_no_token()
  skip_if_offline()

  file <- testthat::test_path("examples", "table.pdf")
  response <- dai_sync(file)
  tables <- tables_from_dai_response(response)
  expect_true(is.list(tables))
  expect_true(all(sapply(tables, is.data.frame)))
})

## TABLES_FROM_DAI_FILE --------------------------------------------------------

test_that("tables_from_dai_file() warns of input errors", {
  expect_error(tables_from_dai_file(NULL), "Invalid file input.")
  expect_error(tables_from_dai_file(12345), "Invalid file input.")
  expect_error(tables_from_dai_file(mtcars), "Invalid file input.")
  expect_error(tables_from_dai_file(as.matrix(mtcars)), "Invalid file input.")
  expect_error(tables_from_dai_file(c("string", "vector")), "Invalid file input.")
  expect_error(tables_from_dai_file(list("a", "list")), "Invalid file input.")
  expect_error(tables_from_dai_file("wrong.txt"), "Input file not .json. Is the file in your working directory?")
  expect_error(tables_from_dai_file("fake.json"), "Input file not .json. Is the file in your working directory?")
})

test_that("tables_from_dai_file() warns of file not containing tables", {
  random <- list("a" = 1, "b" = 2)
  json <- jsonlite::toJSON(random)
  madeup <- tempfile(fileext = ".json")
  write(json, madeup)
  expect_error(tables_from_dai_file(madeup), "JSON not in right format. Is it from DAI?")

  blank <- testthat::test_path("examples", "output_blank.json")
  expect_error(tables_from_dai_file(blank), "DAI found no text. Was the document blank?")
  unlink(madeup, force = TRUE)

  no_tables <- testthat::test_path("examples", "output.json")
  expect_error(tables_from_dai_file(no_tables), "DAI found no tables in the document.")
})

test_that("tables_from_dai_file() reads real dai output file with tables", {
  sample <- testthat::test_path("examples", "table_output.json")
  tables <- tables_from_dai_file(sample)
  expect_true(is.list(tables))
  expect_true(all(sapply(tables, is.data.frame)))
})
