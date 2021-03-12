## BUILD_TOKEN_DF --------------------------------------------------------------

test_that("build_token_df() warns of input errors", {

  expect_error(build_token_df(NULL), "Input file not .json.")
  expect_error(build_token_df(12345), "Input file not .json.")
  expect_error(build_token_df(mtcars), "Input file not .json.")
  expect_error(build_token_df(as.matrix(mtcars)), "Input file not .json.")
  expect_error(build_token_df("string"), "Input file not .json.")
  expect_error(build_token_df(c("string", "vector")), "Input file not .json.")
  expect_error(build_token_df(list("a", "list")), "Input file not .json.")
  expect_error(build_token_df("wrongfile.csv"), "Input file not .json.")
  expect_error(build_token_df("madeup.json"), "Input file not .json.")
})

test_that("build_token_df() warns of files not containing tokens", {

  # Wrong type of json
  random <- list("a" = 1, "b" = 2)
  json <- jsonlite::toJSON(random)
  madeup <- tempfile(fileext = ".json")
  write(json, madeup)
  expect_error(build_token_df(madeup), "JSON not in right format. Is it from DAI?")

  # from DAI but blank
  blank <- testthat::test_path("examples", "output_blank.json")
  expect_error(text_from_dai_file(blank), "DAI found no tokens. Was the document blank?")
  unlink(madeup, force = TRUE)
})

test_that("build_token_df() builds a token dataframe", {

  json <- testthat::test_path("examples", "output.json")
  # is df
  output <- build_token_df(json)
  expect_true(is.data.frame(output))

  # has right properties

})


## BUILD_BLOCK_DF --------------------------------------------------------------

test_that("build_block_df() warns of input errors", {


})


## SPLIT_BLOCK -----------------------------------------------------------------

test_that("split_block() warns of input errors", {


})

## REASSIGN_TOKENS -------------------------------------------------------------

test_that("reassign_tokens() warns of input errors", {


})

## REASSIGN_TOKENS2 ------------------------------------------------------------

test_that("reassign_tokens2() warns of input errors", {


})
