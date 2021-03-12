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
  expect_error(build_token_df(blank), "DAI found no tokens. Was the document blank?")
  unlink(madeup, force = TRUE)
})

test_that("build_token_df() builds a token dataframe", {

  json <- testthat::test_path("examples", "output.json")

  # is df
  df <- build_token_df(json)
  expect_true(is.data.frame(df))

  # has right properties
  expect_equal(ncol(df), 9)
  expect_setequal(colnames(df), c("token", "start_ind", "end_ind", "left", "right", "top", "bottom", "page", "block"))
  expect_true(is.character(df$token))
  expect_true(is.numeric(df$left))
  expect_lt(max(df$right, na.rm = TRUE), 1)
  expect_gt(min(df$top, na.rm = TRUE), 0)
  jsonlist <- jsonlite::fromJSON(json)
  expect_equal(max(df$page), nrow(jsonlist[["pages"]]))
  words <- ngram::wordcount(text_from_dai_file(json))
  expect_lt(nrow(df), 1.5*words)
  expect_gt(nrow(df), 0.5*words)
  expect_false(is.unsorted(df$start_ind))
})

## BUILD_BLOCK_DF --------------------------------------------------------------

test_that("build_block_df() warns of input errors", {

  expect_error(build_block_df(NULL), "Input file not .json.")
  expect_error(build_block_df(12345), "Input file not .json.")
  expect_error(build_block_df(mtcars), "Input file not .json.")
  expect_error(build_block_df(as.matrix(mtcars)), "Input file not .json.")
  expect_error(build_block_df("string"), "Input file not .json.")
  expect_error(build_block_df(c("string", "vector")), "Input file not .json.")
  expect_error(build_block_df(list("a", "list")), "Input file not .json.")
  expect_error(build_block_df("wrongfile.csv"), "Input file not .json.")
  expect_error(build_block_df("madeup.json"), "Input file not .json.")

})

test_that("build_token_df() warns of files not containing blocks", {

  # Wrong type of json
  random <- list("a" = 1, "b" = 2)
  json <- jsonlite::toJSON(random)
  madeup <- tempfile(fileext = ".json")
  write(json, madeup)
  expect_error(build_block_df(madeup), "JSON not in right format. Is it from DAI?")

  # from DAI but blank
  blank <- testthat::test_path("examples", "output_blank.json")
  expect_error(build_block_df(blank), "DAI found no blocks. Was the document blank?")
  unlink(madeup, force = TRUE)
})

test_that("build_block_df() builds a block dataframe", {

  json <- testthat::test_path("examples", "output.json")

  # is df
  df <- build_block_df(json)
  expect_true(is.data.frame(df))

  # has right properties
  expect_equal(ncol(df), 6)
  expect_setequal(colnames(df), c("page", "block", "left", "right", "top", "bottom"))
  expect_true(is.numeric(df$left))
  expect_lt(max(df$right, na.rm = TRUE), 1)
  expect_gt(min(df$top, na.rm = TRUE), 0)
  jsonlist <- jsonlite::fromJSON(json)
  expect_equal(max(df$page), nrow(jsonlist[["pages"]]))
  expect_false(is.unsorted(df$block))
})

## SPLIT_BLOCK -----------------------------------------------------------------

test_that("split_block() warns of input errors", {
  expect_error(split_block(df = NULL), "Input not a data frame.")
  expect_error(split_block(df = 12345), "Input not a data frame.")
  expect_error(split_block(df = "string"), "Input not a data frame.")
  expect_error(split_block(df = c(1,2)), "Input not a data frame.")
  expect_error(split_block(df = c("string", "vector")), "Input not a data frame.")
  expect_error(split_block(df = list("a", "list")), "Input not a data frame.")
  expect_error(split_block(df = as.matrix(mtcars)), "Input not a data frame.")
  expect_error(split_block(df = mtcars), "Dataframe not recognized. Was it made with build_block_df?")

  json <- testthat::test_path("examples", "output.json")
  df <- build_block_df(json)
  expect_error(split_block(df = df, page = 1-2), "Invalid page parameter.")
  expect_error(split_block(df = df, page = "one"), "Invalid page parameter.")
  expect_error(split_block(df = df, page = c(1,2)), "Invalid page parameter.")
  expect_error(split_block(df = df, page = 10), "No such page number in this dataframe.")
  expect_error(split_block(df = df, block = 1-2), "Invalid block parameter.")
  expect_error(split_block(df = df, block = "one"), "Invalid block parameter.")
  expect_error(split_block(df = df, block = c(1,2)), "Invalid block parameter.")
  expect_error(split_block(df = df, block = 50), "No such block number on this page.")
  expect_error(split_block(df = df, block = 1, cut_point = "middle"), "Invalid cut point parameter.")
  expect_error(split_block(df = df, block = 1, cut_point = 150), "Cut point out of range.")
  expect_error(split_block(df = df, block = 1, cut_point = 50, direction = 1), "Invalid direction parameter.")
  expect_error(split_block(df = df, block = 1, cut_point = 50, direction = "horizontal"), 'Split direction must be either "h" or "v".')
})


## REASSIGN_TOKENS -------------------------------------------------------------

test_that("reassign_tokens() warns of input errors", {


})

## REASSIGN_TOKENS2 ------------------------------------------------------------

test_that("reassign_tokens2() warns of input errors", {


})
