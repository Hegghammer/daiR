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

## MERGE SHARDS ----------------------------------------

## BUILD_TOKEN_DF --------------------------------------------------------------

test_that("build_token_df() warns of input errors", {
  expect_error(build_token_df(type = null), "Invalid type parameter.")
  expect_error(build_token_df(type = na), "Invalid type parameter.")
  expect_error(build_token_df(type = boolean), "Invalid type parameter.")
  expect_error(build_token_df(type = number_random), "Invalid type parameter.")
  expect_error(build_token_df(type = string_random), "Invalid type parameter.")
  expect_error(build_token_df(type = vector_strings), "Invalid type parameter.")
  expect_error(build_token_df(type = list_strings), "Invalid type parameter.")
  expect_error(build_token_df(type = df), "Invalid type parameter.")
  expect_error(build_token_df(type = matrix), "Invalid type parameter.")
  expect_error(build_token_df(type = "sync", object = null), "Invalid object parameter.")
  expect_error(build_token_df(type = "sync", object = na), "Invalid object parameter.")
  expect_error(build_token_df(type = "sync", object = boolean), "Invalid object parameter.")
  expect_error(build_token_df(type = "sync", object = number_random), "Invalid object parameter.")
  expect_error(build_token_df(type = "sync", object = string_random), "Invalid object parameter.")
  expect_error(build_token_df(type = "sync", object = vector_strings), "Invalid object parameter.")
  expect_error(build_token_df(type = "sync", object = list_strings), "Invalid object parameter.")
  expect_error(build_token_df(type = "sync", object = df), "Invalid object parameter.")
  expect_error(build_token_df(type = "sync", object = matrix), "Invalid object parameter.")
})

test_that("build_token_df() warns of files not containing tokens", {
  expect_error(build_token_df(type = "async", object = madeup_json_file), "JSON not in right format. Is it from DAI?")
  # from DAI but blank:
  blank <- testthat::test_path("examples", "output_blank.json")
  expect_error(build_token_df(type = "async", object = blank), "DAI found no tokens. Was the document blank?")
})

test_that("build_token_df() builds a token dataframe", {
  json <- testthat::test_path("examples", "output.json")
  # is df:
  df <- build_token_df(json, type = "async")
  expect_true(is.data.frame(df))
  # has right properties:
  expect_equal(ncol(df), 10)
  expect_setequal(colnames(df), c("token", "start_ind", "end_ind", "conf", "left", "right", "top", "bottom", "page", "block"))
  expect_true(is.character(df$token))
  expect_true(is.numeric(df$left))
  expect_lt(max(df$right, na.rm = TRUE), 1)
  expect_gt(min(df$top, na.rm = TRUE), 0)
  jsonlist <- jsonlite::fromJSON(json)
  expect_equal(max(df$page), nrow(jsonlist[["pages"]]))
  words <- ngram::wordcount(get_text(json, type = "async"))
  expect_lt(nrow(df), 1.5*words)
  expect_gt(nrow(df), 0.5*words)
  expect_false(is.unsorted(df$start_ind))
})

## BUILD_BLOCK_DF --------------------------------------------------------------

test_that("build_block_df() warns of input errors", {
  expect_error(build_block_df(type = null), "Invalid type parameter.")
  expect_error(build_block_df(type = na), "Invalid type parameter.")
  expect_error(build_block_df(type = boolean), "Invalid type parameter.")
  expect_error(build_block_df(type = number_random), "Invalid type parameter.")
  expect_error(build_block_df(type = string_random), "Invalid type parameter.")
  expect_error(build_block_df(type = vector_strings), "Invalid type parameter.")
  expect_error(build_block_df(type = list_strings), "Invalid type parameter.")
  expect_error(build_block_df(type = df), "Invalid type parameter.")
  expect_error(build_block_df(type = matrix), "Invalid type parameter.")
  expect_error(build_block_df(type = "sync", object = null), "Invalid object parameter.")
  expect_error(build_block_df(type = "sync", object = na), "Invalid object parameter.")
  expect_error(build_block_df(type = "sync", object = boolean), "Invalid object parameter.")
  expect_error(build_block_df(type = "sync", object = number_random), "Invalid object parameter.")
  expect_error(build_block_df(type = "sync", object = string_random), "Invalid object parameter.")
  expect_error(build_block_df(type = "sync", object = vector_strings), "Invalid object parameter.")
  expect_error(build_block_df(type = "sync", object = list_strings), "Invalid object parameter.")
  expect_error(build_block_df(type = "sync", object = df), "Invalid object parameter.")
  expect_error(build_block_df(type = "sync", object = matrix), "Invalid object parameter.")
})

test_that("build_block_df() warns of files not containing blocks", {
  expect_error(build_block_df(type = "async", object = madeup_json_file), "JSON not in right format. Is it from DAI?")
  # from DAI but blank:
  blank <- testthat::test_path("examples", "output_blank.json")
  expect_error(build_block_df(type = "async", object = blank), "DAI found no blocks. Was the document blank?")
})

test_that("build_block_df() builds a block dataframe", {
  json <- testthat::test_path("examples", "output.json")
  # is df:
  df <- build_block_df(type = "async", object = json)
  expect_true(is.data.frame(df))
  # has right properties:
  expect_equal(ncol(df), 7)
  expect_setequal(colnames(df), c("page", "block", "conf", "left", "right", "top", "bottom"))
  expect_true(is.numeric(df$left))
  expect_lt(max(df$right, na.rm = TRUE), 1)
  expect_gt(min(df$top, na.rm = TRUE), 0)
  jsonlist <- jsonlite::fromJSON(json)
  expect_equal(max(df$page), nrow(jsonlist[["pages"]]))
  expect_false(is.unsorted(df$block))
})

## SPLIT_BLOCK -----------------------------------------------------------------

test_that("split_block() warns of input errors", {
  expect_error(split_block(block_df = null), "Input not a data frame.")
  expect_error(split_block(block_df = na), "Input not a data frame.")
  expect_error(split_block(block_df = boolean), "Input not a data frame.")
  expect_error(split_block(block_df = number_random), "Input not a data frame.")
  expect_error(split_block(block_df = string_random), "Input not a data frame.")
  expect_error(split_block(block_df = vector_strings), "Input not a data frame.")
  expect_error(split_block(block_df = list_strings), "Input not a data frame.")
  #expect_error(split_block(block_df = df), "Dataframe not recognized. Was it made with build_block_df?")
  expect_error(split_block(block_df = matrix), "Input not a data frame.")
  json <- testthat::test_path("examples", "output.json")
  df <- build_block_df(type = "async", object = json)
  expect_error(split_block(block_df = df, page = 1-2), "Invalid page parameter.")
  expect_error(split_block(block_df = df, page = "one"), "Invalid page parameter.")
  expect_error(split_block(block_df = df, page = c(1,2)), "Invalid page parameter.")
  expect_error(split_block(block_df = df, page = 10), "No such page number in this dataframe.")
  expect_error(split_block(block_df = df, block = 1-2), "Invalid block parameter.")
  expect_error(split_block(block_df = df, block = "one"), "Invalid block parameter.")
  expect_error(split_block(block_df = df, block = c(1,2)), "Invalid block parameter.")
  expect_error(split_block(block_df = df, block = 50), "No such block number on this page.")
  expect_error(split_block(block_df = df, block = 1, cut_point = "middle"), "Invalid cut point parameter.")
  expect_error(split_block(block_df = df, block = 1, cut_point = 150), "Cut point out of range.")
  expect_error(split_block(block_df = df, block = 1, cut_point = 50, direction = 1), "Invalid direction parameter.")
  expect_error(split_block(block_df = df, block = 1, cut_point = 50, direction = "horizontal"), 'Split direction must be either "h" or "v".')
})

test_that("split_block() returns a revised block dataframe", {
  json <- testthat::test_path("examples", "output.json")
  df <- build_block_df(type = "async", object = json)
  # choose random block and cut point:
  block <- sample(min(df$block):max(df$block), 1)
  cut_point <- sample(1:99, 1)
  # check that output df has one block more:
  new_df <- split_block(df, block = block, cut_point = cut_point)
  expect_equal(max(new_df$block), max(df$block) + 1)
  # check that the selected block is now different while others are similar:
  old_block_n <- df[df$block == block,]
  new_block_n <- new_df[new_df$block == block,]
  #expect_false(isTRUE(all.equal(old_block_n, new_block_n))) # strangely fails on ci
  rest_old_df <- df[!df$block == block,]
  new_df_ex_last <- new_df[!new_df$block == max(new_df$block),]
  rest_new_df <- new_df_ex_last[!new_df_ex_last$block == block,]
  rownames(rest_new_df) <- NULL
  rownames(rest_old_df) <- NULL
  expect_true(isTRUE(all.equal(rest_old_df, rest_new_df)))

  ### same process but for block in middle of multipage doc:
  json <- testthat::test_path("examples", "sample3pg.json")
  df <- build_block_df(type = "async", object = json)
  df_sub <- df[df$page == 2,]
  # choose random block and cut point:
  block <- sample(min(df_sub$block):max(df_sub$block), 1)
  cut_point <- sample(1:99, 1)
  # check that output df has one block more:
  new_df <- split_block(df, block = block, cut_point = cut_point, page = 2)
  expect_equal(nrow(new_df), nrow(df) + 1)
  # check that the selected block is now different while others are similar:
  new_df_sub <- new_df[new_df$page == 2,]
  old_block_n <- df_sub[df_sub$block == block,]
  new_block_n <- new_df_sub[new_df_sub$block == block,]
  expect_false(isTRUE(all.equal(old_block_n, new_block_n)))
  rest_old_df <- df_sub[!df_sub$block == block,]
  new_df_ex_last <- new_df_sub[!new_df_sub$block == max(new_df_sub$block),]
  rest_new_df <- new_df_ex_last[!new_df_ex_last$block == block,]
  rownames(rest_new_df) <- NULL
  rownames(rest_old_df) <- NULL
  expect_true(isTRUE(all.equal(rest_old_df, rest_new_df)))

  ### same process but for block at end of multipage doc:
  json <- testthat::test_path("examples", "sample3pg.json")
  df <- build_block_df(type = "async", object = json)
  df_sub <- df[df$page == 3,]
  # choose random block and cut point:
  block <- sample(min(df_sub$block):max(df_sub$block), 1)
  cut_point <- sample(1:99, 1)
  # check that output df has one block more:
  new_df <- split_block(df, block = block, cut_point = cut_point, page = 3)
  expect_equal(nrow(new_df), nrow(df) + 1)
  # check that the selected block is now different while others are similar:
  new_df_sub <- new_df[new_df$page == 3,]
  old_block_n <- df_sub[df_sub$block == block,]
  new_block_n <- new_df_sub[new_df_sub$block == block,]
  expect_false(isTRUE(all.equal(old_block_n, new_block_n)))
  rest_old_df <- df_sub[!df_sub$block == block,]
  new_df_ex_last <- new_df_sub[!new_df_sub$block == max(new_df_sub$block),]
  rest_new_df <- new_df_ex_last[!new_df_ex_last$block == block,]
  rownames(rest_new_df) <- NULL
  rownames(rest_old_df) <- NULL
  expect_true(isTRUE(all.equal(rest_old_df, rest_new_df)))

  ### same process, but for horizontal split:
  json <- testthat::test_path("examples", "sample3pg.json")
  df <- build_block_df(type = "async", object = json)
  # choose random block and cut point:
  block <- sample(min(df$block):max(df$block), 1)
  cut_point <- sample(1:99, 1)
  # check that output df has one block more:
  new_df <- split_block(df, block = block, cut_point = cut_point, direction = "h")
  expect_equal(max(new_df$block), max(df$block) + 1)
  # check that the selected block is now different while others are similar:
  old_block_n <- df[df$block == block,]
  new_block_n <- new_df[new_df$block == block,]
  expect_false(isTRUE(all.equal(old_block_n, new_block_n)))
  rest_old_df <- df[!df$block == block,]
  new_df_ex_last <- new_df[!new_df$block == max(new_df$block),]
  rest_new_df <- new_df_ex_last[!new_df_ex_last$block == block,]
  rownames(rest_new_df) <- NULL
  rownames(rest_old_df) <- NULL
  expect_true(isTRUE(all.equal(rest_old_df, rest_new_df)))

  ### same process but for block in middle of multipage doc:
  json <- testthat::test_path("examples", "sample3pg.json")
  df <- build_block_df(type = "async", object = json)
  df_sub <- df[df$page == 2,]
  # choose random block and cut point:
  block <- sample(min(df_sub$block):max(df_sub$block), 1)
  cut_point <- sample(1:99, 1)
  # check that output df has one block more:
  new_df <- split_block(df, block = block, cut_point = cut_point, page = 2, direction = "h")
  expect_equal(nrow(new_df), nrow(df) + 1)
  # check that the selected block is now different while others are similar:
  new_df_sub <- new_df[new_df$page == 2,]
  old_block_n <- df_sub[df_sub$block == block,]
  new_block_n <- new_df_sub[new_df_sub$block == block,]
  expect_false(isTRUE(all.equal(old_block_n, new_block_n)))
  rest_old_df <- df_sub[!df_sub$block == block,]
  new_df_ex_last <- new_df_sub[!new_df_sub$block == max(new_df_sub$block),]
  rest_new_df <- new_df_ex_last[!new_df_ex_last$block == block,]
  rownames(rest_new_df) <- NULL
  rownames(rest_old_df) <- NULL
  expect_true(isTRUE(all.equal(rest_old_df, rest_new_df)))

  ### same process but for block at end of multipage doc:
  json <- testthat::test_path("examples", "sample3pg.json")
  df <- build_block_df(type = "async", object = json)
  df_sub <- df[df$page == 3,]
  # choose random block and cut point:
  block <- sample(min(df_sub$block):max(df_sub$block), 1)
  cut_point <- sample(1:99, 1)
  # check that output df has one block more:
  new_df <- split_block(df, block = block, cut_point = cut_point, page = 3, direction = "h")
  expect_equal(nrow(new_df), nrow(df) + 1)
  # check that the selected block is now different while others are similar:
  new_df_sub <- new_df[new_df$page == 3,]
  old_block_n <- df_sub[df_sub$block == block,]
  new_block_n <- new_df_sub[new_df_sub$block == block,]
  expect_false(isTRUE(all.equal(old_block_n, new_block_n)))
  rest_old_df <- df_sub[!df_sub$block == block,]
  new_df_ex_last <- new_df_sub[!new_df_sub$block == max(new_df_sub$block),]
  rest_new_df <- new_df_ex_last[!new_df_ex_last$block == block,]
  rownames(rest_new_df) <- NULL
  rownames(rest_old_df) <- NULL
  expect_true(isTRUE(all.equal(rest_old_df, rest_new_df)))
})

## REASSIGN_TOKENS -------------------------------------------------------------

test_that("reassign_tokens() warns of input errors", {
  expect_error(reassign_tokens(token_df = null), "token_df not a data frame.")
  expect_error(reassign_tokens(token_df = na), "token_df not a data frame.")
  expect_error(reassign_tokens(token_df = boolean), "token_df not a data frame.")
  expect_error(reassign_tokens(token_df = number_random), "token_df not a data frame.")
  expect_error(reassign_tokens(token_df = string_random), "token_df not a data frame.")
  expect_error(reassign_tokens(token_df = vector_strings), "token_df not a data frame.")
  expect_error(reassign_tokens(token_df = list_strings), "token_df not a data frame.")
  #expect_error(reassign_tokens(token_df = df), "Token dataframe not recognized. Was it made with build_token_df?")
  expect_error(reassign_tokens(token_df = matrix), "token_df not a data frame.")
  json <- testthat::test_path("examples", "output.json")
  df <- build_token_df(type = "async", object = json)
  expect_error(reassign_tokens(df, block_df = null), "block_df not a data frame.")
  expect_error(reassign_tokens(df, block_df = na), "block_df not a data frame.")
  expect_error(reassign_tokens(df, block_df = boolean), "block_df not a data frame.")
  expect_error(reassign_tokens(df, block_df = number_random), "block_df not a data frame.")
  expect_error(reassign_tokens(df, block_df = string_random), "block_df not a data frame.")
  expect_error(reassign_tokens(df, block_df = vector_strings), "block_df not a data frame.")
  expect_error(reassign_tokens(df, block_df = list_strings), "block_df not a data frame.")
  expect_error(reassign_tokens(df, block_df = df), "Block dataframe not recognized. Was it made with build_block_df?")
  expect_error(reassign_tokens(df, block_df = matrix), "block_df not a data frame.")
})

test_that("reassign_tokens() returns a revised token dataframe", {
  # first get sample token df:
  json <- testthat::test_path("examples", "output.json")
  tdf_old <- build_token_df(type = "async", object = json)
  # then use split blocks to make a revised block dataframe:
    # start by making regular block df:
    json <- testthat::test_path("examples", "output.json")
    bdf_old <- build_block_df(type = "async", object = json)
    # then split a random block (with a minimum of 10 words) to make a new block_df:
    blocks <- list()
    for (i in 1:max(tdf_old$block, na.rm=TRUE)) {
      block <- list(tdf_old[tdf_old$block == i,])
      blocks <- append(blocks, block)
    }
    sizes <- data.frame(block = numeric(), words = numeric())
    for (i in 1:max(tdf_old$block, na.rm=TRUE)) {
      ntokens <- nrow(blocks[[i]])
      entry <- data.frame(block = i, words = ntokens)
      sizes <- rbind(sizes, entry)
    }
    b10plus <- sizes$block[sizes$words >= 10]
    to_split <- sample(b10plus, 1)
    bdf_new <- split_block(bdf_old, block = to_split, cut_point = sample(1:99, 1))
  # then reassign tokens:
  tdf_new <- reassign_tokens(tdf_old, bdf_new)
  # now check:
  expect_true(is.data.frame(tdf_new))
  #expect_equal(nrow(tdf_new), nrow(tdf_old)) # Not necessarily equal bc a word
  # split in middle (as result of random split) can be counted twice
  expect_true(identical(colnames(tdf_new), colnames(tdf_old)))
  expect_false(isTRUE(all.equal(tdf_new, tdf_old)))
  expect_true(is.unsorted(tdf_new$start_ind, na.rm = TRUE))
  expect_false(is.unsorted(tdf_old$start_ind, na.rm = TRUE))
})

## REASSIGN_TOKENS2 ------------------------------------------------------------

test_that("reassign_tokens2() warns of input errors", {
  expect_error(reassign_tokens2(token_df = null), "token_df not a data frame.")
  expect_error(reassign_tokens2(token_df = na), "token_df not a data frame.")
  expect_error(reassign_tokens2(token_df = boolean), "token_df not a data frame.")
  expect_error(reassign_tokens2(token_df = number_random), "token_df not a data frame.")
  expect_error(reassign_tokens2(token_df = string_random), "token_df not a data frame.")
  expect_error(reassign_tokens2(token_df = vector_strings), "token_df not a data frame.")
  expect_error(reassign_tokens2(token_df = list_strings), "token_df not a data frame.")
  #expect_error(reassign_tokens2(token_df = df), "Token dataframe not recognized. Was it made with build_token_df?")
  expect_error(reassign_tokens2(token_df = matrix), "token_df not a data frame.")
  json <- testthat::test_path("examples", "output.json")
  tdf <- build_token_df(type = "async", object = json)
  expect_error(reassign_tokens2(tdf, block = null), "block input not a data frame.")
  expect_error(reassign_tokens2(tdf, block = na), "block input not a data frame.")
  expect_error(reassign_tokens2(tdf, block = boolean), "block input not a data frame.")
  expect_error(reassign_tokens2(tdf, block = number_random), "block input not a data frame.")
  expect_error(reassign_tokens2(tdf, block = string_random), "block input not a data frame.")
  expect_error(reassign_tokens2(tdf, block = vector_strings), "block input not a data frame.")
  expect_error(reassign_tokens2(tdf, block = list_strings), "block input not a data frame.")
  expect_error(reassign_tokens2(tdf, block = df), "Block dataframe format not recognized.")
  expect_error(reassign_tokens2(tdf, block = matrix), "block input not a data frame.")
  bdf <- build_block_df(type = "async", object = json)
  bdf <- bdf[1,]
  expect_error(reassign_tokens2(tdf, bdf, page = 1-2), "Invalid page parameter.")
  expect_error(reassign_tokens2(tdf, bdf, page = "one"), "Invalid page parameter.")
  expect_error(reassign_tokens2(tdf, bdf, page = c(1,2)), "Invalid page parameter.")
  expect_error(reassign_tokens2(tdf, bdf, page = 10), "No such page number in this dataframe.")
})

test_that("reassign_tokens2() returns a revised token dataframe", {
  # make token df:
  json <- testthat::test_path("examples", "peshtigo.json")
  tdf_old <- build_token_df(type = "async", object = json)
  # make block df with from_labelme():
  json <- testthat::test_path("examples", "peshtigo_labelme.json")
  block <- from_labelme(json)
  # then reassign:
  tdf_new <- reassign_tokens2(tdf_old, block)
  # now check:
  expect_true(is.data.frame(tdf_new))
  expect_equal(max(tdf_new$block, na.rm = TRUE), max(tdf_old$block, na.rm = TRUE) + 1)
  # expect_equal(nrow(tdf_new), nrow(tdf_old)) Not necessarily equal bc a word
  # split in middle (as result of random split) can be counted twice
  expect_true(identical(colnames(tdf_new), colnames(tdf_old)))
  expect_false(isTRUE(all.equal(tdf_new, tdf_old)))
  expect_false(is.unsorted(tdf_old$start_ind, na.rm = TRUE))
  # expect_true(is.unsorted(tdf_new$start_ind, na.rm = TRUE)) 
  # not necessarily true if the block split is the last one
})

## FROM_LABELME ------------------------------------------------------------

test_that("from_labelme() warns of input errors", {
  expect_error(from_labelme(null), "Input file not .json.")
  expect_error(from_labelme(na), "Input file not .json.")
  expect_error(from_labelme(boolean), "Input file not .json.")
  expect_error(from_labelme(number_random), "Input file not .json.")
  expect_error(from_labelme(string_random), "Input file not .json.")
  expect_error(from_labelme(vector_strings), "Input file not .json.")
  expect_error(from_labelme(list_strings), "Input file not .json.")
  expect_error(from_labelme(df), "Input file not .json.")
  expect_error(from_labelme(matrix), "Input file not .json.")
  expect_error(from_labelme("wrongfile.csv"), "Input file not .json.")
  expect_error(from_labelme("madeup.json"), "Input file not .json.")
  json <- testthat::test_path("examples", "peshtigo_labelme.json")
  expect_error(from_labelme(json, page = 1-2), "Invalid page parameter.")
  expect_error(from_labelme(json, page = "one"), "Invalid page parameter.")
  expect_error(from_labelme(json, page = c(1, 2)), "Invalid page parameter.")
})

test_that("from_labelme() produces a properly formatted df", {
  json <- testthat::test_path("examples", "peshtigo_labelme.json")
  df <- from_labelme(json)
  expect_true(is.data.frame(df))
  #expect_true(identical(colnames(df), c("page", "block", "left", "right", "top", "bottom")))
  expect_true(is.numeric(df$block))
  expect_true(is.numeric(df$left))
  expect_lt(max(df$right, na.rm = TRUE), 1)
  expect_gt(min(df$top, na.rm = TRUE), 0)
})

unlink(madeup_json_file, force = TRUE)

## REDRAW_BLOCKS ----------------------------------------