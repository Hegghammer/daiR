#' Merge shards
#'
#' @description Merges text files from Document AI output shards into a
#' single text file corresponding to the parent document.
#'
#' @param source_dir folder path for input files
#' @param dest_dir folder path for output files
#' @return no return value, called for side effects
#'
#' @export
#'
#' @details The function works on .txt files generated from .json output files,
#' not on .json files directly. It also presupposes that the .txt filenames
#' have the same name stems as the .json files from which they were extracted.
#' For the v1 API, this means files ending with "-0.txt", "-1.txt", "-2.txt",
#' and so forth. The safest approach is to generate .txt files using
#' `get_text()` with the `save_to_file` parameter set to TRUE.
#'
#' @examples
#' \dontrun{
#' merge_shards()
#'
#' merge_shards(tempdir(), getwd())
#' }

merge_shards <- function(source_dir = getwd(),
                         dest_dir = getwd()
                        ) {

  if (length(source_dir) > 1) {
    stop("Invalid source_dir argument. Must be a valid folder path.")
  }

  if (!(is.character(source_dir))) {
    stop("Invalid source_dir argument. Must be a valid folder path.")
  }

  if (!(length(dest_dir) == 1)) {
    stop("Invalid dest_dir argument. Must be a valid folder path.")
  }

  if (!(is.character(dest_dir))) {
    stop("Invalid dest_dir argument. Must be a valid folder path.")
  }

  source_dir <- normalizePath(source_dir, winslash = "/")
  dest_dir <- normalizePath(dest_dir, winslash = "/")

  files <- list.files(source_dir, pattern = "*txt$", full.names = TRUE)

  if (length(files) == 0) {
    stop("No .txt files found.")
  }

  if (isFALSE(grepl("-\\d{1,3}\\.txt", files)) && isFALSE(grepl("-page-\\d{1,4}-to-\\d{1,4}", files))) {
    stop("The .txt files are incorrectly formatted. Are they from Document AI output shards?")
  }

  all <- grep("-\\d{1,3}\\.txt", files, value = TRUE)

  v1beta2 <- grep("-page-\\d{1,4}-to-\\d{1,4}", files, value = TRUE)

  v1 <- all[!all %in% v1beta2]

  v1beta2_stems <- unique(gsub("\\-page-\\d{1,4}-to-\\d{1,4}\\.txt", "", v1beta2))

  v1_stems <- unique(gsub("-\\d{1,2}\\.txt", "", v1))

  if (length(v1beta2) > 0) {
    for (i in seq_along(v1beta2_stems)) {
      cluster <- grep(v1beta2_stems[i], v1beta2, value = TRUE)
      cluster_df <- readtext::readtext(cluster)
      text <- paste(cluster_df$text, collapse = "\n")
      write(text, file.path(dest_dir, paste0(basename(v1beta2_stems[i]), ".txt")))
    }
  }

  if (length(v1) > 0) {
    for (i in seq_along(v1_stems)) {
      cluster <- grep(v1_stems[i], v1, value = TRUE)
      cluster_df <- readtext::readtext(cluster)
      text <- paste(cluster_df$text, collapse = "\n")
      write(text, file.path(dest_dir, paste0(basename(v1_stems[i]), ".txt")))
    }
  }

  cli::cli_alert_success("Shards merged.")

}

#' Build token dataframe
#'
#' @description Builds a token dataframe from the text OCRed by
#' Document AI (DAI) in an asynchronous request. Rows are tokens, in the
#' order DAI proposes to read them. Columns are location variables
#' such as page coordinates and block bounding box numbers.
#'
#' @param object either a HTTP response object from 
#' \code{dai_sync()} or the path to a JSON file from 
#' \code{dai_async()}.
#' @param type one of "sync" or "async" depending on
#' the function used to process the original document.
#' @return a token data frame
#'
#' @details The location variables are: token, start index, end index,
#' confidence, left boundary, right boundary, top boundary, bottom boundary,
#' page number, and block number. Start and end indices refer to
#' character position in the string containing the full text.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' resp <- dai_sync("file.pdf")
#' token_df <- build_token_df(resp)
#'
#' token_df <- build_token_df("pdf_output.json", type = "async")
#' }

build_token_df <- function(object,
                           type = "sync"
                           ) {

  # checks
  if (!(length(type) == 1) || !(type %in% c("sync", "async"))) {
    stop("Invalid type parameter.")
  }

  if (!(inherits(object, "response") || is_json(object))) {
    stop("Invalid object parameter.")
  }

  if (type == "sync") {

    output_sync <- httr::content(object)
    text <- output_sync$document$text
    pages_sync <- output_sync$document$pages

    # get pagewise indices
    pages_tokens_sync <- purrr::map(pages_sync, ~.x$tokens)

    # extract all start indices as single vector
    start_ind <- integer()
    for (i in pages_tokens_sync) {
      for (j in i) {
        ind <- as.integer(j[["layout"]][["textAnchor"]][["textSegments"]][[1]][["startIndex"]]) + 1
        start_ind <- c(start_ind, ind)
      }
    }

    # extract all end indices as single vector
    end_ind <- integer()
    for (i in pages_tokens_sync) {
      for (j in i) {
        ind <- j[["layout"]][["textAnchor"]][["textSegments"]][[1]][["endIndex"]]
        end_ind <- c(end_ind, as.integer(ind))
      }
    }

    # get confidence scores as single vector
    conf <- numeric()
    for (i in pages_tokens_sync) {
      for (j in i) {
        con <- j$layout$confidence
        conf <- c(conf, con)
      }
    }

  } else if (type == "async") {

    output_async <- jsonlite::fromJSON(object)

    if (!("pages" %in% names(output_async))) {
      stop("JSON not in right format. Is it from DAI?")
    }

    if (!("text" %in% names(output_async))) {
      stop("DAI found no tokens. Was the document blank?")
    }

    text <- output_async$text

    # get pagewise indices
    pages_tokens_async <- output_async$pages$tokens
    pagewise_token_indices_async <- purrr::map(pages_tokens_async, ~.x$layout$textAnchor$textSegments)

    # extract all start indices as single vector, shifting them by one, because the token really starts at (index + 1)
    start_ind <- as.integer(unlist(purrr::map(pagewise_token_indices_async, ~ purrr::map(.x, ~.x$startIndex)))) + 1

    # extract all end indices as single vector
    end_ind <- unlist(purrr::map(pagewise_token_indices_async, ~ purrr::map(.x, ~.x$endIndex)))

    # get confidence scores as single vector
    conf <- unlist(purrr::map(pages_tokens_async, ~.x$layout$confidence))

  }

  # insert implicit start index
  start_ind <- c(1, start_ind)

  # get all tokens as single vector
  token <- character()
  for (i in seq_along(start_ind)) {
    tok <- substr(text,
                  start_ind[i],
                  end_ind[i]
    )
    token <- c(token, tok)
  }

  # get page numbers as single vector
  if (type == "sync") { pages_tokens <- pages_tokens_sync } else { pages_tokens <- pages_tokens_async }
  page <- integer()
  for (i in seq_along(pages_tokens)) {
    if (is.null(pages_tokens[[i]])) {
      instances <- 0
    } else {
      if (type == "sync") { instances <- length(pages_tokens[[i]]) }
      else { instances <- nrow(pages_tokens[[i]])
      }
    }
    pg <- rep(i, each = instances)
    page <- c(page, pg)
  }

  # get boundaries as single vectors
  if (type == "sync") {
    pagewise_token_coords <- purrr::map(pages_tokens_sync, get_vertices)
    pagewise_token_coords <- purrr::map(pagewise_token_coords, transpose_page)
  } else { pagewise_token_coords <- purrr::map(pages_tokens_async, ~.x$layout$boundingPoly$normalizedVertices)}

  left <-  unlist(purrr::map(pagewise_token_coords, ~ purrr::map(.x, ~ min(.x$x))))
  right <- unlist(purrr::map(pagewise_token_coords, ~ purrr::map(.x, ~ max(.x$x))))
  top <- unlist(purrr::map(pagewise_token_coords, ~ purrr::map(.x, ~ min(.x$y))))
  bottom <- unlist(purrr::map(pagewise_token_coords, ~ purrr::map(.x, ~ max(.x$y))))

  # combine all vectors to dataframe
  df <- data.frame(token,
                   start_ind,
                   end_ind,
                   conf,
                   left,
                   right,
                   top,
                   bottom,
                   page,
                   block = NA
  )

  # get block numbers, assigning by token location
  if (type == "sync") {
    pages_blocks_sync <- purrr::map(pages_sync, ~.x$blocks)
    pages_blocks_coords <- purrr::map(pages_blocks_sync, get_vertices)
    pages_blocks <- purrr::map(pages_blocks_coords, transpose_page)
  } else {
    pages_blocks_async <- output_async$pages$blocks
    pages_blocks <- purrr::map(pages_blocks_async, ~.x$layout$boundingPoly$normalizedVertices)
  }

  for (i in seq_along(pages_blocks)) {
    df_sub <- df[df$page == i, ]
    count <- 1
    for (j in pages_blocks[[i]]) {

      # assign number to token rows that fall within the box
      df_sub$block[df_sub$right > mean(c(j$x[1], j$x[4])) &
                     df_sub$left < mean(c(j$x[2], j$x[3])) &
                     df_sub$bottom > mean(c(j$y[1], j$y[2])) &
                     df_sub$top < mean(c(j$y[3], j$y[4]))] <- count
      count <- count + 1
    }
    df$block[df$page == i] <- df_sub$block
  }

  df

}

#' Build block dataframe
#'
#' @description Creates a dataframe with the block bounding boxes
#' identified by Document AI (DAI) in an asynchronous request.
#' Rows are blocks, in the order DAI proposes to read them. Columns
#' are location variables such as page coordinates and page numbers.
#'
#' @param object either a HTTP response object from 
#' \code{dai_sync()} or the path to a JSON file from 
#' \code{dai_async()}.
#' @param type one of "sync" or "async" depending on
#' the function used to process the original document.
#' @return a block data frame
#'
#' @details The dataframe variables are: page number, block number,
#' confidence score, left boundary, right boundary, top boundary,
#' and bottom boundary.
#' @export
#'
#' @examples
#' \dontrun{
#' resp <- dai_sync("file.pdf")
#' block_df <- build_block_df(resp)
#' 
#' block_df <- build_block_df("pdf_output.json", type = "async")
#' }

build_block_df <- function(object,
                           type = "sync"
                          ) {

  # checks
  if (!(length(type) == 1) || !(type %in% c("sync", "async"))) {
    stop("Invalid type parameter.")
  }

  if (!(inherits(object, "response") || is_json(object))) {
    stop("Invalid object parameter.")
  }

  if (type == "sync") {

    output_sync <- httr::content(object)

    # extract a list with pagewise sets of block boundary boxes
    pages_sync <- output_sync$document$pages
    pages_blocks_sync <- purrr::map(pages_sync, ~.x$blocks)
    pagewise_block_sets_sync <- purrr::map(pages_blocks_sync, get_vertices)
    pagewise_block_sets_sync <- purrr::map(pagewise_block_sets_sync, transpose_page)

    # get confidence scores as single vector
    conf <- numeric()
    for (i in pages_blocks_sync) {
      for (j in i) {
        con <- j$layout$confidence
        conf <- c(conf, con)
      }
    }
  }

  else if (type == "async") {

    output_async <- jsonlite::fromJSON(object)

    if (!("pages" %in% names(output_async))) {
      stop("JSON not in right format. Is it from DAI?")
    }

    if (!("text" %in% names(output_async))) {
      stop("DAI found no blocks. Was the document blank?")
    }

    # extract a list with pagewise sets of block boundary boxes
    pages_blocks_async <- output_async$pages$blocks
    pagewise_block_sets_async <- purrr::map(pages_blocks_async, ~.x$layout$boundingPoly$normalizedVertices)

    # get confidence scores as single vector
    conf <- unlist(purrr::map(pages_blocks_async, ~.x$layout$confidence))

  }

  # get page numbers as single vector
  page <- integer()
  if (type == "sync") { pages_blocks <- pages_blocks_sync } else { pages_blocks <- pages_blocks_async }
  for (i in seq_along(pages_blocks)) {
    if (is.null(pages_blocks[[i]])) {
      instances <- 0
    } else {
      if (type == "sync") { instances <- length(pages_blocks[[i]]) }
      else { instances <- nrow(pages_blocks[[i]])
      }
    }
    pg <- rep(i, each = instances)
    page <- c(page, pg)
  }

  # get block numbers as single vector
  if (type == "sync") { pagewise_block_coords <- pagewise_block_sets_sync } else { pagewise_block_coords <- pagewise_block_sets_async}
  block <- integer()
  for (i in pagewise_block_coords) {
    b <- seq_along(i)
    block <- c(block, b)
  }

  # get coordinates as single vectors
  left <-  unlist(purrr::map(pagewise_block_coords, ~ purrr::map(.x, ~ min(.x$x))))
  right <- unlist(purrr::map(pagewise_block_coords, ~ purrr::map(.x, ~ max(.x$x))))
  top <- unlist(purrr::map(pagewise_block_coords, ~ purrr::map(.x, ~ min(.x$y))))
  bottom <- unlist(purrr::map(pagewise_block_coords, ~ purrr::map(.x, ~ max(.x$y))))

  # combine all vectors to dataframe
  data.frame(page, block, conf, left, right, top, bottom)

}

#' Split a block bounding box
#'
#' @description This function 'splits' (in the sense of changing the
#' coordinates) of an existing block bounding box vertically or
#' horizontally at a specified point. It takes a block data frame as
#' input and modifies it. The splitting produces a new block, which
#' is added to the data frame while the old block's coordinates are
#' updated. The function returns a revised block data frame.
#'
#' @param block_df A dataframe generated by \code{build_block_df()}.
#' @param page The number of the page where the split will be made.
#' Defaults to 1.
#' @param block The number of the block to be split.
#' @param cut_point A number between 0 and 100, where 0 is the
#' existing left/top limit and 100 is the existing right/bottom limit.
#' @param direction "V" for vertical split or "H" for horizontal split.
#' Defaults to "V".
#'
#' @return a block data frame
#' @export
#'
#' @examples
#' \dontrun{
#' new_block_df <- split_block(df = old_block_df, block = 7, cut_point = 33)
#' }

split_block <- function(block_df,
                        page = 1,
                        block,
                        cut_point,
                        direction = "v"
) {

  # checks
  if (!(is.data.frame(block_df))) {
    stop("Input not a data frame.")
  }

  if (!(identical(colnames(block_df), c("page", "block", "conf", "left", "right", "top", "bottom")))) {
    stop("Dataframe not recognized. Was it made with build_block_df?")
  }

  if (!(length(page) == 1 && is.numeric(page) && round(page) == page && page > 0)) {
    stop("Invalid page parameter.")
  }

  if (page > max(block_df$page, na.rm = TRUE)) {
    stop("No such page number in this dataframe.")
  }

  if (!(length(block) == 1 && is.numeric(block) && round(block) == block && block > 0)) {
    stop("Invalid block parameter.")
  }

  if (block > max(block_df$block[block_df$page == page])) {
    stop("No such block number on this page.")
  }

  if (!(length(cut_point) == 1 && is.numeric(cut_point) && round(cut_point) == cut_point && cut_point > 0)) {
    stop("Invalid cut point parameter.")
  }

  if (!(cut_point %in% 1:99)) {
    stop("Cut point out of range.")
  }

  if (!(length(direction) == 1 && is.character(direction))) {
    stop("Invalid direction parameter.")
  }

  direction <- tolower(direction)

  if (!(direction %in% c("h", "v"))) {
    stop('Split direction must be either "h" or "v".')
  }

  # rename for readability
  old_df <- block_df

  # select page and block
  old_page_df <- old_df[old_df$page == page, ]

  old_block <- old_page_df[old_page_df$block == block, ]

  # vertical split
  if (direction == "v") {

    dist <- old_block$right - old_block$left
    cut <- dist / 100 * cut_point
    cut_loc <- old_block$left + cut

    new_block <- data.frame(
      page = as.integer(page),
      block = as.integer(max(old_page_df$block[old_page_df$page == page]) + 1),
      conf = NA,
      left = cut_loc,
      right = old_block$right,
      top = old_block$top,
      bottom = old_block$bottom
    )

    new_page_df <- rbind(old_page_df, new_block)

    new_page_df$right[new_page_df$block == block] <- cut_loc

    if (page == 1 && max(old_df$page) == 1) {
      new_block_df <- new_page_df
    } else if (page == 1 && max(old_df$page) > 1) {
      succeeding <- old_df[old_df$page > page, ]
      new_block_df <- rbind(new_page_df, succeeding)
    } else if (page > 1 && page == max(old_df$page)) {
      preceding <- old_df[old_df$page < page, ]
      new_block_df <- rbind(preceding, new_page_df)
    } else {
      preceding <- old_df[old_df$page < page, ]
      succeeding <- old_df[old_df$page > page, ]
      new_block_df <- rbind(preceding, new_page_df, succeeding)
    }

    # horizontal split
  } else {

    dist <- old_block$bottom - old_block$top
    cut <- dist / 100 * cut_point
    cut_loc <- old_block$top + cut

    new_block <- data.frame(
      page = as.integer(page),
      block = as.integer(max(old_page_df$block[old_page_df$page == page]) + 1),
      conf = NA,
      left = old_block$right,
      right = old_block$right,
      top = cut_loc,
      bottom = old_block$bottom
    )

    new_page_df <- rbind(old_page_df, new_block)

    new_page_df$bottom[new_page_df$block == block] <- cut_loc

    if (page == 1 && max(old_df$page) == 1) {
      new_block_df <- new_page_df
    } else if (page == 1 && max(old_df$page) > 1) {
      succeeding <- old_df[old_df$page > page, ]
      new_block_df <- rbind(new_page_df, succeeding)
    } else if (page > 1 && page == max(old_df$page)) {
      preceding <- old_df[old_df$page < page, ]
      new_block_df <- rbind(preceding, new_page_df)
    } else {
      preceding <- old_df[old_df$page < page, ]
      succeeding <- old_df[old_df$page > page, ]
      new_block_df <- rbind(preceding, new_page_df, succeeding)
    }
  }

  row.names(new_block_df) <- NULL

  new_block_df

}

#' Assign tokens to new blocks
#'
#' @description This is a specialized function for use in connection
#' with text reordering. It modifies a token dataframe by assigning
#' new block bounding box values to a subset of tokens based on
#' prior modifications made to a block dataframe.
#'
#' @param token_df a dataframe generated by \code{build_token_df()}
#' @param block_df a dataframe generated by \code{dair::split_block()}
#' or \code{dair::build_block_df()}
#' @return a token data frame
#'
#' @details The token and block data frames provided as input must be
#' from the same JSON output file.
#' @export
#'
#' @examples
#' \dontrun{
#' new_token_df <- reassign_tokens(token_df, new_block_df)
#' }

reassign_tokens <- function(token_df,
                            block_df
) {

  # checks
  if (!(is.data.frame(token_df))) {
    stop("token_df not a data frame.")
  }

  if (!(identical(colnames(token_df),
                  c("token", "start_ind", "end_ind", "conf", "left", "right", "top", "bottom", "page", "block")))) {
    stop("Token dataframe not recognized. Was it made with build_token_df?")
  }

  if (!(is.data.frame(block_df))) {
    stop("block_df not a data frame.")
  }

  if (!(identical(colnames(block_df), c("page", "block", "conf", "left", "right", "top", "bottom")))) {
    stop("Block dataframe not recognized. Was it made with build_block_df?")
  }

  # get list of pagewise dfs
  token_df_pages <- split(token_df,
                          token_df$page
  )

  block_df_pages <- split(block_df,
                          block_df$page
  )

  # set up empty new df
  colnames <- colnames(token_df)

  new_token_df <- as.data.frame(matrix(ncol = length(colnames), nrow = 0, dimnames = list(NULL, colnames)))

  # loop over each page
  for (i in seq_along(token_df_pages)) {

    # short names for readability
    tokens <- token_df_pages[[i]]
    blocks <- block_df_pages[[i]]

    # set up empty new page dataframe
    new_token_df_page <- as.data.frame(matrix(ncol = length(colnames), nrow = 0, dimnames = list(NULL, colnames)))

    # loop over each block
    for (j in seq_len(nrow(blocks))) {

      tokens_in_block <- tokens[tokens$top >= blocks[j, ]$top &
                                  tokens$bottom <= blocks[j, ]$bottom &
                                  tokens$left >= blocks[j, ]$left &
                                  tokens$right <= blocks[j, ]$right, ]

      if (nrow(tokens_in_block) > 0) {
        tokens_in_block$block <- j
        tokens_in_block$page <- i
        new_token_df_page <- rbind(new_token_df_page, tokens_in_block)
      }
    }

    new_token_df <- rbind(new_token_df, new_token_df_page)

  }

  row.names(new_token_df) <- NULL

  new_token_df

}

#' Assign tokens to a single new block
#'
#' @description This is a specialized function for use in connection
#' with text reordering. It is designed to facilitate manual splitting
#' of block boundary boxes and typically takes a one-row block dataframe
#' generated by \code{from_labelme()}.
#'
#' @param token_df a data frame generated by \code{dair::build_token_df}
#' @param block a one-row data frame of the same format as \code{token_df}
#' @param page the number of the page on which the block belongs
#'
#' @return a token data frame
#' @export
#'
#' @examples
#' \dontrun{
#' new_token_df <- reassign_tokens2(token_df, new_block_df)
#' new_token_df <- reassign_tokens2(token_df, new_block_df, 5)
#' }

reassign_tokens2 <- function(token_df,
                             block,
                             page = 1
) {

  # checks
  if (!(is.data.frame(token_df))) {
    stop("token_df not a data frame.")
  }

  if (!(identical(colnames(token_df),
                  c("token", "start_ind", "end_ind", "conf", "left", "right", "top", "bottom", "page", "block")))) {
    stop("Token dataframe not recognized. Was it made with build_token_df?")
  }

  if (!(is.data.frame(block))) {
    stop("block input not a data frame.")
  }

  if (!(identical(colnames(block), c("page", "block", "conf",  "left", "right", "top", "bottom")))) {
    stop("Block dataframe format not recognized.")
  }

  if (!(length(page) == 1 && is.numeric(page) && round(page) == page && page > 0)) {
    stop("Invalid page parameter.")
  }

  if (page > max(token_df$page, na.rm = TRUE)) {
    stop("No such page number in this dataframe.")
  }

  # get only selected page
  page_df <- token_df[token_df$page == page, ]

  page_df$block[page_df$top >= block$top &
                  page_df$bottom <= block$bottom &
                  page_df$left >= block$left &
                  page_df$right <= block$right] <- as.numeric(block$block)

  # if only page
  if (page == 1 && max(token_df$page) == 1) {
    new_token_df <- page_df

    # if first of several
  } else if (page == 1 && max(token_df$page) > 1) {
    succeeding <- token_df[token_df$page > page, ]
    new_token_df <- rbind(page_df, succeeding)

    # if last of several
  } else if (page > 1 && page == max(token_df$page)) {
    preceding <- token_df[token_df$page < page, ]
    new_token_df <- rbind(preceding, page_df)

    # if in middle
  } else {
    preceding <- token_df[token_df$page < page, ]
    succeeding <- token_df[token_df$page > page, ]
    new_token_df <- rbind(preceding, page_df, succeeding)
  }

  row.names(new_token_df) <- NULL

  new_token_df
}

#' Extract block coordinates from labelme files
#'
#' @description This is a specialized function for use in connection
#' with text reordering. It takes the output from the image
#' annotation tool 'Labelme' <https://github.com/wkentaro/labelme>
#' and turns it into a one-row data frame compatible with other
#' 'daiR' functions for text reordering such as
#' \code{reassign_tokens2()}. See package vignette on text reconstruction
#' for details.
#'
#' @param json a json file generated by 'Labelme'
#' @param page the number of the annotated page
#' @return a data frame with location coordinates for the rectangle
#' marked in 'Labelme'.
#' @export
#'
#' @examples
#' \dontrun{
#' new_block <- from_labelme("document1_blocks.json")
#' new_block <- from_labelme("document5_blocks.json", 5)
#' }

from_labelme <- function(json,
                         page = 1
) {

  # checks
  if (!(is_json(json))) {
    stop("Input file not .json.")
  }

  if (!(length(page) == 1 && is.numeric(page) && round(page) == page && page > 0)) {
    stop("Invalid page parameter.")
  }

  # extract the coordinates
  info <- jsonlite::fromJSON(json)

  height <- info[[6]]

  width <- info[[7]]

  left <- info[[3]][[2]][[1]][1, 1] / width

  right <- info[[3]][[2]][[1]][2, 1] / width

  top <- info[[3]][[2]][[1]][1, 2] / height

  bottom <- info[[3]][[2]][[1]][2, 2] / height

  block <- info[[3]][[1]]

  data.frame(page = page,
             block = as.numeric(block),
             conf = NA,
             left = left,
             right = right,
             top = top,
             bottom = bottom
             )
}

#' Inspect revised block bounding boxes
#'
#' @description Tool to visually check the order of block bounding boxes after
#' manual processing (e.g. block reordering or splitting). Takes as its main
#' input a token dataframe generated with \code{build_token_df()},
#' \code{reassign_tokens()}, or \code{reassign_tokens2()}.
#' The function plots the block bounding boxes onto images of the submitted
#' document. Generates an annotated .png file for each page in the
#' original document.
#'
#' @param json filepath of a JSON file obtained using \code{dai_async()}
#' @param token_df a token data frame generated with \code{build_token_df()},
#' \code{reassign_tokens()}, or \code{reassign_tokens2()}.
#' @param dir path to the desired output directory.
#' @return no return value, called for side effects
#'
#' @details Not vectorized, but documents can be multi-page.
#'
#' @export
#' @examples
#' \dontrun{
#' redraw_blocks("pdf_output.json", revised_token_df, dir = tempdir())
#' }

redraw_blocks <- function(json,
                          token_df,
                          dir = getwd()
) {
  # checks
  if (length(json) > 1) {
    stop("Invalid json input. This function is not vectorised.")
  }

  if (!(is.character(json))) {
    stop("Invalid json input.")
  }

  if (!(is_json(json))) {
    stop("Input 'json' not .json.")
  }

  if (!(is.data.frame(token_df))) {
    stop("token_df not a data frame.")
  }

  if (!(identical(colnames(token_df),
                  c("token", "start_ind", "end_ind", "conf", "left", "right", "top", "bottom", "page", "block")))) {
    stop("Token dataframe format not recognized.")
  }

  dir <- normalizePath(dir, winslash = "/")

  # parse the json
  parsed <- jsonlite::fromJSON(json)

  # create a list with pagewise sets of block boundary box coordinates
  pages_blocks <- split(token_df, token_df$page)

  pagewise_block_sets <- list()
  for (i in seq_along(pages_blocks)) {
    blocks <- split(pages_blocks[[i]], pages_blocks[[i]]$block)
    block_coords <- list()
    for (j in seq_along(blocks)) {
      token_sub_df <- blocks[[j]]
      left <- min(token_sub_df$left)
      right <- max(token_sub_df$right)
      top <- min(token_sub_df$top)
      bottom <- max(token_sub_df$bottom)
      x <- c(left, right, right, left)
      y <- c(top, top, bottom, bottom)
      coords <- list(data.frame(x, y))
      block_coords <- append(block_coords, coords)
    }
    pagewise_block_sets <- append(pagewise_block_sets, list(block_coords))
  }

  # Get vector of base64-encoded images
  page_imgs <- parsed$pages$image$content

  # loop over the pagewise sets
  for (i in seq_along(pagewise_block_sets)) {

    # decode base64
    path <- file.path(tempdir(), glue::glue("page{i}.jpg"))
    outconn <- file(path, "wb")
    base64enc::base64decode(page_imgs[i], outconn)
    close(outconn)

    # read image into magick
   img_decoded <- magick::image_read(path)

    # get image dimensions
    info <- magick::image_info(img_decoded)

    # prepare for plotting on image
    canvas <- magick::image_draw(img_decoded)

    # set counter for box number
    counter <- 1

    #loop over boxes on the page
    for (box in pagewise_block_sets[[i]]) {

      # transform from relative to absolute coordinates
      box$x1 <- box$x * info$width

      box$y1 <- box$y * info$height

      # draw polygon
      graphics::polygon(x = box$x1,
                        y = box$y1,
                        border = "red",
                        lwd = 3
      )

      graphics::text(x = box$x1[1],
                     y = box$y1[1],
                     label = counter,
                     cex = 4,
                     col = "blue",
                     family = "Liberation Sans"
      )

      counter <- counter + 1

    }

    # write annotated image to file

    filename <- glue::glue("page{i}_blocks_revised.png")

    dest <- file.path(dir, filename)

    magick::image_write(canvas, format = "png", dest)

    grDevices::dev.off()

  }

  pages <- length(pages_blocks)

  cli::cli_alert_success(glue::glue("Generated {pages} annotated image(s)."))

}

#' Get vertices
#'
#' @description Helper function for extracting coordinates from DAI response objects
#'
#' @param lst a list
#'
#' @noRd

get_vertices <- function(lst) {
  purrr::map(lst, ~.x$layout$boundingPoly$normalizedVertices)
}

#' Get vertices for entities
#'
#' @description Helper function for extracting coordinates from DAI response objects
#'
#' @param lst a list
#'
#' @noRd

get_vertices_entities <- function(lst) {
  purrr::map(lst, ~.x$pageAnchor$pageRefs[[2]]$boundingPoly$normalizedVertices)
}

#' Transpose blocks
#'
#' @description Helper function for converting coordinates from DAI response objects
#'
#' @param lst a list
#'
#' @noRd
transpose_block <- function(lst) {
  xs <- c(lst[[1]][["x"]], lst[[2]][["x"]], lst[[3]][["x"]], lst[[4]][["x"]])
  ys <- c(lst[[1]][["y"]], lst[[2]][["y"]], lst[[3]][["y"]], lst[[4]][["y"]])
  data.frame(xs, ys)
}

#' Transpose page
#'
#' @description Helper function for converting coordinates from DAI response objects
#'
#' @param lst a list
#'
#' @noRd

transpose_page <- function(lst) {
  purrr::map(lst, transpose_block)
}

#' Transpose page for entities
#'
#' @description Helper function for converting coordinates from DAI response objects
#'
#' @param lst a list
#'
#' @noRd

transpose_page_entities <- function(lst) {
  purrr::map(lst, ~ .x$boundingPoly$normalizedVertices[[2]])
}
