#' Build dataframe with token location data
#'
#' This function takes a json output file from Google Document AI (DAI)
#' and builds a dataframe with page location data for all identified tokens.
#'
#' @param json a json file from DAI
#' @return a dataframe
#' @details the dataframe has tokens as rows and location variables as columns
#' (start index, end index, left boundary, right boundary, top boundary, and bottom boundary.)
#' @export
#' @examples
#' \dontrun{
#' build_token_df("pdf_output.json")
#' }

build_token_df <- function(json) {

  # check
  if (!(is_json(json))){
    stop("Input file not .json.")
    }

  output_as_list <- jsonlite::fromJSON(json)

  if (!("pages" %in% names(output_as_list))) {
    stop("JSON not in right format. Is it from DAI?")
    }

  if (!("text" %in% names(output_as_list))) {
    stop("DAI found no tokens. Was the document blank?")
    }

  # get pagewise indices
  pages_tokens <- output_as_list$pages$tokens

  pagewise_token_indices <- purrr::map(pages_tokens, ~.x$layout$textAnchor$textSegments)

  # extract all start indices, shifting them by one,
  # because the token really starts at (index + 1)
  start_ind_raw <- unlist(purrr::map(pagewise_token_indices, ~ purrr::map(.x, ~.x$startIndex)))
  start_ind_shift <- as.integer(start_ind_raw) + 1
  start_ind <- c(1, start_ind_shift)

  # extract all start indices
  end_ind <- unlist(purrr::map(pagewise_token_indices, ~ purrr::map(.x, ~.x$endIndex)))

  # extract all tokens
  text <- output_as_list$text

  token <- character()

  for (i in 1:length(start_ind)) {

    tok <- substr(text,
                  start_ind[i],
                  end_ind[i]
                  )

    token <- c(token, tok)

    }

  # extract all boundaries
  pagewise_token_coords <- purrr::map(pages_tokens, ~.x$layout$boundingPoly$normalizedVertices)

  left <-  unlist(purrr::map(pagewise_token_coords, ~ purrr::map(.x, ~ min(.x$x))))

  right <- unlist(purrr::map(pagewise_token_coords, ~ purrr::map(.x, ~ max(.x$x))))

  top <- unlist(purrr::map(pagewise_token_coords, ~ purrr::map(.x, ~ min(.x$y))))

  bottom <- unlist(purrr::map(pagewise_token_coords, ~ purrr::map(.x, ~ max(.x$y))))

  # get page numbers
  page <- list()

  for (i in 1:length(pages_tokens)) {

    if (is.null(pages_tokens[[i]])) {
      instances <- 0
      } else {
      instances <- nrow(pages_tokens[[i]])
      }

    pg <- rep(i, each = instances)

    page <- append(page, pg)

    }

  page <- unlist(page)

  # build dataframe
  df <- data.frame(token,
                   start_ind,
                   end_ind,
                   left,
                   right,
                   top,
                   bottom,
                   page,
                   block = NA
                   )

  # get block numbers

  # first extract vector of pagewise sets of blocks
  pages_blocks <- output_as_list$pages$blocks

  pagewise_block_sets <- purrr::map(pages_blocks, ~.x$layout$boundingPoly$normalizedVertices)

  # then assign block numbers by location

  # loop over each page
  for (i in 1:length(pagewise_block_sets)) {

    # create temporary sub-dataframe for that page
    df_sub <- df[df$page == i, ]

    # set counter for block number
    count = 1

    #loop over blocks
    for (j in pagewise_block_sets[[i]]) {

      # assign number to token rows that fall within the box
      df_sub$block[df_sub$right > mean(c(j$x[1], j$x[4])) &
                     df_sub$left < mean(c(j$x[2],j$x[3])) &
                     df_sub$bottom > mean(c(j$y[1],j$y[2])) &
                     df_sub$top < mean(c(j$y[3],j$y[4]))] <- count

      count = count + 1

      }

    # add the block numbers from that page to the main df
    df$block[df$page == i] <- df_sub$block

    }

  return(df)

  }

#' Build dataframe with block location data
#'
#' @param json a json file from DAI
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' build_block_df("pdf_output.json")
#' }

build_block_df <- function(json) {

  # check
  if (!(is_json(json))){
    stop("Input file not .json.")
    }

  output_as_list <- jsonlite::fromJSON(json)

  if (!("pages" %in% names(output_as_list))) {
    stop("JSON not in right format. Is it from DAI?")
    }

  if (!("text" %in% names(output_as_list))) {
    stop("DAI found no blocks. Was the document blank?")
    }

  # extract a list with pagewise sets of block boundary boxes
  pages_blocks <- output_as_list$pages$blocks

  pagewise_block_sets <- purrr::map(pages_blocks, ~.x$layout$boundingPoly$normalizedVertices)

  # get block numbers

  block <- list()

  for (i in pagewise_block_sets) {

    b <- 1:length(i)

    block <- append(block, b)

    }

  block <- unlist(block)

  # extract all boundaries
  pagewise_block_coords <- purrr::map(pages_blocks, ~.x$layout$boundingPoly$normalizedVertices)

  left <-  unlist(purrr::map(pagewise_block_coords, ~ purrr::map(.x, ~ min(.x$x))))

  right <- unlist(purrr::map(pagewise_block_coords, ~ purrr::map(.x, ~ max(.x$x))))

  top <- unlist(purrr::map(pagewise_block_coords, ~ purrr::map(.x, ~ min(.x$y))))

  bottom <- unlist(purrr::map(pagewise_block_coords, ~ purrr::map(.x, ~ max(.x$y))))

  # get page numbers
  page <- list()

  for (i in 1:length(pages_blocks)) {

    if (is.null(pages_blocks[[i]])) {
      instances <- 0
      } else {
      instances <- nrow(pages_blocks[[i]])
      }

    pg <- rep(i, each = instances)

    page <- append(page, pg)

    }

  page <- unlist(page)

  df <- data.frame(page, block, left, right, top, bottom)

  return(df)

  }

#' Split a block bounding box
#'
#' This function splits an existing block bounding box vertically or horizontally at a specified point.
#' The new block generated by the split is added to the block dataframe while the old block's coordinates
#' are updated. The function returns a revised block dataframe.
#'
#' @param block_df A dataframe generated by `dair::build_block_df()`.
#' @param page The number of the page where the split will be made. Defaults to 1.
#' @param block The number of the block to be split.
#' @param cut_point A number between 0 and 100, where 0 is the existing left/top limit and 100 is the existing right/bottom limit.
#' @param direction "V" for vertical split or "H" for horizontal split. Defaults to "V".
#'
#' @return A dataframe
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

  if (!(identical(colnames(block_df), c("page", "block", "left", "right", "top", "bottom")))) {
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
      block = as.integer(max(old_page_df$block[old_page_df$page == page]) + 1),
      page = as.integer(page),
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
      block = as.integer(max(old_page_df$block[old_page_df$page == page]) + 1),
      page = as.integer(page),
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

  return(new_block_df)

  }

#' Assign tokens to new blocks
#'
#' @param token_df a dataframe generated by `dair::build_token_df`
#' @param block_df a dataframe generated by `dair::split_block()` or `dair::build_block_df`
#'
#' @return a dataframe of tokens
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
  if (!(is.data.frame(token_df))){
    stop("token_df not a data frame.")
    }

  if (!(identical(colnames(token_df), c("token", "start_ind", "end_ind", "left", "right", "top", "bottom", "page", "block")))) {
    stop("Token dataframe not recognized. Was it made with build_token_df?")
    }

  if (!(is.data.frame(block_df))){
    stop("block_df not a data frame.")
    }

  if (!(identical(colnames(block_df), c("page", "block", "left", "right", "top", "bottom")))) {
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
  for (i in 1:length(token_df_pages)) {

    # short names for readability
    tokens <- token_df_pages[[i]]
    blocks <- block_df_pages[[i]]

    # set up empty new page dataframe
    new_token_df_page <- as.data.frame(matrix(ncol = length(colnames), nrow = 0, dimnames = list(NULL, colnames)))

    # loop over each block
    for (j in 1:nrow(blocks)) {

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

  return(new_token_df)

  }

#' Assign tokens to a single new block
#'
#' @param token_df a dataframe generated by `dair::build_token_df`
#' @param block a one-row dataframe of the same format as token_df
#' @param page the number of the page on which the block belongs
#'
#' @return a dataframe of tokens
#' @export
#'
#' @details This function is designed to facilitate manual splitting
#' of blocks and typically takes a one-row block dataframe
#' generated by daiR::labelme(). See vignette "Correcting text output
#' from Google Document AI" for an example.
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

  if (!(identical(colnames(token_df), c("token", "start_ind", "end_ind", "left", "right", "top", "bottom", "page", "block")))) {
    stop("Token dataframe not recognized. Was it made with build_token_df?")
    }

  if (!(is.data.frame(block))) {
    stop("block input not a data frame.")
    }

  if (!(identical(colnames(block), c("page", "block", "left", "right", "top", "bottom")))) {
    stop("Block dataframe format not recognized.")
    }

  if (!(length(page) == 1 && is.numeric(page) && round(page) == page && page > 0)) {
    stop("Invalid page parameter.")
    }

  if (page > max(token_df$page, na.rm = TRUE)){
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
        preceeding <- token_df[token_df$page < page, ]
        new_token_df <- rbind(preceding, page_df)

  # if in middle
        } else {
          preceding <- token_df[token_df$page < page, ]
          succeeding <- token_df[token_df$page > page, ]
          new_token_df <- rbind(preceding, page_df, succeeding)
        }

  row.names(new_token_df) <- NULL

  return(new_token_df)

  }

#' Extract block coordinates from labelme files
#'
#'
#' @param json a json file generated by Labelme
#' @param page the number of the annotated page
#'
#' @return a dataframe with location coordinates for the rectangle marked in labelme
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

  left <- info[[3]][[2]][[1]][1,1] / width

  right <- info[[3]][[2]][[1]][2,1] / width

  top <- info[[3]][[2]][[1]][1,2] / height

  bottom <- info[[3]][[2]][[1]][2,2] / height

  block <- info[[3]][[1]]

  blockdata <- data.frame(page = page,
                          block = as.numeric(block),
                          left = left,
                          right = right,
                          top = top,
                          bottom = bottom
                          )

  return(blockdata)

  }
