#' Draw block bounding boxes
#'
#' @description Plots the block bounding boxes identified by
#' Document AI (DAI) onto images of the submitted document.
#' Generates an annotated .png file for each page in the original
#' document.
#'
#' @param object either a HTTP response object from 
#' \code{dai_sync()} or the path to a JSON file from 
#' \code{dai_async()}.
#' @param type one of "sync" or "async", depending on
#' the function used to process the original document.
#' @param prefix string to be prepended to the output png filename.
#' @param dir path to the desired output directory.
#' @param linecol color of the bounding box line.
#' @param linewd width of the bounding box line.
#' @param fontcol color of the box numbers.
#' @param fontsize size of the box numbers.
#' @return no return value, called for side effects.
#'
#' @details Not vectorized, but documents can be multi-page.
#'
#' @export
#' @examples
#' \dontrun{
#' resp <- dai_sync("page.pdf")
#' draw_blocks(resp)
#'
#' draw_blocks("page.json", type = "async")
#' }

draw_blocks <- function(object,
                        type = "sync",
                        prefix = NULL,
                        dir = getwd(),
                        linecol = "red",
                        linewd = 3,
                        fontcol = "blue",
                        fontsize = 4
) {

  # checks
  if (!(length(type) == 1) || !(type %in% c("sync", "async"))) {
    stop("Invalid type parameter.")
  }

  if (length(prefix) > 1 || is.numeric(prefix)) {
    stop("Invalid prefix parameter.")
  }

  if (length(dir) > 1 || !(is.character(dir))) {
    stop("Invalid dir parameter. Must be a valid folder path.")
  }

  if (!(length(linecol) == 1) || !(daiR::is_colour(linecol)) || is.na(linecol)) {
    stop("Invalid linecol parameter. Must be a single valid colour representation.")
  }

  if (!(is.numeric(linewd) && length(linewd) == 1)) {
    stop("Invalid linewd parameter. Must be a single number.")
  }

  if (!(length(fontcol) == 1) || !(daiR::is_colour(fontcol)) || is.na(fontcol)) {
    stop("Invalid fontcol parameter. Must be a single valid colour representation.")
  }

  if (!(is.numeric(fontsize) && length(fontsize) == 1)) {
    stop("Invalid fontsize parameter. Must be a single number.")
  }

  dir <- normalizePath(dir, winslash = "/")

  if (type == "sync") {

    if (!(inherits(object, "response"))) {
      stop("Object parameter not pointing to valid response object.")
    }

    # extract a list with pagewise sets of block boundary boxes
    parsed <- httr::content(object)
    pages <- parsed$document$pages
    pages_blocks <- purrr::map(pages, ~.x$blocks)
    pagewise_block_sets <- purrr::map(pages_blocks, get_vertices)
    pagewise_block_sets <- purrr::map(pagewise_block_sets, transpose_page)

    # decode base64 and save to temp images
    page_imgs_base64 <- purrr::map_chr(pages, ~.x$image$content)
    imgs <- purrr::map2_chr(page_imgs_base64, seq_along(page_imgs_base64), decode_and_save)

  } else if (type == "async") {

    if (!(is_json(object))) {
      stop("Object parameter not pointing to valid JSON file.")
    }

    # extract a list with pagewise sets of block boundary boxes
    parsed <- jsonlite::fromJSON(object)
    pages_blocks <- parsed$pages$blocks
    pagewise_block_sets <- purrr::map(pages_blocks, ~.x$layout$boundingPoly$normalizedVertices)

    # decode base64 and save to temp images
    page_imgs_base64 <- parsed$pages$image$content
    imgs <- purrr::map2_chr(page_imgs_base64, seq_along(page_imgs_base64), decode_and_save)

  }

  # Plot bounding boxes
  purrr::map2(pagewise_block_sets, seq_along(pagewise_block_sets), ~ process_image(.x, .y, imgs, type, object, prefix, dir, filename, dest, linecol, linewd, fontcol, fontsize, boxtype = "blocks"))

  cli::cli_alert_success(glue::glue("Generated {length(pages_blocks)} image(s) with block bounding boxes."))

}

#' Draw paragraph bounding boxes
#'
#' @description Plots the paragraph bounding boxes identified by
#' Document AI (DAI) onto images of the submitted document.
#' Generates an annotated .png file for each page in the original
#' document.
#'
#' @param object either a HTTP response object from 
#' \code{dai_sync()} or the path to a JSON file from 
#' \code{dai_async()}.
#' @param type one of "sync" or "async", depending on
#' the function used to process the original document.
#' @param prefix string to be prepended to the output png filename.
#' @param dir path to the desired output directory.
#' @param linecol color of the bounding box line.
#' @param linewd width of the bounding box line.
#' @param fontcol color of the box numbers.
#' @param fontsize size of the box numbers.
#' @return no return value, called for side effects.
#'
#' @details Not vectorized, but documents can be multi-page.
#'
#' @export
#' @examples
#' \dontrun{
#' resp <- dai_sync("page.pdf")
#' draw_paragraphs(resp)
#'
#' draw_paragraphs("page.json", type = "async")
#' }

draw_paragraphs <- function(object,
                            type = "sync",
                            prefix = NULL,
                            dir = getwd(),
                            linecol = "red",
                            linewd = 3,
                            fontcol = "blue",
                            fontsize = 4
) {

  # checks
  if (!(length(type) == 1) || !(type %in% c("sync", "async"))) {
    stop("Invalid type parameter.")
  }

  if (length(prefix) > 1 || is.numeric(prefix)) {
    stop("Invalid prefix parameter.")
  }

  if (length(dir) > 1 || !(is.character(dir))) {
    stop("Invalid dir parameter. Must be a valid folder path.")
  }

  if (!(length(linecol) == 1) || !(daiR::is_colour(linecol)) || is.na(linecol)) {
    stop("Invalid linecol parameter. Must be a single valid colour representation.")
  }

  if (!(is.numeric(linewd) && length(linewd) == 1)) {
    stop("Invalid linewd parameter. Must be a single number.")
  }

  if (!(length(fontcol) == 1) || !(daiR::is_colour(fontcol)) || is.na(fontcol)) {
    stop("Invalid fontcol parameter. Must be a single valid colour representation.")
  }

  if (!(is.numeric(fontsize) && length(fontsize) == 1)) {
    stop("Invalid fontsize parameter. Must be a single number.")
  }

  dir <- normalizePath(dir, winslash = "/")

  if (type == "sync") {

    if (!(inherits(object, "response"))) {
      stop("Object parameter not pointing to valid response object.")
    }

    # extract a list with pagewise sets of block boundary boxes
    parsed <- httr::content(object)
    pages <- parsed$document$pages
    pages_paragraphs <- purrr::map(pages, ~.x$paragraphs)
    pagewise_block_sets <- purrr::map(pages_paragraphs, get_vertices)
    pagewise_block_sets <- purrr::map(pagewise_block_sets, transpose_page)

    # decode base64 and save to temp images
    page_imgs_base64 <- purrr::map_chr(pages, ~.x$image$content)
    imgs <- purrr::map2_chr(page_imgs_base64, seq_along(page_imgs_base64), decode_and_save)

  } else if (type == "async") {

    if (!(is_json(object))) {
      stop("Object parameter not pointing to valid JSON file.")
    }

    # extract a list with pagewise sets of block boundary boxes
    parsed <- jsonlite::fromJSON(object)
    pages_paragraphs <- parsed$pages$paragraphs
    pagewise_block_sets <- purrr::map(pages_paragraphs, ~.x$layout$boundingPoly$normalizedVertices)

    # decode base64 and save to temp images
    page_imgs_base64 <- parsed$pages$image$content
    imgs <- purrr::map2_chr(page_imgs_base64, seq_along(page_imgs_base64), decode_and_save)

  }

  # Plot bounding boxes
  purrr::map2(pagewise_block_sets, seq_along(pagewise_block_sets), ~ process_image(.x, .y, imgs, type, object, prefix, dir, filename, dest, linecol, linewd, fontcol, fontsize, boxtype = "paragraphs"))

  cli::cli_alert_success(glue::glue("Generated {length(pages_paragraphs)} image(s) with paragraph bounding boxes."))

}

#' Draw line bounding boxes
#'
#' @description Plots the line bounding boxes identified by
#' Document AI (DAI) onto images of the submitted document.
#' Generates an annotated .png file for each page in the original
#' document.
#'
#' @param object either a HTTP response object from 
#' \code{dai_sync()} or the path to a JSON file from 
#' \code{dai_async()}.
#' @param type one of "sync" or "async", depending on
#' the function used to process the original document.
#' @param prefix string to be prepended to the output png filename.
#' @param dir path to the desired output directory.
#' @param linecol color of the bounding box line.
#' @param linewd width of the bounding box line.
#' @param fontcol color of the box numbers.
#' @param fontsize size of the box numbers.
#' @return no return value, called for side effects.
#'
#' @details Not vectorized, but documents can be multi-page.
#'
#' @export
#' @examples
#' \dontrun{
#' resp <- dai_sync("page.pdf")
#' draw_lines(resp)
#'
#' draw_lines("page.json", type = "async")
#' }

draw_lines <- function(object,
                       type = "sync",
                       prefix = NULL,
                       dir = getwd(),
                       linecol = "red",
                       linewd = 3,
                       fontcol = "blue",
                       fontsize = 4
) {

  # checks
  if (!(length(type) == 1) || !(type %in% c("sync", "async"))) {
    stop("Invalid type parameter.")
  }

  if (length(prefix) > 1 || is.numeric(prefix)) {
    stop("Invalid prefix parameter.")
  }

  if (length(dir) > 1 || !(is.character(dir))) {
    stop("Invalid dir parameter. Must be a valid folder path.")
  }

  if (!(length(linecol) == 1) || !(daiR::is_colour(linecol)) || is.na(linecol)) {
    stop("Invalid linecol parameter. Must be a single valid colour representation.")
  }

  if (!(is.numeric(linewd) && length(linewd) == 1)) {
    stop("Invalid linewd parameter. Must be a single number.")
  }

  if (!(length(fontcol) == 1) || !(daiR::is_colour(fontcol)) || is.na(fontcol)) {
    stop("Invalid fontcol parameter. Must be a single valid colour representation.")
  }

  if (!(is.numeric(fontsize) && length(fontsize) == 1)) {
    stop("Invalid fontsize parameter. Must be a single number.")
  }

  dir <- normalizePath(dir, winslash = "/")

  if (type == "sync") {

    if (!(inherits(object, "response"))) {
      stop("Object parameter not pointing to valid response object.")
    }

    # extract a list with pagewise sets of block boundary boxes
    parsed <- httr::content(object)
    pages <- parsed$document$pages
    pages_lines <- purrr::map(pages, ~.x$lines)
    pagewise_block_sets <- purrr::map(pages_lines, get_vertices)
    pagewise_block_sets <- purrr::map(pagewise_block_sets, transpose_page)

    # decode base64 and save to temp images
    page_imgs_base64 <- purrr::map_chr(pages, ~.x$image$content)
    imgs <- purrr::map2_chr(page_imgs_base64, seq_along(page_imgs_base64), decode_and_save)

  } else if (type == "async") {

    if (!(is_json(object))) {
      stop("Object parameter not pointing to valid JSON file.")
    }

    # extract a list with pagewise sets of block boundary boxes
    parsed <- jsonlite::fromJSON(object)
    pages_lines <- parsed$pages$lines
    pagewise_block_sets <- purrr::map(pages_lines, ~.x$layout$boundingPoly$normalizedVertices)

    # decode base64 and save to temp images
    page_imgs_base64 <- parsed$pages$image$content
    imgs <- purrr::map2_chr(page_imgs_base64, seq_along(page_imgs_base64), decode_and_save)

  }

  # Plot bounding boxes
  purrr::map2(pagewise_block_sets, seq_along(pagewise_block_sets), ~ process_image(.x, .y, imgs, type, object, prefix, dir, filename, dest, linecol, linewd, fontcol, fontsize, boxtype = "lines"))

  cli::cli_alert_success(glue::glue("Generated {length(pages_lines)} image(s) with line bounding boxes."))

}

#' Draw token bounding boxes
#'
#' @description Plots the token (i.e., word) bounding boxes identified
#' by Document AI (DAI) onto images of the submitted document.
#' Generates an annotated .png file for each page in the original
#' document.
#'
#' @param object either a HTTP response object from 
#' \code{dai_sync()} or the path to a JSON file from 
#' \code{dai_async()}.
#' @param type one of "sync" or "async", depending on
#' the function used to process the original document.
#' @param prefix string to be prepended to the output png filename.
#' @param dir path to the desired output directory.
#' @param linecol color of the bounding box line.
#' @param linewd width of the bounding box line.
#' @param fontcol color of the box numbers.
#' @param fontsize size of the box numbers.
#' @return no return value, called for side effects.
#'
#' @details Not vectorized, but documents can be multi-page.
#'
#' @export
#' @examples
#' \dontrun{
#' resp <- dai_sync("page.pdf")
#' draw_tokens(resp)
#'
#' draw_tokens("page.json", type = "async")
#'}

draw_tokens <- function(object,
                        type = "sync",
                        prefix = NULL,
                        dir = getwd(),
                        linecol = "red",
                        linewd = 3,
                        fontcol = "blue",
                        fontsize = 4
) {

  # checks
  if (!(length(type) == 1) || !(type %in% c("sync", "async"))) {
    stop("Invalid type parameter.")
  }

  if (length(prefix) > 1 || is.numeric(prefix)) {
    stop("Invalid prefix parameter.")
  }

  if (length(dir) > 1 || !(is.character(dir))) {
    stop("Invalid dir parameter. Must be a valid folder path.")
  }
 
  if (!(length(linecol) == 1) || !(daiR::is_colour(linecol)) || is.na(linecol)) {
    stop("Invalid linecol parameter. Must be a single valid colour representation.")
  }

  if (!(is.numeric(linewd) && length(linewd) == 1)) {
    stop("Invalid linewd parameter. Must be a single number.")
  }

  if (!(length(fontcol) == 1) || !(daiR::is_colour(fontcol)) || is.na(fontcol)) {
    stop("Invalid fontcol parameter. Must be a single valid colour representation.")
  }

  if (!(is.numeric(fontsize) && length(fontsize) == 1)) {
    stop("Invalid fontsize parameter. Must be a single number.")
  }

  dir <- normalizePath(dir, winslash = "/")

  if (type == "sync") {

    if (!(inherits(object, "response"))) {
      stop("Object parameter not pointing to valid response object.")
    }

    # extract a list with pagewise sets of block boundary boxes
    parsed <- httr::content(object)
    pages <- parsed$document$pages
    pages_tokens <- purrr::map(pages, ~.x$tokens)
    pagewise_block_sets <- purrr::map(pages_tokens, get_vertices)
    pagewise_block_sets <- purrr::map(pagewise_block_sets, transpose_page)

    # decode base64 and save to temp images
    page_imgs_base64 <- purrr::map_chr(pages, ~.x$image$content)
    imgs <- purrr::map2_chr(page_imgs_base64, seq_along(page_imgs_base64), decode_and_save)

  } else if (type == "async") {

    if (!(is_json(object))) {
      stop("Object parameter not pointing to valid JSON file.")
    }

    # extract a list with pagewise sets of block boundary boxes
    parsed <- jsonlite::fromJSON(object)
    pages_tokens <- parsed$pages$tokens
    pagewise_block_sets <- purrr::map(pages_tokens, ~.x$layout$boundingPoly$normalizedVertices)

    # decode base64 and save to temp images
    page_imgs_base64 <- parsed$pages$image$content
    imgs <- purrr::map2_chr(page_imgs_base64, seq_along(page_imgs_base64), decode_and_save)

  }

  # Plot bounding boxes
  purrr::map2(pagewise_block_sets, seq_along(pagewise_block_sets), ~ process_image(.x, .y, imgs, type, object, prefix, dir, filename, dest, linecol, linewd, fontcol, fontsize, boxtype = "tokens"))

  cli::cli_alert_success(glue::glue("Generated {length(pages_tokens)} image(s) with token bounding boxes."))

}

#' Draw entity bounding boxes
#'
#' @description Plots the entity bounding boxes identified
#' by a Document AI form parser processor onto images of the 
#' submitted document. Generates an annotated .png file for 
#' each page in the original document.
#'
#' @param object either a HTTP response object from 
#' \code{dai_sync()} or the path to a JSON file from 
#' \code{dai_async()}.
#' @param type one of "sync" or "async", depending on
#' the function used to process the original document.
#' @param prefix string to be prepended to the output png filename.
#' @param dir path to the desired output directory.
#' @param linecol color of the bounding box line.
#' @param linewd width of the bounding box line.
#' @param fontcol color of the box numbers.
#' @param fontsize size of the box numbers.
#' @return no return value, called for side effects.
#'
#' @details Not vectorized, but documents can be multi-page.
#'
#' @export
#' @examples
#' \dontrun{
#' resp <- dai_sync("page.pdf")
#' draw_entities(resp)
#'
#' draw_tokens("page.json", type = "async")
#'
#' }

draw_entities <- function(object,
                          type = "sync",
                          prefix = NULL,
                          dir = getwd(),
                          linecol = "red",
                          linewd = 3,
                          fontcol = "blue",
                          fontsize = 4
) {

  # checks
  if (!(length(type) == 1) || !(type %in% c("sync", "async"))) {
    stop("Invalid type parameter.")
  }

  if (length(prefix) > 1 || is.numeric(prefix)) {
    stop("Invalid prefix parameter.")
  }

  if (length(dir) > 1 || !(is.character(dir))) {
    stop("Invalid dir parameter. Must be a valid folder path.")
  }

  if (!(length(linecol) == 1) || !(daiR::is_colour(linecol)) || is.na(linecol)) {
    stop("Invalid linecol parameter. Must be a single valid colour representation.")
  }

  if (!(is.numeric(linewd) && length(linewd) == 1)) {
    stop("Invalid linewd parameter. Must be a single number.")
  }

  if (!(length(fontcol) == 1) || !(daiR::is_colour(fontcol)) || is.na(fontcol)) {
    stop("Invalid fontcol parameter. Must be a single valid colour representation.")
  }

  if (!(is.numeric(fontsize) && length(fontsize) == 1)) {
    stop("Invalid fontsize parameter. Must be a single number.")
  }

  dir <- normalizePath(dir, winslash = "/")

  if (type == "sync") {

    if (!(inherits(object, "response"))) {
      stop("Object parameter not pointing to valid response object.")
    }

    # extract a list with pagewise sets of entity boundary boxes
    parsed <- httr::content(object)
    entities <- parsed$document$entities # each item is a page
    pages_entities <- purrr::map(entities, ~.x$properties)
    pagewise_entities_sets <- purrr::map(pages_entities, get_vertices_entities)
    pagewise_entities_sets <- purrr::map(pagewise_entities_sets, transpose_page)

    # decode base64 and save to temp images
    pages <- parsed$document$pages
    page_imgs_base64 <- purrr::map_chr(pages, ~.x$image$content)
    imgs <- purrr::map2_chr(page_imgs_base64, seq_along(page_imgs_base64), decode_and_save)

  } else if (type == "async") {

    if (!(is_json(object))) {
      stop("Object parameter not pointing to valid JSON file.")
    }

    # extract a list with pagewise sets of entity boundary boxes
    parsed <- jsonlite::fromJSON(object)
    pages_entities <- parsed$entities$properties
    pagewise_entities_sets <- purrr::map(pages_entities, ~.x$pageAnchor$pageRefs)
    pagewise_entities_sets <- purrr::map(pagewise_entities_sets, transpose_page_entities)

    # decode base64 and save to temp images
    page_imgs_base64 <- parsed$pages$image$content
    imgs <- purrr::map2_chr(page_imgs_base64, seq_along(page_imgs_base64), decode_and_save)
  }

  # Plot bounding boxes
  purrr::map2(pagewise_entities_sets, seq_along(pagewise_entities_sets), ~ process_image(.x, .y, imgs, type, object, prefix, dir, filename, dest, linecol, linewd, fontcol, fontsize, boxtype = "entities"))

  cli::cli_alert_success(glue::glue("Generated {length(pages_entities)} image(s) with entity bounding boxes."))

}

#' Decode and save base64 image
#'
#' @description Helper function for decoding base64 images and saving them to temporary files
#'
#' @param base64_string a string with a base64-encoded image.
#' @param index an integer representing the index of the element in the vector of base64-encoded images 
#'
#' @noRd

decode_and_save <- function(base64_string, index) {
  path <- file.path(tempdir(), glue::glue("page{index}.jpg"))
  outconn <- file(path, "wb")
  base64enc::base64decode(base64_string, outconn)
  close(outconn)
  return(path)
}

#' Plot bounding box
#'
#' @description Helper function to plot bounding box on image
#'
#' @param box a dataframe of bounding box coordinates.
#' @param index an integer representing the index of the element in the vector of bounding box dataframes
#' @param info interitance from parent function
#' @param linecol interitance from parent function
#' @param linewd interitance from parent function
#' @param fontcol interitance from parent function
#' @param fontsize interitance from parent function
#'
#' @noRd

plot_box <- function(box, index, info, linecol, linewd, fontcol, fontsize) {
  if (is.na(box$y[1])) box$y[1] <- 0
  if (is.na(box$y[2])) box$y[2] <- 0
  if (is.na(box$x[1])) box$x[1] <- 0
  if (is.na(box$x[4])) box$x[4] <- 0
  box$x1 <- box$x * info$width
  box$y1 <- box$y * info$height
  graphics::polygon(
    x = box$x1,
    y = box$y1,
    border = linecol,
    lwd = linewd
  )
  graphics::text(
    x = box$x1[1],
    y = box$y1[1],
    label = index,
    col = fontcol,
    cex = fontsize,
    #family = "Liberation Sans"
  )
}

#' Process image
#'
#' @description Helper function to save image of page with drawn bounding boxes
#'
#' @param pagewise_block_set a list of dataframes of bounding box coordinates.
#' @param index an integer representing the index of the element in the list of dataframes of bounding box coordinates
#' @param imgs vector of paths to the unannotated images
#' @param type interitance from parent function
#' @param object interitance from parent function
#' @param prefix interitance from parent function
#' @param dir interitance from parent function
#' @param filename interitance from parent function
#' @param dest interitance from parent function
#' @param linecol interitance from parent function
#' @param linewd interitance from parent function
#' @param fontcol interitance from parent function
#' @param fontsize interitance from parent function
#' @param boxtype string with the type of bounding box to be drawn
#'
#' @noRd

process_image <- function(pagewise_block_set, index, imgs, type, object, prefix, dir, filename, dest, linecol, linewd, fontcol, fontsize, boxtype) {
  img <- magick::image_read(imgs[index])
  info <- magick::image_info(img)
  canvas <- magick::image_draw(img)
  purrr::map2(pagewise_block_set, seq_along(pagewise_block_set), ~ plot_box(.x, .y, info, linecol, linewd, fontcol, fontsize))
  if (type == "async") {
    default_prefix <- substr(basename(object), 1, nchar(basename(object)) - 5)
  } else {
    default_prefix <- "document"
  }
  if (is.null(prefix)) {
    filename <- glue::glue("{default_prefix}_page{index}_{boxtype}.png")
  } else {
    filename <- glue::glue("{prefix}_page{index}_{boxtype}.png")
  }
  dest <- file.path(dir, filename)
  magick::image_write(canvas, format = "png", dest)
  grDevices::dev.off()
}
