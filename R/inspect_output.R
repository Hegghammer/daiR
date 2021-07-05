#' Get text from HTTP response object
#'
#' @description Extracts the text OCRed by Document AI (DAI) in a
#' synchronous processing request.
#'
#' @param response an HTTP response object returned by \code{dai_sync()}
#' @return a string
#' @export
#'
#' @examples
#' \dontrun{
#' text <- text_from_dai_response(response)
#' }

text_from_dai_response <- function(response) {

  # checks
  if (!(inherits(response, "response"))) {
    stop("Input is not a valid HTTP response.")
    }

  parsed <- httr::content(response, as="parsed")

  if (!("pages" %in% names(parsed$document))) {
    stop("Input not recognized. Is it from dai_async?")
    }

  if (!("text" %in% names(parsed$document))) {
    stop("DAI found no text. Was the page blank?")
    }

  # get text
  text <- parsed$document$text

  return(text)

  }

#' Get text from output file
#'
#' @description Extracts the text OCRed by Document AI (DAI) in an
#' asynchronous processing request.
#'
#' @param file filepath of a JSON file obtained using \code{dai_async()}
#' @return a string
#' @export
#'
#' @examples
#' \dontrun{
#' text <- text_from_dai_file("output.json")
#' }

text_from_dai_file <- function(file) {

  # checks
  if (!(is.character(file) && length(file) == 1)) {
    stop("Invalid file input.")
    }

  if (!(is_json(file))){
    stop("Input file not .json. Is the file in your working directory?")
    }

  output <- jsonlite::fromJSON(file)

  if (!("pages" %in% names(output))) {
    stop("JSON not in right format. Is it from DAI?")
    }

  if (!("text" %in% names(output))) {
    stop("DAI found no text. Was the document blank?")
    }

  # get text
  text <- output$text

  return(text)

  }

#' Inspect block bounding boxes
#'
#' @description Plots the block bounding boxes identified by
#' Document AI (DAI) onto images of the submitted document.
#' Generates an annotated .png file for each page in the original
#' document.
#'
#' @param json filepath of a JSON file obtained using \code{dai_async()}
#' @param dir path to the desired output directory
#' @return no return value, called for side effects
#'
#' @details Not vectorized, but documents can be multi-page.
#'
#' @export
#' @examples
#' \dontrun{
#' draw_blocks("pdf_output.json", dir = tempdir())
#' }

draw_blocks <- function(json,
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

  # parse the json
  parsed <- jsonlite::fromJSON(json)

  # extract a list with pagewise sets of block boundary boxes
  pages_blocks <- parsed$pages$blocks

  pagewise_block_sets <- purrr::map(pages_blocks, ~.x$layout$boundingPoly$normalizedVertices)

  # Get vector of base64-encoded images
  page_imgs <- parsed$pages$image$content

  # loop over the pagewise sets
  for (i in 1:length(pagewise_block_sets)) {

    # decode base64
    path <- file.path(tempdir(), glue::glue("page{i}.jpg"))
    outconn <- file(path,"wb")
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
    for(box in pagewise_block_sets[[i]]) {

      # handle NAs in boxes on top or left edge
      if (is.na(box$y[1])) box$y[1] <- 0
      if (is.na(box$y[2])) box$y[2] <- 0
      if (is.na(box$x[1])) box$x[1] <- 0
      if (is.na(box$x[4])) box$x[4] <- 0

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

    filename <- glue::glue("page{i}_blocks.png")

    dest <- file.path(dir, filename)

    magick::image_write(canvas, format = "png", dest)

    grDevices::dev.off()

    }

  pages <- length(pages_blocks)

  message(glue::glue("Generated {pages} annotated image(s)."))

  }

#' Inspect paragraph bounding boxes
#'
#' @description Plots the paragraph bounding boxes identified by
#' Document AI (DAI) onto images of the submitted document.
#' Generates an annotated .png file for each page in the original
#' document.
#'
#' @param json filepath of a JSON file obtained using \code{dai_async()}
#' @param dir path to the desired output directory.
#' @return no return value, called for side effects
#'
#' @details Not vectorized, but documents can be multi-page.
#'
#' @export
#' @examples
#' \dontrun{
#' draw_paragraphs("pdf_output.json", dir = tempdir())
#' }

draw_paragraphs <- function(json,
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

  # parse the json
  parsed <- jsonlite::fromJSON(json)

  # extract a list with pagewise sets of paragraph boundary boxes
  pages_paras <- parsed$pages$paragraphs

  pagewise_para_sets <- purrr::map(pages_paras, ~.x$layout$boundingPoly$normalizedVertices)

  # Get vector of base64-encoded images
  page_imgs <- parsed$pages$image$content

  # loop over the pagewise sets
  for (i in 1:length(pagewise_para_sets)) {

    # decode base64
    path <- file.path(tempdir(), glue::glue("page{i}.jpg"))
    outconn <- file(path,"wb")
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
    for(box in pagewise_para_sets[[i]]){

      # handle NAs in boxes on top or left edge
      if (is.na(box$y[1])) box$y[1] <- 0
      if (is.na(box$y[2])) box$y[2] <- 0
      if (is.na(box$x[1])) box$x[1] <- 0
      if (is.na(box$x[4])) box$x[4] <- 0

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
                     cex = 3,
                     col = "blue",
                     family = "Liberation Sans"
                     )

      counter <- counter + 1
      }

    # write annotated image to file

    filename <- glue::glue("page{i}_paragraphs.png")

    dest <- file.path(dir, filename)

    magick::image_write(canvas, format = "png", dest)

    grDevices::dev.off()

    }

  pages <- length(pages_paras)

  message(glue::glue("Generated {pages} annotated image(s)."))

  }

#' Inspect line bounding boxes
#'
#' @description Plots the line bounding boxes identified by
#' Document AI (DAI) onto images of the submitted document.
#' Generates an annotated .png file for each page in the original
#' document.
#'
#' @param json filepath of a JSON file obtained using \code{dai_async()}
#' @param dir path to the desired output directory.
#' @return no return value, called for side effects
#'
#' @details Not vectorized, but documents can be multi-page.
#'
#' @export
#' @examples
#' \dontrun{
#' draw_lines("pdf_output.json", dir = tempdir())
#' }

draw_lines <- function(json,
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

  # parse the json
  parsed <- jsonlite::fromJSON(json)

  # extract a list with pagewise sets of line boundary boxes
  pages_lines <- parsed$pages$lines

  pagewise_line_sets <- purrr::map(pages_lines, ~.x$layout$boundingPoly$normalizedVertices)

  # Get vector of base64-encoded images
  page_imgs <- parsed$pages$image$content

  # loop over the pagewise sets
  for (i in 1:length(pagewise_line_sets)) {

    # decode base64
    path <- file.path(tempdir(), glue::glue("page{i}.jpg"))
    outconn <- file(path,"wb")
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
    for(box in pagewise_line_sets[[i]]) {

      # handle NAs in boxes on top or left edge
      if (is.na(box$y[1])) box$y[1] <- 0
      if (is.na(box$y[2])) box$y[2] <- 0
      if (is.na(box$x[1])) box$x[1] <- 0
      if (is.na(box$x[4])) box$x[4] <- 0

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
                     cex = 2,
                     col = "blue",
                     family = "Liberation Sans"
                     )

      counter <- counter + 1

      }

    # write annotated image to file

    filename <- glue::glue("page{i}_lines.png")

    dest <- file.path(dir, filename)

    magick::image_write(canvas, format = "png", dest)

    grDevices::dev.off()

    }

  pages <- length(pages_lines)

  message(glue::glue("Generated {pages} annotated image(s)."))

  }

#' Inspect token bounding boxes
#'
#' @description Plots the token (i.e., word) bounding boxes identified
#' by Document AI (DAI) onto images of the submitted document.
#' Generates an annotated .png file for each page in the original
#' document.
#'
#' @param json filepath of a JSON file obtained using \code{dai_async()}
#' @param dir path to the desired output directory.
#' @return no return value, called for side effects
#'
#' @details Not vectorized, but documents can be multi-page.
#'
#' @export
#' @examples
#' \dontrun{
#' draw_tokens("pdf_output.json", dir = tempdir())
#' }

draw_tokens <- function(json,
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

  # parse the json
  parsed <- jsonlite::fromJSON(json)

  # extract a list with pagewise sets of token boundary boxes
  pages_tokens <- parsed$pages$tokens

  pagewise_token_sets <- purrr::map(pages_tokens, ~.x$layout$boundingPoly$normalizedVertices)

  # Get vector of base64-encoded images
  page_imgs <- parsed$pages$image$content

  # loop over the pagewise sets
  for (i in 1:length(pagewise_token_sets)) {

    # decode base64
    path <- file.path(tempdir(), glue::glue("page{i}.jpg"))
    outconn <- file(path,"wb")
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
    for(box in pagewise_token_sets[[i]]) {

      # handle NAs in boxes on top or left edge
      if (is.na(box$y[1])) box$y[1] <- 0
      if (is.na(box$y[2])) box$y[2] <- 0
      if (is.na(box$x[1])) box$x[1] <- 0
      if (is.na(box$x[4])) box$x[4] <- 0

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
                     cex = 2,
                     col = "blue",
                     family = "Liberation Sans"
                     )

      counter <- counter + 1

      }

    # write annotated image to file

    filename <- glue::glue("page{i}_tokens.png")

    dest <- file.path(dir, filename)

    magick::image_write(canvas, format = "png", dest)

    grDevices::dev.off()

    }

  pages <- length(pages_tokens)

  message(glue::glue("Generated {pages} annotated image(s)."))

  }
