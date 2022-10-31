#' Get text from HTTP response object
#'
#' @description Extracts the text OCRed by Document AI (DAI) in a
#' synchronous processing request.
#'
#' @param response an HTTP response object returned by \code{dai_sync()}
#' @param save_to_file boolean; whether to save the text as a .txt file 
#' @param dest_dir folder path for the .txt output file if \code{save_to_file = TRUE}
#' @param filename string to form the stem of the .txt output file
#' @return a string (if \code{save_to_file = FALSE})
#' @export
#'
#' @examples
#' \dontrun{
#' text <- text_from_dai_response(response)
#' 
#' text_from_dai_response(response, save_to_file = TRUE)
#' }

text_from_dai_response <- function(response,
                                   save_to_file = FALSE,
                                   dest_dir = getwd(),
                                   filename = "output"
                                   ) {

  # checks
  if (!(inherits(response, "response"))) {
    stop("Input is not a valid HTTP response.")
    }

  parsed <- httr::content(response, as="parsed")

  #if (!("pages" %in% names(parsed$document) || "pages" %in% names(parsed))) {
  #  stop("Input not recognized. Is it from dai_async?")
  #}
  
  if (!("text" %in% names(parsed$document) || "text" %in% names(parsed))) {
    stop("DAI found no text. Was the page blank?")
  }
  
  if (!(save_to_file %in% c(TRUE, FALSE))) {
    stop("Invalid save_to_file argument. Must be either TRUE or FALSE.")
  }
  
  if (!(length(dest_dir) == 1)) {
    stop("Invalid dest_dir argument. Must be a valid folder path.")
  }
  
  if (!(is.character(dest_dir))) {
    stop("Invalid dest_dir argument. Must be a valid folder path.")
  }  
  
  if (!(length(filename) == 1)) {
    stop("Invalid dest_dir argument. Must be a valid folder path.")
  }
  
  if (!(is.character(filename))) {
    stop("Invalid dest_dir argument. Must be a valid folder path.")
  }
  
  if (nchar(filename) > 200) {
    stop("Filename too long.")
  }

  # get text
  if ("text" %in% names(parsed$document)) {
    text <- parsed$document$text
  } else {
  	text <- parsed$text
  }  

  # save to file if requested
  if (isTRUE(save_to_file)) {
    write(text, file.path(dest_dir, paste0(filename, ".txt")))
    } else {
      return(text)
      }

  }

#' Get text from output file
#'
#' @description Extracts the text OCRed by Document AI (DAI) in an
#' asynchronous processing request.
#'
#' @param file filepath of a JSON file obtained using \code{dai_async()}
#' @param save_to_file boolean; whether to save the text as a .txt file 
#' @param dest_dir folder path for the .txt output file if save_to_file=TRUE
#' @return a string (if \code{save_to_file = FALSE})
#' @export
#'
#' @examples
#' \dontrun{
#' text <- text_from_dai_file("mydoc-0.json")
#' text_from_dai_file("mydoc-0.json", save_to_file = TRUE)
#' }

text_from_dai_file <- function(file,
                               save_to_file = FALSE,
                               dest_dir = getwd()
                               ) {

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

  if (!(save_to_file %in% c(TRUE, FALSE))) {
    stop("Invalid save_to_file argument. Must be either TRUE or FALSE.")
  }
  
  if (!(length(dest_dir) == 1)) {
    stop("Invalid dest_dir argument. Must be a valid folder path.")
  }
  
  if (!(is.character(dest_dir))) {
    stop("Invalid dest_dir argument. Must be a valid folder path.")
  }  
  
  # get text
  text <- output$text

  if (isTRUE(save_to_file)) {
    stem <- gsub("\\.json", "", basename(file))
    write(text, file.path(dest_dir, paste0(stem, ".txt")))
    } else {
      return(text)
    }
  }

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
#' and so forth. For the v1beta2 API, it means files ending with 
#' "-page-1-to-100.txt", "-page-101-to-200.txt", etc. The safest approach is 
#' to generate .txt files using `text_from_dai_file()` with the `save_to_file` 
#' parameter set to TRUE.
#' 
#' @examples
#' \dontrun{
#' merge_shards(source_dir = getwd(), dest_dir = tempdir())
#' }

merge_shards <- function(source_dir,
                         dest_dir
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
    for (i in 1:length(v1beta2_stems)) {
      cluster <- grep(v1beta2_stems[i], v1beta2, value = TRUE)
      cluster_df <- readtext::readtext(cluster)
      text <- paste(cluster_df$text, collapse = "\n")
      write(text, file.path(dest_dir, paste0(basename(v1beta2_stems[i]), ".txt")))
      }
    }
  
  if (length(v1) > 0) {
    for (i in 1:length(v1_stems)) {
      cluster <- grep(v1_stems[i], v1, value = TRUE)
      cluster_df <- readtext::readtext(cluster)
      text <- paste(cluster_df$text, collapse = "\n")
      write(text, file.path(dest_dir, paste0(basename(v1_stems[i]), ".txt")))
      }
    }
  
  message("Shards merged.")
  
}

#' Inspect block bounding boxes
#'
#' @description Plots the block bounding boxes identified by
#' Document AI (DAI) onto images of the submitted document.
#' Generates an annotated .png file for each page in the original
#' document.
#'
#' @param json filepath of a JSON file obtained using \code{dai_async()}
#' @param prefix string to be prepended to output filename
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
                        prefix = NULL,
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

  if (length(prefix) > 1) {
    stop("Invalid prefix.")
  }

  if (!(is.character(prefix) || is.null(prefix))) {
    stop("Invalid prefix.")
    }

  if (length(dir) > 1) {
    stop("Invalid dir parameter. Must be a valid folder path.")
  }

  if (!(is.character(dir))) {
    stop("Invalid dir parameter. Must be a valid folder path.")
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

    default_prefix <- substr(basename(json), 1, nchar(basename(json)) -5)

    if (is.null(prefix)) {
      filename <- glue::glue("{default_prefix}_page{i}_blocks.png")
    } else {
      filename <- glue::glue("{prefix}_page{i}_blocks.png")
    }

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
#' @param prefix string to be prepended to output filename
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
                            prefix = NULL,
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

  if (length(prefix) > 1) {
    stop("Invalid prefix.")
  }

  if (!(is.character(prefix) || is.null(prefix))) {
    stop("Invalid prefix.")
    }

  if (length(dir) > 1) {
    stop("Invalid dir parameter. Must be a valid folder path.")
  }

  if (!(is.character(dir))) {
    stop("Invalid dir parameter. Must be a valid folder path.")
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

    default_prefix <- substr(basename(json), 1, nchar(basename(json)) -5)

    if (is.null(prefix)) {
      filename <- glue::glue("{default_prefix}_page{i}_paragraphs.png")
    } else {
      filename <- glue::glue("{prefix}_page{i}_paragraphs.png")
    }

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
#' @param prefix string to be prepended to output filename
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
                       prefix = NULL,
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

  if (length(prefix) > 1) {
    stop("Invalid prefix.")
  }

  if (!(is.character(prefix) || is.null(prefix))) {
    stop("Invalid prefix.")
  }

  if (length(dir) > 1) {
    stop("Invalid dir parameter. Must be a valid folder path.")
  }

  if (!(is.character(dir))) {
    stop("Invalid dir parameter. Must be a valid folder path.")
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

    default_prefix <- substr(basename(json), 1, nchar(basename(json)) -5)

    if (is.null(prefix)) {
      filename <- glue::glue("{default_prefix}_page{i}_lines.png")
    } else {
      filename <- glue::glue("{prefix}_page{i}_lines.png")
    }

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
#' @param prefix string to be prepended to output filename
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
                        prefix = NULL,
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

  if (length(prefix) > 1) {
    stop("Invalid prefix.")
  }

  if (!(is.character(prefix) || is.null(prefix))) {
    stop("Invalid prefix.")
  }

  if (length(dir) > 1) {
    stop("Invalid dir parameter. Must be a valid folder path.")
  }

  if (!(is.character(dir))) {
    stop("Invalid dir parameter. Must be a valid folder path.")
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

    default_prefix <- substr(basename(json), 1, nchar(basename(json)) -5)

    if (is.null(prefix)) {
      filename <- glue::glue("{default_prefix}_page{i}_tokens.png")
    } else {
      filename <- glue::glue("{prefix}_page{i}_tokens.png")
    }

    dest <- file.path(dir, filename)

    magick::image_write(canvas, format = "png", dest)

    grDevices::dev.off()

    }

  pages <- length(pages_tokens)

  message(glue::glue("Generated {pages} annotated image(s)."))

  }
