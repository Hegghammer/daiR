#' Make hOCR file
#'
#' @description Creates a hOCR file from Document AI output.
#'
#' @param type one of "sync" or "async" depending on
#' the function used to process the original document.
#' @param output either a HTTP response object (from `dai_sync()`) or
#' the path to a JSON file (from `dai_async`).
#' @param outfile_name a string with the desired filename. Must end with
#' either `.hocr`, `.html`, or `.xml`.
#' @param dir a string with the path to the desired output directory.
#' @return no return value, called for side effects.
#'
#' @details hOCR is an open standard of data representation for formatted
#' text obtained from optical character recognition. It can be used to
#' generate searchable PDFs and many other things. This function generates
#' a file compliant with the official hOCR specification
#' (https://github.com/kba/hocr-spec) complete with token-level confidence
#' scores. It also works with non-latin scripts and right-to-left languages.
#'
#' @export

make_hocr <- function(
  type,
  output,
  outfile_name = "out.hocr",
  dir = getwd()
) {
  # checks
  if (!(length(type) == 1) || !(type %in% c("sync", "async"))) {
    stop("Invalid type parameter.")
  }

  if (!(inherits(output, "response") || is_json(output))) {
    stop("Invalid output parameter.")
  }

  if (length(dir) > 1 || !(is.character(dir))) {
    stop("Invalid dir parameter. Must be a valid folder path.")
  }

  if (!(length(outfile_name) == 1) || !(is.character(outfile_name))) {
    stop("Invalid outfile_name parameter. Must be a string ending with .hocr, .html, or .xml.")
  }

  extension <- tolower(stringr::str_extract(outfile_name, "(?<=\\.)\\w{3,4}$"))
  supported <- c("hocr", "html", "xml")

  if (!(extension %in% supported)) {
    stop("outfile_name extension must be of type .hocr, .html, or .xml")
  }

  if (grepl("/|\\\\", outfile_name)) {
    stop("outfile_name cannot contain slashes. Use the dir parameter for directory selection.")
  }

  # create doc
  hocr_stem <- fs::path_package("extdata", "hocr_stem.xml", package = "daiR")
  doc <- xml2::read_xml(hocr_stem)

  message("Generating hOCR file. This may take a few seconds.")

  if (type == "sync") {
    parsed_sync <- httr::content(output)

    # add metadata
    langs <- paste(parsed_sync[["document"]][["pages"]][[1]][["detectedLanguages"]][[1]][["languageCode"]])
    n_pages <- length(parsed_sync[["document"]][["pages"]])
    head <- xml2::xml_children(doc)[[1]]
    lang_node <- xml2::xml_children(head)[[4]]
    xml2::xml_attrs(lang_node)[[2]] <- langs
    pages_node <- xml2::xml_children(head)[[5]]
    xml2::xml_attrs(pages_node)[[2]] <- n_pages

    # add text content
    text <- parsed_sync[["document"]][["text"]]
    body <- xml2::xml_children(doc)[[2]]
    pages <- parsed_sync[["document"]][["pages"]]

    for (i in seq_along(pages)) {
      message(glue::glue("Processing page {i} of {n_pages} .."))
      process_page_sync(body, i, pages, text)
    }
  } else if (type == "async") {
    parsed <- jsonlite::fromJSON(output)

    # add metadata
    langs <- paste(parsed[["pages"]][["detectedLanguages"]][[1]][["languageCode"]], collapse = " ")
    n_pages <- nrow(parsed$pages)
    head <- xml2::xml_children(doc)[[1]]
    lang_node <- xml2::xml_children(head)[[4]]
    xml2::xml_attrs(lang_node)[[2]] <- langs
    pages_node <- xml2::xml_children(head)[[5]]
    xml2::xml_attrs(pages_node)[[2]] <- n_pages

    # add text content
    text <- parsed$text
    body <- xml2::xml_children(doc)[[2]]
    page_coords <- parsed[["pages"]][["layout"]][["boundingPoly"]][["vertices"]]

    for (i in seq_along(page_coords)) {
      message(glue::glue("Processing page {i} of {n_pages} .."))
      process_page_async(body, i, parsed, text)
    }
  }

  dest <- file.path(dir, outfile_name)
  xml2::write_xml(doc, dest)
  cli::cli_alert_success(glue::glue("hOCR file named '{outfile_name}' generated in {dir}."))
}

#' Process page (sync)
#' @noRd
process_page_sync <- function(
  body,
  page_index,
  pages,
  text
  ) {

  page <- pages[[page_index]]
  id <- paste0("page_", page_index)
  x2_page <- page[["dimension"]][["width"]]
  y2_page <- page[["dimension"]][["height"]]
  bbox <- paste0("bbox 0 0 ", x2_page, " ", y2_page)
  xml2::xml_add_child(body, "div", class = 'ocr_page', id = id, title = bbox)
  div1 <- xml2::xml_children(body)[[page_index]]
  
  blocks <- page[["blocks"]]
  block_coords <- get_vertices(blocks)
  block_coords <- purrr::map(block_coords, transpose_block)
  block_coords <- purrr::map(block_coords, ~ process_coord(.x, x2_page, y2_page))
  block_segments <- purrr::map(blocks, ~.x[["layout"]][["textAnchor"]][["textSegments"]][[1]])
  if (is.null(block_segments[[1]][["startIndex"]])) block_segments[[1]][["startIndex"]] <- 0

  for (j in seq_along(block_coords)) {
    process_block_sync(
      div1,
      j,
      page,
      page_index,
      x2_page,
      y2_page,
      block_coords,
      block_segments,
      text
    )
  }
}

#' Process block (sync)
#' @noRd
process_block_sync <- function(
  div1,
  block_index,
  page,
  page_index,
  x2_page,
  y2_page,
  block_coords,
  block_segments,
  text
  ) {

  id <- paste0("block_", page_index, "_", block_index)
  bbox <- mk_bbox(block_coords, block_index)
  xml2::xml_add_child(div1, "div", class = 'ocr_carea', id = id, title = bbox)
  div2 <- xml2::xml_children(div1)[[block_index]]
  
  paras <- page[["paragraphs"]]
  all_para_coords <- purrr::map(paras, ~.x[["layout"]][["boundingPoly"]][["normalizedVertices"]])
  all_para_coords <- purrr::map(all_para_coords, transpose_block)
  all_para_coords <- purrr::map(all_para_coords, ~ process_coord(.x, x2_page, y2_page))
  all_para_segments <- purrr::map(paras, ~.x[["layout"]][["textAnchor"]][["textSegments"]][[1]])
  if (is.null(all_para_segments[[1]][["startIndex"]])) all_para_segments[[1]][["startIndex"]] <- 0
  
  block_start_ind <- as.integer(block_segments[[block_index]][["startIndex"]])
  block_end_ind <- as.integer(block_segments[[block_index]][["endIndex"]])
  para_data <- filter_by_text_range(
    all_para_coords,
    all_para_segments,
    block_start_ind,
    block_end_ind
  )

  for (k in seq_along(para_data$coords)) {
      process_paragraph_sync(
        div2,
        k,
        page,
        page_index,
        block_index,
        x2_page,
        y2_page,
        para_data$coords,
        para_data$segments,
        text
      )
  }
}

#' Process paragraph (sync)
#' @noRd
process_paragraph_sync <- function(
  div2,
  para_index,
  page,
  page_index,
  block_index,
  x2_page,
  y2_page,
  para_coords,
  para_segments,
  text
  ) {

  id <- paste0("par_", page_index, "_", block_index, "_", para_index)
  bbox <- mk_bbox(para_coords, para_index)
  xml2::xml_add_child(div2, "p", class = 'ocr_par', id = id, title = bbox)
  p <- xml2::xml_children(div2)[[para_index]]
  
  lines <- page[["lines"]]
  all_line_coords <- purrr::map(lines, ~.x[["layout"]][["boundingPoly"]][["normalizedVertices"]])
  all_line_coords <- purrr::map(all_line_coords, transpose_block)
  all_line_coords <- purrr::map(all_line_coords, ~ process_coord(.x, x2_page, y2_page))
  all_line_segments <- purrr::map(lines, ~.x[["layout"]][["textAnchor"]][["textSegments"]][[1]])
  if (is.null(all_line_segments[[1]][["startIndex"]])) all_line_segments[[1]][["startIndex"]] <- 0
  
  para_start_ind <- as.integer(para_segments[[para_index]][["startIndex"]])
  para_end_ind <- as.integer(para_segments[[para_index]][["endIndex"]])
  line_data <- filter_by_text_range(
    all_line_coords,
    all_line_segments,
    para_start_ind,
    para_end_ind
  )

  for (l in seq_along(line_data$coords)) {
    process_line_sync(
      p,
      l,
      page,
      page_index,
      block_index,
      para_index,
      x2_page,
      y2_page,
      line_data$coords,
      line_data$segments,
      text
    )
  }
}

#' Process line (sync)
#' @noRd
process_line_sync <- function(
  p,
  line_index,
  page,
  page_index,
  block_index,
  para_index,
  x2_page,
  y2_page,
  line_coords,
  line_segments, text
  ) {

  id <- paste0("line_", page_index, "_", block_index, "_", para_index, "_", line_index)
  bbox <- mk_bbox(line_coords, line_index)
  xml2::xml_add_child(p, "span", class = 'ocr_line', id = id, title = bbox)
  span1 <- xml2::xml_children(p)[[line_index]]
  
  tokens <- page[["tokens"]]
  all_token_coords <- purrr::map(tokens, ~.x[["layout"]][["boundingPoly"]][["normalizedVertices"]])
  all_token_coords <- purrr::map(all_token_coords, transpose_block)
  all_token_coords <- purrr::map(all_token_coords, ~ process_coord(.x, x2_page, y2_page))
  all_token_segments <- purrr::map(tokens, ~.x[["layout"]][["textAnchor"]][["textSegments"]][[1]])
  if (is.null(all_token_segments[[1]][["startIndex"]])) all_token_segments[[1]][["startIndex"]] <- 0
  all_token_confs <- unlist(purrr::map(tokens, ~.x[["layout"]][["confidence"]]))
  all_token_confs <- round(all_token_confs * 100, 2)
  
  line_start_ind <- as.integer(line_segments[[line_index]][["startIndex"]])
  line_end_ind <- as.integer(line_segments[[line_index]][["endIndex"]])
  token_data <- filter_by_text_range_with_conf(
    all_token_coords,
    all_token_segments,
    all_token_confs,
    line_start_ind,
    line_end_ind
  )

  for (m in seq_along(token_data$coords)) {
    process_token(
      span1,
      m,
      page_index,
      block_index,
      para_index,
      line_index,
      token_data$coords,
      token_data$segments,
      token_data$confs,
      text
    )
  }
}

#' Process page (async)
#' @noRd
process_page_async <- function(
  body,
  page_index,
  parsed,
  text
  ) {
  id <- paste0("page_", page_index)
  page_coords <- parsed[["pages"]][["layout"]][["boundingPoly"]][["vertices"]]
  x2_page <- page_coords[[page_index]][["x"]][[3]]
  y2_page <- page_coords[[page_index]][["y"]][[3]]
  bbox <- paste0("bbox 0 0 ", x2_page, " ", y2_page)
  xml2::xml_add_child(body, "div", class = 'ocr_page', id = id, title = bbox)
  div1 <- xml2::xml_children(body)[[page_index]]
  
  block_coords <- parsed[["pages"]][["blocks"]][[page_index]][["layout"]][["boundingPoly"]][["normalizedVertices"]]
  block_coords <- purrr::map(block_coords, ~ process_coord(.x, x2_page, y2_page))    
  block_segments <- parsed[["pages"]][["blocks"]][[page_index]][["layout"]][["textAnchor"]][["textSegments"]]
  if (is.null(block_segments[[1]][["startIndex"]])) block_segments[[1]][["startIndex"]] <- 0

  for (j in seq_along(block_coords)) {
    process_block_async(
      div1,
      j,
      page_index,
      parsed,
      x2_page,
      y2_page,
      block_coords,
      block_segments,
      text
    )
  }
}

#' Process block (async)
#' @noRd
process_block_async <- function(
  div1,
  block_index,
  page_index,
  parsed,
  x2_page,
  y2_page,
  block_coords, 
  block_segments, 
  text
  ) {

  id <- paste0("block_", page_index, "_", block_index)
  bbox <- mk_bbox(block_coords, block_index)
  xml2::xml_add_child(div1, "div", class = 'ocr_carea', id = id, title = bbox)
  div2 <- xml2::xml_children(div1)[[block_index]]
  
  all_para_coords <- parsed[["pages"]][["paragraphs"]][[page_index]][["layout"]][["boundingPoly"]][["normalizedVertices"]]
  all_para_coords <- purrr::map(all_para_coords , ~ process_coord(.x, x2_page, y2_page))
  all_para_segments <- parsed[["pages"]][["paragraphs"]][[page_index]][["layout"]][["textAnchor"]][["textSegments"]]
  if (is.null(all_para_segments[[1]][["startIndex"]])) all_para_segments[[1]][["startIndex"]] <- 0
  
  block_start_ind <- as.integer(block_segments[[block_index]][["startIndex"]])
  block_end_ind <- as.integer(block_segments[[block_index]][["endIndex"]])
  para_data <- filter_by_text_range(
    all_para_coords,
    all_para_segments,
    block_start_ind,
    block_end_ind
  )

  for (k in seq_along(para_data$coords)) {
    process_paragraph_async(
      div2,
      k,
      page_index,
      block_index,
      parsed,
      x2_page,
      y2_page,
      para_data$coords,
      para_data$segments,
      text
    )
  }
}

#' Process paragraph (async)
#' @noRd
process_paragraph_async <- function(
  div2,
  para_index,
  page_index,
  block_index,
  parsed,
  x2_page,
  y2_page,
  para_coords,
  para_segments,
  text) {

  id <- paste0("par_", page_index, "_", block_index, "_", para_index)
  bbox <- mk_bbox(para_coords, para_index)
  xml2::xml_add_child(div2, "p", class = 'ocr_par', id = id, title = bbox)
  p <- xml2::xml_children(div2)[[para_index]]
  
  all_line_coords <- parsed[["pages"]][["lines"]][[page_index]][["layout"]][["boundingPoly"]][["normalizedVertices"]]
  all_line_coords <- purrr::map(all_line_coords, ~ process_coord(.x, x2_page, y2_page))
  all_line_segments <- parsed[["pages"]][["lines"]][[page_index]][["layout"]][["textAnchor"]][["textSegments"]]
  if (is.null(all_line_segments[[1]][["startIndex"]])) all_line_segments[[1]][["startIndex"]] <- 0
  
  para_start_ind <- as.integer(para_segments[[para_index]][["startIndex"]])
  para_end_ind <- as.integer(para_segments[[para_index]][["endIndex"]])
  line_data <- filter_by_text_range(
    all_line_coords,
    all_line_segments,
    para_start_ind,
    para_end_ind
  )

  for (l in seq_along(line_data$coords)) {
    process_line_async(
      p,
      l,
      page_index,
      block_index,
      para_index,
      parsed,
      x2_page,
      y2_page,
      line_data$coords,
      line_data$segments,
      text
    )
  }
}

#' Process line (async)
#' @noRd
process_line_async <- function(
  p,
  line_index,
  page_index,
  block_index,
  para_index,
  parsed,
  x2_page,
  y2_page,
  line_coords,
  line_segments,
  text
  ) {
  id <- paste0("line_", page_index, "_", block_index, "_", para_index, "_", line_index)
  bbox <- mk_bbox(line_coords, line_index)
  xml2::xml_add_child(p, "span", class = 'ocr_line', id = id, title = bbox)
  span1 <- xml2::xml_children(p)[[line_index]]
  
  all_token_coords <- parsed[["pages"]][["tokens"]][[page_index]][["layout"]][["boundingPoly"]][["normalizedVertices"]]
  all_token_coords <- purrr::map(all_token_coords, ~ process_coord(.x, x2_page, y2_page))
  all_token_segments <- parsed[["pages"]][["tokens"]][[page_index]][["layout"]][["textAnchor"]][["textSegments"]]
  if (is.null(all_token_segments[[1]][["startIndex"]])) all_token_segments[[1]][["startIndex"]] <- 0
  all_token_confs <- parsed[["pages"]][["tokens"]][[page_index]][["layout"]][["confidence"]]
  all_token_confs <- round(all_token_confs * 100, 2)
  
  line_start_ind <- as.integer(line_segments[[line_index]][["startIndex"]])
  line_end_ind <- as.integer(line_segments[[line_index]][["endIndex"]])
  token_data <- filter_by_text_range_with_conf(
    all_token_coords,
    all_token_segments,
    all_token_confs,
    line_start_ind,
    line_end_ind
  )

  for (m in seq_along(token_data$coords)) {
    process_token(
      span1,
      m,
      page_index,
      block_index,
      para_index,
      line_index,
      token_data$coords,
      token_data$segments,
      token_data$confs,
      text
    )
  }
}

#' Filter elements within a parent element's text range
#' @noRd
filter_by_text_range <- function(
  all_coords,
  all_segments,
  parent_start,
  parent_end
  ) {

  filtered_coords <- list()
  filtered_segments <- list()
  
  for (z in seq_along(all_segments)) {
      start <- as.integer(all_segments[[z]][["startIndex"]])
      end <- as.integer(all_segments[[z]][["endIndex"]])
      
      if ((start >= parent_start) && (end <= parent_end)) {
          filtered_coords <- append(filtered_coords, all_coords[z])
          filtered_segments <- append(filtered_segments, all_segments[z])
      }
  }
  
  list(coords = filtered_coords, segments = filtered_segments)
}

#' Filter tokens with confidence scores
#' @noRd
filter_by_text_range_with_conf <- function(
  all_coords,
  all_segments,
  all_confs,
  parent_start,
  parent_end
  ) {

  filtered_coords <- list()
  filtered_segments <- list()
  filtered_confs <- list()
  
  for (z in seq_along(all_segments)) {
      start <- as.integer(all_segments[[z]][["startIndex"]])
      end <- as.integer(all_segments[[z]][["endIndex"]])
      
      if ((start >= parent_start) && (end <= parent_end)) {
          filtered_coords <- append(filtered_coords, all_coords[z])
          filtered_segments <- append(filtered_segments, all_segments[z])
          filtered_confs <- append(filtered_confs, all_confs[z])
      }
  }
  
  list(coords = filtered_coords, segments = filtered_segments, confs = filtered_confs)
}

#' Process a single token
#' @noRd
process_token <- function(
  line_span,
  token_index,
  page_index,
  block_index,
  para_index,
  line_index,
  token_coords,
  token_segments,
  token_confs,
  text) {

  id <- paste0("word_", page_index, "_", block_index, "_", para_index, "_", line_index, "_", token_index)

  bbox <- mk_bbox(token_coords, token_index)
  bbox <- paste0(bbox, "; x_wconf ", token_confs[[token_index]])
  xml2::xml_add_child(line_span, "span", class = 'ocrx_word', id = id, title = bbox)
  span2 <- xml2::xml_children(line_span)[[token_index]]
  
  start_ind <- token_segments[[token_index]][["startIndex"]]
  if (is.null(start_ind)) start_ind <- 0
  end_ind <- token_segments[[token_index]][["endIndex"]]
  
  word <- substr(text, start_ind, end_ind)
  if (substr(word, 2, 2) %in% c(".", ",", ";")) {
      word <- sub('^.', '', word)
  }
  if (substr(word, 1, 1) == "-") {
      word <- sub('^-', '', word)
  }
  xml2::xml_text(span2) <- word
}

#' Process coordinates
#' @noRd
process_coord <- function(
  coord,
  x2_page,
  y2_page
  ) {
  # handle async format (data frame with x, y columns)
  if (is.data.frame(coord)) {
      coord[coord < 0] <- 0
      xs <- round(coord[["x"]] * x2_page)
      ys <- round(coord[["y"]] * y2_page)
      return(list(xs = xs, ys = ys))
  }
  # handle sync format (list with xs, ys fields)
  else {
      coord[coord < 0] <- 0
      coord[["xs"]] <- round(coord[["xs"]] * x2_page)
      coord[["ys"]] <- round(coord[["ys"]] * y2_page)
      return(coord)
  }
}

#' Make bbox string
#' @noRd
mk_bbox <- function(
  coord,
  index
  ) {

  x1 <- coord[[index]][["xs"]][[1]]
  y1 <- coord[[index]][["ys"]][[1]]
  x2 <- coord[[index]][["xs"]][[3]]
  y2 <- coord[[index]][["ys"]][[3]]
  paste0("bbox ", x1, " ", y1, " ", x2, " ", y2)
}
