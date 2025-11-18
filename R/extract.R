#' Get text
#'
#' @description Extracts the text OCRed by Document AI (DAI)
#'
#' @param object either a HTTP response object from
#' \code{dai_sync()} or the path to a JSON file from
#' \code{dai_async()}.
#' @param type one of "sync" or "async", depending on
#' the function used to process the original document.
#' @param save_to_file boolean; whether to save the text as a .txt file
#' @param dest_dir folder path for the .txt output file if \code{save_to_file = TRUE}
#' @param outfile_stem string to form the stem of the
#' .txt output file
#' @return a string (if \code{save_to_file = FALSE})
#' @export
#'
#' @examples
#' \dontrun{
#' text <- get_text(dai_sync("file.pdf"))
#'
#' text <- get_text("file.json", type = "async", save_to_file = TRUE)
#' }
get_text <- function(object,
                     type = "sync",
                     save_to_file = FALSE,
                     dest_dir = getwd(),
                     outfile_stem = NULL) {

  # checks
  if (!(length(type) == 1) || !(type %in% c("sync", "async"))) {
    stop("Invalid type parameter.")
  }

  if (!(is.logical(save_to_file) && length(save_to_file) == 1 && !is.na(save_to_file))) {
    stop("Invalid save_to_file argument. Must be either TRUE or FALSE.")
  }

  if (!(length(dest_dir) == 1) || !(is.character(dest_dir))) {
    stop("Invalid dest_dir argument. Must be a valid folder path.")
  }

  if (!(length(outfile_stem) <= 1)) {
    stop("Invalid outfile_stem argument. Must be NULL or a string.")
  }

  if ((length(outfile_stem) == 1) && !(is.character(outfile_stem))) {
    stop("Invalid outfile_stem argument. Must be NULL or a string.")
  }

  dest_dir <- normalizePath(dest_dir, winslash = "/")

  if (type == "sync") {
    if (!(inherits(object, "response"))) {
      stop("Invalid object: not a valid HTTP response. Did you supply a JSON filepath without type = 'async'?")
    }

    parsed <- httr::content(object, as = "parsed")

    if (!("pages" %in% names(parsed$document) || "pages" %in% names(parsed))) {
      stop("Input not recognized. Is it from dai_async?")
    }

    if (!("text" %in% names(parsed$document) || "text" %in% names(parsed))) {
      warning("DAI found no text. The document may be blank.")
      text <- ""
    }

    # get text
    if ("text" %in% names(parsed$document)) {
      text <- parsed$document$text
    } else {
      text <- parsed$text
    }
  } else if (type == "async") {
    if (!(is.character(object) && length(object) == 1)) {
      stop("Invalid object: must be a single character string filepath.")
    }

    if (!(is_json(object))) {
      stop("Invalid object: file is not a .json file or does not exist. Is the file in your working directory?")
    }

    parsed <- jsonlite::fromJSON(object)

    if (!("pages" %in% names(parsed))) {
      stop("JSON not in right format. Is it from DAI?")
    }

    if (!("text" %in% names(parsed))) {
      warning("DAI found no text. The document may be blank.")
      text <- ""
    }

    if ("text" %in% names(parsed)) {
      text <- parsed$text
    }
  }

  # save to file if requested
  if (isTRUE(save_to_file) && !(is.null(outfile_stem))) {
    write(text, file.path(dest_dir, paste0(outfile_stem, ".txt")))
  } else if (isTRUE(save_to_file) && (is.null(outfile_stem))) {
    if (type == "sync") {
      stem <- "output"
    } else {
      stem <- gsub("\\.json", "", basename(object))
    }
    write(text, file.path(dest_dir, paste0(stem, ".txt")))
  }

  text
}

#' Get tables
#'
#' @description Extracts tables identified by a Document AI
#' form parser processor.
#' @param object either a HTTP response object from
#' \code{dai_sync()} or the path to a JSON file from
#' \code{dai_async()}.
#' @param type one of "sync" or "async", depending on
#' the function used to process the original document.
#' @return a list of data frames
#' @export
#'
#' @examples
#' \dontrun{
#'
#' tables <- get_tables(dai_sync("file.pdf"))
#'
#' tables <- get_tables("file.json", type = "async")
#' }
get_tables <- function(
  object,
  type = "sync"
  ) {


  if (!(length(type) == 1) || !(type %in% c("sync", "async"))) {
    stop("Invalid type parameter.")
  }

  if (type == "sync") {
    # checks
    if (!(inherits(object, "response"))) {
      stop("Invalid object: not a valid HTTP response. Did you supply a JSON filepath without type = 'async'?")
    }

    parsed <- httr::content(object, as = "parsed")

    if (!("pages" %in% names(parsed) || "pages" %in% names(parsed$document))) {
      stop("The supplied object is not from a successful HTTP request.")
    }

    if (!("text" %in% names(parsed) || "text" %in% names(parsed$document))) {
      stop("DAI found no text. Was the page blank?")
    }

    # compile list of table entries
    if ("pages" %in% names(parsed$document)) {
      table_list_raw <- purrr::map(parsed$document$pages, ~ .x$tables)
    } else {
      table_list_raw <- purrr::map(parsed$pages, ~ .x$tables)
    }

    if (all(sapply(table_list_raw, is.null))) {
      stop("DAI found no tables in the document.")
    }

    table_list <- purrr::flatten(table_list_raw)
    text <- get_text(object)
    purrr::map(table_list, ~ resp_build_table(.x, text))

  } else if (type == "async") {

    # checks
    if (!(is.character(object) && length(object) == 1)) {
      stop("Invalid object: must be a single character string filepath.")
    }

    if (!(is_json(object))) {
      stop("Invalid object: file is not a .json file or does not exist. Is the file in your working directory?")
    }

    parsed <- jsonlite::fromJSON(object)

    if (!("pages" %in% names(parsed))) {
      stop("JSON not in right format. Is it from DAI?")
    }

    if (!("text" %in% names(parsed))) {
      stop("DAI found no text. Was the document blank?")
    }

    if (!("tables" %in% names(parsed$pages))) {
      stop("DAI found no tables in the document.")
    }

    table_list_raw <- parsed$pages$tables
    table_list_by_page <- purrr::map(table_list_raw, file_get_table_objects)
    table_list <- purrr::flatten(table_list_by_page)
    text <- get_text(object, type = "async")
    purrr::map(table_list, ~ file_build_table(.x, text))
  }
}

#' Get entities
#'
#' @description Extracts entities Document AI (DAI) identified by a Document AI
#' form parser processor.
#'
#' @param object either a HTTP response object from
#' \code{dai_sync()} or the path to a JSON file from
#' \code{dai_async()}.
#' @param type one of "sync" or "async", depending on
#' the function used to process the original document.
#' @return a list of dataframes, one per page
#' @export
#'
#' @examples
#' \dontrun{
#' entities <- get_entities(dai_sync("file.pdf"))
#'
#' entities <- get_entities("file.json", type = "async")
#' }
get_entities <- function(
  object,
  type = "sync"
  ) {

  if (!(length(type) == 1) || !(type %in% c("sync", "async"))) {
    stop("Invalid type parameter.")
  }

  if (type == "sync") {
    # checks
    if (!(inherits(object, "response"))) {
      stop("Invalid object: not a valid HTTP response. Did you supply a JSON filepath without type = 'async'?")
    }

    parsed <- httr::content(object, as = "parsed")

    if (!("pages" %in% names(parsed) || "pages" %in% names(parsed$document))) {
      stop("The supplied object is not from a successful HTTP request.")
    }

    if (!("text" %in% names(parsed) || "text" %in% names(parsed$document))) {
      stop("DAI found no text. Was the page blank?")
    }

    parsed <- httr::content(object)

    if (!("entities" %in% names(parsed$document))) {
      message("Document AI identified no entities in the document.")
      return(NULL)
    } else {
      entity_pages <- parsed$document$entities
      purrr::map(entity_pages, build_sync_entity_df)
    }

  } else if (type == "async") {
    # checks
    if (!(is.character(object) && length(object) == 1)) {
      stop("Invalid object: must be a single character string filepath.")
    }

    if (!(is_json(object))) {
      stop("Invalid object: file is not a .json file or does not exist. Is the file in your working directory?")
    }

    parsed <- jsonlite::fromJSON(object)

    if (!("pages" %in% names(parsed))) {
      stop("JSON not in right format. Is it from DAI?")
    }

    if (!("text" %in% names(parsed))) {
      stop("DAI found no text. Was the document blank?")
    }

    if (!("entities" %in% names(parsed))) {
      message("Document AI identified no entities in the document.")
      return(NULL)
    } else {
      parsed <- jsonlite::fromJSON(object)
      entity_pages <- parsed$entities$properties
      purrr::map(entity_pages, build_async_entity_df)
    }
  }
}

#' Get cell text from response
#'
#' @description Helper function to get the text of an individual cell
#' @param cell a list from a parsed Document AI response object
#' @param text a string
#'
#' @noRd

resp_get_cell_text <- function(
  cell, 
  text
  ) {

  anchors <- cell$layout$textAnchor

  if (length(anchors) == 0) {
    txt <- ""
  } else {
    indices <- cell$layout$textAnchor$textSegments
    txt <- character()
    for (i in indices) {
      if (is.null(i$startIndex)) {
        line_start <- 1
      } else {
        line_start <- i$startIndex
      }
      line_end <- i$endIndex
      line_txt <- substr(text, line_start, line_end)
      txt <- paste(txt, line_txt, sep = "\n")
    }
  }
  txt
}

#' Compile cells into row from response
#'
#' @description Helper function to compile cell entries into a row vector
#' @param elem a list from a parsed Document AI response object
#' @param text a string
#'
#' @noRd

resp_get_row_vector <- function(
  elem, 
  text
  ) {
  cells <- elem$cells
  purrr::map_chr(cells, ~ resp_get_cell_text(.x, text))
}

#' Build table from rows in response
#'
#' @description Helper function to build a table from row vectors
#' @param table a list from a parsed Document AI response object
#' @param text a string
#'
#' @noRd

resp_build_table <- function(
  table,
  text
  ) {
  headers_list <- table$headerRows
  rows_list <- table$bodyRows
  headervectors <- purrr::map(headers_list, ~ resp_get_row_vector(.x, text))
  rowvectors <- purrr::map(rows_list, ~ resp_get_row_vector(.x, text))
  table <- data.frame(matrix(nrow = 0, ncol = 6))
  for (i in rowvectors) {
    table <- rbind(table, as.data.frame(t(i)))
  }
  stats::setNames(table, headervectors[[1]])
}

#' Get table objects from file
#'
#' @description Helper function to extract and reorganize
#' table-related elements from a parsed JSON file
#' @param page a list from a parsed JSON file from Document AI
#' @param text a string
#'
#' @noRd

file_get_table_objects <- function(page) {

  if (is.null(page)) {
    return(NULL)
  } else {
    pagewise_list_of_header_objs <- page$headerRows
    pagewise_list_of_row_objs <- page$bodyRows
    table_objects <- list()
    for (i in seq_along(pagewise_list_of_header_objs)) {
      table_object <- list(list(
        headerRows = pagewise_list_of_header_objs[[i]],
        bodyRows = pagewise_list_of_row_objs[[i]]
      ))
      table_objects <- append(table_objects, table_object)
    }
    table_objects
  }
}

#' Get cell text from file
#'
#' @description Helper function to get the text of an individual cell
#' @param cell a list from a parsed JSON file from Document AI
#' @param text a string
#'
#' @noRd

file_get_cell_text <- function(
  cell, 
  text
  ) {

  if (is.null(cell)) {
    txt <- ""
  } else {
    txt <- character()
    for (i in seq_len(nrow(cell))) {
      if (is.null(cell[i, "startIndex"])) {
        line_start <- 1
      } else {
        line_start <- cell[i, "startIndex"]
      }
      line_end <- cell[i, "endIndex"]
      line_txt <- substr(text, line_start, line_end)
      txt <- paste(txt, line_txt, sep = "\n")
    }
  }
  txt
}

#' Compile cells into rows from file
#'
#' @description Helper function to compile cell entries into a row vector
#' @param elem a list from a parsed JSON file from Document AI
#' @param text a string
#'
#' @noRd

file_get_row_vector <- function(
  elem,
  text
  ) {

  cells <- elem$layout$textAnchor$textSegments
  purrr::map_chr(cells, ~ file_get_cell_text(.x, text))
}

#' Build table from rows from file
#'
#' @description Helper function to build a table from row vectors
#' @param table_object a list from a parsed JSON file from Document AI
#' @param text a string
#'
#' @noRd

file_build_table <- function(
  table_object, 
  text
  ) {

  headers_list <- table_object$headerRows$cells
  rows_list <- table_object$bodyRows$cells
  headervectors <- purrr::map(headers_list, ~ file_get_row_vector(.x, text))
  rowvectors <- purrr::map(rows_list, ~ file_get_row_vector(.x, text))
  table <- data.frame(matrix(nrow = 0, ncol = 6))

  if (length(rowvectors) == 0) {
    table <- as.data.frame(t(headervectors[[1]]))
  } else {
    for (i in seq_along(rowvectors)) {
      if (is.null(rowvectors[[i]])) {
        if (i == 1) {
          table_width <- length(rowvectors[[i + 1]])
        } else {
          table_width <- length(rowvectors[[i - 1]])
        }
        rowvectors[[i]] <- rep("", times = table_width)
        table <- rbind(table, as.data.frame(t(rowvectors[[i]])))
      } else {
        table <- rbind(table, as.data.frame(t(rowvectors[[i]])))
      }
    }
    table <- stats::setNames(table, headervectors[[1]])
  }
  table
}

#' Build dataframe of entities from response
#'
#' @description Helper function to build dataframe of entities
#' @param lst a list from a parsed HTTP object from Document AI
#'
#' @noRd

build_sync_entity_df <- function(lst) {

  props <- lst$properties
  ids <- as.numeric(unlist(purrr::map(props, ~ .x$id)))
  mentionTexts <- purrr::map_chr(props, ~ .x$mentionText)
  types <- purrr::map_chr(props, ~ .x$type)
  confs <- as.numeric(unlist(purrr::map(props, ~ .x$confidence)))

  start_inds <- as.numeric(unlist(purrr::map(props, ~ .x$textAnchor$textSegments[[1]]$startIndex)))
  end_inds <- as.numeric(unlist(purrr::map(props, ~ .x$textAnchor$textSegments[[1]]$endIndex)))

  lefts <- as.numeric(unlist(purrr::map(props, ~ .x$pageAnchor$pageRefs[[2]][[1]][[1]][[1]]$x)))
  rights <- as.numeric(unlist(purrr::map(props, ~ .x$pageAnchor$pageRefs[[2]][[1]][[1]][[2]]$x)))
  tops <- as.numeric(unlist(purrr::map(props, ~ .x$pageAnchor$pageRefs[[2]][[1]][[1]][[1]]$y)))
  bottoms <- as.numeric(unlist(purrr::map(props, ~ .x$pageAnchor$pageRefs[[2]][[1]][[1]][[4]]$y)))

  data.frame(
    id = ids,
    mentionText = mentionTexts,
    type = types,
    confidence = confs,
    start_ind = start_inds,
    end_ind = end_inds,
    left = lefts,
    right = rights,
    top = tops,
    bottom = bottoms
  )
}

#' Build dataframe of entities from file
#'
#' @description Helper function to build dataframe of entities
#' @param lst a list from a parsed JSON file from Document AI
#'
#' @noRd

build_async_entity_df <- function(x) {

  anchors <- x$pageAnchor$pageRefs

  lefts <- as.numeric(unlist(purrr::map(anchors, ~ .x$boundingPoly$normalizedVertices[[2]]$x[1])))
  rights <- as.numeric(unlist(purrr::map(anchors, ~ .x$boundingPoly$normalizedVertices[[2]]$x[2])))
  tops <- as.numeric(unlist(purrr::map(anchors, ~ .x$boundingPoly$normalizedVertices[[2]]$y[1])))
  bottoms <- as.numeric(unlist(purrr::map(anchors, ~ .x$boundingPoly$normalizedVertices[[2]]$y[3])))

  data.frame(
    id = as.numeric(x$id),
    mentionText = x$mentionText,
    type = x$type,
    confidence = as.numeric(x$confidence),
    start_ind = as.numeric(unlist(purrr::map(x$textAnchor$textSegments, ~ .x$startIndex))),
    end_ind = as.numeric(unlist(purrr::map(x$textAnchor$textSegments, ~ .x$endIndex))),
    left = lefts,
    right = rights,
    top = tops,
    bottom = bottoms
  )
}
