#' Get tables from response object
#'
#' @description Extracts all the tables that Document AI (DAI)
#' identified in a synchronous processing request.
#' @param object an HTTP response object returned by \code{dai_sync_tab()}
#' @return a list of data frames
#' @export
#'
#' @examples
#' \dontrun{
#' tables <- tables_from_dai_response(response)
#' }

tables_from_dai_response <- function(object) {

  # checks
  if (!(inherits(object, "response"))) {
    stop("Object is not a valid HTTP response.")
    }

  parsed <- httr::content(object, as = "parsed")

  if (!("pages" %in% names(parsed) || "pages" %in% names(parsed$document))) {
    stop("Object not a positive dai_sync response.")
    }

  if (!("text" %in% names(parsed) || "text" %in% names(parsed$document))) {
    stop("DAI found no text. Was the page blank?")
    }

  # Compile a list of table entries

  if ("pages" %in% names(parsed$document)) {
  	table_list_raw <- purrr::map(parsed$document$pages, ~ .x$tables)
  } else {
  	table_list_raw <- purrr::map(parsed$pages, ~ .x$tables)
  }

  if (all(sapply(table_list_raw, is.null))) {
    stop("DAI found no tables in the document.")
    }

  table_list <- purrr::flatten(table_list_raw)

  # Function to get the text of an individual cell
  resp_get_cell_text <- function(cell) {
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
    return(txt)

  }

  # Function to compile cell entries into a row vector
  resp_get_row_vector <- function(elem) {
    cells <- elem$cells
    vector <- unlist(purrr::map(cells, resp_get_cell_text))
    return(vector)

    }

  # Function to build a table from row vectors
  resp_build_table <- function(table) {
    headers_list <- table$headerRows
    rows_list <- table$bodyRows
    headervectors <- purrr::map(headers_list, resp_get_row_vector)
    rowvectors <- purrr::map(rows_list, resp_get_row_vector)
    table <- data.frame(matrix(nrow = 0, ncol = 6))
    for (i in rowvectors) {
      table <- rbind(table, as.data.frame(t(i)))
    }
    table <- stats::setNames(table, headervectors[[1]])
    return(table)

    }

  # Get reference text for indices
  if ("text" %in% names(parsed$document)) {
  	text <- parsed$document$text
  } else {
  	text <- parsed$text
  }

  # Build all tables
  all_tables <- purrr::map(table_list, resp_build_table)

  return(all_tables)

  }

#' Get tables from output file
#'
#' @description Extracts all the tables that Document AI (DAI)
#' identified in an asynchronous processing request.
#' @param file filepath of a JSON file obtained using \code{dai_async_tab()}
#' @return a list of data frames
#' @export
#'
#' @examples
#' \dontrun{
#' tables <- tables_from_dai_file("document.json")
#' }

tables_from_dai_file <- function(file) {

  # checks
  if (!(is.character(file) && length(file) == 1)) {
    stop("Invalid file input.")
    }

  if (!(is_json(file))) {
    stop("Input file not .json. Is the file in your working directory?")
    }

  parsed <- jsonlite::fromJSON(file)

  if (!("pages" %in% names(parsed))) {
    stop("JSON not in right format. Is it from DAI?")
    }

  if (!("text" %in% names(parsed))) {
    stop("DAI found no text. Was the document blank?")
    }

  if (!("tables" %in% names(parsed$pages))) {
    stop("DAI found no tables in the document.")
    }

  message("Reading file...")

  # Get the table-related elements
  table_list_raw <- parsed$pages$tables

  # Function to extract and reorganize the table-related sub-elements
  file_get_table_objects <- function(page) {
    if (is.null(page)) {
      return(NULL)
    } else {
      pagewise_list_of_header_objs <- page$headerRows
      pagewise_list_of_row_objs <- page$bodyRows
      table_objects <- list()
      for (i in seq_along(pagewise_list_of_header_objs)){
        table_object <- list(list(headerRows = pagewise_list_of_header_objs[[i]],
                                  bodyRows = pagewise_list_of_row_objs[[i]]))
        table_objects <- append(table_objects, table_object)
      }
      return(table_objects)
    }
  }

  # Compile a list of table entries
  table_list_by_page <- purrr::map(table_list_raw, file_get_table_objects)

  table_list <- purrr::flatten(table_list_by_page)

  # Function to get the text of an individual cell
  file_get_cell_text <- function(cell) {
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
      return(txt)
    }
  }

  # Function to compile cell entries into a row vector
  file_get_row_vector <- function(elem) {
    cells <- elem$layout$textAnchor$textSegments
    vector <- unlist(purrr::map(cells, file_get_cell_text))
    return(vector)
  }

  # Function to build a table from row vectors
  file_build_table <- function(table_object) {
    headers_list <- table_object$headerRows$cells
    rows_list <- table_object$bodyRows$cells
    headervectors <- purrr::map(headers_list, file_get_row_vector)
    rowvectors <- purrr::map(rows_list, file_get_row_vector)
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
    return(table)
  }

  message("Building tables...")

  # Get reference text for indices
  text <- text_from_dai_file(file)

  # Build all tables
  all_tables <- purrr::map(table_list, file_build_table)

  return(all_tables)

}
