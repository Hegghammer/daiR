#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' If there's a known replacement, calling the function
#' will tell you about it.
#'
#' @keywords internal
#' @name deprecated
NULL

#' Get text from HTTP response object
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' `text_from_dai_response()` is deprecated; please use `get_text()` instead.
#' 
#' @param response an HTTP response object returned by \code{dai_sync()}
#' @param save_to_file boolean; whether to save the text as a .txt file
#' @param dest_dir folder path for the .txt output file if \code{save_to_file = TRUE}
#' @param filename string to form the stem of the .txt output file
#' @return a string (if \code{save_to_file = FALSE})
#' @export
#' @keywords internal
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

	lifecycle::deprecate_warn("1.0.0", "daiR::text_from_dai_response()", "daiR::get_text()", always = TRUE)

  # checks
  if (!(inherits(response, "response"))) {
    stop("Input is not a valid HTTP response.")
    }

  parsed <- httr::content(response, as = "parsed")

  if (!("pages" %in% names(parsed$document) || "pages" %in% names(parsed))) {
    stop("Input not recognized. Is it from dai_async?")
  }

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

	dest_dir <- normalizePath(dest_dir, winslash = "/")

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
#' @description
#' `r lifecycle::badge("deprecated")`
#' `text_from_dai_file()` is deprecated; please use `get_text()` instead.
#'
#' @param file filepath of a JSON file obtained using \code{dai_async()}
#' @param save_to_file boolean; whether to save the text as a .txt file
#' @param dest_dir folder path for the .txt output file if save_to_file=TRUE
#' @return a string (if \code{save_to_file = FALSE})
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' text <- text_from_dai_file("mydoc-0.json")
#' text_from_dai_file("mydoc-0.json", save_to_file = TRUE)
#' }

text_from_dai_file <- function(file,
                               save_to_file = FALSE,
                               dest_dir = getwd()
                               ) {
	lifecycle::deprecate_warn("1.0.0", "daiR::text_from_dai_file()", "daiR::get_text()", always = TRUE)

  # checks
  if (!(is.character(file) && length(file) == 1)) {
    stop("Invalid file input.")
  }

  if (!(is_json(file))) {
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

	dest_dir <- normalizePath(dest_dir, winslash = "/")

  # get text
  text <- output$text

  if (isTRUE(save_to_file)) {
    stem <- gsub("\\.json", "", basename(file))
    write(text, file.path(dest_dir, paste0(stem, ".txt")))
  } else {
    return(text)
  }
}

#' Get tables from response object
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' `tables_from_dai_response()` is deprecated; please use `get_tables()` instead.
#' 
#' @param object an HTTP response object returned by \code{dai_sync_tab()}
#' @return a list of data frames
#' @export
#'
#' @examples
#' \dontrun{
#' tables <- tables_from_dai_response(response)
#' }

tables_from_dai_response <- function(object) {

  lifecycle::deprecate_warn("1.0.0", "daiR::tables_from_dai_response()", "daiR::get_tables()", always = TRUE)
  
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
    vector <- purrr::map_chr(cells, resp_get_cell_text)
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
#' @description
#' `r lifecycle::badge("deprecated")`
#' `tables_from_dai_file()` is deprecated; please use `get_text()` instead.
#' 
#' @param file filepath of a JSON file obtained using \code{dai_async_tab()}
#' @return a list of data frames
#' @export
#'
#' @examples
#' \dontrun{
#' tables <- tables_from_dai_file("document.json")
#' }

tables_from_dai_file <- function(file) {

  lifecycle::deprecate_warn("1.0.0", "daiR::tables_from_dai_file()", "daiR::get_tables()", always = TRUE)

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
    vector <- purrr::map_chr(cells, file_get_cell_text)
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