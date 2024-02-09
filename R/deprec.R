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