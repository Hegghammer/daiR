#' Defunct functions
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' If there's a known replacement, calling the function
#' will tell you about it.
#'
#' @keywords internal
#' @name defunct
NULL

#' @usage # Deprecated in 1.0.0 -------------------------------------
#' @name defunct
NULL

#' @export
#' @rdname defunct

dai_sync_tab <- function(...) {
  lifecycle::deprecate_stop(when = "1.0.0", what = "daiR::dai_sync_tab()", details = "Please use dai_sync() with a processor of type FORM_PARSER_PROCESSOR instead.")
  }

#' @export
#' @rdname defunct

dai_async_tab <- function(...) {
  lifecycle::deprecate_stop(when = "1.0.0", what = "daiR::dai_async_tab()", details = "Please use dai_async() with a processor of type FORM_PARSER_PROCESSOR instead.")
  }
