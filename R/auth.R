#' Check authentication
#'
#' @description Checks whether the user can obtain an access token for
#' Google Cloud Services (GCS) using a service account key stored on file.
#' @param path path to a JSON file with a service account key
#' @param scopes GCS auth scopes for the token
#' @return no return value, called for side effects
#' @details daiR takes a very parsimonious approach to authentication,
#' with the native auth functions only supporting service account files.
#' Those who prefer other authentication methods can pass those directly
#' to the \code{token} parameter in the various functions that call the
#' Document AI API.
#' @export
#'
#' @examples
#' \dontrun{
#' dai_auth()
#'}

dai_auth <- function(
  path = Sys.getenv("GCS_AUTH_FILE"),
  scopes = "https://www.googleapis.com/auth/cloud-platform"
  ) {

  if (!is_json(path)) {
    token <- NULL
  } else {
    token <- gargle::credentials_service_account(scopes = scopes, path = path)
  }

  if (inherits(token, "Token2.0")) {
    packageStartupMessage("Access token available.")
  } else {
    packageStartupMessage("Access token not available. Have you provided a valid service account key file?")
  }
}

#' Produce access token
#'
#' @description Produces an access token for Google Cloud Services (GCS)
#' @param scopes GCS auth scopes for the token
#' @param path path to a JSON file with a service account key
#' @return a GCS access token object (if credentials are valid) or a message (if not).
#' @export
#'
#' @examples
#' \dontrun{
#' token <- dai_token()
#'}

dai_token <- function(
  path = Sys.getenv("GCS_AUTH_FILE"),
  scopes = "https://www.googleapis.com/auth/cloud-platform"
  ) {

  if (!is_json(path)) {
    token <- NULL
  } else {
    token <- gargle::credentials_service_account(scopes = scopes, path = path)
  }

  if (!inherits(token, "Token2.0")) {
    cli::cli_alert_danger("Invalid GCS credentials. No token produced.")
    return(invisible(NULL))
  } else {
    return(token)
  }
}

#' Get user information
#'
#' @description Fetches the Google Cloud Services (GCS) user information
#' associated with a service account key.
#' @return a list of user information elements
#' @export
#'
#' @examples
#' \dontrun{
#' dai_user()
#'}

dai_user <- function() {

  response <- httr::GET("https://www.googleapis.com/oauth2/v1/userinfo",
                    httr::config(token = dai_token()))
  httr::content(response)

  }

#' Get project id
#'
#' @description Fetches the Google Cloud Services (GCS) project id
#' associated with a service account key.
#' @param path path to the JSON file with your service account key
#' @return a string with a GCS project id
#' @export
#'
#' @examples
#' \dontrun{
#' project_id <- get_project_id()
#'}

get_project_id <- function(
  path = Sys.getenv("GCS_AUTH_FILE")
  ) {

  if (!(length(path) == 1 && is.character(path)) || path == "") {
    stop("Error: invalid path parameter.")
    }

  jsonlite::fromJSON(path)$project_id

  }
