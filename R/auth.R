#' Get service account token
#'
#' @param scopes GCS auth scopes for the token.
#' @param path path to a json file with a service account key.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dai_auth()
#'}

dai_auth <- function(scopes = "https://www.googleapis.com/auth/cloud-platform",
                     path = Sys.getenv("GCS_AUTH_FILE")
) {

  if (!(length(scopes) >= 1 && is.character(scopes))) {
    stop("Error: invalid scopes parameter.")
  }

  if (!(all(grepl("^https://www.googleapis.com/auth/", scopes)))) {
    stop("Error: invalid scope URLs.")
  }

  if (!(length(path) == 1 && is.character(path)) | path == ""){
    stop("Error: invalid path parameter.")
  }

  cred <- gargle::credentials_service_account(scopes = scopes, path = path)

  .auth$set_cred(cred)

  .auth$set_auth_active(TRUE)

  if (dai_has_token()) {
  message("Token obtained and stored in .auth.")
  } else {
  message("Token not obtained. Have you provided a valid service account key file?")
  }

}

#' Produce token
#'
#' @return GCS access token
#' @export
#'
#' @examples
#' \dontrun{
#' dai_token()
#'}

dai_token <- function() {

  if (isFALSE(.auth$auth_active)) {
    return(NULL)
  }

  if (!dai_has_token()) {
    dai_auth()
  }

  return(.auth$cred)
}

#' Check that token is available
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dai_has_token()
#'}

dai_has_token <- function() {

  inherits(.auth$cred, "Token2.0")
}


#' Get user info associated with service account key
#'
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

  return(response)
}

#' Get project id from service account file
#'
#' @param path path to the json file with your service account key.
#'
#' @return a string with a project id
#' @export
#'
#' @examples
#' \dontrun{
#'project_id <- get_project_id()
#'}

get_project_id <- function(path = Sys.getenv("GCS_AUTH_FILE")) {

  if (!(length(path) == 1 && is.character(path)) | path == ""){
    stop("Error: invalid path parameter.")
  }

  json <- jsonlite::fromJSON(path)

  project_id <- json[[2]]

  return(project_id)
}

#' Delete token from environment
#'
#' @export
#' @examples
#' \dontrun{
#' dai_deauth()
#' }

dai_deauth <- function() {

  .auth$set_auth_active(FALSE)

  .auth$clear_cred()

  invisible()
}
