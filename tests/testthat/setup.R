# based on
# - https://gargle.r-lib.org/articles/articles/managing-tokens-securely.html
# - https://github.com/tidyverse/googledrive/blob/master/tests/testthat/helper.R

has_internet <- !is.null(curl::nslookup(host = "r-project.org", error = FALSE))

if (has_internet && gargle:::secret_can_decrypt("daiR")) {
  json <- gargle:::secret_read("daiR", "dair_testing.json")
  dai_auth(path = rawToChar(json))
  googleCloudStorageR::gcs_auth(json_file = rawToChar(json))
  #drive_empty_trash()
}

skip_if_no_token <- function() {
  testthat::skip_if_not(dai_has_token(), "No DAI token")
}
