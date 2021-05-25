# Authenticate during testing
#
# based on:
# - https://gargle.r-lib.org/articles/articles/managing-tokens-securely.html
# - https://github.com/tidyverse/googledrive/blob/master/tests/testthat/helper.R

test_auth <- function() {
  has_internet <- !is.null(curl::nslookup(host = "r-project.org", error = FALSE))
  if (has_internet && gargle:::secret_can_decrypt("daiR")) {
    json <- gargle:::secret_read("daiR", "dair_testing.json")
    dai_auth(path = rawToChar(json))
    googleCloudStorageR::gcs_auth(json_file = rawToChar(json))
  }
}

test_auth()

# Tell tester to skip test if no token
#
skip_if_no_token <- function() {
  testthat::skip_if_not(dai_has_token(), "No DAI token")
}
