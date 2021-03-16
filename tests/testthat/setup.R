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

# Get a (nearly) random pdf
#
# Takes a random, small-sized pdf from among the 76,000 Tobacco Truth Documents
# on archive.org, downloads it to tempdir, and returns the filepath.

get_random_pdf <- function() {

  results <- internetarchive::ia_keyword_search("industrydocuments", num_results = 1000)
  random <- sample(results, 1)
  item <- internetarchive::ia_get_items(random)
  page_ct <- item[[1]][["metadata"]][["pages"]]
  pdf <- grep(".pdf$", names(item[[1]][["files"]]), value = TRUE)
  size <- item[[1]][["files"]][[pdf]][["size"]]

  while (size > 500000 && page_ct > 6) {
    results <- internetarchive::ia_keyword_search("industrydocuments", num_results = 1000)
    random <- sample(results, 1)
    item <- internetarchive::ia_get_items(random)
    page_ct <- item[[1]][["metadata"]][["pages"]]
    pdf <- grep(".pdf$", names(item[[1]][["files"]]), value = TRUE)
    size <- item[[1]][["files"]][[pdf]][["size"]]
  }

  server <- item[[1]][["server"]]
  dir <-  item[[1]][["dir"]]
  link <- glue::glue("http://{server}{dir}{pdf}")
  dest <- file.path(tempdir(), basename(pdf))
  download.file(link, dest, mode = "wb")

  return(dest)
}
