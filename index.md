# daiR: OCR with Google Document AI in R

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/daiR)](https://CRAN.R-project.org/package=daiR)
<!-- badges: end -->

**daiR** is an R package for [Google Document AI](https://cloud.google.com/document-ai), a powerful server-based OCR processor. The package provides a wrapper for the Document AI API and comes with additional tools for output file parsing and text reconstruction.

<img src="man/figures/frontpage_image.png" width="500" align="center">

## Requirements

Google Document AI is a [paid service](https://cloud.google.com/document-ai/pricing) that requires a [Google Cloud](https://console.cloud.google.com/) account and a [Google Storage](https://cloud.google.com/storage) bucket. I recommend using Mark Edmondson's `googleCloudStorageR` [package](https://github.com/cloudyr/googleCloudStorageR) in combination with `daiR`. See [vignettes](http://dair.info/) for more on authentication and setup.

## Installation

`daiR` is not yet on CRAN, but you can install the latest development version from Github:

```R
devtools::install_github("hegghammer/daiR")
```

