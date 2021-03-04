# daiR: OCR with Google Document AI in R

**daiR** is an R package for [Google Document AI](https://cloud.google.com/document-ai), a powerful server-based OCR processor. The package provides a wrapper for the Document AI API and comes with additional tools for output file parsing and text reconstruction. See the `daiR` [website](http://dair.info/) for more details.

## Requirements

Google Document AI is a [paid service](https://cloud.google.com/document-ai/pricing) that requires a [Google Cloud](https://console.cloud.google.com/) account and a [Google Storage](https://cloud.google.com/storage) bucket. I recommend using Mark Edmondson's `googleCloudStorageR` [package](https://github.com/cloudyr/googleCloudStorageR) in combination with `daiR`. 

## Installation

Install the latest development version from Github:

```R
devtools::install_github("hegghammer/daiR")
```
<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/daiR)](https://CRAN.R-project.org/package=daiR)
[![Travis build status](https://travis-ci.com/hegghammer/dair.svg?branch=master)](https://travis-ci.com/hegghammer/dair)
<!-- badges: end -->
