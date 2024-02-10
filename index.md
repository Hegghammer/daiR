# daiR: OCR with Google Document AI in R

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/daiR)](https://CRAN.R-project.org/package=daiR)
[![R-CMD-check](https://github.com/Hegghammer/daiR/actions/workflows/package-check.yml/badge.svg)](https://github.com/Hegghammer/daiR/actions/workflows/package-check.yml)
[![Codecov test coverage](https://codecov.io/gh/Hegghammer/daiR/branch/master/graph/badge.svg)](https://codecov.io/gh/Hegghammer/daiR?branch=master)
<!-- badges: end -->

**daiR** is an R package for [Google Document AI](https://cloud.google.com/document-ai), a powerful server-based OCR processor with support for over 60 languages. The package provides an interface for the Document AI API and comes with additional tools for output file parsing and text reconstruction.

<img src="man/figures/frontpage_image.png" width="400" class="center">

## Use

Quick OCR short documents:

```R
## NOT RUN
library(daiR)
get_text(dai_sync("file.pdf"))
```

Turn images of tables into R dataframes:

```R
## NOT RUN:
# Assumes a default processor of type "FORM_PARSER_PROCESSOR"
get_tables(dai_sync("file.pdf"))
```

Draw bounding boxes on the source image:

```R
## NOT RUN:
draw_blocks(dai_sync("file.pdf"))
```

## Requirements

Google Document AI is a [paid service](https://cloud.google.com/document-ai/pricing) that requires a [Google Cloud](https://console.cloud.google.com/) account and a [Google Storage](https://cloud.google.com/storage) bucket. I recommend using Mark Edmondson's `googleCloudStorageR` [package](https://github.com/cloudyr/googleCloudStorageR) in combination with `daiR`. See [vignettes](http://dair.info/) for more on authentication and setup.

## Installation

Install `daiR` from CRAN:

```R
install.packages("daiR")
```

Or install the latest development version from Github:

```R
devtools::install_github("hegghammer/daiR")
```

## Citation

To cite `daiR` in publications, please use

>Hegghammer, T., (2021). daiR: an R package for OCR with Google Document AI. *Journal of Open Source Software*, 6(68), 3538, https://doi.org/10.21105/joss.03538

Bibtex:
```
@article{Hegghammer2021,
  doi = {10.21105/joss.03538},
  url = {https://doi.org/10.21105/joss.03538},
  year = {2021},
  publisher = {The Open Journal},
  volume = {6},
  number = {68},
  pages = {3538},
  author = {Thomas Hegghammer},
  title = {daiR: an R package for OCR with Google Document AI},
  journal = {Journal of Open Source Software}
}
```