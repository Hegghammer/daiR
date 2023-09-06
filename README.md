# daiR: OCR with Google Document AI in R

<img align="right" src="man/figures/logo.png" width="120">

**daiR** is an R package for [Google Document AI](https://cloud.google.com/document-ai), a powerful server-based OCR processor with support for over 60 languages. The package provides an interface for the Document AI API and comes with additional tools for output file parsing and text reconstruction. See the `daiR` [website](https://dair.info/) and this [journal article](https://joss.theoj.org/papers/10.21105/joss.03538#) for more details.

## Use

Quick OCR short documents:

```R
## NOT RUN
library(daiR)
response <- dai_sync("file.pdf")
text <- text_from_dai_response(response)
cat(text)
```

Batch process asynchronously via Google Storage:

```R
## NOT RUN
library(googleCloudStorageR)
library(purrr)
my_files <- c("file1.pdf", "file2.pdf", "file3.pdf")
map(my_files, gcs_upload)
dai_async(my_files)
contents <- gcs_list_objects()
output_files <- grep("json$", contents$name, value = TRUE)
map(output_files, ~ gcs_get_object(.x, saveToDisk = file.path(tempdir(), .x)))
sample_text <- text_from_dai_file(file.path(tempdir(), output_files[1]))
cat(sample_text)
```

Turn images of tables into R dataframes:

```R
## NOT RUN:
response <- dai_sync_tab("tables.pdf")
dfs <- tables_from_dai_response(response)
```

## Requirements

Google Document AI is a [paid service](https://cloud.google.com/document-ai/pricing) that requires a [Google Cloud](https://console.cloud.google.com/) account and a [Google Storage](https://cloud.google.com/storage) bucket. I recommend using Mark Edmondson's `googleCloudStorageR` [package](https://github.com/cloudyr/googleCloudStorageR) in combination with `daiR`.

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

## Acknowledgments

Thanks to Mark Edmondson, Hallvar Gisnås, Will Hanley, Neil Ketchley, Trond Arne Sørby, Chris Barrie, and Geraint Palmer for contributions to the project.

## Code of conduct

Please note that the daiR project is released with a [Contributor Code of Conduct](https://www.contributor-covenant.org/version/2/0/code_of_conduct/). By contributing to this project, you agree to abide by its terms.

<!-- badges: start -->
[![DOI](https://joss.theoj.org/papers/10.21105/joss.03538/status.svg)](https://doi.org/10.21105/joss.03538)
[![CRAN status](https://www.r-pkg.org/badges/version/daiR)](https://CRAN.R-project.org/package=daiR)
[![R-CMD-check](https://github.com/Hegghammer/daiR/actions/workflows/package-check.yml/badge.svg)](https://github.com/Hegghammer/daiR/actions/workflows/package-check.yml)
[![Codecov test coverage](https://codecov.io/gh/Hegghammer/daiR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/Hegghammer/daiR?branch=master)
<!-- badges: end -->
