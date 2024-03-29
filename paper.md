---
title: 'daiR: an R package for OCR with Google Document AI'
tags:
  - R
  - optical character recognition
  - cloud computing
  - text mining
  - natural language processing
authors:
  - name: Thomas Hegghammer^[corresponding author]
    orcid: 0000-0001-6253-1518
    affiliation: 1
affiliations:
  - name: Senior Research Fellow, Norwegian Defence Research Establishment (FFI)
    index: 1
date: 31 August 2021
bibliography: paper.bib
---

# Statement of need

Optical character recognition (OCR) promises to open up centuries worth of text to computational analysis. But OCR software has long been sensitive to visual noise and weak on non-Western languages. In April 2021, Google launched Document AI (DAI), a server-based processor offering high-accuracy OCR for over sixty languages [@vanguri:2021]. The `daiR` [@hegghammer:2021] package provides an R interface to the Document AI API along with additional tools for output parsing and visualization.

# Summary

Text as data is a growing field in the social sciences and digital humanities, but computational access to text produced before the late 20th century has been limited by the difficulty of extracting text from document scans. Established OCR libraries such as Tesseract [@tesseract:2021] are highly sensitive to noise and often require extensive corpus-specific adaptations to render text accurately.

The past two years have seen the introduction of server-based OCR processors, such as Amazon Textract [@amazon:2021] and Google Document AI (DAI), which offer very high accuracy out of the box [@hegghammer:2021b]. Of the two, DAI performs better in benchmarking tests and offers broader language support.

In R, where many scholars do their text analysis work, there are packages for Tesseract [@ooms:2021] and Amazon Textract [@kretch:2021], but not for Document AI. The primary objective of `daiR` is therefore to provide access, from within R, to all the main functionalities of the Document AI API. The secondary aim is to offer tools to help parse the output returned by
the DAI processor.

DAI is part of Google Cloud Services (GCS), a suite of cloud computing services for storage, analytics, and machine learning. `daiR` joins a family of existing R packages that interface with GCS, such as `googleLanguageR` [@edmondson:2020] and `googleCloudStorageR` [@edmondson:2021], that together allow for the implementation of multiple GCS tools into an R-based text mining workflow.

`daiR` also includes a range of tools to process DAI's output, which comes in complex JSON files. One set of functions extracts text and table data from the JSON files and brings them into R as character vectors or data frames. Another set draws block, paragraph, line, and token boundary boxes on images of the submitted documents, to help with visual inspection. A third group of functions helps rearrange text blocks in the cases where Document AI has misread their order. Document AI has near-perfect character recognition, but
its parsing of complex page layouts is fallible. This problem is likely to diminish over time as Document AI's algorithm trains on ever larger document data sets. In the meantime, `daiR` makes it relatively easy to correct DAI's errors and obtain an accurately rendered text.

`daiR` is the first R tool to offer high-accuracy text extraction from noisy historical documents out of the box. Until now, scholars have often dealt with Tesseract's high error rates by treating error as noise and using bag-of-words techniques such as topic modeling. Low-error OCR opens up for wider use of natural language processing and other methods that require correctly parsed and ordered text. DAI's improved language coverage may also help reduce the prevalence of English-language data in computational text analysis.

# Acknowledgements
I am grateful to Mark Edmondson, Trond Arne Sørby, Neil Ketchley, and Hallvar Gisnås for contributions to this project and to Christopher Barrie for valuable reviewer comments.

# References
