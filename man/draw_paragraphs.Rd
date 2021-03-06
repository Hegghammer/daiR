% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inspect_output.R
\name{draw_paragraphs}
\alias{draw_paragraphs}
\title{Inspect paragraph boxes in Google Document AI json output}
\usage{
draw_paragraphs(pdf, json, dir = tempdir())
}
\arguments{
\item{pdf}{filepath of the pdf sent for processing.}

\item{json}{filepath of the json output file.}

\item{dir}{path to output directory}
}
\description{
This function extracts the paragraph bounding box coordinates
from the json file returned by Google Document AI
and plots them on a copy of the source document
for human inspection.
}
\details{
Not vectorized, but documents can be multi-page.
Takes only pdf documents (if you processed .gif or .tiff files,
convert them to pdf first). Generates an annotated .png file
for each page in the original pdf. Assumes equal page length
between the pdf and the json, so if the json output was split
into shards, split the pdf accordingly first.
}
\examples{
\dontrun{
draw_paragraphs("pdf_page1.pdf", "pdf_output.json")
}
}
