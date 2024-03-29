% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manipulate.R
\name{from_labelme}
\alias{from_labelme}
\title{Extract block coordinates from labelme files}
\usage{
from_labelme(json, page = 1)
}
\arguments{
\item{json}{a json file generated by 'Labelme'}

\item{page}{the number of the annotated page}
}
\value{
a data frame with location coordinates for the rectangle
marked in 'Labelme'.
}
\description{
This is a specialized function for use in connection
with text reordering. It takes the output from the image
annotation tool 'Labelme' \url{https://github.com/labelmeai/labelme}
and turns it into a one-row data frame compatible with other
'daiR' functions for text reordering such as
\code{reassign_tokens2()}. See package vignette on text reconstruction
for details.
}
\examples{
\dontrun{
new_block <- from_labelme("document1_blocks.json")
new_block <- from_labelme("document5_blocks.json", 5)
}
}
