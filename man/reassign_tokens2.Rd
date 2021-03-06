% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_output.R
\name{reassign_tokens2}
\alias{reassign_tokens2}
\title{Assign tokens to a single new block}
\usage{
reassign_tokens2(token_df, block, page = 1)
}
\arguments{
\item{token_df}{a dataframe generated by \code{dair::build_token_df}}

\item{block}{a one-row dataframe of the same format as token_df}

\item{page}{the number of the page on which the block belongs}
}
\value{
a dataframe of tokens
}
\description{
Assign tokens to a single new block
}
\details{
This function is designed to facilitate manual splitting
of blocks and typically takes a one-row block dataframe
generated by daiR::labelme(). See vignette "Correcting text output
from Google Document AI" for an example.
}
\examples{
\dontrun{
new_token_df <- reassign_tokens2(token_df, new_block_df)
new_token_df <- reassign_tokens2(token_df, new_block_df, 5)
}
}
