% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_rmd_header.R
\name{get_rmd_header_indices}
\alias{get_rmd_header_indices}
\title{Get the line indices of the header in an Rmd file}
\usage{
get_rmd_header_indices(filename, file_lines = NULL, header_delimiter = "---")
}
\arguments{
\item{filename}{the name of the Rmd file to read}

\item{file_lines}{a character vector containing the lines of a source Rmd file.}

\item{header_delimiter}{the delimiter for the beginning and ending of the Rmd header section.}
}
\description{
Get the line indices of the header in an Rmd file
}
