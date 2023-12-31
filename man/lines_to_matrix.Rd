% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrangle-helpers.R
\name{lines_to_matrix}
\alias{lines_to_matrix}
\title{Convert vectors to a matrix}
\usage{
lines_to_matrix(lines, split = "")
}
\arguments{
\item{lines}{a vector}

\item{split}{character. The string to split the input on. Default is \code{""}, i.e. one character per column}
}
\value{
A matrix where the number of rows is the length of \code{lines} and, for a split on \verb{"}, the number of columns is the same as the number of characters in each element of \code{lines}.
}
\description{
For the default split of \verb{"}, assumes that each element of \code{lines} has the same number of characters.
}
\examples{
lines_to_matrix(c("#.#.", "..#.", "##.."))
}
