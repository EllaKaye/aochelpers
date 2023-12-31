% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup.R
\name{aoc_delete_day}
\alias{aoc_delete_day}
\alias{aoc_delete_year}
\title{Delete the directory for a day or year}
\usage{
aoc_delete_day(day, year = NULL)

aoc_delete_year(year = NULL)
}
\arguments{
\item{day}{An integer between 1 and 25}

\item{year}{An integer representing the year, from 2015 to the current year. Defaults to the current year.}
}
\value{
The path of the day that was deleted (invisibly)
}
\description{
Assumes that the directory for the year has the relative path \code{"./YYYY"} and
the directory for the day has the relative path \code{"./YYYY/day/DD"}, where
\code{YYYY} and \code{DD} are the values of \code{year} and \code{day}. This will be the case if
the post was created using \code{\link[=aoc_new_day]{aoc_new_day()}}. If the directory for the day or
year has a corresponding directory in \code{"./_freeze"}, that will also be
deleted. Additionally, for \code{aoc_delete_year()}, the listing page \code{./YYYY.qmd}
(where \code{YYYY} is the value of year) will be deleted.
}
\examples{
\dontrun{aoc_delete_day(1, 2022)}
\dontrun{aoc_delete_year(2022)}
}
