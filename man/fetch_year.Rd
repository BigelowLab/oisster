% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch.R
\name{fetch_year}
\alias{fetch_year}
\title{Fetch and store an entire year}
\usage{
fetch_year(
  year = 1981,
  param = "sst.day.mean",
  left = -180,
  path = oisst_path("world")
)
}
\arguments{
\item{year}{num, the year to fetch}

\item{param}{char, the parameter to fetch}

\item{left}{num, see \code{\link{get_one}}}

\item{path}{char, the output path}

\item{logical, }{TRUE if successful}
}
\description{
Fetch and store an entire year
}
