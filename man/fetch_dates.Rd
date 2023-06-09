% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch.R
\name{fetch_dates}
\alias{fetch_dates}
\title{Fetch and store one or more dates}
\usage{
fetch_dates(
  dates = Sys.Date() + c(-10, -9, -8),
  param = "sst.day.mean",
  left = -180,
  path = oisst_path("world")
)
}
\arguments{
\item{dates}{Date, the dates}

\item{param}{char, the parameter to fetch}

\item{left}{num, see \code{\link{get_one}}}

\item{path}{char, the output path}

\item{tibble}{database (possibly with no wrows)}
}
\description{
Fetch and store one or more dates
}
