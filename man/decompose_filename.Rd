% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database.R
\name{decompose_filename}
\alias{decompose_filename}
\title{Given one or more filename, decompose into a database}
\usage{
decompose_filename(
  x = c("/foo/bar/sst.day.mean.1981-09-01.tif", "/foo/bar/icec.mon.max.1981-09-01.tif")
)
}
\arguments{
\item{x}{char vector of filenames}
}
\value{
tibble database
}
\description{
Given one or more filename, decompose into a database
}
