% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database.R
\name{parse_oisst_filename}
\alias{parse_oisst_filename}
\title{Parse OISST filenames}
\usage{
parse_oisst_filename(
  x = c("icec.day.mean.1981.nc", "icec.day.mean.ltm.1991-2020.nc", "icec.mon.mean.nc",
    "icec.week.mean.nc", "icec.day.mean.ltm.nc", "sst.day.mean.1981.nc",
    "sst.day.mean.ltm.1991-2020.nc", "sst.mon.mean.nc", "sst.week.mean.nc",
    "sst.day.mean.ltm.nc")
)
}
\arguments{
\item{x}{char, one or more filenames}
}
\value{
a tibble of filename constituent parts `date`, `param`, `per1, 'trt` and `ltm`
}
\description{
Not to be confused with \code{decompose_filename}
}
