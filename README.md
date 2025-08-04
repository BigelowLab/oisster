oisster
================

R tools to download and manage
[OISST](https://psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html)
data.

<figure>
<img
src="https://psl.noaa.gov/data/gridded/images/small/noaahighres.png"
alt="image" />
<figcaption aria-hidden="true">image</figcaption>
</figure>

### Requirements

[R v4.2+](https://www.r-project.org/)

From CRAN

    + [rlang](https://CRAN.R-project.org/package=rlang)
    + [stars](https://CRAN.R-project.org/package=stars)
    + [ncdf4](https://CRAN.R-project.org/package=ncdf4)
    + [thredds](https://CRAN.R-project.org/package=thredds)
    + [dplyr](https://CRAN.R-project.org/package=dplyr)
    + [readr](https://CRAN.R-project.org/package=readr)

From github

    + [charlier](https://github.com/BigelowLab/charlier)

### Installation

    remotes::install_gitgub("BigelowLab/charlier")
    remotes::install_gitgub("BigelowLab/oisster")

### Data path

This package can be used to download and manage many GEOTIFF files in a
location **of your choosing.** You need to create and set the path once
before using the package (and if you ever change the data storage
location.) Let’s suppose that your data path is
`/mnt/ecocast/coredata/oisst`. Use the `set_root_path()` function first.

    oisster::set_root_path(`/mnt/s1/projects/ecocast/coredata/oisst`)

The function will save this information in a hidden file securely in
your home folder. Using the above allows you to return to working with
the data in later R sessions without the fuss of setting things up each
time.

### Get some data for the first time.

The package provides a single function for getting data,
`fetch_oisst()`. What gets downloaded depends upon the product that you
select. See `?fetch_oisst` for details. You can save data for a
particular region or the entire world, but let’s just do the Gulf of St
Lawrence in Eastern Canada for now. Note that the longitude values for
OISST range from 0 to 360, so we define our bounding box this way, too,
rather than in the range of -180 to 180.

First we load what we need for software, and then define the region
nickname, ‘gosl’, and bounding box. Then we make the data path.

``` r
suppressPackageStartupMessages({
  library(oisster)
  library(stars)
  library(dplyr)
})
region = 'gosl'
bb = c(xmin = 293, xmax = 303.5, ymin = 44.4, ymax = 50.5)
path = oisst_path(region)
ok = dir.create(path, recursive = TRUE, showWarnings = FALSE)
```

#### Daily mean data

Now download a short sequence of daily mean values. Note that OISST
actually starts in September of 1981 and runs through “yesterday”, so
there’s more data you can download. This can take a while to run
depending upon yoyr connection, server activity and the weather
(kidding).

We are passing in a a pair of dates for the start and stop, but the
reality is that the function downloads all data between the two dates.

``` r
db_day = oisster::fetch_oisst(dates = as.Date(c("1982-01-01", "1983-12-31")),
                          param = "sst.day.mean",
                          path = path,
                          bb = bb) |>
  dplyr::glimpse()
```

The database return is a simple table we can use to reconstruct
filenames. Note that there is no directory path saved in the database;
this omission is intentional. Also not that the `ltm` column is all NA.
We’ll use that column for recording the period over which the long term
mean (ltm) is computed.

#### Monthly mean data

Let’s also collect some monthly mean data for the same time period.

``` r
db_month = oisster::fetch_oisst(dates = as.Date(c("1982-01-01", "1983-12-31")),
                          param = "sst.mon.mean",
                          path = path,
                          bb = bb) |>
  dplyr::glimpse()
```

    ## Rows: 24
    ## Columns: 5
    ## $ date  <date> 1982-01-01, 1982-02-01, 1982-03-01, 1982-04-01, 1982-05-01, 198…
    ## $ param <chr> "sst", "sst", "sst", "sst", "sst", "sst", "sst", "sst", "sst", "…
    ## $ per   <chr> "mon", "mon", "mon", "mon", "mon", "mon", "mon", "mon", "mon", "…
    ## $ trt   <chr> "mean", "mean", "mean", "mean", "mean", "mean", "mean", "mean", …
    ## $ ltm   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …

#### Long term mean data

Or even long-term means for which we don’t specify the date.

``` r
db_ltm = oisster::fetch_oisst(param = "sst.mon.ltm.1991-2020",
                              path = path,
                              bb = bb) |>
  dplyr::glimpse()
```

    ## Rows: 12
    ## Columns: 5
    ## $ date  <date> 1991-01-01, 1991-02-01, 1991-03-01, 1991-04-01, 1991-05-01, 199…
    ## $ param <chr> "sst", "sst", "sst", "sst", "sst", "sst", "sst", "sst", "sst", "…
    ## $ per   <chr> "mon", "mon", "mon", "mon", "mon", "mon", "mon", "mon", "mon", "…
    ## $ trt   <chr> "ltm", "ltm", "ltm", "ltm", "ltm", "ltm", "ltm", "ltm", "ltm", "…
    ## $ ltm   <chr> "1991-2020", "1991-2020", "1991-2020", "1991-2020", "1991-2020",…

Note that the `ltm` column is now populated.

#### Build a database

We have the databases for each downloaded file; we could bind these and
save for later use. But we also have built in tools for creating,
writing and reading the database. `build_database` will automatically
scan the path provided for all the raster files, and decompose the
filenames into a database

``` r
DB = oisster::build_database(path, save_db = TRUE) |>
  dplyr::glimpse()
```

    ## Rows: 770
    ## Columns: 5
    ## $ date  <date> 1981-09-01, 1981-10-01, 1981-11-01, 1981-12-01, 1982-01-01, 198…
    ## $ param <chr> "sst", "sst", "sst", "sst", "sst", "sst", "sst", "sst", "sst", "…
    ## $ per   <chr> "mon", "mon", "mon", "mon", "day", "mon", "day", "day", "day", "…
    ## $ trt   <chr> "mean", "mean", "mean", "mean", "mean", "mean", "mean", "mean", …
    ## $ ltm   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …


The database may show that you have variants on the period (`per`) or
treatment (`trt`).

``` r
dplyr::count(DB, per, trt, ltm)
```
    ## # A tibble: 3 × 4
    ##   per   trt   ltm           n
    ##   <chr> <chr> <chr>     <int>
    ## 1 day   mean  <NA>        730
    ## 2 mon   ltm   1991-2020    12
    ## 3 mon   mean  <NA>         28


### Opening a series

If you filter the database to a subset of interest, say the beginning of
first of each month in 1992, then you can load them into a `stars`
array.

``` r
db <- dplyr::filter(DB,
                    param == 'sst',
                    per == 'day',
                    trt == 'mean',
                    date %in% seq(as.Date("1982-01-01"), length = 12, by = "month"))
db
```

    ## # A tibble: 12 × 5
    ##    date       param per   trt   ltm  
    ##    <date>     <chr> <chr> <chr> <chr>
    ##  1 1982-01-01 sst   day   mean  <NA> 
    ##  2 1982-02-01 sst   day   mean  <NA> 
    ##  3 1982-03-01 sst   day   mean  <NA> 
    ##  4 1982-04-01 sst   day   mean  <NA> 
    ##  5 1982-05-01 sst   day   mean  <NA> 
    ##  6 1982-06-01 sst   day   mean  <NA> 
    ##  7 1982-07-01 sst   day   mean  <NA> 
    ##  8 1982-08-01 sst   day   mean  <NA> 
    ##  9 1982-09-01 sst   day   mean  <NA> 
    ## 10 1982-10-01 sst   day   mean  <NA> 
    ## 11 1982-11-01 sst   day   mean  <NA> 
    ## 12 1982-12-01 sst   day   mean  <NA>

Now read them in and plot

``` r
S <- read_oisst(db, path)
plot(S)
```

![](README_files/figure-gfm/read-1.png)<!-- -->

### Generating annual summaries

You can use [this script](inst/scripts/annual_summaries.R) to generate
annual summaries from the dailies. Here we load 12 years of annual means
(computed from daily means) starting in 1992.

``` r
db <- dplyr::filter(DB,
                    param == 'sst',
                    per == 'ann',
                    trt == 'mean',
                    date %in% seq(as.Date("1992-01-01"), length = 12, by = "year"))
if (nrow(db) > 0){
  S <- read_oisst(db, path)
  plot(S)
} else {
  cat("no annual summaries found")
}
```

    ## no annual summaries found
