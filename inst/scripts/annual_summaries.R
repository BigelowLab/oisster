#' Compute annual summaries mean, min, max, range, sum for a given year.  Range can only be
#' computed if min and max for the given year have been computed.
#' 
#' usage: annual_summaries [--] [--help] [--year YEAR] [--region REGION]
#' 
#' Annual summary compute script
#' 
#' flags:
#'   -h, --help    show this help message and exit
#' 
#' optional arguments:
#'  -y, --year    one or more years as YYYY or a range YYYY:YYYY [default: 2023]
#'  -r, --region  region to operate upon [default: world]
#'  -v, --vars    variables to compute in style of mean+min+max+... [default: mean+min+max+range+sum]


suppressPackageStartupMessages({
  library(dplyr)
  library(stars)
  library(oisster)
  library(argparser)
}) 


#' Convert a year string as YYYY or YYYY:YYYY to a sequence of one or more
#' consecutive years
parse_year <- function(x){
  y = strsplit(x, ":", fixed = TRUE)[[1]] |>
    as.numeric()
  if (length(y) > 1){
    y <- seq(from = y[1], to = y[2], by = 1)
  }
  y
}

#' Parse variable names
parse_vars <- function(x){
  strsplit(x, "+", fixed = TRUE)[[1]] |>
    tolower()
}





Args <- argparser::arg_parser("Annual summary compute script",
                              name = "annual_summaries",
                              hide.opts = TRUE) |>
  argparser::add_argument("--year", 
                          help = "one or more years as YYYY or a range YYYY:YYYY, default is last year",
               default = format(Sys.Date() - 365, "%Y")) |>
  argparser::add_argument("--region", help = "region to operate upon",
               default = "world") |>
  argparser::add_argument("--vars", help = "variables to compute in style of mean+min+max+...",
               default = "mean+sum+min+max+range") |>
  argparser::parse_args()

PATH <- oisster::oisst_path(Args$region)
DB <- oisster::read_database(PATH) |>
  dplyr::mutate(year = format(date, "%Y"))
YEARS <- parse_year(Args$year)
VARS <- parse_vars(Args$vars)
ix <- VARS %in% "range"
if (any(ix)){
  VARS <- VARS[!ix]
  RANGE = TRUE
} else {
  RANGE = FALSE
}

db <- lapply(YEARS,
  function(y){
    db <- dplyr::filter(DB, year == y, param == 'sst', per == 'day')
    S <- read_oisst(db, PATH)
    lapply(VARS,
      function(v){
        s = stars::st_apply(S, c("x", "y"), v, na.rm = TRUE) |>
          rlang::set_names("sst") |>
          dplyr::mutate(sst = dplyr::if_else(is.infinite(.data$sst), NA, .data$sst))
        d <- dplyr::slice(db, 1) |>
          dplyr::mutate(per = "ann", trt = v)
        f <- oisster::compose_filename(d, PATH)
        stars::write_stars(s, f)
        d
      }) |>
      dplyr::bind_rows()
  }) |>
  dplyr::bind_rows()

DB <- dplyr::bind_rows(DB, db) |> 
  dplyr::distinct()


if (RANGE){
  db <- lapply(YEARS,
    function(y){
      db <- dplyr::filter(DB, year == y, param == 'sst', per == 'ann', 
                          trt %in% c("min", "max")) |>
        dplyr::arrange()
      if (nrow(db) < 2) return(DB |> slice(0))
      
      d <- dplyr::slice(db, 1) |>
        dplyr::mutate(trt = "range")
      f <- oisster::compose_filename(d, PATH)
      mini = read_oisst(dplyr::filter(db, trt == "min") |> dplyr::slice(1), PATH)
      maxi = read_oisst(dplyr::filter(db, trt == "max") |> dplyr::slice(1), PATH)
      rng <- (maxi - mini) |>
        stars::write_stars(f)
      d
    }) |>
    dplyr::bind_rows()
  
  DB <- dplyr::bind_rows(DB, db) |> 
    dplyr::distinct()
  
}

DB = dplyr::select(DB, -year) |>
  dplyr::distinct() |>
  dplyr::arrange(date, param, per,  trt) |>
  oisster::write_database(PATH)
  
