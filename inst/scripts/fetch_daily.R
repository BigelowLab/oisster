#' Runs daily, compares the dates in the the database to the 
#' date sequence of the last X days leading up to today.
#' Queries for the missing dates; fetch those found and add to the database.
#' 
#' usage: oisst_fetch_daily.R [--] [--help] [--first_date
#'                                           FIRST_DATE] [--region REGION]
#' 
#' oisst_fetch_daily
#' 
#' flags:
#'   -h, --help        show this help message and exit
#' 
#' optional arguments:
#'   -f, --first_date  check for new data starting on this date [default: 2022-03-22]
#'   -r, --region      the region to fetch [default: world]


suppressPackageStartupMessages({
  library(stars)
  library(oisster)
  library(argparser)
  library(dplyr)
})

ARGS <- argparser::arg_parser("Daily update of the local OISST data set",
                              name = "oisst_fetch_daily.R",
                              hide.opts = TRUE) |>
  argparser::add_argument("--first_date", 
                          default = format(Sys.Date() - 365, "%Y-%m-%d"),
                          help = "check for new data staring on this date") |>
  argparser::add_argument("--region", 
                          default = "world",
                          help = "the region to fetch") |>
  argparser::add_argument("--param",
                          default = "sst.day.mean",
                          help = "Search for this parameter, but for future. Ignored for now.") |>
  argparser::parse_args()


parts <- strsplit(ARGS$param, ".", fixed = TRUE)[[1]]
PATH = oisster::oisst_path(ARGS$region)
DB = oisster::read_database(PATH)
db = dplyr::filter(DB, per == parts[2], param == parts[1], trt == parts[3])
dates = seq(from = as.Date(ARGS$first_date), to = Sys.Date(), by = 'day')

missing_dates <- dates[!(dates %in% db$date)]

DB = dplyr::bind_rows(DB,
                      oisster::fetch_oisst(missing_dates,
                              param = ARGS$param,
                              path = PATH)) |>
  dplyr::distinct() |>
  oisster::write_database(PATH)


