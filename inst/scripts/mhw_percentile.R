# usage: mhw_percentiles.R [--] [--help] [--prob PROB] [--n N] [--email EMAIL]
# 
# Computes N-year daily of p percentiles for each day of year
# 
# flags:
#   -h, --help   show this help message and exit
# 
# optional arguments:
#   -p, --prob   probability 0-1 [default: 0.9]
#   -n, --n      number of recent years to compute percentiles [default: 30]
#   -e, --email  email to send at end, 'none' to skip [default: none]

suppressPackageStartupMessages({
  library(oisster)
  library(stars)
  library(dplyr)
  library(argparser)
  library(charlier)
})

main = function(prob = 0.9, N = 30){
  r = oisster::read_database(oipath) |>
    dplyr::filter(per == "day") |>
    dplyr::arrange(date) |>
    dplyr::mutate(doy = format(date, "%j")) |>
    dplyr::group_by(doy) |>
    dplyr::slice_tail(n = N[1]) |>
    dplyr::group_map(
      function(grp, key){
        cat(key$doy[1], "\n")
        ofile = file.path(opath, sprintf("%s.tif", key$doy[1]))
        r = oisster::read_oisst(grp, oipath) |>
          stars::st_apply(1:2, quantile, probs = prob[1], na.rm = TRUE) |>
          stars::write_stars(ofile)
        0
      })
  return(0)
}

Args = arg_parser("Computes N-year daily of p percentiles for each day of year",
                  name = "mhw_percentiles.R",
                  hide.opts = TRUE) |>
  argparser::add_argument("--prob",
                          help = "probability 0-1",
                          default = 0.9) |>
  argparser::add_argument("--n",
                          help = "number of recent years to compute percentiles",
                          default = 30) |>
  argparser::add_argument("--email",
                          help = "email to send at end, 'none' to skip",
                          default = "none") |>
  argparser::parse_args()


oipath = oisster::oisst_path("world")
prob = Args$prob
N = Args$n
opath = oisster::oisst_path("mhw", sprintf("q%0.2i", prob * 100))
if (!dir.exists(opath)) ok = dir.create(opath, recursive = TRUE)

if (!interactive()){
  ok = main(prob = prob, N = N)
  if (grepl("@", Args$email, fixed = TRUE)){
    charlier::sendmail(to = Args$email,
                       subject = "mhw_percentile is done",
                       message = "You are welcome!")
  }
  quit(save = "no", status = ok)
}
