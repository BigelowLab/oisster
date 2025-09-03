suppressPackageStartupMessages({
  library(stars)
  library(oisster)
  library(argparser)
  library(dplyr)
})


path = oisst_path("world")
aff = list_files(path, full.names =TRUE)
bff = basename(aff)
len = nchar(bff)

x = tibble(bff, len) |>
  group_by(len) |>
  slice(1)

# A tibble: 4 × 2
# Groups:   len [4]
# bff                              len
# <chr>                          <int>
# 1 sst.ann.max.1982-01-01.tif        26
# 2 sst.day.mean.1981-09-01.tif       27  <- missing ltm segment
# 3 sst.ann.range.1982-01-01.tif      28
# 4 sst.day.mean.NA.2025-08-04.tif    30


fixme = aff[len == 27]
newme = sub("sst.day.mean.", "sst.day.mean.NA.", fixme, fixed = TRUE)
stop("do not go past this - it is over")
ok = file.rename(fixme, newme)

db = build_database(path, save_db = TRUE)

ff = list_files(path)
x = tail(ff)
ext = ".tif"


