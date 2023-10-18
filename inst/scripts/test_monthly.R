suppressPackageStartupMessages({
  library(stars)
  library(sf)
  library(dplyr)
})

date = as.Date("1981-09-01")
bb = c(xmin = -76, ymin = 35, xmax = -58, ymax = 46)
bb = to_360_bb(bb)
path = "."

orig_s2 = sf::sf_use_s2(FALSE)
on.exit({
  sf::sf_use_s2(orig_s2)
})

if (inherits(bb, "bbox")) {
  sf::st_crs(bb) = oisster_crs()
  BB = sf::st_as_sfc(bb)
} else {
  BB = sf::st_bbox(bb, crs = oisster_crs()) |>
    sf::st_as_sfc()
}
uri <- query_oisst(param = "sst.mon.mean")
x <- ncdf4::nc_open(uri)

idate = 1

filename = format(date[idate], "sst.mon.mean_%Y-%m-%d.tif")

r = get_one(x, time = date[idate])
s = sf::st_crop(r, BB)

st_bbox(r)
st_bbox(BB)


att = names(x$var)[1]
time = date[idate]
left = c(0, -180)[1]


