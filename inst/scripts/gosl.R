library(sf)
library(stars)
library(dplyr)
devtools::load_all()

l = list_oisst() |>
  dplyr::filter(name == 'sst.day.mean.1992.nc')
x = ncdf4::nc_open(l$opendap[1])
time = c(1,1)

region = 'gosl'
bb = c(xmin = 293, xmax = 303.5, ymin = 44.4, ymax = 50.5)
path = oisst_path("gosl")
dates = as.Date(c("1992-10-01", "1993-02-01"))

params = c('sst.day.mean', 'icec.day.mean',
           'sst.week.mean', 'sst.mon.mean',
           'sst.mon.ltm.1991-2020')
uris = sapply(params, function(p) query_oisst(param = p)[[1]])
xx = lapply(uris, ncdf4::nc_open)
ff = lapply(xx, generate_filename) |> lapply(head)
dd = lapply(ff, decompose_filename)
ss = lapply(dd, compose_filename)


param = 'sst.day.mean'
db = fetch_oisst(dates, bb = bb, path = path, param = param)
s = read_oisst(db, path)
plot(s[,,,1:4])

param = 'icec.day.mean'
db = fetch_oisst(dates, bb = bb, path = path, param = param)
s = read_oisst(db, path)
plot(s[,,,1:4])

param = 'sst.week.mean'
db = fetch_oisst(dates, bb = bb, path = path, param = param)
s = read_oisst(db, path)
plot(s[,,,1:4])

param = 'sst.mon.mean'
db = fetch_oisst(dates, bb = bb, path = path, param = param)
s = read_oisst(db, path)
plot(s[,,,1:4])

param = 'sst.mon.ltm.1991-2020'
db = fetch_oisst(bb = bb, path = path, param = param)
s = read_oisst(db, path)
plot(s[,,,1:4])
