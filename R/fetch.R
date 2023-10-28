#' Fetch the land-sea mask
#' 
#' @export
#' @param bb numeric bounding box for subsetting
#' @return stars object
fetch_mask = function(bb = c(xmin = 0, ymin = 0, xmax = 360, ymax = 90)){
  orig_s2 = suppressMessages(sf::sf_use_s2(FALSE))
  on.exit({
    suppressMessages(sf::sf_use_s2(orig_s2))
  })
  
  if (inherits(bb, "bbox")) {
    sf::st_crs(bb) = oisster_crs()
    BB = sf::st_as_sfc(bb)
  } else {
    BB = sf::st_bbox(bb, crs = oisster_crs()) |>
      sf::st_as_sfc()
  }
  uri <- query_oisst(param = "lsmask.oisst")
  x <- ncdf4::nc_open(uri)
  s = get_one(x)
  ncdf4::nc_close(x)
  stars::st_crop(s, BB) |>
    rlang::set_names("lsmask")
}




#' Retrieve the current month
#' 
#' @export
#' @param x Date the date to compute upon (useful for "offset")
#' @param offset numeric, days to offset. If 0 (the default) then the current month, 
#' if -1 then the prior month, to 2 months before try -32, to get the next month try
#' 32.  It's a hack.
#' @return Date class object
current_month = function(x = Sys.Date(), offset = 0){
  x = x |>
    format("%Y-%m-01") |>
    as.Date()
  
  if (offset != 0){
    x = current_month(x + offset)
  }
  
  x
}

#' Match a range of dates to the the sequence of weekly dates in OISST
#' 
#' @export
#' @param dates a two element Date vector of start and stop
#' @return a sequence of dates
match_weeks = function(dates){
  lut = seq(from = as.Date("1981-09-06"), to = Sys.Date(), by = "7 days")
  ix = findInterval(lut, dates)
  lut[ix == 1]
}


#' Fetch one or more weeks
#' 
#' @export
#' @param dates two element range of dates
#' @param bb numeric 4 element bounding box in the range of 
#'   (xmin = 0, ymin = -90, xmax= 360, ymax = 90)
#' @param param char, the identifier of the data set
#' @param path char, the output path
#' @return tibble database for existing files
fetch_week = function(dates = seq(from = as.Date("1981-09-06"), 
                                  to = Sys.Date()-1,
                                  by = "7 days"),
                      param = "sst.week.mean",
                      bb = c(xmin = 0, ymin = 0, xmax = 360, ymax = 90),
                      path = oisst_path("world")){
  
  dates = match_weeks(dates)
  
  uri <- query_oisst(param = param)
  x <- ncdf4::nc_open(uri)
  
  # here we craft the filenames from the NCDF and the dates
  # and then the db, and 
  ofile <- generate_filename(x)
  db <- decompose_filename(ofile) |>
    dplyr::filter(date %in% dates)
  ofile <- compose_filename(db, path)
  
  ok <- sapply(unique(dirname(ofile)), dir.create, 
               recursive = TRUE, showWarnings = FALSE)
  
  
  ok = sapply(seq_len(nrow(db)),
              function(idate){
                r = try(suppressMessages(get_one(x, att = db$param[idate], bb = bb, time = idate)))
                if (inherits(r, "try-error")){
                  print(r)
                  r = FALSE
                } else {
                  r = stars::write_stars(r, ofile[idate])
                  r = file.exists( ofile[idate])
                }
                r
              })
  
  ncdf4::nc_close(x)
  dplyr::filter(db, ok)
  
}

#' Fetch long time series means
#' 
#' @export
#' @param param char the parameter
#' @param bb numeric 4 element bounding box in the range of xmin = 0, ymin = -90, xmax= 360, ymax = 90
#' @param path char, the output path
#' @return tibble database for existing files
fetch_ltm = function(param = "sst.mon.ltm.1991-2020", 
                     bb = c(xmin = 0, ymin = 0, xmax = 360, ymax = 90),
                     path = oisst_path("world")){
  
  filename = paste0(param[1], ".nc")
  
  uri = query_oisst(param = param)
  
  if (length(uri) == 0){
    stop("param not matched among available resources:", paste(lut$name, sep = ", "))
  }
  
  x <- ncdf4::nc_open(uri)
  dates = get_time(x)
  # here we craft the filenames from the NCDF and the dates
  # and then the db, and 
  ofile <- generate_filename(x)
  db <- decompose_filename(ofile)
  ofile <- compose_filename(db, path)
  ok <- sapply(unique(dirname(ofile)), dir.create, 
               recursive = TRUE, showWarnings = FALSE)
  nav = get_nav(x, bb = bb)
  ok = sapply(seq_along(dates),
              function(idate){
                nav$start[3] = idate
                r = try(suppressMessages(get_one(x, nav = nav)))
                if (inherits(r, "try-error")){
                  print(r)
                  r = FALSE
                } else {
                  r = stars::write_stars(r, ofile[idate])
                  r = file.exists(ofile[idate])
                }
                r
              })
  
  ncdf4::nc_close(x)
  dplyr::filter(db, ok)
}
#' Fetch a series of months
#'
#' @export
#' @param dates Date, one or more dates
#' @param bb numeric 4 element bounding box in the range of 
#'   (xmin = 0, ymin = -90, xmax= 360, ymax = 90)
#' @param param char, the identifier of the data set
#' @param path char, the output path
#' @return tibble database for existing files
fetch_month = function(dates = seq(from = as.Date("1981-09-01"), 
                                  to = current_month(offset = -1),
                                  by = "month"),
                       param = "sst.mon.mean",
                       bb = c(xmin = 0, ymin = 0, xmax = 360, ymax = 90),
                       path = oisst_path("world")){

  
  uri <- query_oisst(param = param)
  x <- ncdf4::nc_open(uri)
  alldates = get_time(x)
  # here we craft the filenames from the NCDF and the dates
  # and then the db, and 
  ofile <- generate_filename(x, dates)
  db <- decompose_filename(ofile) |>
    dplyr::filter(date %in% dates)
  ofile <- compose_filename(db, path)
  ok <- sapply(unique(dirname(ofile)), dir.create, 
               recursive = TRUE, showWarnings = FALSE)
  
  nav = get_nav(x, bb = bb)
  ok = sapply(seq_len(nrow(db)),
    function(idate){
      nav$start[3] = get_time_index(x, db$date[idate])
      r = try(suppressMessages(get_one(x, nav = nav)))
      if (inherits(r, "try-error")){
        print(r)
        r = FALSE
      } else {
        r = stars::write_stars(r, ofile[idate])
        r = file.exists(ofile[idate])
      }
      r
    })
  
  ncdf4::nc_close(x)
  dplyr::filter(db, ok)
}

#' Fetch and store an entire year
#'
#' @export
#' @param year num, the year to fetch
#' @param param char, the parameter to fetch
#' @param path char, the output path
#' @return tibble database of existing files
fetch_year <- function(year = 1981,
                       param = 'sst.day.mean',
                       path = oisst_path("world")){
  
  uri <- query_oisst(year = year, param = param)
  x <- ncdf4::nc_open(uri)
  
  # awkwardly generate filename, decompose into database, and then compose into
  # full local storage paths
  ofile <- generate_filename(x)
  db <- decompose_filename(ofile)
  ofile <- compose_filename(db, path)
  
  # make sure the subdirectories exist
  ok <- sapply(unique(dirname(ofile)), dir.create, recursive = TRUE, showWarnings = FALSE)
  
  #Now iterate through the days
  times <- get_time(x)
  
  ok <- sapply(seq_along(times),
      function(itime){
         s <- get_one(x, time = times[itime]) |>
           stars::write_stars(ofile[itime])
         file.exists(ofile[itime])
      })
  
  ncdf4::nc_close(x)
  dplyr::filter(db, ok)
}


#' Fetch and store one or more dates
#'
#' @export
#' @param dates Date, the dates
#' @param param char, the parameter to fetch
#' @param path char, the output path
#' @return tibble database (possibly with no wrows)
fetch_day <- function(dates = Sys.Date() + c(-10, -9, -8),
                       param = 'sst.day.mean',
                       bb = c(xmin = 0, ymin = -90, xmax = 360, ymax = 90),
                       path = oisst_path("world")){
  
  get_year <- function(tbl, key){
  
    uri <- query_oisst(year = key$year, param = param)
    x <- ncdf4::nc_open(uri)
    
    # awkwardly generate filename, decompose into database, and then compose into
    # full local storage paths
    ofile <- generate_filename(x)
    # trim the dates to those requested
    db <- decompose_filename(ofile) |>
      dplyr::filter(date %in% tbl$date)
    
    if (nrow(db) == 0) {return(dplyr::slice(db, 0))}
    
    ofile <- compose_filename(db, path)
    
    # make sure the subdirectories exist
    ok <- sapply(unique(dirname(ofile)), dir.create, recursive = TRUE, showWarnings = FALSE)
    
    ok <- sapply(seq_along(db$date),
                 function(i){
                   s <- get_one(x, bb = bb, time = db$date[i]) |>
                     #sf::st_crop(BB) |>
                     stars::write_stars(ofile[i])
                   file.exists(ofile[i])
                 })
    
    ncdf4::nc_close(x)
    dplyr::filter(db, ok)
  }
  
  
  db = dplyr::tibble(date = dates, year = format(date, "%Y")) |>
    dplyr::group_by(year) |>
    dplyr::group_map(get_year) |>
    dplyr::bind_rows()
  db
}

#' Fetch OISST data for a given time range, parameter and bounding box.
#' 
#' @export
#' @param dates Date, two element vector of dates specifying the start and stop range
#' @param param char, identifies the parameter including
#' \itemize{
#'  \item{sst.day.mean}
#'  \item{icec.day.mean}
#'  \item{sst.week.mean}
#'  \item{icec.week.mean}
#'  \item{sst.month.mean}
#'  \item{icec.month.mean}
#'  \item{sst.day.mean.ltm.<start>-<stop>  include stars and stop years}
#'  \item{icec.day.mean.ltm.<start>-<stop> include stars and stop years}
#' }
#' @param bb numeric, named vector of xmin, xmax, ymin and ymax
#' @param path char the root path where data is saved
fetch_oisst = function(dates = c(as.Date("1991-09-01"), Sys.Date()),
                       param = 'sst.day.mean',
                       bb = c(xmin = 0, ymin = -90, xmax = 360, ymax = 90),
                       path = oisst_path("world")){
   
  if (grepl("lsmask", param[1], fixed = TRUE)){
    stop("please use `fetch_mask()` to fetch the land-sea mask")
  }
                                               
  if (grepl("day", param[1], fixed = TRUE)){
    dates = seq(from = dates[1], to = dates[length(dates)], by = "day")
    db = fetch_day(dates, param = param, bb = bb, path = path)
  }
  
  if (grepl("mon.mean", param[1] ,fixed = TRUE)){
    dates = seq(from = current_month(dates[1]),
                to = current_month(dates[length(dates)]),
                by = "month")
    db = fetch_month(dates, param = param, bb = bb, path = path)
  }
  
  if (grepl("week", param[1],fixed = TRUE)){
    dates = c(dates[1], dates[length(dates)])
    db = fetch_week(dates, param = param, bb = bb, path = path)
  }
  
  if (grepl("ltm", param[1], fixed = TRUE)){
    db = fetch_ltm(param = param, bb = bb, path = path)
  }
  return(db)
}


