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

#' Fetch a series of months
#'
#' @export
#' @param dates Date, one or more dates
#' @param bb numeric 4 element bounding box in the range of 
#'   (xmin = 0, ymin = -90, xmax= 360, ymax = 90)
#' @param path char, the output path
#' @param logical, TRUE if successful
fetch_month = function(dates = seq(from = as.Date("1981-09-01"), 
                                  to = current_month(offset = -1),
                                  by = "month"),
                       bb = c(xmin = 0, ymin = 0, xmax = 360, ymax = 90),
                       path = oisst_path("world")){
  
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
  uri <- query_oisst(param = "sst.mon.mean")
  x <- ncdf4::nc_open(uri)
  
  # here we craft the filenames from the NCDF and the dates
  # and then the db, and 
  ofile <- generate_filename(x, dates)
  db <- decompose_filename(ofile)
  ofile <- compose_filename(db, path)
  ok <- sapply(unique(dirname(ofile)), dir.create, 
               recursive = TRUE, showWarnings = FALSE)
  
  
  xx = lapply(seq_along(dates),
    function(idate){
      r = try(suppressMessages(get_one(x, time = dates[idate])))
      if (inherits(r, "try-error")){
        print(r)
        r = NULL
      } else {
        r = suppressMessages(sf::st_crop(r, BB)) |>
          stars::write_stars(ofile[idate])
      }
      r
    })
  
  do.call(c, append(xx, list(along = list(date = dates))))
}

#' Fetch and store an entire year
#'
#' @export
#' @param year num, the year to fetch
#' @param param char, the parameter to fetch
#' @param left num, see \code{\link{get_one}}
#' @param path char, the output path
#' @param logical, TRUE if successful
fetch_year <- function(year = 1981,
                       param = 'sst.day.mean',
                       left = -180,
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
         s <- get_one(x, time = times[itime], left = left) |>
           stars::write_stars(ofile[itime])
         file.exists(ofile[itime])
      })
  
  ncdf4::nc_close(x)
  all(ok)
}


#' Fetch and store one or more dates
#'
#' @export
#' @param dates Date, the dates
#' @param param char, the parameter to fetch
#' @param left num, see \code{\link{get_one}}
#' @param path char, the output path
#' @param tibble database (possibly with no wrows)
fetch_dates <- function(dates = Sys.Date() + c(-10, -9, -8),
                       param = 'sst.day.mean',
                       left = -180,
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
                   s <- get_one(x, time = db$date[i], left = left) |>
                     stars::write_stars(ofile[i])
                   file.exists(ofile[i])
                 })
    
    ncdf4::nc_close(x)
    dplyr::filter(db, ok)
  }
  
  
  dplyr::tibble( date = dates, year = format(date, "%Y")) |>
    dplyr::group_by(year) |>
    dplyr::group_map(get_year) |>
    dplyr::bind_rows()
}



