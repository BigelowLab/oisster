#' Generate filenames for ncdf4 contents
#' 
#' @export
#' @param x ncdf4 object
#' @param dates Date, the dates to format, by default all available
#' @param format char, the date format to use
#' @param ext char extension to apply to filename (default ".tif")
#' @return char vector of "param.per.trt.YYYY-mm-dd.ext"
#'   or "param.per.trt.YYYY-YYYY.YYYY-mm-dd.ext" for ltm
generate_filename <- function(x, 
                              dates = get_time(x),
                              format = "%Y-%m-%d",
                              ext = ".tif"){
  
  s = gsub(".nc", "", basename(x$filename), fixed = TRUE)
  ss = strsplit(s, ".", fixed = TRUE)[[1]]
  dates = get_time(x)
  if ("ltm" %in% ss) {
    #icec.day.mean.ltm.1991-2020.nc  # 5 all have 5 segements
    #sst.day.mean.ltm.1971-2000.nc   # 5
    #sst.day.mean.ltm.1982-2010.nc   # 5
    #sst.day.mean.ltm.1991-2020.nc   # 5
    #sst.day.mean.ltm.nc             # 4 this gets special treatment
    #sst.mon.ltm.1991-2020.nc        # 5 while here no mean like the others?
    
    if ("mon" %in% ss){
      # "sst.mon.ltm.1991-2020.nc"
      cb = ncdf4::ncvar_get(x, varid = "climatology_bounds",
                            start = c(1,1), count = c(2,1)) |>
        as.Date(origin = get_epoch(x))
      yy = paste(format(cb, "%Y"), collapse = "-")
      dates = seq(from= cb[1], length.out = 12, by = "month") |>
        format( ".%Y-%m-%d")
      
    } else if (length(ss) <= 4) {
      # "sst.day.mean.ltm" always gets first_year
      #the climatology bounds contains the start stop for each element
      cb = ncdf4::ncvar_get(x, varid = "climatology_bounds",
                            start = c(1,1), count = c(2,1)) |>
        as.Date(origin = get_epoch(x))
      yy = paste(format(cb, "%Y"), collapse = "-")
      ss[3] = "ltm"
      ss = c(ss, yy)
      s = paste(ss, collapse = ".")
      if (ss[2] == "day"){
        dates = seq(from= cb[1], length.out = length(dates), by = "day")
      } else {
        dates = seq(from= cb[1], length.out = length(dates), by = "month")
      }
      dates = format(dates, ".%Y-%m-%d")
    } else {
      # "icec.day.mean.ltm.1991-2020" "sst.day.mean.ltm.1971-2000"  "sst.day.mean.ltm.1982-2010" 
      # "sst.day.mean.ltm.1991-2020" "sst.mon.ltm.1991-2020"   
      ss[3] = "ltm"
      s = paste(ss, collapse = ".")
      startdate = as.Date(paste0(substring(ss[[5]],1,4), "-01-01"))
      if (ss[2] == "day"){
        dates = seq(from = startdate, by = "day", length.out = length(dates))
      } else {
        dates = seq(from = startdate, by = "month", length.out = length(dates))
      }
      dates = format(dates, ".%Y-%m-%d")
    }
    
  } else {
    s = paste(ss[1:3], collapse = ".")
    dates = format(dates, ".%Y-%m-%d")  
  }
  paste0(s, dates, ext)
}


#' Retrieve the initial epoch as Date class
#'
#' @export
#' @param x ncdf4 object
#' @return Date class object 
get_epoch <- function(x){
  
  # ltm has a different epoch format
  fmt = if(x$dim$time$units == "days since 1800-01-01 00:00:0.0") {
    "days since %Y-%m-%d 00:00:0.0"
  } else {
    "days since %Y-%m-%d 00:00:00"
  }
  
 as.Date(x$dim$time$units, format = fmt)
}

#' Retrieve the time values as Date class
#' 
#' @export
#' @param x ncdf4 object
#' @param epoch Date the epoch from which time is figured
#' @return Date class vector
get_time <- function(x, epoch = get_epoch(x)){
  time = x$dim$time$vals + epoch
  # known problem dates earlier than some date are off by 2 days
  # related to this bug? https://github.com/dankelley/oce/issues/738  
  ix = time < as.Date("1800-01-01")
  time[ix] = time + 2
  time
}


#' Get the indices for specified times
#' 
#' @export
#' @param x ncdf4 object
#' @param time Date class times
#' @return index of where the specified time falls relative to the times in \code{x}
get_time_index <- function(x, time){
  ix = findInterval(time, get_time(x))
  if (any(ix <= 0)) warning("requested time prior to available time")
  ix
}

#' Get latitude values
#'
#' @export
#' @param x ncdf4 object
#' @return a vector of latitudes
get_lat = function(x){
  x$dim$lat$vals
}

#' Get longitude values
#'
#' @export
#' @param x ncdf4 object
#' @return a vector of longitude
get_lon = function(x){
  x$dim$lon$vals
}

#' Get spatial resolution
#'
#' @export
#' @param x ncdf4 object
#' @return a vector of x and y resolution
get_res = function(x){
  c( abs(diff(x$dim$lon$vals[1:2])),
     abs(diff(x$dim$lat$vals[1:2])) )
}



#' Retrieve the a list of geometry info
#' 
#' @export
#' @param x ncdf4 object
#' @return list of [x/y] limits and [x/y] steps
get_geometry <- function(x){
  
  dx <- abs(diff(x$dim$lon$vals[1:2]))
  dy <- abs(diff(x$dim$lat$vals[1:2]))
  xlim = range(x$dim$lon$vals) + dx/c(-2, 2)
  ylim = range(x$dim$lat$vals) + dy/c(-2, 2)
  bbox = sf::st_bbox(c(xmin = xlim[1], xmax = xlim[2], 
                       ymin = ylim[1], ymax = ylim[2]), 
                 crs = oisster_crs())

  list(
    xlim = xlim,
    ylim = ylim,
    nx = x$dim$lon$len,
    ny = x$dim$lat$len,
    dx = dx,
    dy = dy,
    bbox = bbox
  )
}

#' Get ncdf variable names and return the most likely candidate.
#'
#' If most likely candidate is not found then the first is returned
#' 
#' @export
#' @param x ncdf4 object
#' @param likely char, one or more most likely candidates
#' @return character variable name
get_varname = function(x, likely = c("sst", "icec")){
  nm = names(x$var)
  ix = nm %in% likely
  if (any(ix)){
    r = nm[ix][1]
  } else {
    r = nm[1]
  }
  r
}
 


#' Get the navigation list for extracting form the ncdf object
#' 
#' @export
#' @param x ncfd object
#' @param varid char the name of the variable to be extracted
#' @param bb numeric, named vector of xmin, xmax, ymin and ymax for extraction
#' @param time numeric, 2 element time indices (or Dates over which to extract)
#'   in the form (start, count) or (startDate, endDate)  
#' @return a navigation structure with
#' \itemize{
#'   \item{varid the id of the variable}
#'   \item{bb the requested bounding box}
#'   \item{bbox the actual bounding box}
#'   \item{ndim the number of dimensions}
#'   \item{start numeric vector of start indices}
#'   \item{count numeric vector of extraction sizes}
#'   }
get_nav = function(x, 
                   varid = get_varname(x),
                   bb = c(xmin = 0, ymin = -90, xmax = 360, ymax = 90),
                   time = c(1,1)){
  
  stopifnot(varid %in% names(x$var))
  
  # convert time to indices
  is_real_time = inherits(time, c("Date", "POSIXt"), which = TRUE) |>
    as.logical() |>
    any()
  # make sure time is either start-stop or start-count
  if (length(time) == 1){
    time = if(is_real_time){
      c(time, time)
    } else {
      c(time, 1)
    }
  }
  
  if (is_real_time) {
    ix = findInterval(time, get_time(x))
    time = c(ix[1], ix[2] - ix[1] + 1)
  }
  if (time[1] <= 0) stop("time[1] must be at or later than:", 
                      format(X$get_time()[1], "%Y-%m-%d"))
  if (time[2] <= 0) stop("time[2] implies zero-length")
  
  if (inherits(bb, "bbox")) bb = as.vector(bb)
  if (!all(names(bb) %in% c("xmin", "xmax", "ymin", "ymax"))){
    stop('names of bb must include "xmin", "xmax", "ymin" and "ymax"' )
  }
  
  res = get_res(x)
  r2 = res/2
  lon = get_lon(x)
  lat = get_lat(x)
  
  closest_index = function(x, vec){
    which.min(abs(vec-x))
  } 
  
  ix = unname(sapply(bb[c('xmin', "xmax")] + c(-r2[1], r2[1]), closest_index, lon))
  nx = ix[2] - ix[1] + 1
  xmin = lon[ix[1]] - r2[1]
  xmax = lon[ix[2]] + r2[1]
  
  iy = unname(sapply(bb[c("ymin", "ymax")] + c(-r2[2], r2[2]), closest_index, lat))
  if (iy[1] >= iy[2]) {
    ny = iy[1] - iy[2] + 1
    ymin = lat[iy[1]] - r2[2]
    ymax = lat[iy[2]] + r2[1]
    iy = rev(iy)
  } else {
    ny = iy[2] - iy[1] + 1
    ymin = lat[iy[1]] - r2[2]
    ymax = lat[iy[2]] + r2[1]
  }
  
  bbox = c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
  
  list(
    bb = bb,
    varid = varid,
    bbox = bbox,
    ndim = x$var[[varid]]$ndims,
    start = c(ix[1], iy[1],  time[1]),
    count = c(nx, ny, time[2]), 
    res = res)
}

#' Retrieve a single layer (date) for one attribute
#' 
#' @export
#' @param x ncdf4 object
#' @param att char, the name of the attribute (variable)
#' @param time Date or index of the date to retrieve
#' @param bb numeric, named vector of xmin, xmax, ymin and ymax
#' @param nav list of navigation info, or NULL
#' @return stars object
get_one <- function(x,
                    att = get_varname(x),
                    bb = c(xmin = 0, ymin = -90, xmax = 360, ymax = 90),
                    time = 1,
                    nav = NULL){
  
  if (is.null(nav)) nav = get_nav(x, varid = att, bb = bb, time = time)
  
  m <- ncdf4::ncvar_get(x, nav$varid,
                        start = nav$start,
                        count = nav$count) 

 s = stars::st_as_stars(
    sf::st_bbox(nav$bbox, crs = oisster_crs()),
    values = m,
    nx = nav$count[1],
    ny = nav$count[2],
    xlim = nav$bbox[c("xmin", "xmax")],
    ylim = nav$bbox[c("ymin", "ymax")]) |>
   stars::st_flip("y")
 s
}
