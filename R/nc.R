#' Generate filenames for ncdf4 contents
#' 
#' @export
#' @param x ncdf4 object
#' @param dates Date, the dates to format, by default all available
#' @param format char, the date format to use
#' @param ext char extension to apply to filename (default ".tif")
#' @return char vector of "param.per.trt.YYYY-mm-dd.ext"
generate_filename <- function(x, 
                              dates = get_time(x),
                              format = "%Y-%m-%d",
                              ext = ".tif"){
  
  s = gsub(".nc", "", basename(x$filename), fixed = TRUE)
  dates = format(dates, ".%Y-%m-%d")  
  paste0(s, dates, ext)
}


#' Retrieve the initial epoch as Date class
#'
#' @export
#' @param x ncdf4 object
#' @return Date class object 
get_epoch <- function(x){
  as.Date(x$dim$time$units, format = "days since %Y-%m-%d 00:00:00")
}

#' Retrieve the time values as Date class
#' 
#' @export
#' @param x ncdf4 object
#' @param epoch Date the epoch from which time is figured
#' @return Date class vector
get_time <- function(x, epoch = get_epoch(x)){
  x$dim$time$vals + epoch
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


#' Retrieve a single layer (date) for one attribute
#' 
#' @export
#' @param x ncdf4 object
#' @param att char, the name of the attribute (variable)
#' @param time Date or index of the date to retrieve
#' @param left numeric one of 0 (default) or -180.  If the latter the x values
#'   span [-180, 180] otherwise [0,360]
#' @return stars object
get_one <- function(x,
                    att = names(x$var)[1],
                    time = get_time(x)[1],
                    left = c(0, -180)[1]){
  
  if (!att %in% names(x$var)) stop("attribute not found: ", att)
  if (inherits(time, "Date")) time <- get_time_index(x, time)
  
  m <- ncdf4::ncvar_get(x, att,
                        start = c(1,1,time),
                        count = c(-1, -1, 1))
  
  g <- get_geometry(x)
  if (left <= -180){
   v <- t(m)
   d <- dim(v)
   nhalf <- d[2]/2
   v <- cbind(v[,nhalf + seq_len(nhalf)], v[,seq_len(nhalf)])
   g$xlim = c(-180, 180) 
   g$bbox = sf::st_bbox(c(xmin = g$xlim[1], xmax = g$xlim[2], 
                          ymin = g$ylim[1], ymax = g$ylim[2]), 
                        crs = oisster_crs())
   v <- as.vector(t(v))
  } else{
    v = as.vector(m)
  }
  stars::st_as_stars(g$bbox,
    values = v,
    nx = g$nx,
    ny = g$ny,
    xlim = g$xlim,
    ylim = rev(g$ylim),
    dx = g$dx,
    dy = g$dy)
}
