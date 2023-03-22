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



