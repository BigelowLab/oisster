#' Read OISST data files
#' 
#' @export
#' @param x database (tibble) of common param, per type (no checking done)
#' @param path char, the path to the data
#' @param along NULL or somehting user specified.  If NULL then 
#'   we assume the user wants to stack by date, but the user might 
#'   want one attribute per database row.  
#' @return stars object
read_oisst <- function(x, path = oisst_path(), along = NULL){
  
  x <- dplyr::arrange(x, date)
  filename = compose_filename(x, path)
  r = if (is.null(along)){
    stars::read_stars(filename, along = list(date = x$date))
  } else {
    stars::read_stars(filename, along = along)
  }
  r
}



#' Read an OISST mask
#' 
#' @export
#' @param x char, the name of the mask to read (default "world")
#' @param path char the path to the mask data
#' @return stars class object
read_mask = function(x = "world", path = oisst_path("mask")){
  filename = file.path(path, paste0(x, ".tif"))
  if (!file.exists(filename)) stop("mask not found: ", filename)
  stars::read_stars(filename) |>
    rlang::set_names("mask")
}