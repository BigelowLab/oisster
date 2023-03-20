#' Read OISST data files
#' 
#' @export
#' @param x database (tibble) of common param, per type (no checking done)
#' @param path char, the path to the data
#' @return stars object
read_oisst <- function(x, path){
  
  x <- dplyr::arrange(x, date)
  filename = compose_filename(x, path)
  stars::read_stars(filename, along = list(date = x$date))
}