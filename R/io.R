#' Read OISST data files
#' 
#' @export
#' @param x database (tibble) of common param, per type (no checking done)
#' @param path char, the path to the data
#' @return stars object
read_oisst <- function(x, path){
  x <- dplyr::arrange(x, date)
  filename = compose_filename(x, path)
  stars::read_stars(filename, along = list(date = x$date)) |>
    rlang::set_names(x$param[1])
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