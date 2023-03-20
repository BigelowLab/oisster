#' Given one or more filename, decompose into a database
#'
#' @export
#' @param x char vector of filenames
#' @return tibble database
decompose_filename <- function(x = 
  c("/foo/bar/sst.day.mean.1981-09-01.tif",
    "/foo/bar/icec.mon.max.1981-09-01.tif")){
  
  ss = strsplit(basename(x), ".", fixed = TRUE)
  
  dplyr::tibble(
    date = as.Date(sapply(ss, '[[', 4)),
    param = sapply(ss, '[[', 1),
    per = sapply(ss, '[[', 2),
    trt = sapply(ss, '[[', 3))
}


#' Given a database and path, compose filenames
#'
#' @export
#' @param x tibble database
#' @param path char, path description
#' @param ext char the extension for the files
#' @return fully qualified paths
compose_filename <- function(x = decompose_filename(), 
                             path = "/foo/bar",
                             ext = "tif"){
  
  file.path(path,
            format(x$date, "%Y"),
            format(x$date, "%m%d"),
            sprintf("%s.%s.%s.%s.%s",
                    x$param, x$per, x$trt, format(x$date, "%Y-%m-%d"), ext))
  
}


#' Build and optionally save a database
#' 
#' @export
#' @param path char, path to the dataset
#' @param pattern char pattern to search for (as regular expression)
#' @param save_db logical, if TRUE save the database in the specified path as database.csv.gz
build_database <- function(path, 
                           pattern = "^.*\\.tif$",
                           save_db = FALSE){
  
  x <- list.files(path,
                   recursive = TRUE,
                   pattern = pattern) |>
    decompose_filename()
  
  if (save_db) write_database(x, path)
  x
}


#' Read a OISST database
#' @export
#' @param path char, path description to the database
#' @param filename char, name of the file in the path
read_database <- function(path, filename = "database.csv.gz"){
  readr::read_csv(file.path(path, filename),
                  show_col_types = FALSE)
}


#' Write a OISST database
#' @export
#' @param x tibble database
#' @param path char, path description to the database
#' @param filename cahr, the filename to save to
#' @return the input database
write_database <- function(x, path, filename = "database.csv.gz"){
  readr::write_csv(x, file.path(path, filename))
}
