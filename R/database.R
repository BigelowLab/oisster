#' Parse OISST filenames
#'
#' Not to be confused with \code{decompose_filename}
#'
#' @export
#' @param x char, one or more filenames
#' @return a tibble of filename constituent parts `date`, `param`, `per1, 'trt` and `ltm`
parse_oisst_filename = function(x = c("icec.day.mean.1981.nc", "icec.day.mean.ltm.1991-2020.nc",
                                      "icec.mon.mean.nc", "icec.week.mean.nc",
                                      "icec.day.mean.ltm.nc",
                                      "sst.day.mean.1981.nc", "sst.day.mean.ltm.1991-2020.nc",
                                      "sst.mon.mean.nc", "sst.week.mean.nc",
                                      "sst.day.mean.ltm.nc")){
                
  x = basename(x)
  x = sub(".nc", "", x, fixed = TRUE)
  x = sub(".tif", "", x, fixed = TRUE)
  iltm = grepl("ltm", x, fixed = TRUE)
  iday = grepl("day", x, fixed = TRUE)
  ss = strsplit(x, ".", fixed = TRUE)
  lapply(seq_len(length(x)),
    function(i){
      if (iltm[i]){
        ltm = if (length(ss[[i]]) == 4){
          paste0("1981-" , format(Sys.Date(), "%Y"))
        } else {
          ss[[i]][5]
        }
        r = dplyr::tibble(
          date = as.Date(paste0(substr(ltm, 1,4), "-01-01"), format = "%Y-%m-%d"),
          param = ss[[i]][1],
          per = ss[[i]][2],
          trt = "ltm",
          ltm = ltm)
      } else if (iday[i]){
        r = dplyr::tibble(
          date = as.Date(paste0(substr(ss[[i]][4], 1,4), "-01-01"), format = "%Y-%m-%d"),
          param = ss[[i]][1],
          per = ss[[i]][2],
          trt = ss[[i]][3],
          ltm = NA_character_)
      } else {
        r = dplyr::tibble(
          date = as.Date("1981-09-01", format = "%Y-%m-%d"),
          param = ss[[i]][1],
          per = ss[[i]][2],
          trt = ss[[i]][3],
          ltm = NA_character_)
      } 
      r
    }) |>
    dplyr::bind_rows()
    
}

#' Given one or more filename, decompose into a database
#'
#' @export
#' @param x char vector of filenames
#' @param ext char the extension to remove before processing. NA to skip.
#' @return tibble database
decompose_filename <- function(x =  c("sst.day.mean.1981-09-01.tif",
                                      "icec.mon.max.1981-09-01.tif",
                                      "icec.mon.max.ltm.1991-2020.tif",
                                      "sst.day.mean.ltm.1971-2000.1971-01-01.tif",
                                      "sst.mon.ltm.1991-2020.1991-11-01.tif"),
                               ext = ".tif"){
    
  if (!is.na(ext)) x = gsub(ext, "", x, fixed = TRUE)
  
  ss = strsplit(basename(x), ".", fixed = TRUE)
  
  lapply(ss,
    function(s){
      if (all(c("mon","ltm") %in% s)){
        # sst.mon.ltm.1991-2020.1991-11-01
        r = dplyr::tibble(
          date = as.Date(s[5], format = "%Y-%m-%d"),
          param = s[1],
          per = s[2],
          trt = "ltm",
          ltm = s[4])
      } else if (length(s) >= 6){
        r = dplyr::tibble(
          date = as.Date(s[6], format = "%Y-%m-%d"),
          param = s[1],
          per = s[2],
          trt = s[3],
          ltm = s[5])
      } else if (length(s) >= 5){
        r = dplyr::tibble(
          date = as.Date(paste0(substr(s[5], 1,4), "-01-01"), format = "%Y-%m-%d"),
          param = s[1],
          per = s[2],
          trt = s[3],
          ltm = s[5])
      } else {
        r = dplyr::tibble(
          date = as.Date(s[4]),
          param = s[1],
          per = s[2],
          trt = s[3],
          ltm = NA_character_)
      }
      r
    }) |>
    dplyr::bind_rows()
}


#' Given a database and path, compose filenames
#'
#' @export
#' @param x tibble database
#' @param path char, path description
#' @param ext char the extension for the files
#' @return fully qualified paths inlcluding <path>/YYYY/mmdd/filename.<ext>"
compose_filename <- function(x = decompose_filename(), 
                             path = "/foo/bar",
                             ext = "tif"){
  
  x |>
  dplyr::rowwise() |>
  dplyr::group_map(
    function(tbl, key){
      if (!is.na(tbl$ltm)){
        r = file.path(path,
                      format(tbl$date, "%Y"),
                      format(tbl$date, "%m%d"),
                      sprintf("%s.%s.%s.%s.%s.%s",
                              tbl$param, tbl$per, tbl$trt, tbl$ltm, format(tbl$date, "%Y-%m-%d"), ext))
  
      } else {
        r= file.path(path,
                    format(tbl$date, "%Y"),
                    format(tbl$date, "%m%d"),
                    sprintf("%s.%s.%s.%s.%s",
                            tbl$param, tbl$per, tbl$trt, format(tbl$date, "%Y-%m-%d"), ext))
  
      }
      r
    }) |>
    unlist()
                                 

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
