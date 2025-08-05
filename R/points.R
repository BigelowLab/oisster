#' Extract point data from a stars object
#' 
#' @export
#' @param x stars object (with or without time dimension)
#' @param y sf point data
#' @param form chr one of either "long" (the default) or "wide" to control 
#'   output column layout. 
#' @param pnames NULL or chr, names to apply to the output point data. If NULL then
#'   names are assinged as "p01" or "p001" etc depending upon the number of input points.
#' @return table of variable data for each input point
extract_points = function(x, y, 
                          pnames = NULL, 
                          form = c("long", "wide")[1]){
  
  if (is.null(pnames)){
    n = nrow(y)
    N = floor(log10(n)) + 1
    fmt = paste0("p%0.", N, "i")
    pnames = sprintf(fmt, seq_len(n))
  }
  xy = sf::st_coordinates(y)
  if (length(dim(x)) == 2){
    m = stars::st_extract(x, xy)
    rownames(m) <- pnames
    p = dplyr::as_tibble(m, rownames = "point",
                         .name_repair = "check_unique") |>
      tidyr::pivot_longer(-dplyr::all_of("point"),
                          values_to = "value",
                          names_to = "name")
    
  } else {
    nms = stars::st_get_dimension_values(x, 3)
    dtime = stars::st_dimensions(x)[[3]]
    p = lapply(names(x),
               function(nm){
                 m = stars::st_extract(x[nm], xy)
                 colnames(m) <- nms
                 rownames(m) <- pnames
                 m |>
                   dplyr::as_tibble(rownames = "point") |>
                   dplyr::mutate(name = nm, .after = 1)
               }) |>
      dplyr::bind_rows() |>
      tidyr::pivot_longer(-dplyr::all_of(c("point", "name")),
                          names_to = "time",
                          values_to = "value")
    
    if (inherits(dtime$offset, "POSIXt")){
      timev = as.POSIXct(as.numeric(p$time), 
                         origin = as.POSIXct("1970-01-01", tz = "UTC"),
                         tz = "UTC")
      p = dplyr::mutate(p, time = timev)
    } else if (inherits(dtime$offset, "Date")){
      timev = as.Date(as.numeric(p$time), 
                      origin = as.Date("1970-01-01"))
      p = dplyr::mutate(p, time = timev)
    }
    
  }
  
  if (tolower(form[1]) == "wide"){
    p = tidyr::pivot_wider(p,
                           names_from = "name",
                           values_from = "value")
  }
  
  p
}