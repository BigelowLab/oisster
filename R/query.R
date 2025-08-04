#' Retrieve the base URL for the catralog as HTML or XML
#'
#' @export
#' @param what char one of 'html' or 'xml' (default)
#' @return URL
oisst_uri <- function(what = c("html", "xml")[1]){
  
  switch(tolower(what)[1],
  "xml" = "https://psl.noaa.gov/thredds/catalog/Datasets/noaa.oisst.v2.highres/catalog.xml",
  "html" = "https://psl.noaa.gov/thredds/catalog/Datasets/noaa.oisst.v2.highres/catalog.html",
  "odap" = "http://psl.noaa.gov")
}


#' List the data sets offered by OISST thredds catalog
#'
#' @export
#' @param base_uri char, the base URI to the top level catalog (as .xml)
#' @return dataframe of available resources
list_oisst = function(base_uri = oisst_uri("xml")){
  Top = thredds::get_catalog(base_uri)
  
  odap_uri <- paste0(oisst_uri("odap"), Top$list_services()[['odap']][['base']])
  dd <- Top$list_datasets()
  DD <- Top$get_datasets(names(dd))
  DDurl <- sapply(names(DD),
         function(nm){
           paste0(odap_uri, DD[[nm]]$get_url())
          })
          
  dplyr::bind_rows(dd) |>
  dplyr::mutate(opendap = DDurl)
}

#' Query the OISST thredds catalog
#' 
#' @export
#' @param year num, one or more years to query
#' @param param char, one parameter to query such as 'sst.day.mean' or 'sst.mon.mean'
#' @param base_uri char, the base URI to the top level catalog (as .xml)
#' @return named list of URLs to datastes matching the query
query_oisst <- function(year = seq(from = 1981, 
                                   to = as.numeric(format(Sys.Date(), "%Y")),
                                   by = 1),
                        param = "sst.day.mean",
                        base_uri = oisst_uri("xml")){
  
  # some don't need to be searched for
  
  if (tolower(param[1]) %in% c("sst.mon.mean", "sst.week.mean","lsmask.oisst","icec.mon.mean", "icec.week.mean") ){
    return(sprintf("http://psl.noaa.gov/thredds/dodsC/Datasets/noaa.oisst.v2.highres/%s.nc", param[1]))
  }
  
  
  Top = thredds::get_catalog(base_uri)
  
  odap_uri <- paste0(oisst_uri("odap"), Top$list_services()[['odap']][['base']])
  
  dd <- Top$list_datasets()
  nmdd <- names(dd)
  
  if (grepl("ltm", param[1])){
    ix = grepl(paste0(param[1], ".nc"), nmdd, fixed = TRUE)
    if (!any(ix)){
      stop("ltm product not found:", param[1])
    }
  } else {
    pat <- sprintf("%s.%0.4i", param, as.numeric(year))
    ix <- charlier::mgrepl(pat, nmdd, fixed = TRUE)
  }
  
  dd <- Top$get_datasets(nmdd[ix])
  
  sapply(names(dd),
         function(nm){
           paste0(odap_uri, dd[[nm]]$get_url())
          })
}