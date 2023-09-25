#' Convert [0,360] longitudes to [-180, 180]
#'
#' @seealso \url{https://gis.stackexchange.com/questions/201789/verifying-formula-that-will-convert-longitude-0-360-to-180-to-180/201793}
#' @export
#' @param x numeric vector, no check is done for being withing [0, 360] range
#' @return numeric vector
to_180 <- function(x) { ((x + 180) %% 360) - 180 }

#' Convert [-180,180] longitudes to [0, 360]
#'
#' @seealso \url{https://gis.stackexchange.com/questions/201789/verifying-formula-that-will-convert-longitude-0-360-to-180-to-180/201793}
#' @export
#' @param x numeric vector, no check is done for being within [0,3 60] range
#' @return numeric vector
to_360 <- function(x) {x %% 360}
