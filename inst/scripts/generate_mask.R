#' Generate a mask from the annual max summaries, which assumes that 
#' northern/southern hemisphere ice retreat exposes as much non-open-water 
#' as we might expect.
#' 


suppressPackageStartupMessages({
  library(oisster)
  library(stars)
  library(dplyr)
})

MASKPATH = oisst_path("mask")
if (!dir.exists(MASKPATH)) ok = dir.create(MASKPATH)
PATH = oisst_path("world")
DB = read_database(PATH) |>
  dplyr::filter(per == "ann",
                param == "sst",
                trt  == "max")
mask = read_oisst(DB, PATH) |>
  stars::st_apply(c("x", "y"), max, na.rm = TRUE)|>
  rlang::set_names("mask") |>
  dplyr::mutate(mask = dplyr::if_else(is.infinite(.data$mask), NA, .data$mask)) |>
  dplyr::mutate(mask = dplyr::if_else(is.na(.data$mask), NA, 1)) |>
  stars::write_stars(file.path(MASKPATH, "world.tif"))

