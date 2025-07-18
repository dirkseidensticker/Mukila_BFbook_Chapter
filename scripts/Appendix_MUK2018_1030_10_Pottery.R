# Appendix_MUK2018_1030_10_Pottery

library(tidyverse)

d <- xlsx::read.xlsx2("../MUK2018_1030_10_Pottery.xlsx", sheetIndex = 1) %>%
  dplyr::rename(act = Act) %>%
  dplyr::left_join(geojsonsf::geojson_sf("gis/MUK2018_excav.geojson") %>%
                     dplyr::select(act, square, z), 
                   by = "act")

sf::st_geometry(d) <- NULL

d %>%
  dplyr::select(act, square, z, Obj, VU, n, Typ, Size, Wgt, Wall, Color, Fabric, Morph, Decor, Note) # %>%
  # xlsx::write.xlsx2("Appendix_MUK2018_1030_10_Pottery.xlsx", row.names = F)
