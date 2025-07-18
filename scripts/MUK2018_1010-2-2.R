# MUK 2018/1010


library(ggplot2)
library(ggnewscale)

library(tidyr)

dem <- raster::raster("input/MUK2018-1010-2-1/dem.tif")
raster::crs(dem) <- "+init=epsg:32733"

dem <- raster::aggregate(dem, fact=6)

slope = raster::terrain(dem, opt='slope')
aspect = raster::terrain(dem, opt='aspect')
hill = raster::hillShade(slope, aspect, 40, 270)

dem_spdf <- as(dem, "SpatialPixelsDataFrame")
dem_spdf <- as.data.frame(dem_spdf)
colnames(dem_spdf) <- c("value", "x", "y")

hist(dem_spdf$value)



hill_spdf <- as(hill, "SpatialPixelsDataFrame")
hill_spdf <- as.data.frame(hill_spdf)
colnames(hill_spdf) <- c("value", "x", "y")

MUK2018_1010_22line <- geojsonsf::geojson_sf("gis/MUK2018_1010_2-2_line.geojson") %>%
  sf::st_transform(crs = 32733)

MUK2018_1010_22poly <- geojsonsf::geojson_sf("gis/MUK2018_1010_2-2_poly.geojson") %>%
  sf::st_transform(crs = 32733)

MUK2018_1010_22dotted <- geojsonsf::geojson_sf("gis/MUK2018_1010_2-2_dotted.geojson")
sf::st_crs(MUK2018_1010_22dotted) = 32733

MUK2018_1010_22triangles <- geojsonsf::geojson_sf("gis/MUK2018_1010_2-2_triangles.geojson")
sf::st_crs(MUK2018_1010_22triangles) = 32733

MUK1952_Trenches <- geojsonsf::geojson_sf("gis/MUK1952_trenches.geojson")
sf::st_crs(MUK1952_Trenches) = 32733

MUK2018_Trenches <- geojsonsf::geojson_sf("gis/MUK2018_excav.geojson") %>%
  sf::st_transform(crs = 32733)



fig.sfm <- ggplot() + 
  geom_tile(data = hill_spdf, aes(x = x, y = y, fill = value), show_guide = FALSE) + 
  scale_fill_gradient(low = "black", high = "white") + 
  new_scale_fill() + 
  geom_tile(data = dem_spdf, aes(x = x, y = y, fill = value), alpha = 0.4) + 
  scale_fill_gradientn(colours = rainbow(5)) +
  #scale_fill_viridis_c("m", option = "inferno") + 
  #scale_fill_gradientn("m", colours = terrain.colors(10)) + 
  scale_x_continuous("", expand = c(0, 0)) + 
  scale_y_continuous("", expand = c(0, 0)) + 
  coord_sf(xlim = c(720156, 720169), 
           ylim = c(9445238, 9445252)) + 
  theme_bw() + 
  theme(legend.justification = c(0, 0), legend.position = c(0.02, 0.02))
ggsave("output/Fig_MUK2018-1010-5_alt.jpg", width = 11/1.5, height = 11/1.5)


fig.A <- ggplot() + 
  geom_tile(data = hill_spdf, aes(x = x, y = y, fill = value), show_guide = FALSE) + 
  scale_fill_gradient(low = "black", high = "white") + 
  new_scale_fill() + 
  geom_tile(data = dem_spdf, aes(x = x, y = y, fill = value), alpha = 0.4) + 
  scale_fill_gradientn("m", colours = terrain.colors(10)) + 
  geom_sf(data = MUK2018_Trenches, fill = '#ecb361', color = "white", alpha = .75, linetype = "dotdash", linewidth = .5) + 
  geom_sf(data = MUK2018_1010_22line, color = "white", alpha = .5, linewidth = .5) + 
  geom_sf(data = MUK2018_1010_22dotted, color = "white", alpha = .5, linetype = "dashed", linewidth = .5) + 
  geom_sf(data = MUK2018_1010_22triangles, fill = 'white', color = NA, alpha = .5, linewidth = .5) + 
  geom_sf(data = MUK2018_1010_22poly, fill = NA, color = "white", alpha = .5, linewidth = .5) + 
  geom_sf(data = MUK1952_Trenches, fill = NA, color = "white", linetype = "dashed", linewidth = .5) + 
  
  shadowtext::geom_shadowtext(aes(x = 720163.2, y = 9445245.3), label = "Charcoal kiln", fontface = "bold") + 
  shadowtext::geom_shadowtext(aes(x = 720158.75, y = 9445244.75), label = "Ridge", hjust = 0) + 
  shadowtext::geom_shadowtext(aes(x = 720165.85, y = 9445245.5), label = "Ridge", hjust = 0) + 
  shadowtext::geom_shadowtext(aes(x = 720163.5, y = 9445248), label = "Ridge", angle = 87, hjust = 0) + 
  shadowtext::geom_shadowtext(aes(x = 720164.5, y = 9445242), label = "Ridge", angle = 90, hjust = 0) + 
  
  shadowtext::geom_shadowtext(aes(x = 720158.9528, y = 9445240.5), label = "Putative extension", fontface = "bold", hjust = 0) + 
  shadowtext::geom_shadowtext(aes(x = 720158.9528, y = 9445240), label = "Gite II A", fontface = "bold.italic", hjust = 0) + 
  
  shadowtext::geom_shadowtext(aes(x = 720165.487, y = 9445239.791), label = "MUK 2018/1010/5", fontface = "bold", hjust = 0) + 
  
  scale_x_continuous("", expand = c(0, 0)) + 
  scale_y_continuous("", expand = c(0, 0)) + 
  coord_sf(xlim = c(720156, 720169), 
           ylim = c(9445238, 9445252)) + 
  theme_bw() + 
  theme(legend.justification = c(0, 0), legend.position = c(0.02, 0.02))
ggsave("output/Fig_MUK2018-1010-5.jpg", fig.A, width = 11/1.5, height = 11/1.5)
ggsave("output/Fig_MUK2018-1010-5.pdf", fig.A, width = 11/1.5, height = 11/1.5)