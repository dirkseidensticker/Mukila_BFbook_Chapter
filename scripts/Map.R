# Fig 1: Maps

library(cowplot)
library(dplyr)
library(geojsonsf)
library(ggplot2)
library(ggrepel)
library(ggnewscale)
library(ggsflabel)
library(ggsn)
library(ggthemes)
library(metR)
library(raster)
library(rgdal)
library(rnaturalearth)
library(sf)
library(tidyr)

# Panel A: Map of the study area ----

# sites & cities on the map ----
sites <- rbind(
  data.frame(NAME = c("Mukila"),
             TYPE = "ArchSite",
             X = c(16.9862),
             Y = c(-5.0156)), 
  data.frame(NAME = c("Mashita Mbanza","Dinga Kitu", "Wombali"),
             TYPE = "ArchSite2",
             X = c(18.89, 16.54, 17.369777),
             Y = c(-5.65, -5.4, -3.275516))) %>% 
  st_as_sf(coords = c("X", "Y"), 
           crs = 4326, remove = F)

places <- geojsonsf::geojson_sf("gis/naturalearth/ne_10m_populated_places_simple.geojson") %>%
  #rnaturalearth::ne_download(scale = 10, type = "populated_places", category = "cultural", returnclass="sf") %>%
  dplyr::filter(name %in% c("Kinshasa", "Brazzaville", 
                            "Kenge", "Kikwit", "Bandundu", 
                            "Pointe Noire", "Boma")) %>%
  dplyr::select(name) %>%
  dplyr::mutate(NAME = name, 
                TYPE = "PopPlace") %>%
  dplyr::select(-name)

places <- cbind(places, 
                st_coordinates(places))


sites <- rbind(sites, places)

# other geodata ----
osm.rivers.line <- geojsonsf::geojson_sf("gis/osm_rivers_line.geojson")
st_crs(osm.rivers.line) = 4326

osm.rivers.poly <- geojsonsf::geojson_sf("gis/osm_rivers_poly.geojson")
st_crs(osm.rivers.poly) = 4326

#ocean10 <- rnaturalearth::ne_download(scale = 10, type = "ocean", category = "physical", returnclass="sf")
ocean10 <- geojsonsf::geojson_sf("gis/naturalearth/ne_10m_ocean.geojson")
#coast10 <- rnaturalearth::ne_download(scale = 10, type = "coastline", category = "physical", returnclass="sf")
coast10 <- geojsonsf::geojson_sf("gis/naturalearth/ne_10m_ocean.geojson")
#boundary_lines_land10 <- rnaturalearth::ne_download(scale = 10, type = "boundary_lines_land", category = "cultural", returnclass="sf")
boundary_lines_land10 <- geojsonsf::geojson_sf("gis/naturalearth/ne_10m_admin_0_boundary_lines_land.geojson")
#roads10 <- rnaturalearth::ne_download(scale = 10, type = "roads", category = "cultural", returnclass="sf")
roads10 <- geojsonsf::geojson_sf("gis/naturalearth/ne_10m_roads.geojson") %>%
  sf::st_crop(xmin = 10, xmax = 25, ymin = 0, ymax = -10)

# rainforest ----
white <- sf::st_read("gis/whitesveg/Whites vegetation.shp") %>% 
  dplyr::filter(DESCRIPTIO %in% c("Anthropic landscapes",
                                  "Dry forest and thicket",
                                  "Swamp forest and mangrove",
                                  "Tropical lowland rainforest"))
st_crs(white) = 4326


## landcover data from "Global Land Cover 2000 Project (GLC 2000)" https://ec.europa.eu/jrc/en/scientific-tool/global-land-cover
#temp <- tempfile()
#download.file("https://forobs.jrc.ec.europa.eu/data/products/glc2000/Africa_v5_Grid.zip",temp)
#unzip(temp)
#unlink(temp)
#dpath <- "Grid/africa_v5/hdr.adf"
#x <- new("GDALReadOnlyDataset", dpath)
#getDriver(x)
#getDriverLongName(getDriver(x))
#hdr <- asSGDF_GROD(x)
#hdr <- raster(hdr)
## extent: xmin,xmax,ymin,ymax
#e  <- extent(13, 23, -7, -2) 
#rfs <- crop(hdr, e) 
#rfs.p <- rasterToPoints(rfs)
#rfs.df <- data.frame(rfs.p)


# geological basemap ----
bbox = c(xmin = 11, xmax = 23,
         ymin = -7, ymax = -2)

geo7 <- st_read("gis/geology/geo7_2ag.shp")
geo7.1 <- st_crop(geo7, bbox)

geo7.labs <- read.csv("gis/geology/glg.csv")

geo7.1 <- dplyr::left_join(geo7.1, geo7.labs)

glg.units <- data.frame(GLG = unique(geo7.1$GLG))
glg.units <- dplyr::left_join(glg.units, geo7.labs)
glg.units <- glg.units[order(glg.units$Label),]

# dem ----
library(elevatr)
library(raster)
locations1 <- data.frame(X1 = c(bbox[[1]], bbox[[2]]), 
                        X2 = c(bbox[[4]], bbox[[3]]))

dem1 <- elevatr::get_elev_raster(locations = locations1, 
                                prj = "EPSG:4326", #sf::st_crs(4326), 
                                z = 7,  
                                clip = "bbox")

plot(dem1)

# create slope and hillshade
slope1 = raster::terrain(dem1, opt='slope')
aspect1 = raster::terrain(dem1, opt='aspect')
hill1 = raster::hillShade(slope1, aspect1, 40, 270)

plot(hill1)

dem1_spdf <- as(dem1, "SpatialPixelsDataFrame")
dem1_spdf <- as.data.frame(dem1_spdf)
colnames(dem1_spdf) <- c("value", "x", "y")

dem1_spdf$z.class <- cut(dem1_spdf$value, c(-Inf, 330, 560, 790, Inf))

dem1_spdf <- cbind(
  dem1_spdf, 
  z = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", dem1_spdf$z.class)))

hill1_spdf <- as(hill1, "SpatialPixelsDataFrame")
hill1_spdf <- as.data.frame(hill1_spdf)
colnames(hill1_spdf) <- c("value", "x", "y")


# multiply dem & hill ----
# cf https://ieqgis.com/2015/04/04/create-great-looking-topographic-maps-in-qgis-2/
# & https://en.wikipedia.org/wiki/Blend_modes#Multiply

unique(dem1_spdf$z.class)

dem1_spdf.rgb <- dem1_spdf %>%
  dplyr::mutate(
    R = ifelse(z.class == "(-Inf,330]", col2rgb("#54e596")[1] / 255,
               ifelse(z.class == "(330,560]", col2rgb("#cefe90")[1] / 255, 
                      ifelse(z.class == "(560,790]", col2rgb("#fdee91")[1] / 255, 
                             ifelse(z.class == "(790, Inf]", col2rgb("#f3c589")[1] / 255,
                                    NA)))), 
    G = ifelse(z.class == "(-Inf,330]", col2rgb("#54e596")[2] / 255,
               ifelse(z.class == "(330,560]", col2rgb("#cefe90")[2] / 255, 
                      ifelse(z.class == "(560,790]", col2rgb("#fdee91")[2] / 255, 
                             ifelse(z.class == "(790, Inf]", col2rgb("#f3c589")[2] / 255,
                                    NA)))),
    B = ifelse(z.class == "(-Inf,330]", col2rgb("#54e596")[3] / 255,
               ifelse(z.class == "(330,560]", col2rgb("#cefe90")[3] / 255, 
                      ifelse(z.class == "(560,790]", col2rgb("#fdee91")[3] / 255, 
                             ifelse(z.class == "(790, Inf]", col2rgb("#f3c589")[3] / 255,
                                    NA)))))
#dem_spdf.rgb$col <- rgb(dem_spdf.rgb$R, dem_spdf.rgb$G, dem_spdf.rgb$B, maxColorValue = 255)
#ggplot() + 
#  geom_tile(data = dem_spdf.rgb, aes(x = x, y = y, fill = col)) + 
#  scale_fill_identity()

hill1_spdf.rgb <- hill1_spdf %>%
  dplyr::mutate(
    R = (hill1_spdf$value - min(hill1_spdf$value)) / (max(hill1_spdf$value) - min(hill1_spdf$value)) * 1.25,
    G = (hill1_spdf$value - min(hill1_spdf$value)) / (max(hill1_spdf$value) - min(hill1_spdf$value)) * 1.25,
    B = (hill1_spdf$value - min(hill1_spdf$value)) / (max(hill1_spdf$value) - min(hill1_spdf$value)) * 1.25)

hill1_spdf.rgb$R[hill1_spdf.rgb$R > 1] <- 1 
hill1_spdf.rgb$G[hill1_spdf.rgb$G > 1] <- 1 
hill1_spdf.rgb$B[hill1_spdf.rgb$B > 1] <- 1 

#hill_spdf.rgb$col <- rgb(hill_spdf.rgb$R * 255, 
#                         hill_spdf.rgb$G * 255, 
#                         hill_spdf.rgb$B * 255, 
#                         maxColorValue = 255)
#ggplot() + 
#  geom_tile(data = hill_spdf.rgb, aes(x = x, y = y, fill = col)) + 
#  scale_fill_identity()

dem1.hill.rgb_spdf <- dem1_spdf.rgb %>%
  dplyr::left_join(hill1_spdf.rgb, by = c("x", "y")) %>%
  dplyr::mutate(R = R.x * R.y,
                G = G.x * G.y,
                B = B.x * B.y) %>%
  dplyr::filter(!is.na(R))

dem1.hill.rgb_spdf$col <- rgb(dem1.hill.rgb_spdf$R * 255, 
                              dem1.hill.rgb_spdf$G * 255, 
                              dem1.hill.rgb_spdf$B * 255, 
                              maxColorValue = 255)






# minimap ----

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

minimap <- ggplot(data = world) +
  geom_sf(color = NA, fill = "grey") + 
  geom_rect(xmin = 11.5, xmax = 22.5, 
            ymin = -6, ymax = -3, 
            fill = NA, color = "black") + 
  coord_sf(xlim = c(-15, 50), 
           ylim = c(-35, 35)) + 
  theme_void() + 
  theme(panel.border = element_rect(colour = "darkgrey", 
                                    fill = NA, size = .5), 
        panel.background = element_rect(fill = "white"))

# MAP ----
pltA.map <- ggplot() +
  geom_tile(data = dem1.hill.rgb_spdf, aes(x = x, y = y, fill = col)) + 
  scale_fill_identity() + 
  #geom_tile(data = hill1_spdf, aes(x = x, y = y, fill = value)) +
  #scale_fill_gradient(low = "black", high = "white") + 
  #new_scale_fill() +
  #geom_tile(data = dem1_spdf, aes(x = x, y = y, fill = as.character(z)), alpha = .3) +
  #scale_fill_manual(values = c("#54e596", "#cefe90", "#fdee91", "#f3c589")) + 
  
  #geom_tile(data = dem1_spdf, aes(x = x, y = y, fill = as.character(z))) +
  #scale_fill_manual(values = c("#54e596", "#cefe90", "#fdee91", "#f3c589")) + 
  #new_scale_fill() +
  #geom_tile(data = hill1_spdf, aes(x = x, y = y, fill = value), alpha = .3) +
  #scale_fill_gradient(low = "black", high = "white") + 
  new_scale_fill() +
  geom_sf(data = ocean10, fill = "#dff1f9", color = NA) + 
  geom_sf(data = coast10, size = .5, color = '#44afe3', fill = NA) + 
  geom_sf(data = osm.rivers.poly, color = '#44afe3', fill = '#44afe3') + 
  geom_sf(data = osm.rivers.line, size = .3, color = '#44afe3', fill = NA) + 
  geom_sf(data = roads10, linewidth = 1, col = "black") + 
  geom_sf(data = roads10, linewidth = .5, col = "white") + 
  geom_sf(data = boundary_lines_land10, linetype = "dashed", color = "white", fill = NA) + 
  geom_text_repel(data = sites, 
                  aes(x = X, y = Y, 
                      label = NAME, 
                      size = TYPE, color = TYPE), 
                  min.segment.length = 0, fontface = "bold", 
                  bg.r = .15, bg.color = "white", 
                  max.overlaps = Inf) + 
  geom_sf(data = sites, aes(size = TYPE, color = TYPE)) + 
  
  shadowtext::geom_shadowtext(aes(x = 15.9, y = -3.7, label = "CONGO"), fontface  = "bold", color = "#44afe3", bg.colour = "white", size = 1.5, angle = 60) + 
  shadowtext::geom_shadowtext(aes(x = 19.5, y = -3.9, label = "KASAÏ"), fontface  = "bold", color = "#44afe3", bg.colour = "white", size = 1.5, angle = -20) + 
  shadowtext::geom_shadowtext(aes(x = 16.4, y = -5, label = "KWANGO"), fontface  = "bold", color = "#44afe3", bg.colour = "white", size = 1.5, angle = -90) + 
  shadowtext::geom_shadowtext(aes(x = 17.2, y = -5.75, label = "WAMBA"), fontface  = "bold", color = "#44afe3", bg.colour = "white", size = 1.5, angle = -70) + 
  shadowtext::geom_shadowtext(aes(x = 18.25, y = -3.9, label = "KWILU"), fontface  = "bold", color = "#44afe3", bg.colour = "white", size = 1.5, angle = -45) + 
  
  #scale_fill_manual(values = c("black", "grey", "white")) + 
  scale_color_manual(values = c("black", "black", "#6F7378")) + 
  scale_size_manual(values = c(2.5, 2.5, 2)) + 
  scale_x_continuous(breaks = seq(11, 23, 1)) + 
  scale_y_continuous(breaks = seq(-2, -7, -1)) + 
  ggsn::scalebar(sites,
                 location  = "topright",
                 anchor = c(x = 22, y = -5.85),
                 dist = 100, dist_unit = "km",
                 transform = TRUE, model = "WGS84", 
                 height = .025, st.dist = .05, 
                 border.size = .1, st.size = 3) + 
  coord_sf(xlim = c(12, 22),
           ylim = c(-6, -3)) +
  theme_few() + 
  theme(legend.position = "none", 
        axis.title = element_blank())
  
pltA <- cowplot::ggdraw() +
  draw_plot(pltA.map) +
  draw_plot(minimap, 
            x = .78, y = .6, width = .3, height = .3)

# standalone version
minimap <- ggplot(data = world) +
  geom_sf(color = "white", fill = "grey") + 
  geom_rect(xmin = 12, xmax = 19, 
            ymin = -6, ymax = -3, 
            fill = NA, color = "black") + 
  coord_sf(xlim = c(-15, 50), 
           ylim = c(-35, 35)) + 
  theme_void() + 
  theme(panel.border = element_rect(colour = "black", 
                                    fill = NA, size = .5), 
        panel.background = element_rect(fill = "white"))

pltA.map <- ggplot() +
  geom_tile(data = dem1.hill.rgb_spdf, aes(x = x, y = y, fill = col)) + 
  scale_fill_identity() + 
  #geom_tile(data = hill1_spdf, aes(x = x, y = y, fill = value)) +
  #scale_fill_gradient(low = "black", high = "white") + 
  #new_scale_fill() +
  #geom_tile(data = dem1_spdf, aes(x = x, y = y, fill = as.character(z)), alpha = .3) +
  #scale_fill_manual(values = c("#54e596", "#cefe90", "#fdee91", "#f3c589")) + 
  
  #geom_tile(data = dem1_spdf, aes(x = x, y = y, fill = as.character(z))) +
  #scale_fill_manual(values = c("#54e596", "#cefe90", "#fdee91", "#f3c589")) + 
  #new_scale_fill() +
  #geom_tile(data = hill1_spdf, aes(x = x, y = y, fill = value), alpha = .3) +
  #scale_fill_gradient(low = "black", high = "white") + 
  new_scale_fill() +
  geom_sf(data = ocean10, fill = "#dff1f9", color = NA) + 
  geom_sf(data = coast10, size = .5, color = '#44afe3', fill = NA) + 
  geom_sf(data = osm.rivers.poly, color = '#44afe3', fill = '#44afe3') + 
  geom_sf(data = osm.rivers.line, size = .3, color = '#44afe3', fill = NA) + 
  geom_sf(data = roads10, linewidth = 1, col = "black") + 
  geom_sf(data = roads10, linewidth = .5, col = "white") + 
  geom_sf(data = boundary_lines_land10, linetype = "dashed", color = "black", fill = NA) + 

  geom_sf(data = sites %>% 
            #dplyr::arrange(TYPE) %>%
            dplyr::mutate(TYPE = factor(TYPE, levels = rev(unique(TYPE)))), 
          aes(size = TYPE, fill = TYPE), 
          shape = 21, color = "white") + 
  geom_text_repel(data = sites, 
                  aes(x = X, y = Y, 
                      label = NAME, 
                      size = TYPE, color = TYPE), 
                  segment.color = NA, fontface = "bold", 
                  bg.r = .15, bg.color = "white", 
                  max.overlaps = Inf) + 
  
  shadowtext::geom_shadowtext(aes(x = 15.9, y = -3.7, label = "CONGO"), fontface  = "bold", color = "#44afe3", bg.colour = "white", size = 1.5, angle = 60) + 
  #shadowtext::geom_shadowtext(aes(x = 19.5, y = -3.9, label = "KASAÏ"), fontface  = "bold", color = "#44afe3", bg.colour = "white", size = 1.5, angle = -20) + 
  #shadowtext::geom_shadowtext(aes(x = 16.4, y = -5, label = "KWANGO"), fontface  = "bold", color = "#44afe3", bg.colour = "white", size = 1.5, angle = -90) + 
  #shadowtext::geom_shadowtext(aes(x = 17.2, y = -5.75, label = "WAMBA"), fontface  = "bold", color = "#44afe3", bg.colour = "white", size = 1.5, angle = -70) + 
  #shadowtext::geom_shadowtext(aes(x = 18.25, y = -3.9, label = "KWILU"), fontface  = "bold", color = "#44afe3", bg.colour = "white", size = 1.5, angle = -45) + 
  
  #scale_fill_manual(values = c("black", "grey", "white")) + 
  scale_fill_manual(values = c("#6F7378", "black", "black")) + 
  scale_color_manual(values = c("black", "black", "#6F7378")) + 
  scale_size_manual(values = c(2, 2, 3)) + 
  scale_x_continuous(breaks = seq(11, 23, 1)) + 
  scale_y_continuous(breaks = seq(-2, -7, -1)) + 
  ggsn::north(sites, symbol = 4,
              location = "topright", anchor = c(x = 19.75, y = -5.5)) + 
  ggsn::scalebar(sites,
                 location  = "topright",
                 anchor = c(x = 19.5, y = -5.95),
                 dist = 100, dist_unit = "km",
                 transform = TRUE, model = "WGS84", 
                 height = .025, st.dist = .025, 
                 border.size = .1, st.size = 3) + 
  coord_sf(xlim = c(12.5, 19.5),
           ylim = c(-6, -3)) +
  theme_few() + 
  theme(legend.position = "none", 
        axis.title = element_blank()) #, 
        #axis.text = element_text(size = 5))

pltA <- cowplot::ggdraw() +
  draw_plot(pltA.map) +
  draw_plot(minimap, 
            x = .77, y = .65, width = .3, height = .3)
ggsave("output/Fig_Map_1_Overview.jpg", pltA, width = 8, height = 3.75, bg = "white")
ggsave("output/Fig_Map_1_Overview.pdf", pltA, width = 8, height = 3.75, bg = "white")


# Panel B: Site of Mukila & surrounding hills ----

locations2 <- data.frame(X1 = c(16.965, 17), 
                        X2 = c(-5.03, -5))

dem2 <- elevatr::get_elev_raster(locations = locations2, 
                                prj = "EPSG:4326", #sf::st_crs(4326), 
                                z = 14,  # max z = 14 (~250MB)
                                clip = "bbox")

plot(dem2)

# create slope and hillshade
slope2 = raster::terrain(dem2, opt='slope')
aspect2 = raster::terrain(dem2, opt='aspect')
hill2 = raster::hillShade(slope2, aspect2, 40, 270)

plot(hill2)

hill2_spdf <- as(hill2, "SpatialPixelsDataFrame")
hill2_spdf <- as.data.frame(hill2_spdf)
colnames(hill2_spdf) <- c("value", "x", "y")

dem2_spdf <- as(dem2, "SpatialPixelsDataFrame")
dem2_spdf <- as.data.frame(dem2_spdf)
colnames(dem2_spdf) <- c("value", "x", "y")

sites2 <- data.frame(Site = c("Mukila","Mukambo"),
                      X = c(16.985, 17),
                      Y = c(-5.0156,-5.04)) %>% 
  st_as_sf(coords = c("X", "Y"), 
           crs = 4326, remove = F)

# TODO: add geom_rect with bb box from panel C (xlim = c(720245, 720280), ylim = c(9445245, 9445280)) ----
# TODO: add label for rect

rect.panelC <- sf::st_polygon(
  list(
    cbind(
      c(720146.5,720281.5,720281.5,720146.5,720146.5),
      c(9445229.6,9445229.6,9445317.6,9445317.6,9445229.6)))) %>%
  sf::st_sfc()
st_crs(rect.panelC) = 32733
rect.panelC <- st_transform(rect.panelC, 4326)

pltB <- ggplot() + 
  geom_tile(data = hill2_spdf, aes(x = x, y = y, fill = value)) +
  scale_fill_gradient(low = "black", high = "white") + 
  new_scale_fill() +
  geom_tile(data = dplyr::filter(dem2_spdf, value > 0), aes(x = x, y = y, fill = value), alpha = .3) + 
  scale_fill_gradientn(colours = terrain.colors(10)) + 
  geom_contour(data = dem2_spdf, aes(x = x, y = y, z = value), breaks = seq(380, 500, 10), colour = "white") + 
  geom_text_contour(data = dem2_spdf, aes(x = x, y = y, z = value), breaks = seq(390, 500, 10), colour = "white") + 
  geom_sf(data = osm.rivers.poly, color = '#44afe3', fill = '#44afe3') + 
  geom_sf(data = osm.rivers.line, size = .5, color = '#44afe3') + 
  geom_sf(data = rect.panelC) + 
  #geom_sf(data = sites2) + 
  shadowtext::geom_shadowtext(data = sites2, 
                  aes(x = X-.0003, y = Y, label = Site), 
                  #color = "white", fill = "black", 
                  color = "black", bg.colour = "white",
                  fontface = "bold") + 
  annotate("text", x = 16.97825, y = -5.007, label = "Wamba River", angle = 70, color = "white", fontface = "bold") + 
  #annotate("text", x = 16.988, y = -5.0157, label = "Panel C") + 
  #geom_label_repel(data = sites2, 
  #                 aes(x = X, y = Y, label = Site), 
  #                 min.segment.length = 0) + 
  ggsn::north(sites2, symbol = 4, scale = .075,
              location = "topright", anchor = c(x = 16.987, y = -5.024)) + 
  ggsn::scalebar(sites2,
                 location  = "topright",
                 anchor = c(x = 16.9925, y = -5.025),
                 dist = 250, dist_unit = "m",
                 transform = TRUE, model = "WGS84", 
                 height = .01, st.dist = .02, 
                 border.size = .1, st.size = 4) + 
  coord_sf(xlim = c(16.97, 16.9925), 
           ylim = c(-5.025, -5.005)) +
  theme_few() + 
  theme(legend.position = "none", 
        axis.title = element_blank())
ggsave("output/Fig_Map_2_Region.jpg", pltB, width = 9.5, height = 8)
ggsave("output/Fig_Map_2_Region.pdf", pltB, width = 9.5, height = 8)

# Panel C: Site plan of Mukila ----

MUK_buildings <- geojsonsf::geojson_sf("gis/MUK2018_buildings.geojson")
st_crs(MUK_buildings) = 4326
MUK_buildings <- st_transform(MUK_buildings, 32733)

MUK1952_Trenches <- geojsonsf::geojson_sf("gis/MUK1952_trenches.geojson")
st_crs(MUK1952_Trenches) = 32733

MUK2018_Trenches <- geojsonsf::geojson_sf("gis/MUK2018_excav.geojson")
st_crs(MUK2018_Trenches) = 4326
MUK2018_Trenches <- st_transform(MUK2018_Trenches, 32733)

MUK_2018_Niv <- read.csv("gis/MUK2018_Nivellements.csv")

MUK_2018_SChool_Niv <- MUK_2018_Niv %>% dplyr::filter(X > 720200)

MUK_2018_Church_Niv <- MUK_2018_Niv %>% dplyr::filter(X < 720200)

NivKrig <- function(MUK_2018_Niv){
  # Interpolating Nivellement
  # see:
  # - https://rpubs.com/nabilabd/118172
  # - https://rstudio-pubs-static.s3.amazonaws.com/46259_d328295794034414944deea60552a942.html
  
  MUK_2018_Niv_sf <- sf::st_as_sf(MUK_2018_Niv, 
                                  coords = c("X", "Y"), 
                                  crs = 32733)
  
  
  library(sp)
  library(gstat)
  library(metR)
  coordinates(MUK_2018_Niv) <- ~ X + Y
  MUK_2018_Niv.bbox <- bbox(MUK_2018_Niv)
  
  # grid to interpolate to
  grd <- expand.grid(x = seq(from = MUK_2018_Niv.bbox[1] - 3, 
                             to = MUK_2018_Niv.bbox[3] + 3, 
                             by = 1), 
                     y = seq(from = MUK_2018_Niv.bbox[2] - 3, 
                             to = MUK_2018_Niv.bbox[4] + 3, 
                             by = 1))
  coordinates(grd) <- ~ x+y
  grd.bbox <- bbox(grd)
  
  #plot(grd)
  #plot(MUK_2018_Niv, add = T, col = "red")
  grd <- st_as_sf(grd)
  st_crs(grd) = 32733
  
  # see https://swilke-geoscience.net/post/spatial_interpolation/
  library(automap)
  fit_KRIG <- automap::autoKrige(
    formula = z ~ 1,
    input_data = as(MUK_2018_Niv_sf, "Spatial"),
    new_data = as(grd, "Spatial")
  )
  
  plot(fit_KRIG, 
       sp.layout = list(pts = list("sp.points", MUK_2018_Niv)))
  
  fit_KRIG <- fit_KRIG %>% 
    .$krige_output %>%
    as.data.frame() %>%
    dplyr::select(X = coords.x1, Y = coords.x2, Z = var1.pred)
  
  
  #  coord_sf(xlim = c(720245, 720280),
  # ylim = c(9445245, 9445280)) + 
  
  # # manual way:
  # # setup model
  # variog <- gstat::variogram(z~1, MUK_2018_Niv)
  # model.variog <- gstat::fit.variogram(variog, model = vgm(psill = .05, 
  #                                                          model = "Exp", 
  #                                                          nugget = .1, 
  #                                                          range = 4))
  # 
  # plot(variog, model.variog)
  # 
  # # kriging
  # krig <- gstat::krige(formula = z ~ 1, 
  #                      locations = MUK_2018_Niv, 
  #                      newdata = grd, 
  #                      model = model.variog)
  # 
  # krig.output = as.data.frame(krig)
  
  # plotting
  
  summary(fit_KRIG)
  
  return(fit_KRIG)
}


MUK_2018_SChool_Niv_KRIG <- NivKrig(MUK_2018_SChool_Niv %>% dplyr::filter(!is.na(z)))

MUK_2018_Church_Niv_KRIG <- NivKrig(MUK_2018_Church_Niv %>% dplyr::filter(!is.na(z)))

MUK2018_Trenches %>%
  dplyr::distinct(square, geometry)

MUK2018.corings <- MUK_2018_Niv %>%
  sf::st_as_sf(coords = c("X", "Y"), 
               crs = 32733, 
               remove = F) %>%
  dplyr::filter(grepl("/", id))


# DEM
bbox.panelC <- sf::st_bbox(rect.panelC)

locations3 <- data.frame(X1 = c(bbox.panelC[[1]]-.001, bbox.panelC[[3]]+.001), 
                         X2 = c(bbox.panelC[[2]]-.001, bbox.panelC[[4]]+.001))

dem3 <- elevatr::get_elev_raster(locations = locations3, 
                                 prj = "EPSG:4326", #sf::st_crs(4326), 
                                 z = 13,  # max z = 14 (~250MB)
                                 clip = "bbox")

plot(dem3)

dem3 <- raster::projectRaster(dem3, crs = "EPSG:32733")

dem3_spdf <- as(dem3, "SpatialPixelsDataFrame")
dem3_spdf <- as.data.frame(dem3_spdf)
colnames(dem3_spdf) <- c("value", "X", "Y")


pltC <- ggplot() + 
  #geom_raster(data = fit_KRIG, aes(x = X, y = Y, fill = Z)) +
  #scale_fill_gradientn(colours = terrain.colors(10)) + 
  geom_contour(data = dem3_spdf, 
               aes(x = X, 
                   y = Y, 
                   z = value), 
               breaks = 477.5,
               colour = "lightgrey", 
               linetype = "dotted") + 
  geom_text_contour(data = dem3_spdf, 
                    aes(x = X, 
                        y = Y, 
                        z = value), 
                    breaks = 477.5, 
                    colour = "lightgrey") + 
  geom_contour(data = MUK_2018_SChool_Niv_KRIG, 
               aes(x = X, y = Y, z = Z), colour = "grey", linetype = "dashed", breaks = seq(-3, 4, .25)) + 
  geom_text_contour(data = MUK_2018_SChool_Niv_KRIG, 
                    aes(x = X, y = Y, z = Z), color = "darkgrey", breaks = seq(-3, 4, .2), skip = 0, label.placer = label_placer_fraction(frac = 0.75)) + 
  geom_contour(data = MUK_2018_Church_Niv_KRIG, 
               aes(x = X, y = Y, z = Z), colour = "grey", linetype = "dashed", breaks = seq(-3, 4, .25)) + 
  geom_text_contour(data = MUK_2018_Church_Niv_KRIG, 
                    aes(x = X, y = Y, z = Z), color = "darkgrey", breaks = seq(-3, 4, .2), skip = 0, label.placer = label_placer_fraction(frac = .2)) + 
  geom_sf(data = MUK_buildings) + 
  geom_sf(data = MUK1952_Trenches, fill = "#f5f5f5", linetype = "dashed") + 
  #geom_sf(data = dplyr::filter(MUK1952_Trenches, code == "M"), fill = "#f8766d") + 
  geom_sf(data = MUK2018_Trenches %>%
            dplyr::distinct(square, geometry), 
          fill = "#f8766d") +

  
  #geom_sf(data = MUK2018_Trenches %>% 
  #          dplyr::filter(grepl("1", square)) %>%
  #          dplyr::distinct(square, geometry), 
  #        fill = "#f8766d") + 
  
  geom_sf(data = MUK2018.corings) +
  geom_text_repel(data = MUK2018.corings, 
                  aes(x = X, y = Y, label = id), 
                  size = 3) + 
  geom_point(aes(x = 720170.2826, y = 9445256.1833)) + 
  annotate("text", x = 720170, y = 9445255, label = "Local reference point", hjust = 0) + 
  
  annotate("text", x = 720176.5, y = 9445280, label = "Church", fontface = "bold") + 
  annotate("text", x = 720270, y = 9445275, label = "School", fontface = "bold") + 

  annotate("text", x = 720170.5, y = 9445238.5, label = "MUK 2018/1010", fontface = "bold") + 
  annotate("text", x = 720160, y = 9445300, label = "MUK 2018/1020") + 
  annotate("text", x = 720269, y = 9445263, label = "MUK 2018/1030/10", hjust = 0, fontface = "bold") + 

  annotate("text", x = 720163.12, y = 9445245.2, label = "Gite II A", fontface = "italic") + 
  annotate("text", x = 720263, y = 9445250, label = "Gite II B", fontface = "italic") + 

  #geom_sf_text(data = MUK1952_Trenches %>% 
  #               dplyr::filter(code %in% c(LETTERS[3:14], "N'")), aes(label = code)) + 
  #geom_sf_text(data = MUK2018_Trenches %>% dplyr::distinct(square, geometry), aes(label = square), size = 3)

  geom_segment(aes(x = 720270, xend = 720280, y = 9445232, yend = 9445232), size = 1) + 
  geom_text(aes(x = 720275, y = 9445234, label = "10 m")) + 
  
  ggsn::north(MUK2018.corings, symbol = 4,
              location = "topright", anchor = c(x = 720290, y = 9445238)) + 
  
  #ggsn::scalebar(fit_KRIG,
  #               location  = "topright",
  #               anchor = c(x = 720280.0, y = 9445244.5),
  #               dist = 5, dist_unit = "m",
  #               transform = FALSE, #model = "WGS84", 
  #               height = .001, st.dist = .02, 
  #               border.size = .1, st.size = 3) + 
  coord_sf(xlim = c(720146.5, 720290),
           ylim = c(9445229.6, 9445317.6)) + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) + 
  theme_few() + 
  theme(axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank())
ggsave("output/Fig_Map_3_Site.jpg", pltC, width = 12, height = 7.5)
ggsave("output/Fig_Map_3_Site.pdf", pltC, width = 12, height = 7.5)


# z value for corings at the church:
plot(raster::rasterFromXYZ(MUK_2018_Church_Niv_KRIG))
plot(MUK2018.corings["id"], add = T, color = "black")

cbind(MUK2018.corings, 
      raster::extract(raster::rasterFromXYZ(MUK_2018_Church_Niv_KRIG),
                      MUK2018.corings))

# z value for corings at the school:
plot(raster::rasterFromXYZ(MUK_2018_SChool_Niv_KRIG))
plot(MUK2018.corings["id"], add = T, color = "black")

cbind(MUK2018.corings, 
      raster::extract(raster::rasterFromXYZ(MUK_2018_SChool_Niv_KRIG),
                      MUK2018.corings)) %>%
  View()
