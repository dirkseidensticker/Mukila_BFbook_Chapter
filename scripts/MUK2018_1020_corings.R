# Corings

library(ggplot2)
library(tidyr)

library(munsell)

# TODO:
# [ ] adjust Top according to nivellment
# [ ] jitter finds x-axis



c.soil <- data.table::fread("input/MUK2018_Corings_Soil.csv")
c.soil$col <- munsell::mnsl(c.soil$MUNSELL)

c.finds <- data.table::fread("input/MUK2018_Corings_Finds.csv")

c.loc <- data.table::fread("input/MUK2018_Corings_Locations.csv")


c.soil %>%
  dplyr::left_join(c.loc %>% dplyr::select(CORE, Z, SITE), by = "CORE") %>%
  #dplyr::filter(SITE == "Church") %>%
  dplyr::mutate(T.z = Z*100 - T, 
                B.z = Z*100 - B) %>%
  ggplot() + 
  geom_point(data = c.finds %>% 
               dplyr::left_join(c.loc %>% dplyr::select(CORE, Z, SITE), by = "CORE") %>%
               #dplyr::filter(SITE == "Church") %>%
               dplyr::mutate(T.z = Z*100 - T, 
                             B.z = Z*100 - B),
             aes(x = FINDS, 
                 y = ((B.z + T.z) / 2)/100), 
             color = NA, fill = NA) + 
  geom_rect(aes(xmin = -1, xmax = length(unique(c.finds$FINDS))+2, 
                ymin = T.z/100, ymax = B.z/100,
                fill = col)) + 
  geom_rect(data = c.loc %>% dplyr::mutate(T = Z, B = Z - 3), 
            aes(xmin = -1, xmax = length(unique(c.finds$FINDS))+2, 
                ymin = T, ymax = B), 
            fill = NA, color = "black") +
  geom_point(data = c.finds %>% 
               dplyr::left_join(c.loc %>% dplyr::select(CORE, Z, SITE), by = "CORE") %>%
               #dplyr::filter(SITE == "Church") %>%
               dplyr::mutate(FINDS = factor(FINDS, 
                                            levels = c("Roots",
                                                       "Glas",
                                                       "Palmnut", 
                                                       "Charcoal", 
                                                       "Fired Clay", 
                                                       "Pottery", 
                                                       "Lithics")), 
                             T.z = Z*100 - T, 
                             B.z = Z*100 - B),
             aes(x = FINDS, 
                 y = ((B.z + T.z) / 2)/100,
                 color = FINDS, 
                 shape = FINDS), 
             size = 2) + 
  scale_shape_manual(values = c(16, 16, 20, 18, 17, 17, 15)) + 
  scale_color_manual(values = c("brown", "grey", "black", "black", "orange", "red", "blue")) + 
  scale_fill_identity() + 
  facet_grid(.~CORE) + 
  scale_y_continuous("Depth [m]") +
  theme_classic() + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.line = element_blank())
ggsave("output/MUK2018-1020_corings_Z_diff.jpg", width = 8, height = 8)
ggsave("output/MUK2018-1020_corings_Z_diff.pdf", width = 8, height = 8)


# bw version:




mun <- data.frame(MUNSELL = sort(unique(c.soil$MUNSELL))) %>%
  dplyr::mutate(sort = gsub(" .*$","", MUNSELL)) %>%
  dplyr::mutate(sort = as.numeric(gsub("Y|R|YR", "", sort))) %>%
  dplyr::arrange(sort, MUNSELL) %>%
  tibble::rowid_to_column("lab") %>%
  dplyr::select(-sort)


c.soil %>%
  dplyr::left_join(c.loc %>% dplyr::select(CORE, Z, SITE), by = "CORE") %>%
  #dplyr::filter(SITE == "Church") %>%
  dplyr::mutate(T.z = Z*100 - T, 
                B.z = Z*100 - B) %>%
  dplyr::left_join(mun, by = "MUNSELL") %>%
  #dplyr::mutate(col.bw = DescTools::ColToGrey(col)) %>%
  ggplot() + 
  geom_point(data = c.finds %>% 
               dplyr::left_join(c.loc %>% dplyr::select(CORE, Z, SITE), by = "CORE") %>%
               #dplyr::filter(SITE == "Church") %>%
               dplyr::mutate(T.z = Z*100 - T, 
                             B.z = Z*100 - B),
             aes(x = FINDS, 
                 y = ((B.z + T.z) / 2)/100), 
             color = NA, fill = NA) + 
  geom_rect(aes(xmin = -1, xmax = length(unique(c.finds$FINDS))+2, 
                ymin = T.z/100, ymax = B.z/100,
                fill = col)) + 
  geom_rect(data = c.loc %>% dplyr::mutate(T = Z, B = Z - 3), 
            aes(xmin = -1, xmax = length(unique(c.finds$FINDS))+2, 
                ymin = T, ymax = B), 
            fill = NA, color = "black") +
  geom_point(data = c.finds %>% 
               dplyr::left_join(c.loc %>% dplyr::select(CORE, Z, SITE), by = "CORE") %>%
               #dplyr::filter(SITE == "Church") %>%
               dplyr::mutate(FINDS = factor(FINDS, 
                                            levels = c("Roots",
                                                       "Glas",
                                                       "Palmnut", 
                                                       "Charcoal", 
                                                       "Fired Clay", 
                                                       "Pottery", 
                                                       "Lithics")), 
                             T.z = Z*100 - T, 
                             B.z = Z*100 - B),
             aes(x = FINDS, 
                 y = ((B.z + T.z) / 2)/100,
                 color = FINDS, 
                 shape = FINDS), 
             size = 2) + 
  geom_text(aes(x = 2, y = (((B.z + T.z) / 2)/100), label = lab)) + 
  scale_shape_manual(values = c(16, 16, 20, 18, 17, 17, 15)) + 
  scale_color_manual(values = c("brown", "grey", "black", "black", "orange", "red", "blue")) + 
  scale_fill_identity() + 
  facet_grid(.~CORE) + 
  scale_y_continuous("Depth [m]") +
  theme_classic() + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.line = element_blank())
ggsave("output/MUK2018-1020_corings_Z_diff_bw.jpg", width = 8, height = 8)















c.soil %>%
  ggplot() + 
  geom_rect(aes(xmin = 1, xmax = 2, 
                ymin = T, ymax = B,
                fill = col)) + 
  geom_rect(aes(xmin = 1, xmax = 2, 
                ymin = 0, ymax = 300), 
            fill = NA, color = "black") + 
  geom_point(data = c.finds, 
             aes(x = 1.5, 
                 y = ((B+T)/2),
                 color = FINDS)) + 
  scale_fill_identity() + 
  facet_grid(. ~ CORE) +
  scale_y_reverse("Depth [cm]") + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())
ggsave("output/MUK2018-1020_corings.jpg", width = 8, height = 6)
