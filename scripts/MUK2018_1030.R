#############################
# MUK 2018/1030/10
#############################

library(c14bazAAR)
library(cowplot)
library(dplyr)
library(geojsonsf)
library(ggplot2)
library(ggridges)
library(readxl)
library(sf)
library(tidyr)

excav.2018 <- geojson_sf("gis/MUK2018_excav.geojson")

# c14 ----
c14 <- data.table::fread("input/MUK_C14.csv") %>%
  dplyr::filter(LABNR != '/' & YEAR == 2018) %>%
  dplyr::select(DEPTH, LABNR, C14AGE, C14STD) %>%
  dplyr::rename(c14age = C14AGE, 
                c14std = C14STD) %>%
  c14bazAAR::as.c14_date_list() %>%
  c14bazAAR::calibrate(choices = c("calprobdistr", 
                                   "calrange"), 
                       sigma = 2)

c14.calprobdist <- c14 %>% 
  tidyr::unnest(cols = c("calprobdistr"))

# normalize to 100% for max Age
labnr <- unique(c14$LABNR)
calprobdist.lst <- list()
for(i in 1:length(labnr)){
  calprobdist.sel <- c14.calprobdist %>% 
    dplyr::filter(LABNR == labnr[[i]])
  
  calprobdist.sel$density <- calprobdist.sel$density / max(calprobdist.sel$density, na.rm = TRUE)
  calprobdist.lst[[i]] <- calprobdist.sel
}
c14.18.calprobdist.norm <- do.call(rbind, calprobdist.lst)

#c14.calrange <- c14 %>%
#  tidyr::unnest(cols = c("calrange"))

c14 <- data.table::fread("input/MUK_C14.csv") %>%
  dplyr::filter(LABNR != '/' & YEAR != 2018) %>%
  dplyr::select(DEPTH, LABNR, C14AGE, C14STD) %>%
  dplyr::rename(c14age = C14AGE, 
                c14std = C14STD) %>%
  c14bazAAR::as.c14_date_list() %>%
  c14bazAAR::calibrate(choices = c("calprobdistr", 
                                   "calrange"), 
                       sigma = 2)

c14.calprobdist <- c14 %>% 
  tidyr::unnest(cols = c("calprobdistr"))

# normalize to 100% for max Age
labnr <- unique(c14$LABNR)
calprobdist.lst <- list()
for(i in 1:length(labnr)){
  calprobdist.sel <- c14.calprobdist %>% 
    dplyr::filter(LABNR == labnr[[i]])
  
  calprobdist.sel$density <- calprobdist.sel$density / max(calprobdist.sel$density, na.rm = TRUE)
  calprobdist.lst[[i]] <- calprobdist.sel
}
c14.52.calprobdist.norm <- do.call(rbind, calprobdist.lst)


plt.c14 <- ggplot(c14.18.calprobdist.norm) + 
  ggridges::geom_ridgeline(data = c14.52.calprobdist.norm, 
                           aes(x = (-calage + 1950)/1000, 
                               y = DEPTH, 
                               height = density, 
                               group = LABNR), 
                           scale = 15, fill = "#f5f5f5", color = "grey") + 
  ggridges::geom_ridgeline(aes(x = (-calage + 1950)/1000, 
                               y = DEPTH-2, 
                               height = density, 
                               group = LABNR), 
                           scale = 15) + 
  scale_x_continuous(breaks = c(seq(-40, 2, 5)),
                     limits = c(-38, 2),
                     #minor_breaks = NULL,
                     expand = c(0, 0)) + 
  scale_y_reverse(limits = c(660, 0), # 40 
                  breaks = c(seq(0, 660, 20)),
                  minor_breaks = NULL,
                  expand = c(0, 0)) + 
  annotate("rect", ymin = 302, ymax = 358, xmin = -Inf, xmax = -12.570,
           alpha = .1) + 
  annotate("rect", ymin = 302, ymax = Inf, xmin = -12.031, xmax = -12.570,
           alpha = .15) + 
  #annotate("text", y = 330, x = -13500, label = "VIII") + 
  annotate("rect", ymin = 222, ymax = 298, xmin = -Inf, xmax = -9.355,
           alpha = .1) + 
  annotate("rect", ymin = 222, ymax = Inf, xmin = -9.890, xmax = -9.355,
           alpha = .15) + 
  #annotate("text", y = 270, x = -13500, label = "VII") + 
  #annotate("text", y = 220, x = -13500, label = "VI") + 
  annotate("rect", ymin = 162, ymax = 178, xmin = -Inf, xmax = -3.014,
           alpha = .1) + 
  annotate("rect", ymin = 162, ymax = Inf, xmin = -3.341, xmax = -3.014,
           alpha = .15) + 
  #annotate("text", y = 180, x = -13500, label = "V") + 
  annotate("rect", ymin = 102, ymax = 158, xmin = -Inf, xmax = -1.453,
           alpha = .1) + 
  annotate("rect", ymin = 102, ymax = Inf, xmin = -1.771, xmax = -1.453,
           alpha = .15) + 
  #annotate("text", y = 130, x = -13500, label = "IV") + 
  annotate("rect", ymin = 82, ymax = 98, xmin = -Inf, xmax = -.068,
           alpha = .1) + 
  annotate("rect", ymin = 82, ymax = Inf, xmin = -.352, xmax = -.068,
           alpha = .15) + 
  #annotate("text", y = 90, x = -13500, label = "III") + 
  annotate("rect", ymin = 62, ymax = 78, xmin = -Inf, xmax = .116,
           alpha = .1) + 
  annotate("rect", ymin = 62, ymax = Inf, xmin = -.045, xmax = .116,
           alpha = .15) + 
  #annotate("text", y = 70, x = -13500, label = "II") + 
  annotate("rect", ymin = 42, ymax = 58, xmin = -Inf, xmax = 1.633,
           alpha = .1) + 
  annotate("rect", ymin = 42, ymax = Inf, xmin = 1.326, xmax = 1.633,
           alpha = .15) + 
  #annotate("text", y = 50, x = -13500, label = "I") + 
  scale_size(range = c(0, 6)) + 
  #geom_hline(yintercept = 60, linetype="dashed") + 
  #geom_hline(yintercept = 80, linetype="dashed") + 
  #geom_hline(yintercept = 100, linetype="dashed") + 
  #geom_hline(yintercept = 160, linetype="dashed") + 
  #geom_hline(yintercept = 200, linetype="dashed") + 
  #geom_hline(yintercept = 240, linetype="dashed") + 
  #geom_hline(yintercept = 300, linetype="dashed") + 
  labs(y = "Depth cm", x = "Age [k yrs cal BCE/CE]") + 
  theme_light() + 
  theme(legend.position = "none", 
        panel.grid.minor.y = element_line(color = "grey80"), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# pottery ----

po.2018 <- data.table::fread("input/MUK2018_1030_10_Pottery.csv", encoding = "UTF-8") %>%
  dplyr::mutate(Act = as.character(Act))

po.2018.geom <- merge(x = po.2018, by.x = "Act",
                      y = excav.2018, by.y = "act")

po.2018.wgt <- po.2018.geom %>%
  dplyr::group_by(z) %>%
  dplyr::summarise(Pottery = sum(Wgt))

plt.po.2018.wgt <- ggplot(po.2018.wgt, aes(x = (z-10), y = Pottery)) + 
  geom_bar(stat = "identity", fill = "#f8766d", color = "black") + 
  scale_x_reverse("", limits = c(660, 0), # 40 
                  breaks = c(seq(0, 660, 20)),
                  minor_breaks = NULL,
                  expand = c(0, 0)) +
  scale_y_continuous("Weight [g]", #breaks = c(seq(0, 15000, 2000)),
                     limits = c(0, 1220),
                     #minor_breaks = NULL,
                     expand = c(0, 0)) + 
  geom_vline(xintercept = 60, linetype="dashed") + 
  coord_flip() + 
  theme_light() + 
  theme(axis.text.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


# lithics ----
li.2018.qty <- data.table::fread(
    "input/MUK2018_1030_10_lithics_size_qty.csv", 
    encoding = "UTF-8")

li.2018.qty %>% 
  dplyr::mutate(sum = rowSums(dplyr::across(3:7))) %>%
  dplyr::select(SQUARE, DEPTH, sum) %>%
  reshape2::dcast(DEPTH ~ SQUARE, 
                  value.var = "sum")

li.2018.qty <- li.2018.qty %>%
  dplyr::filter(DEPTH > 40 & !(SQUARE %in% c("1a", "1b") & DEPTH == 60))

li.2018.qty[,"< 2 cm"] <- rowSums(li.2018.qty[,c(5:7)]) # sum class for larger then 2 cm
li.2018.qty[,"> 2 cm"] <- rowSums(li.2018.qty[,c(3:4)]) # sum class for larger then 2 cm

li.2018.qty <- li.2018.qty[,c(2, 8:9)] %>%
  replace(is.na(.), 0) %>%
  dplyr::group_by(DEPTH) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::everything(), 
      list(sum))) # sum per unit/depth

li.2018.qty <- reshape2::melt(li.2018.qty, id.vars = "DEPTH")

li.2018.qty <- li.2018.qty %>%
  dplyr::mutate(variable = gsub("_1","", variable)) %>%
  dplyr::mutate(variable = factor(variable, levels = c("> 2 cm", "< 2 cm")))

li.2018.qty[li.2018.qty$variable == "< 2 cm","value"] <- li.2018.qty[li.2018.qty$variable == "< 2 cm","value"] * -1

plt.li.2018.qty <- ggplot(li.2018.qty %>% dplyr::filter(value != 0), 
                          aes(
                            x = (DEPTH-10), 
                            y = value, 
                            fill = variable)) + 
  #geom_rect(xmin = 0, xmax = -40, ymin = 0, ymax = 130, fill = "#d3d3d3") + 
  geom_bar(stat = "identity", color = "black") + 
  scale_fill_manual("", values = c("#619cff", "#d9e6fa"), 
                    guide = guide_legend(reverse = TRUE)) + 
  scale_x_reverse("", limits = c(660, 0), # 40 
                  breaks = c(seq(0, 660, 20)),
                  minor_breaks = NULL,
                  expand = c(0, 0)) +
  scale_y_continuous("Quantity", breaks = c(seq(-80, 0, 10), seq(0, 20, 5)), 
                     labels = abs) +
  geom_vline(xintercept = 80, linetype="dashed") + 
  geom_vline(xintercept = 140, linetype="dashed") + 
  geom_vline(xintercept = 280, linetype="dashed") + 
  geom_vline(xintercept = 360, linetype="dashed") + 
  geom_hline(yintercept = 0) + 
  coord_flip() + 
  theme_light() + 
  theme(axis.text.y = element_blank(), 
        legend.position = "bottom", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank()) +
  guides(fill = guide_legend(
    ncol = 1, 
    reverse = TRUE))

# combined figure ----

plt <- cowplot::plot_grid(
  plt.c14, 
  plt.li.2018.qty, 
  NULL,
  labels = c("A", "B", ""),
  nrow = 1, rel_widths = c(1, 1, 2),
  align = 'h', axis = "tb")
ggsave("output/Fig_MUK2018_1030_10.pdf", plt, width = 16, height = 10)
ggsave("output/Fig_MUK2018_1030_10.jpg", plt, width = 16, height = 10)

ggsave("output/Fig_MUK2018_1030_10_p.jpg", plt, width = 8, height = 10)
ggsave("output/Fig_MUK2018_1030_10_p.pdf", plt, width = 8, height = 10)
