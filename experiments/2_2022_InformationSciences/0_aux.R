library(broom)
library(stringi)
library(rgdal)
library(tidyverse)

plot_mapa_clusters <- function(clusters, title = "") {
  data_provincias_mapa <- data_provincias_mapa %>%
    left_join(clusters, by = c("provincia" = "provincia"))

  data_provincias_mapa %>%
    ggplot() +
    geom_polygon(aes(x= long,
                     y = lat,
                     group = group,
                     fill = cluster),
                 color = "black",
                 alpha = 0.6, size = 0.2) +
    theme_void() +
    theme(panel.background = element_rect(size= 0.5, color = "black", fill = "white")) +
    ggtitle(title) +
    theme(legend.position = "none")
}
