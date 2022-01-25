library(broom)
library(stringi)
library(rgdal)
library(tidyverse)

shapefile_provincias <- readOGR("experiments/data/weather/maps/Provincias_ETRS89_30N/Provincias_ETRS89_30N.shp")
data_provincias <- tidy(shapefile_provincias)
nombre_provincias <- tibble(provincia = shapefile_provincias$Texto,
                                ca = shapefile_provincias$CCAA) %>%
  mutate(id = as.character(seq(0, nrow(.)-1)))

data_provincias_mapa <- data_provincias %>%
  left_join(nombre_provincias, by = "id") %>%
  mutate(provincia = tolower(provincia),
         provincia = stri_trans_general(provincia, id = "Latin-ASCII"),
         lat = ifelse(provincia %in% c("las palmas", "santa cruz de tenerife"), lat+650000, lat),
         long = ifelse(provincia %in% c("las palmas", "santa cruz de tenerife"), long+650000, long))
plot_mapa_clusters(clusters)
#


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

plot_mapa_altitud <- function(altitudes, title = "") {
  altitudes <- altitudes %>%
    mutate(provincia = tolower(provincia),
           provincia = str_replace(provincia, "araba/alava", "alava"),
           provincia = str_replace(provincia, "illes balears", "islas baleares"),
           provincia = str_replace(provincia, "a coruna", "la coruna"),
           provincia = str_replace(provincia, "gipuzkoa", "guipuzcoa"),
           provincia = str_replace(provincia, "girona", "gerona"),
           provincia = str_replace(provincia, "bizkaia", "vizcaya"),
           provincia = str_replace(provincia, "ourense", "orense"),
           provincia = str_replace(provincia, "sta. cruz de tenerife", "santa cruz de tenerife")
    ) %>%
    mutate(altitud = as.numeric(altitud))
  data_provincias_mapa <- data_provincias_mapa %>%
    left_join(altitudes, by = c("provincia" = "provincia"))

  data_provincias_mapa %>%
    ggplot() +
    geom_polygon(aes(x = long,
                     y = lat,
                     group = group,
                     fill = altitud),
                 color = "black",
                 alpha = 0.6, size = 0.2) +
    theme_void() +
    theme(panel.background = element_rect(size= 0.5, color = "black", fill = "white")) +
    ggtitle(title)
}
plot_mapa_altitud(altitudes)

