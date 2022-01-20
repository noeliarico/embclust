library(tidyverse)
library(stringr)
library(rgdal)
library(rgeos)
library(plotly)

# Mapa municipios
mapa <- rgdal::readOGR(
  paste0("data/weather/SHP_ETRS89/recintos_municipales_inspire_peninbal_etrs89/recintos_municipales_inspire_peninbal_etrs89.shp")
)

mapa_df <- fortify(model = mapa, region = "NATCODE")
mapa_df %>% head()
info_municipios <- mapa@data
info_municipios %>% head()
info_municipios <- info_municipios %>%
  mutate(
    pais       = str_sub(string = NATCODE, start = 1, end = 2),
    c_autonoma = str_sub(string = NATCODE, start = 3, end = 4),
    provincia  = str_sub(string = NATCODE, start = 5, end = 6),
    municipio  = str_sub(string = NATCODE, start = 7, end = -1)
  ) %>%
  rename(nombre_municipio = NAMEUNIT)

# Se seleccionan las columnas de interés
info_municipios <- info_municipios %>%
  select(
    NATCODE, nombre_municipio, c_autonoma, provincia, municipio
  )

# Se añade la información de los municipios
mapa_df <- mapa_df %>%
  left_join(info_municipios, by = c("id" = "NATCODE"))

# Se eliminan puntos (se reduce la resolución)
mapa_df <- mapa_df %>%
  slice(seq(1, nrow(mapa_df), 5))



mapa_df %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "gray20", fill = "white") +
  coord_map("mercator") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks =  element_blank(),
    axis.title = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


####


library(broom)
library(stringi)
shapefile_provincias <- readOGR("data/Provincias_ETRS89_30N/Provincias_ETRS89_30N.shp")
data_provincias <- tidy(shapefile_provincias)
nombres_provincias <- tibble(shapefile_provincias$Texto) %>%
  mutate(id = as.character(seq(0, nrow(.)-1)))

data_provincias_mapa <- data_provincias %>%
  left_join(nombres_provincias, by = "id") %>%
  rename(c(nombre = `shapefile_provincias$Texto`)) %>%
  mutate(nombre = tolower(nombre),
         nombre = stri_trans_general(nombre, id = "Latin-ASCII")) %>%
  left_join(clusters, by = c("nombre" = "provincia"))
data_provincias_mapa %>%
  ggplot() +
  geom_polygon(aes( x= long, y = lat, group = group, fill = cluster),
               alpha = 0.8, size = 0.05 ) +
  theme_void() +
  theme(panel.background = element_rect(size= 0.5, color = "white", fill = "white")) +
  labs(title = "CCAA", subtitle = "España")


# https://www.cienciadedatos.net/documentos/58_mapas_con_r.html

####

library(highcharter)
library(jsonlite)
library(magrittr)


mydata <- fromJSON("https://code.highcharts.com/mapdata/countries/es/es-all.geo.json",
                   flatten = TRUE)
provincias <- tibble(clave = mydata$features$`properties.hc-key`,
                     nombre = mydata$features$properties.name)

provincias %<>% mutate(valor = sample.int(1000, 53))
(mes <- hcmap("countries/es/es-all",
              data = provincias,
              name = "clave",
              value = "valor", joinBy = c("hc-key", "clave"),
              borderColor = "transparent") %>%
    hc_colorAxis(dataClasses = color_classes(seq(0, 1000, by = 100))) %>%
    hc_legend(layout = "vertical", align = "right",
              floating = TRUE, valueDecimals = 0))
