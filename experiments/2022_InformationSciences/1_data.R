library(jsonlite)
library(tidyverse)
library(lubridate)

path <- "experiments/2022_InformationSciences/data"
files <- list.files(path, full.names = T)
files <- files[str_detect(files, "json")]
# Give the input file name to the function.
meteo_is <- lapply(files, fromJSON)
meteo_is <- bind_rows(meteo_is)
meteo_is <- meteo_is %>%
  as_tibble()

meteo_is <- meteo_is %>%
  as_tibble() %>%
  mutate(fecha = as_date(fecha),
         year = year(fecha),
         day = day(fecha),
         month = month(fecha),
         nombre = as.factor(nombre),
         tmed = as.numeric(str_replace(tmed, ",", ".")),
         tmin = as.numeric(str_replace(tmin, ",", ".")),
         tmax = as.numeric(str_replace(tmax, ",", "."))
  ) %>%
  mutate(provincia = tolower(provincia),
         provincia = str_replace(provincia, "araba/alava", "alava"),
         provincia = str_replace(provincia, "illes balears", "islas baleares"),
         provincia = str_replace(provincia, "a coru.a", "la coruna"),
         provincia = str_replace(provincia, "gipuzkoa", "guipuzcoa"),
         provincia = str_replace(provincia, "girona", "gerona"),
         provincia = str_replace(provincia, "bizkaia", "vizcaya"),
         provincia = str_replace(provincia, "ourense", "orense"),
         provincia = str_replace(provincia, "sta. cruz de tenerife", "santa cruz de tenerife"),
         provincia = as.factor(provincia)
  ) %>%
  filter(year == 2021) %>%
  select(nombre, provincia, tmed, tmin, tmax, month)
meteo_is

glimpse(meteo_is)

# Check, una estación por provincia:
# meteo_is %>% group_by(nombre, provincia) %>% distinct() %>% count()

# Para la normalización luego
n <- function(x, mi, ma) {
  return((x-mi)/(ma-mi))
}

# Nombres de las provincias
data_provinces <- meteo_is %>%
  select(provincia) %>%
  distinct() %>%
  arrange(provincia) %>%
  pull(provincia)


meteo_is <- meteo_is %>%
  drop_na() # original 18,964 after drop 18,882

# Min and max temperature for normalization
the_min <- min(meteo_is$tmin)
the_max <- max(meteo_is$tmax)

meteo_is_intervals <- meteo_is %>%
  select(provincia, month, tmax, tmin) %>%
  group_by(provincia, month) %>%
  mutate(month = factor(month)) %>%
  summarise(min = min(tmin),
            max = max(tmax),
            .groups = "drop")

ggplot(meteo_is_intervals %>%
         left_join(ids_names) %>%
         mutate(provincia = as.character(provincia),
                prov.shortname.es = ifelse(prov.shortname.es == "Santa Cruz de Tenerife", "Sta cruz de\nTenerife", prov.shortname.es)), aes(x=month)) +
  geom_segment(aes(xend = month, y = min, yend = max)) +
  geom_point(aes(month, min)) +
  geom_point(aes(month, max)) +
  facet_wrap(~prov.shortname.es, nrow = 4)+
  xlab("Month") +
  ylab("Temperature range") +
  theme_bw() +
  theme(text = element_text(family = "Gulliver", size = 12),
        axis.text.x = element_text(angle = 90, size = 6),
        axis.title.x = element_text(margin = margin(t=10)),
        axis.title.y = element_text(margin = margin(r=10)))# +
# ggtitle("Rango de temperatura por mes teniendo en cuenta\n todas las seasones meteorológicas")


# By month ----------------------------------------------------------------

# Normalize
meteo_is <- meteo_is %>% mutate(
  maxN = n(tmax, the_min, the_max),
  minN = n(tmin, the_min, the_max)
)

range(meteo_is$tmin)
range(meteo_is$tmax)
range(meteo_is$maxN)
range(meteo_is$minN)

meteo_is_intervals <- meteo_is %>%
  select(provincia, month, maxN, minN) %>%
  group_by(provincia, month) %>%
  mutate(month = factor(month)) %>%
  summarise(min = min(minN),
            max = max(maxN),
            .groups = "drop")

mins <- meteo_is_intervals %>%
  pivot_wider(-max, names_from = month, names_prefix = "min",
              values_from = min)
maxs <- meteo_is_intervals %>%
  pivot_wider(-min, names_from = month, names_prefix = "max",
              values_from = max)

meteo_is_intervals_dist <- inner_join(mins, maxs)
meteo_is_intervals_dist <- meteo_is_intervals_dist[c(1, order(rep(1:12, 2)) + 1)]
print(meteo_is_intervals_dist, n = 52)

meteo_is_intervals_dist_variables <- meteo_is_intervals_dist[,-1]
meteo_is_intervals_dist_provinces <- meteo_is_intervals_dist %>% pull(provincia)

