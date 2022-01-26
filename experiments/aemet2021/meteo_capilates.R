library(jsonlite)
library(tidyverse)
library(lubridate)

getSeason <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231))
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Invierno","Primavera","Verano","Otoño","Invierno")
  return(cuts)
}

path <- "experiments/aemet2021/capitales/data"
files <- list.files(path, full.names = T)
files <- files[str_detect(files, "json")]
# Give the input file name to the function.
meteo_capitales <- lapply(files, fromJSON)
meteo_capitales <- bind_rows(meteo_capitales)
meteo_capitales <- meteo_capitales %>% as_tibble()

meteo_capitales <- meteo_capitales %>%
  as_tibble() %>%
  mutate(fecha = as_date(fecha),
         year = year(fecha),
         day = day(fecha),
         month = month(fecha),
         nombre = as.factor(nombre),
         altitud = as.numeric(altitud),
         season = getSeason(fecha),
         tmed = as.numeric(str_replace(tmed, ",", ".")),
         tmin = as.numeric(str_replace(tmin, ",", ".")),
         tmax = as.numeric(str_replace(tmax, ",", ".")),
         presMin = as.numeric(str_replace(presMin, ",", ".")),
         presMax = as.numeric(str_replace(presMax, ",", ".")),
         prec = as.numeric(str_replace(prec, ",", ".")),
         sol = as.numeric(str_replace(sol, ",", ".")),
         velmedia = as.numeric(str_replace(velmedia, ",", ".")),
         racha = as.numeric(str_replace(racha, ",", "."))
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
  )
meteo_capitales

# Mirar cuantas seasones hay por comunidad autonoma
print(meteo_capitales %>%
  as_tibble() %>%
  select(nombre, provincia) %>%
  distinct() %>%
  group_by(provincia) %>%
  count(), n = 52)

print(meteo_capitales %>%
        as_tibble() %>%
        group_by(provincia, nombre, season) %>%
        count() %>%
        pivot_wider(names_from = season, values_from = n),
      n = 246)

glimpse(meteo_capitales)

# Para la normalización luego
n <- function(x, mi, ma) {
  return((x-mi)/(ma-mi))
}

data_provinces <- meteo_capitales %>% select(provincia) %>% distinct() %>% arrange(provincia) %>%
  pull(provincia)

