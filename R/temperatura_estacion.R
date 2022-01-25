meteo

meteo_temp_min_max <- meteo %>%
  select(provincia, nombre, estacion, tmin, tmax)

table(complete.cases(meteo_temp_min_max))
meteo_temp_min_max <- meteo_temp_min_max %>% drop_na()

# Min and max temperature for normalization
minT <- min(meteo_temp_min_max$tmin)
maxT <- max(meteo_temp_min_max$tmax)
n <- function(x) {
  return((x-minT)/(maxT-minT))
}
meteo_temp_min_max <- meteo_temp_min_max %>% mutate(
  maxN = n(tmax),
  minN = n(tmin)
)

range(meteo_temp_min_max$tmin)
range(meteo_temp_min_max$tmax)
range(meteo_temp_min_max$maxN)
range(meteo_temp_min_max$minN)

meteo_temp_min_max_season <- meteo_temp_min_max %>%
  select(provincia, estacion, maxN, minN) %>%
  group_by(provincia, estacion) %>%
  summarise(min = min(minN),
            max = max(maxN),
            .groups = "drop")

meteo_temp_min_max_season <- meteo_temp_min_max %>%
  select(provincia, estacion, tmin, tmax) %>%
  group_by(provincia, estacion) %>%
  summarise(min = min(tmin),
            max = max(tmax),
            .groups = "drop")

ggplot(meteo_temp_min_max_season, aes(x= estacion)) +
  geom_segment(aes(xend = estacion, y = min, yend = max)) +
  geom_point(aes(estacion, min)) +
  geom_point(aes(estacion, max)) +
  # scale_x_discrete(n.breaks = 12) +
  facet_wrap(~provincia)+
  xlab("Month") +
  ylab("Temperature range") +
  theme_bw()
