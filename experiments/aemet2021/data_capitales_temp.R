data_capitales_temp <- meteo_capitales %>%
  select(provincia, nombre, month, season, tmed, tmin, tmax)
# print(data_capitales_temp[!complete.cases(data_capitales_temp),], n = 500)
data_capitales_temp <- data_capitales_temp %>%
  drop_na()

# Min and max temperature for normalization
the_min <- min(data_capitales_temp$tmin)
the_max <- max(data_capitales_temp$tmax)

data_capitales_temp <- data_capitales_temp %>% mutate(
  maxN = n(tmax, the_min, the_max),
  minN = n(tmin, the_min, the_max)
)

range(data_capitales_temp$tmin)
range(data_capitales_temp$tmax)
range(data_capitales_temp$maxN)
range(data_capitales_temp$minN)

# By month ----------------------------------------------------------------

data_capitales_month_temp <- data_capitales_temp %>%
  select(provincia, month, maxN, minN) %>%
  group_by(provincia, month) %>%
  summarise(min = min(minN),
            max = max(maxN),
            .groups = "drop")

ggplot(data_capitales_month_temp, aes(x=month)) +
  geom_segment(aes(xend = month, y = min, yend = max)) +
  geom_point(aes(month, min)) +
  geom_point(aes(month, max)) +
  facet_wrap(~provincia)+
  xlab("Month") +
  ylab("Temperature range") +
  theme_bw() +
  ggtitle("Rango de temperatura por mes teniendo en cuenta\n todas las seasones meteorológicas")

mins <- data_capitales_month_temp %>%
  pivot_wider(-max, names_from = month, names_prefix = "min",
              values_from = min)
maxs <- data_capitales_month_temp %>%
  pivot_wider(-min, names_from = month, names_prefix = "max",
              values_from = max)

data_capitales_month_temp_dist <- inner_join(mins, maxs)
data_capitales_month_temp_dist <- data_capitales_month_temp_dist[c(1, order(rep(1:12, 2)) + 1)]
print(data_capitales_month_temp_dist, n = 52)

data_capitales_month_temp_dist_variables <- data_capitales_month_temp_dist[,-1]
data_capitales_month_temp_dist_provinces <- data_capitales_month_temp_dist %>% pull(provincia)

# By season ----------------------------------------------------------------

data_capitales_season_temp <- data_capitales_temp %>%
  select(provincia, season, maxN, minN) %>%
  group_by(provincia, season) %>%
  summarise(min = min(minN),
            max = max(maxN),
            .groups = "drop")

ggplot(data_capitales_season_temp, aes(x=season)) +
  geom_segment(aes(xend = season, y = min, yend = max)) +
  geom_point(aes(season, min)) +
  geom_point(aes(season, max)) +
  facet_wrap(~provincia)+
  xlab("Month") +
  ylab("Temperature range") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Rango de temperatura por season teniendo en cuenta\n todas las estaciones meteorológicas")

mins <- data_capitales_season_temp %>%
  pivot_wider(-max, names_from = season, names_prefix = "min",
              values_from = min)
maxs <- data_capitales_season_temp %>%
  pivot_wider(-min, names_from = season, names_prefix = "max",
              values_from = max)

data_capitales_season_temp_dist <- inner_join(mins, maxs)
data_capitales_season_temp_dist <- data_capitales_season_temp_dist[c(1, order(rep(1:4, 2)) + 1)]
print(data_capitales_season_temp_dist, n = 52)

data_capitales_season_temp_dist_variables <- data_capitales_season_temp_dist[,-1]
# data_capitales_season_temp_dist_provinces <- data_capitales_season_temp_dist %>% pull(provincia)
