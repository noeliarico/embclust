data_all_pres <- meteo %>%
  select(provincia, nombre, month, season, presMin, presMax)
# print(data_all_pres[!complete.cases(data_all_pres),], n = 500)
data_all_pres <- data_all_pres %>%
  drop_na()

# Min and max preserature for normalization
the_min <- min(data_all_pres$presMin)
the_max <- max(data_all_pres$presMax)

data_all_pres <- data_all_pres %>% mutate(
  maxN = n(presMax, the_min, the_max),
  minN = n(presMin, the_min, the_max)
)

range(data_all_pres$presMin)
range(data_all_pres$presMax)
range(data_all_pres$maxN)
range(data_all_pres$minN)

# By month ----------------------------------------------------------------

data_all_month_pres <- data_all_pres %>%
  select(provincia, month, maxN, minN) %>%
  group_by(provincia, month) %>%
  summarise(min = min(minN),
            max = max(maxN),
            .groups = "drop")

ggplot(data_all_month_pres, aes(x=month)) +
  geom_segment(aes(xend = month, y = min, yend = max)) +
  geom_point(aes(month, min)) +
  geom_point(aes(month, max)) +
  facet_wrap(~provincia)+
  xlab("Month") +
  ylab("preserature range") +
  theme_bw() +
  ggtitle("Rango de presión por mes teniendo en cuenta\n todas las seasones meteorológicas")

mins <- data_all_month_pres %>%
  pivot_wider(-max, names_from = month, names_prefix = "min",
              values_from = min)
maxs <- data_all_month_pres %>%
  pivot_wider(-min, names_from = month, names_prefix = "max",
              values_from = max)

data_all_month_pres_dist <- inner_join(mins, maxs)
data_all_month_pres_dist <- data_all_month_pres_dist[c(1, order(rep(1:12, 2)) + 1)]
print(data_all_month_pres_dist, n = 52)

data_all_month_pres_dist_variables <- data_all_month_pres_dist[,-1]
data_all_month_pres_dist_provinces <- data_all_month_pres_dist %>% pull(provincia)

# By season ----------------------------------------------------------------

data_all_season_pres <- data_all_pres %>%
  select(provincia, season, maxN, minN) %>%
  group_by(provincia, season) %>%
  summarise(min = min(minN),
            max = max(maxN),
            .groups = "drop")

ggplot(data_all_season_pres, aes(x=season)) +
  geom_segment(aes(xend = season, y = min, yend = max)) +
  geom_point(aes(season, min)) +
  geom_point(aes(season, max)) +
  facet_wrap(~provincia)+
  xlab("Month") +
  ylab("preserature range") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Rango de preseratura por season teniendo en cuenta\n todas las estaciones meteorológicas")

mins <- data_all_season_pres %>%
  pivot_wider(-max, names_from = season, names_prefix = "min",
              values_from = min)
maxs <- data_all_season_pres %>%
  pivot_wider(-min, names_from = season, names_prefix = "max",
              values_from = max)

data_all_season_pres_dist <- inner_join(mins, maxs)
data_all_season_pres_dist <- data_all_season_pres_dist[c(1, order(rep(1:4, 2)) + 1)]
print(data_all_season_pres_dist, n = 52)

data_all_season_pres_dist_variables <- data_all_season_pres_dist[,-1]
data_all_season_pres_dist_provinces <- data_all_season_pres_dist %>% pull(provincia)
