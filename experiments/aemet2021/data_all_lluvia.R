data_all_lluvia <- meteo %>%
  select(provincia, nombre, month, season, prec)
# print(data_all_sol[!complete.cases(data_all_sol),], n = 500)
data_all_lluvia <- data_all_lluvia %>%
  drop_na()

# Min and max solerature for normalization
the_min <- min(data_all_lluvia$prec)
the_max <- max(data_all_lluvia$prec)
data_all_lluvia <- data_all_lluvia %>% mutate(
  precN = n(prec, the_min, the_max),
)

# Check enough data
print(data_all_lluvia %>%
        group_by(provincia, month) %>% count() %>%
        pivot_wider(names_from = month, values_from = n), n = 52)

range(data_all_lluvia$prec)
range(data_all_lluvia$precN)

# By month ----------------------------------------------------------------

data_all_month_lluvia <- data_all_lluvia %>%
  select(provincia, month, precN) %>%
  group_by(provincia, month) %>%
  summarise(min = min(precN),
            max = max(precN),
            .groups = "drop")

ggplot(data_all_month_lluvia, aes(x=month)) +
  geom_segment(aes(xend = month, y = min, yend = max)) +
  geom_point(aes(month, min)) +
  geom_point(aes(month, max)) +
  facet_wrap(~provincia)+
  xlab("Month") +
  ylab("solerature range") +
  theme_bw() +
  ggtitle("Rango de lluvia por mes teniendo en cuenta\n todas las estaciones meteorológicas")

mins <- data_all_month_lluvia %>%
  pivot_wider(-max, names_from = month, names_prefix = "min",
              values_from = min)
maxs <- data_all_month_lluvia %>%
  pivot_wider(-min, names_from = month, names_prefix = "max",
              values_from = max)

data_all_month_lluvia_dist <- inner_join(mins, maxs)
data_all_month_lluvia_dist <- data_all_month_lluvia_dist[c(1, order(rep(1:12, 2)) + 1)]
print(data_all_month_lluvia_dist, n = 52)

data_all_month_lluvia_dist_variables <- data_all_month_lluvia_dist[,-1]

# By season ----------------------------------------------------------------

data_all_season_lluvia <- data_all_lluvia %>%
  select(provincia, season, precN) %>%
  group_by(provincia, season) %>%
  summarise(min = min(precN),
            max = max(precN),
            .groups = "drop")

ggplot(data_all_season_lluvia, aes(x=season)) +
  geom_segment(aes(xend = season, y = min, yend = max)) +
  geom_point(aes(season, min)) +
  geom_point(aes(season, max)) +
  facet_wrap(~provincia)+
  xlab("Month") +
  ylab("solerature range") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Rango de lluvia por season teniendo en cuenta\n todas las estaciones meteorológicas")

mins <- data_all_season_lluvia %>%
  pivot_wider(-max, names_from = season, names_prefix = "min",
              values_from = min)
maxs <- data_all_season_lluvia %>%
  pivot_wider(-min, names_from = season, names_prefix = "max",
              values_from = max)

data_all_season_lluvia_dist <- inner_join(mins, maxs)
data_all_season_lluvia_dist <- data_all_season_lluvia_dist[c(1, order(rep(1:4, 2)) + 1)]
print(data_all_season_lluvia_dist, n = 52)

data_all_season_lluvia_dist_variables <- data_all_season_lluvia_dist[,-1]
