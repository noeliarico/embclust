data_all_sol <- meteo %>%
  select(provincia, nombre, month, season, sol)
# print(data_all_sol[!complete.cases(data_all_sol),], n = 500)
data_all_sol <- data_all_sol %>%
  drop_na()

# Min and max solerature for normalization
the_min <- min(data_all_sol$sol)
the_max <- max(data_all_sol$sol)

sol_prov <- sol_prov %>% mutate(
  solN = n(sol, the_min, the_max),
) %>% filter(year == 2021) %>% mutate(year = NULL)

# Check enough data
print(sol_prov %>%
        group_by(provincia, month) %>% count() %>%
        pivot_wider(names_from = month, values_from = n), n = 52)

range(data_all_sol$sol)
range(data_all_sol$solN)

# By month ----------------------------------------------------------------

data_all_month_sol <- data_all_sol %>%
  select(provincia, month, solN) %>%
  group_by(provincia, month) %>%
  summarise(min = min(solN),
            max = max(solN),
            .groups = "drop")

ggplot(data_all_month_sol, aes(x=month)) +
  geom_segment(aes(xend = month, y = min, yend = max)) +
  geom_point(aes(month, min)) +
  geom_point(aes(month, max)) +
  facet_wrap(~provincia)+
  xlab("Month") +
  ylab("solerature range") +
  theme_bw() +
  ggtitle("Rango de soleratura por mes teniendo en cuenta\n todas las seasones meteorológicas")

mins <- data_all_month_sol %>%
  pivot_wider(-max, names_from = month, names_prefix = "min",
              values_from = min)
maxs <- data_all_month_sol %>%
  pivot_wider(-min, names_from = month, names_prefix = "max",
              values_from = max)

data_all_month_sol_dist <- inner_join(mins, maxs)
data_all_month_sol_dist <- data_all_month_sol_dist[c(1, order(rep(1:12, 2)) + 1)]
print(data_all_month_sol_dist, n = 52)

data_all_month_sol_dist_variables <- data_all_month_sol_dist[,-1]
data_all_month_sol_dist_provinces <- data_all_month_sol_dist %>% pull(provincia)

# By season ----------------------------------------------------------------

data_all_season_sol <- data_all_sol %>%
  select(provincia, season, solN) %>%
  group_by(provincia, season) %>%
  summarise(min = min(solN),
            max = max(solN),
            .groups = "drop")

ggplot(data_all_season_sol, aes(x=season)) +
  geom_segment(aes(xend = season, y = min, yend = max)) +
  geom_point(aes(season, min)) +
  geom_point(aes(season, max)) +
  facet_wrap(~provincia)+
  xlab("Month") +
  ylab("solerature range") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Rango de soleratura por season teniendo en cuenta\n todas las estaciones meteorológicas")

mins <- data_all_season_sol %>%
  pivot_wider(-max, names_from = season, names_prefix = "min",
              values_from = min)
maxs <- data_all_season_sol %>%
  pivot_wider(-min, names_from = season, names_prefix = "max",
              values_from = max)

data_all_season_sol_dist <- inner_join(mins, maxs)
data_all_season_sol_dist <- data_all_season_sol_dist[c(1, order(rep(1:4, 2)) + 1)]
print(data_all_season_sol_dist, n = 52)

data_all_season_sol_dist_variables <- data_all_season_sol_dist[,-1]
data_all_season_sol_dist_provinces <- data_all_season_sol_dist %>% pull(provincia)
