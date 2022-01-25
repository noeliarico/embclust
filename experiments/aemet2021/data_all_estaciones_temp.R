# Min and max temperature for normalization
minS <- min(lluvias$prec)
maxS <- max(lluvias$prec)
n <- function(x) {
  return((x-minS)/(maxS-minS))
}
lluvias <- lluvias %>% mutate(
  prec = n(prec),
) %>% filter(year == 2021) %>% mutate(year = NULL)


print(lluvias %>%
        group_by(provincia, month) %>% count() %>%
        pivot_wider(names_from = month, values_from = n), n = 52)

# Check range of temperatures
range(lluvias$prec)

lluvias_by_month <- lluvias %>%
  select(provincia, month, prec) %>%
  mutate(month = factor(month)) %>%
  group_by(provincia, month) %>%
  summarise(min = min(prec),
            max = max(prec),
            .groups = "drop")

lluvias_by_month_plot <- lluvias %>%
  select(provincia, month, prec) %>%
  mutate(month = factor(month)) %>%
  group_by(provincia, month) %>%
  summarise(min = min(prec),
            max = max(prec),
            .groups = "drop")



ggplot(lluvias_by_month_plot, aes(x=month)) +
  geom_segment(aes(xend = month, y = min, yend = max)) +
  geom_point(aes(month, min)) +
  geom_point(aes(month, max)) +
  # scale_x_discrete(n.breaks = 12) +
  facet_wrap(~provincia)+
  xlab("Month") +
  ylab("Sun range") +
  theme_bw()

mins <- lluvias_by_month_plot %>%
  pivot_wider(-max, names_from = month, names_prefix = "min",
              values_from = min)

maxs <- lluvias_by_month_plot %>%
  pivot_wider(-min, names_from = month, names_prefix = "max",
              values_from = max)

lluvias_data <- inner_join(mins, maxs)
lluvias_data <- lluvias_data[c(1, order(rep(1:12, 2)) + 1)]
print(lluvias_data, n = 52)

lluvias_data_variables <- lluvias_data[,-1]
lluvias_data_provinces <- lluvias_data %>% pull(provincia)
