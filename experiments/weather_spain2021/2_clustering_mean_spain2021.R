weather_mean_by_month <- weather %>%
  select(provincia, month, tmed) %>%
  mutate(month = factor(month)) %>%
  group_by(provincia, month) %>%
  summarise(mean = mean(tmed),
            .groups = "drop") %>%
  pivot_wider(names_from = month, values_from = mean)

weather_mean_by_month_variables <- weather_mean_by_month[,-1]
weather_mean_by_month_provinces <- weather_mean_by_month %>% pull(provincia)

d <- dist(weather_mean_by_month_variables)
attr(d, "Labels") <- weather_mean_by_month_provinces

hc <- hclust(d, method = "average")

clusters <- data.frame(cluster = factor(cutree(hc, 6))) %>%
  rownames_to_column("provincia")
plot_mapa_clusters(clusters, "Using mean temperature each month")
