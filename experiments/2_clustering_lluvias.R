lluvias_mean_by_month <- lluvias %>%
  select(provincia, month, prec) %>%
  mutate(month = factor(month)) %>%
  group_by(provincia, month) %>%
  summarise(mean = mean(prec),
            .groups = "drop") %>%
  pivot_wider(names_from = month, values_from = mean)

lluvias_mean_by_month_variables <- lluvias_mean_by_month[,-1]
lluvias_mean_by_month_provinces <- lluvias_mean_by_month %>% pull(provincia)

d <- dist(lluvias_mean_by_month_variables)
attr(d, "Labels") <- lluvias_mean_by_month_provinces

hc <- hclust(d, method = "average")

clusters <- data.frame(cluster = factor(cutree(hc, 4))) %>%
  rownames_to_column("provincia")
plot_mapa_clusters(clusters, "Using mean temperature each month")

#####################
