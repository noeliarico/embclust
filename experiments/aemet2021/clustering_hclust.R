data_variables <- data_capitales_month_temp_dist_variables
k <- 5

d <- dist
attr(d, "Labels") <- data_provinces

# Maximum ----------------------------------------------------------------------

maxs <- data_variables[, str_detect(colnames(data_variables), "max")]
d <- dist(maxs)
attr(d, "Labels") <- data_provinces

hc <- hclust(d, method = "single")
clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_single_max <- plot_mapa_clusters(clusters, "Single")

hc <- hclust(d, method = "complete")
clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_complete_max <- plot_mapa_clusters(clusters, "Complete")

hc <- hclust(d, method = "average")
clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_average_max <- plot_mapa_clusters(clusters, "Average")

mapa_maxs <- mapa_single_max / mapa_complete_max / mapa_average_max

# Maximum ----------------------------------------------------------------------

mins <- data_variables[, str_detect(colnames(data_variables), "min")]
d <- dist(mins)
attr(d, "Labels") <- data_provinces

hc <- hclust(d, method = "single")
clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_single_min <- plot_mapa_clusters(clusters, "Single")

hc <- hclust(d, method = "complete")
clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_complete_min <- plot_mapa_clusters(clusters, "Complete")

hc <- hclust(d, method = "average")
clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_average_min <- plot_mapa_clusters(clusters, "Average")

mapa_mins <- mapa_single_min / mapa_complete_min / mapa_average_min

# Maximum --------------------------------------------------------------------

(mapa_single_min | mapa_complete_min | mapa_average_min |
   mapa_single_max | mapa_complete_max | mapa_average_max ) +
  plot_layout(ncol = 2, nrow = 3, byrow = F) +
  plot_annotation("Minimum temp (left)                                    Maximum temp (right)")

# 7.03 x 8.13

# mapas_emb_5_avg
