library(patchwork)
library(extrafont)

data_variables <- meteo_is_intervals_dist_variables
k <- 5

# Maximum ----------------------------------------------------------------------

maxs <- data_variables[, str_detect(colnames(data_variables), "max")]
d <- dist(maxs)
attr(d, "Labels") <- data_provinces
d_maxs <- d

hc <- hclust(d, method = "single")
clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_single_max <- plot_mapa_clusters(clusters, "Single") +
  theme(text = element_text(family = "Gulliver"))
hc_max_single <- hc

hc <- hclust(d, method = "complete")
clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_complete_max <- plot_mapa_clusters(clusters, "Complete") +
  theme(text = element_text(family = "Gulliver"))
hc_max_complete <- hc

hc <- hclust(d, method = "average")
clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_average_max <- plot_mapa_clusters(clusters, "Average") +
  theme(text = element_text(family = "Gulliver"))
hc_max_average <- hc

mapa_maxs <- mapa_single_max / mapa_complete_max / mapa_average_max

# Minimum ----------------------------------------------------------------------

mins <- data_variables[, str_detect(colnames(data_variables), "min")]
d <- dist(mins)
attr(d, "Labels") <- data_provinces
d_mins <- d

hc <- hclust(d, method = "single")
clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_single_min <- plot_mapa_clusters(clusters, "Single") +
  theme(text = element_text(family = "Gulliver"))
hc_min_single <- hc

hc <- hclust(d, method = "complete")
clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_complete_min <- plot_mapa_clusters(clusters, "Complete") +
  theme(text = element_text(family = "Gulliver"))
hc_min_complete <- hc

hc <- hclust(d, method = "average")
clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_average_min <- plot_mapa_clusters(clusters, "Average") +
  theme(text = element_text(family = "Gulliver"))
hc_min_average <- hc

mapa_mins <- mapa_single_min / mapa_complete_min / mapa_average_min

# All --------------------------------------------------------------------------

p <- (mapa_single_min | mapa_complete_min | mapa_average_min |
      mapa_single_max | mapa_complete_max | mapa_average_max ) +
  plot_layout(ncol = 2, nrow = 3, byrow = F) +
  plot_annotation(title = "Minimum temperature                                  Maximum temperature",
                  theme = theme(text = element_text(family = "Gulliver"),
                                plot.title = element_text(margin = margin(t = 0.5, b = 0.5, unit = "cm")))) +
  theme(plot.margin = margin(l = 0.5, r = 0.5, unit = "cm"))

ggsave("mapas_hclust.png", p, width = 7, height = 8, units = "in", dpi = 1000)

# 7 x 8

# mapas_emb_5_avg

