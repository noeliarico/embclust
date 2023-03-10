data_variables <- meteo_is_intervals_dist_variables
data_provinces <- meteo_is_intervals_dist_provinces
linkage_method <- "average"

library(dendextend)

k <- 5

##### EMB_W ###################################################################

s <- sim_emb_matrix(data_variables, sim_w, mean, dist = T)
attr(s, "Labels") <- data_provinces
d <- 1-s
hc <- hclust(d, method = linkage_method)
d_emb_w <- d
hc_emb_w <- hc

clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_w <- plot_mapa_clusters(clusters, "Width") +
  theme(text = element_text(family = "Gulliver"))

##### EMB_LK ###################################################################

s <- sim_emb_matrix(data_variables, sim_lk, mean, dist = T)
attr(s, "Labels") <- data_provinces
d <- 1-s
hc <- hclust(d, method = linkage_method)
d_emb_lk <- d
hc_emb_lk <- hc

clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_lk <- plot_mapa_clusters(clusters, "Lukasiewicz") +
  theme(text = element_text(family = "Gulliver"))

##### EMB_GG ###################################################################

s <- sim_emb_matrix(data_variables, sim_gg, mean, dist = T)
attr(s, "Labels") <- data_provinces
d <- 1-s
hc <- hclust(d, method = linkage_method)
d_emb_gg <- d
hc_emb_gg <- hc

clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_gg <- plot_mapa_clusters(clusters, "Goguen") +
  theme(text = element_text(family = "Gulliver"))


##### EMB_GD ###################################################################

s <- sim_emb_matrix(data_variables, sim_gd, mean, dist = T)
attr(s, "Labels") <- data_provinces
d <- 1-s
hc <- hclust(d, method = linkage_method)
d_emb_gd <- d
hc_emb_gd <- hc

clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_gd <- plot_mapa_clusters(clusters, "Godel") +
  theme(text = element_text(family = "Gulliver"))

##### EMB_FD ###################################################################

s <- sim_emb_matrix(data_variables, sim_fd, mean, dist = T)
attr(s, "Labels") <- data_provinces
d <- 1-s
hc <- hclust(d, method = linkage_method)
d_emb_fd <- d
hc_emb_fd <- hc

clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_fd <- plot_mapa_clusters(clusters, "Fodor") +
  theme(text = element_text(family = "Gulliver"))

##### EMB_RS ###################################################################

s <- sim_emb_matrix(data_variables, sim_rs, mean, dist = T)
attr(s, "Labels") <- data_provinces
d <- 1-s
hc <- hclust(d, method = linkage_method)
d_emb_rs <- d
hc_emb_rs <- hc

clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_rs <- plot_mapa_clusters(clusters, "Rescher") +
  theme(text = element_text(family = "Gulliver"))

############## PLOT

library(patchwork)

p <- (mapa_w | mapa_lk) /
  (mapa_gg | mapa_gd) /
  (mapa_fd | mapa_rs)

ggsave(paste0("mapas_emb_", linkage_method, ".png"), p, width = 7, height = 8, units = "in", dpi = 1000)

