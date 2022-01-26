# Cambiar aquí el conjunto de datos que esté utilizando

# usando todas las estaciones
# data_variables <- data_all_month_sol_dist_variables
# data_variables <- data_all_month_temp_dist_variables

data_variables <- data_capitales_month_temp_dist_variables
linkage_method <- "single"

library(dendextend)

k <- 5

##### EMB_W ###################################################################

s <- sim_emb_matrix(data_variables, sim_w, mean, dist = T)
attr(s, "Labels") <- data_provinces
d <- 1-s
hc <- hclust(d, method = linkage_method)

# d <- as.dendrogram(hc)
# d <- d %>% color_branches(k=k)
# par(mar = c(3,1,1,7))
# plot(d, horiz  = TRUE)

clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_w <- plot_mapa_clusters(clusters, "Width")

##### EMB_LK ###################################################################

s <- sim_emb_matrix(data_variables, sim_lk, mean, dist = T)
attr(s, "Labels") <- data_provinces
d <- 1-s
hc <- hclust(d, method = linkage_method)

# d <- as.dendrogram(hc)
# d <- d %>% color_branches(k=k)
# par(mar = c(3,1,1,7))
# plot(d, horiz  = TRUE)

clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_lk <- plot_mapa_clusters(clusters, "Lukasiewicz")

##### EMB_GG ###################################################################

s <- sim_emb_matrix(data_variables, sim_gg, mean, dist = T)
attr(s, "Labels") <- data_provinces
d <- 1-s
hc <- hclust(d, method = linkage_method)

# d <- as.dendrogram(hc)
# d <- d %>% color_branches(k=k)
# par(mar = c(3,1,1,7))
# dend_gg <- plot(d, horiz  = TRUE)

clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_gg <- plot_mapa_clusters(clusters, "Goguen")


##### EMB_GD ###################################################################

s <- sim_emb_matrix(data_variables, sim_gd, mean, dist = T)
attr(s, "Labels") <- data_provinces
d <- 1-s
hc <- hclust(d, method = linkage_method)

# d <- as.dendrogram(hc)
# d <- d %>% color_branches(k=k)
# par(mar = c(3,1,1,7))
# plot(d, horiz  = TRUE)

clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_gd <- plot_mapa_clusters(clusters, "Godel")

##### EMB_FD ###################################################################

s <- sim_emb_matrix(data_variables, sim_fd, mean, dist = T)
attr(s, "Labels") <- data_provinces
d <- 1-s
hc <- hclust(d, method = linkage_method)

# d <- as.dendrogram(hc)
# d <- d %>% color_branches(k=k)
# par(mar = c(3,1,1,7))
# dend_fd <- plot(d, horiz  = TRUE)

clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_fd <- plot_mapa_clusters(clusters, "Fodor")

##### EMB_RS ###################################################################

s <- sim_emb_matrix(data_variables, sim_rs, mean, dist = T)
attr(s, "Labels") <- data_provinces
d <- 1-s
hc <- hclust(d, method = linkage_method)

# d <- as.dendrogram(hc)
# d <- d %>% color_branches(k=k)
# par(mar = c(3,1,1,7))
# dend_rs <- plot(d, horiz  = TRUE)

clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_rs <- plot_mapa_clusters(clusters, "Rescher")

############## PLOT

library(patchwork)

(mapa_w | mapa_lk) /
  (mapa_gg | mapa_gd) /
  (mapa_fd | mapa_rs)

