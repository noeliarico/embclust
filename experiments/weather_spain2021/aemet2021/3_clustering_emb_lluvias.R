library(dendextend)

k <- 5

##### EMB_W ###################################################################

s <- sim_emb_matrix(lluvias_data_variables, emb_w, mean, dist = T)
attr(s, "Labels") <- lluvias_data_provinces
d <- 1-s
hc <- hclust(d, method = "average")

d <- as.dendrogram(hc)
d <- d %>% color_branches(k=k)
par(mar = c(3,1,1,7))
dend_w <- plot(d, horiz  = TRUE)

clusters <- data.frame(cluster = factor(cutree(hc, 4))) %>%
  rownames_to_column("provincia")
mapa_w <- plot_mapa_clusters(clusters, "Width")

##### EMB_LK ###################################################################

s <- sim_emb_matrix(lluvias_data_variables, emb_lk, mean, dist = T)
attr(s, "Labels") <- lluvias_data_provinces
d <- 1-s
hc <- hclust(d, method = "average")

d <- as.dendrogram(hc)
d <- d %>% color_branches(k=k)
par(mar = c(3,1,1,7))
dend_lk <- plot(d, horiz  = TRUE)

clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_lk <- plot_mapa_clusters(clusters, "Lukasiewicz")

##### EMB_GG ###################################################################

s <- sim_emb_matrix(lluvias_data_variables, emb_gg, mean, dist = T)
attr(s, "Labels") <- lluvias_data_provinces
d <- 1-s
hc <- hclust(d, method = "average")

d <- as.dendrogram(hc)
d <- d %>% color_branches(k=k)
par(mar = c(3,1,1,7))
dend_gg <- plot(d, horiz  = TRUE)

clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_gg <- plot_mapa_clusters(clusters, "Goguen")


##### EMB_GD ###################################################################

s <- sim_emb_matrix(lluvias_data_variables, emb_gd, mean, dist = T)
attr(s, "Labels") <- lluvias_data_provinces
d <- 1-s
hc <- hclust(d, method = "average")

d <- as.dendrogram(hc)
d <- d %>% color_branches(k=k)
par(mar = c(3,1,1,7))
plot(d, horiz  = TRUE)

clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_gd <- plot_mapa_clusters(clusters, "Godel")

##### EMB_FD ###################################################################

s <- sim_emb_matrix(lluvias_data_variables, emb_fd, mean, dist = T)
attr(s, "Labels") <- lluvias_data_provinces
d <- 1-s
hc <- hclust(d, method = "average")

d <- as.dendrogram(hc)
d <- d %>% color_branches(k=k)
par(mar = c(3,1,1,7))
dend_fd <- plot(d, horiz  = TRUE)

clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_fd <- plot_mapa_clusters(clusters, "Fodor")

##### EMB_RS ###################################################################

s <- sim_emb_matrix(lluvias_data_variables, emb_rs, mean, dist = T)
attr(s, "Labels") <- lluvias_data_provinces
d <- 1-s
hc <- hclust(d, method = "average")

d <- as.dendrogram(hc)
d <- d %>% color_branches(k=k)
par(mar = c(3,1,1,7))
dend_rs <- plot(d, horiz  = TRUE)

clusters <- data.frame(cluster = factor(cutree(hc, k))) %>%
  rownames_to_column("provincia")
mapa_rs <- plot_mapa_clusters(clusters, "Rescher")

############## PLOT

library(patchwork)

(mapa_w | mapa_lk) /
  (mapa_gg | mapa_gd) /
  (mapa_fd | mapa_rs)

