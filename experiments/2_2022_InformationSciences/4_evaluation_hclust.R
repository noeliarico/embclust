library(fpc)

cluster.stats(dist(d_maxs),  cutree(hc_max_single, k))$dunn
cluster.stats(dist(d_maxs),  cutree(hc_max_complete, k))$dunn
cluster.stats(dist(d_maxs),  cutree(hc_max_average, k))$dunn

cluster.stats(dist(d_mins),  cutree(hc_min_single, k))$dunn
cluster.stats(dist(d_mins),  cutree(hc_min_complete, k))$dunn
cluster.stats(dist(d_mins),  cutree(hc_min_average, k))$dunn


cluster.stats(d_emb_w,  cutree(hc_emb_w, k))$dunn
cluster.stats(d_emb_lk,  cutree(hc_emb_lk, k))$dunn
cluster.stats(d_emb_gg,  cutree(hc_emb_gg, k))$dunn
cluster.stats(d_emb_gd,  cutree(hc_emb_gd, k))$dunn
cluster.stats(d_emb_fd,  cutree(hc_emb_fd, k))$dunn
cluster.stats(d_emb_rs,  cutree(hc_emb_rs, k))$dunn

library(factoextra)
fviz_dend(hc_max_average, show_labels = TRUE,
          palette = "jco", as.ggplot = TRUE)

# load code of A2R function
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")

# colored dendrogram
op = par(bg = "#EFEFEF")
A2Rplot(hc_max_average, k = 5, boxes = TRUE, col.up = "gray50")

library(ggdendro)
ggdendrogram(hc_max_average, rotate = TRUE, size = 2, )

# Así salen distintos

cluster.stats(dist(d_maxs),  cutree(hc_max_single, k))$avg.silwidth
cluster.stats(dist(d_maxs),  cutree(hc_max_complete, k))$avg.silwidth
cluster.stats(dist(d_maxs),  cutree(hc_max_average, k))$avg.silwidth

cluster.stats(dist(d_mins),  cutree(hc_min_single, k))$avg.silwidth
cluster.stats(dist(d_mins),  cutree(hc_min_complete, k))$avg.silwidth
cluster.stats(dist(d_mins),  cutree(hc_min_average, k))$avg.silwidth
