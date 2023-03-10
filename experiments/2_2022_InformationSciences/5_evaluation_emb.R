library(fossil)

#define clusters
method1 <- c(1, 1, 1, 2, 2)
method2 <- c(1, 1, 2, 2, 3)

method1 <- c(1, 1, 1, 2, 2, 3)
method2 <- c(2, 2, 2, 3, 3, 1)

#calculate Rand index between clustering methods
rand.index(method1, method2)

[1] 0.6


# Single
rownames(maxs) <- data_provinces
hc.res <- eclust(maxs, "hclust", k = 5, hc_metric = "euclidean",
                 hc_method = "single", graph = FALSE)
fviz_dend(hc.res, show_labels = TRUE,
          palette = "jco", as.ggplot = TRUE)
# Complete
hc.res <- eclust(maxs, "hclust", k = 5, hc_metric = "euclidean",
                 hc_method = "complete", graph = FALSE)
fviz_dend(hc.res, show_labels = TRUE,
          palette = "jco", as.ggplot = TRUE)
# Average
hc.res <- eclust(maxs, "hclust", k = 5, hc_metric = "euclidean",
                 hc_method = "average", graph = FALSE)
fviz_dend(hc.res, show_labels = TRUE,
          palette = "jco", as.ggplot = TRUE)

# Single
rownames(mins) <- data_provinces
hc.cut <- hcut(mins, k = 5, hc_method = "single")
fviz_dend(hc.cut, show_labels = TRUE, rect = TRUE)
fviz_silhouette(hc.cut)
# Complete
rownames(mins) <- data_provinces
hc.cut <- hcut(mins, k = 5, hc_method = "complete")
fviz_dend(hc.cut, show_labels = TRUE, rect = TRUE)
fviz_silhouette(hc.cut)
# Average
rownames(mins) <- data_provinces
hc.cut <- hcut(mins, k = 5, hc_method = "average")
fviz_dend(hc.cut, show_labels = TRUE, rect = TRUE)
fviz_silhouette(hc.cut)
fviz_dist(d)

hc.res <- eclust(mins, "hclust", k = 5, hc_metric = "euclidean",
                 hc_method = "single", graph = FALSE)
fviz_dend(hc.res, show_labels = TRUE,
          palette = "jco", as.ggplot = TRUE)
# Complete
hc.res <- eclust(mins, "hclust", k = 5, hc_metric = "euclidean",
                 hc_method = "complete", graph = FALSE)
fviz_dend(hc.res, show_labels = TRUE,
          palette = "jco", as.ggplot = TRUE)
# Average
hc.res <- eclust(mins, "hclust", k = 5, hc_metric = "euclidean",
                 hc_method = "average", graph = FALSE)
fviz_dend(hc.res, show_labels = TRUE,
          palette = "jco", as.ggplot = TRUE)
