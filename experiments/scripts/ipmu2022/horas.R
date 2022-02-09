library(lubridate)

horas <- read_delim("experiments/scripts/ipmu2022/horas", delim = ";",
                    skip = 1,
                    col_names = c("ciudad", "sunrise", "sunset", "daylight"))
horas <- horas %>%
  mutate(ciudad = str_remove_all(ciudad, "\t| "),
         ciudad = str_replace_all(ciudad, "_", " "),
         ciudad = as.factor(ciudad)) %>%
  mutate(provincia = c("islas baleares",
                       "barcelona",
                       "barcelona",
                       "valencia",
                       "alicante",
                       "alicante",
                       "zaragoza",
                       "murcia",
                       "vitoria",
                       "alava",
                       "granada",
                       "madrid",
                       "malaga",
                       "valladolid",
                       "cordoba",
                       "asturias",
                       "sevilla",
                       "a coruna",
                       "vigo",
                       "las palmas"))

to_minutes <- function(time) {
  return(hour(time)*60 + minute(time))
}

horas <- horas %>%
  mutate(sunrise = to_minutes(sunrise),
         sunset = to_minutes(sunset),
         daylight = to_minutes(daylight))

ggplot(horas, aes(x = 0, y = ciudad)) +
  geom_segment(aes(x = sunrise, xend = sunset, yend = ciudad)) +
  geom_point(aes(x = sunrise)) +
  geom_point(aes(x = sunset)) +
  theme_bw()

# normalize
horas <- horas %>%
  mutate(sunrise = sunrise/(24*60),
         sunset = sunset/(24*60),
         daylight = daylight/(24*60))
horas_dist <- as.data.frame(horas[, 2:3])
rownames(horas_dist) <- horas %>% pull("ciudad")
horas_dist[,3] <- 0
horas_dist[,4] <- 0


horas_w <- sim_emb_matrix(horas_dist, sim_w, mean, dist = T)
attr(horas_w, "Labels") <- horas %>% pull("ciudad")

library(qgraph)
# jpeg('example_forcedraw.jpg', width=1000, height=1000, unit='px')
qgraph(horas_w, layout='spring', vsize=6)
# dev.off()

# horas_w <- sim_emb_matrix(horas_dist, sim_w, mean, dist = F)
# dim <- ncol(horas_w)
#
# image(1:dim, 1:dim, horas_w, axes = FALSE, xlab="", ylab="")
#
# axis(1, 1:dim, colnames(horas_w), cex.axis = 0.5, las=3)
# axis(2, 1:dim, colnames(horas_w), cex.axis = 0.5, las=1)
# text(expand.grid(1:dim, 1:dim), sprintf("%0.5f", horas_w), cex=0.6)

library(reshape2)
library(ggplot2)

# width

create_matrix <- function(sim) {

  horas <- horas %>% arrange(ciudad)
  res <- lapply(1:(nrow(horas)-1), function(i) {
    a <- horas[i,2:3] %>% as.numeric()
    res <- sapply((i+1):nrow(horas), function(j) {
      b <- horas[j,2:3] %>% as.numeric()
      s <- similarity(a, b, sim)
    })
    res <- tibble(c1 = horas[i,] %>% pull(ciudad),
                  c2 = horas[(i+1):nrow(horas),] %>% pull(ciudad),
                  value = res)
    res
  }) %>%
    bind_rows() # %>%
    # mutate(valueChar = substr(as.character(value), 1, 4),
    #        diag = FALSE) %>%
    # bind_rows(
    #   tibble(c1 = horas %>% arrange(ciudad) %>% pull(ciudad),
    #          c2 = horas %>% arrange(ciudad) %>% pull(ciudad),
    #          valueChar = horas %>% arrange(ciudad) %>% pull(ciudad) %>% as.character(),
    #          diag = TRUE)
    # )

  # no descomentar esto
  # horas_w <- sim_emb_matrix(horas_dist, sim, mean, dist = F)
  # colnames(horas_w) <- rownames(horas_dist)
  # rownames(horas_w) <- rownames(horas_dist)[1:(length(rownames(horas_dist))-1)]
  # melted_horas <- melt(horas_w)
  # ggplot(data = melted_horas %>%
  #          filter(value != 0), aes(x=Var1, y=Var2, fill=value
  #                                  )) +

  ggplot(res, aes(c1, c2, fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label=round(value,3)), color = "white", size = 3) +#, hjust = "left") +
    # scale_color_manual(values = c("white", "black")) +
    scale_x_discrete(position = "top") +
    theme_classic() +
    labs(x = NULL, y = NULL) +
    scale_fill_gradient(low = "red",
                        high = "darkgreen",
                        na.value = NA,
                        limits = c(0.85, 1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0),
          axis.text.y = element_text(angle = 45),
          legend.position = "bottom")

}

create_matrix_dist <- function() {

  h <- horas %>% arrange(ciudad) %>% pull(daylight)
  names(h) <- horas %>% arrange(ciudad) %>% pull(ciudad)
  horas_dist <- as.matrix(dist(h))
  horas_dist <- 1 - horas_dist
  horas_dist[lower.tri(horas_dist, diag = T)] <- 0
  colnames(horas_dist) <- horas %>% arrange(ciudad) %>% pull(ciudad)
  rownames(horas_dist) <- horas %>% arrange(ciudad) %>% pull(ciudad)
  melted_horas <- melt(horas_dist) %>%
    filter(value != 0)
  ggplot(data = melted_horas , aes(x=Var1, y=Var2, fill=value)) +
    geom_tile(color = "white") +
    geom_text(aes(label=round(value, 3)), size = 2, color = "white") +
    scale_x_discrete(position = "top") +
    theme_classic() +
    labs(x = NULL, y = NULL) +
    scale_fill_gradient(low = "red",
                        high = "darkgreen",
                        na.value = NA,
                        limits = c(0.85, 1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0),
          legend.position = "bottom")

}

library(patchwork)
create_matrix("dice") + ggtitle("Dice")
create_matrix("jaccard") + ggtitle("Jaccard")
create_matrix("mean") + ggtitle("Mean")
create_matrix("min") + ggtitle("Min")
create_matrix("product") + ggtitle("Product")

create_ranking <- function(sim, type) {

  horas <- horas %>% arrange(ciudad)
  res <- lapply(1:(nrow(horas)-1), function(i) {
    a <- horas[i,2:3] %>% as.numeric()
    res <- sapply((i+1):nrow(horas), function(j) {
      b <- horas[j,2:3] %>% as.numeric()
      s <- similarity(a, b, sim)
    })
    res <- tibble(c1 = horas[i,] %>% pull(ciudad),
                  c2 = horas[(i+1):nrow(horas),] %>% pull(ciudad),
                  value = res)
    res
  }) %>% bind_rows() %>%
    mutate(pair = paste(c1, c2, sep = " - "))

  if(type == "max") {
    res <- res %>%
      slice_max(value, n = 20)
  }
  else if(type == "min") {
    res <- res %>%
      slice_min(value, n = 20)
  }
  else {
    stop("error create ranking")
  }
  ggplot(res, aes(reorder(pair, -value), value, fill = value)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(pair, " (", round(value,4), ")")),
              angle = 90,
              color = "white",
              size = 2,
              position = position_stack(vjust = 0.5)) +
    scale_fill_gradient(low = "red",
                        high = "darkgreen",
                        na.value = NA,
                        limits = c(0.85, 1)) +
    labs(x = NULL, y  = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_blank()) +
    scale_y_continuous(limits = c(0, 1)) +
    ggtitle(paste(type, sim))
}

create_ranking("dice", "min") +
  create_ranking("jaccard", "min") +
  create_ranking("mean", "min") +
  create_ranking("min", "min") +
  create_ranking("product", "min") +
  plot_layout(ncol = 5, guides = "collect")

create_ranking("dice", "max") +
  create_ranking("jaccard", "max") +
  create_ranking("mean", "max") +
  create_ranking("min", "max") +
  create_ranking("product", "max") +
  plot_layout(ncol = 5, guides = "collect")
