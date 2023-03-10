library(lubridate)

horas <- read_delim("experiments/scripts/ipmu2022/horas_enero", delim = ";",
                    skip = 1,
                    col_names = c("ciudad", "sunrise", "sunset", "daylight"))
horas <- readxl::read_excel("experiments/scripts/ipmu2022/horas_capitales.xlsx",
                    col_names = c("país", "ciudad", "sunrise", "sunset"))


horas <- horas %>%
  mutate(ciudad = str_remove_all(ciudad, "\t| "),
         ciudad = str_replace_all(ciudad, "_", " "),
         ciudad = as.factor(ciudad))


to_minutes <- function(time) {
  return(hour(time)*60 + minute(time))
}

horas_filtered <-
  horas %>% filter(ciudad %in%
                     c("Madrid",
                       "Viena",
                       "Estocolmo",
                       "Paris",
                       "Sarajevo",
                       "Dublin",
                       "Roma")) %>%
  mutate(
                         ciudad = as.character(ciudad),
                         ciudad = ifelse(ciudad == "Viena", "Vienna", ciudad),
                         ciudad = ifelse(ciudad == "Roma", "Rome", ciudad),
                         ciudad = ifelse(ciudad == "Estocolmo", "Stockholm", ciudad),
                         ciudad = as.factor(ciudad)
                       ) %>%
  mutate(sunrise_print = paste(str_pad(hour(sunrise), 2, "left", 0),str_pad(minute(sunrise), 2, "left", 0), sep=":"),
         sunset_print = paste(str_pad(hour(sunset), 2, "left", 0),str_pad(minute(sunset), 2, "left", 0), sep=":"),
         sunrise = to_minutes(sunrise),
         sunset = to_minutes(sunset))

ggplot(horas_filtered, aes(x = 0, y = ciudad)) +
  geom_segment(aes(x = sunrise, xend = sunset, yend = ciudad)) +
  geom_point(aes(x = sunrise)) +
  geom_point(aes(x = sunset)) +
  geom_text(size = 3, aes(x = sunrise+50, label = sunrise_print), nudge_y = 0.45, family = "Times New Roman") +
  geom_text(size = 3, aes(x = sunset-50, label = sunset_print), nudge_y = 0.45, family = "Times New Roman") +
  labs(x = "", y = "City") +
  scale_y_discrete(expand = c(0, 1.4)) +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

horas <- horas %>%
  mutate(sunrise = to_minutes(sunrise),
         sunset = to_minutes(sunset))

ggplot(horas, aes(x = 0, y = ciudad)) +
  geom_segment(aes(x = sunrise, xend = sunset, yend = ciudad)) +
  geom_point(aes(x = sunrise)) +
  geom_point(aes(x = sunset)) +
  geom_text(aes(x = sunrise, label = sunrise), nudge_y = 0.5) +
  geom_text(aes(x = sunset, label = sunset), nudge_y = 0.5) +
  theme_bw() +
  theme()

horas <- horas_filtered %>% select(ciudad, sunrise, sunset)

# mirar intersección -----------------------------------------------------------

perc_inter <- function(a, b) {
  # left <- min(a[1], b[1])
  # right <- max(a[2], b[2])
  # inter <- embclust::intersection(a, b, interval = T)
  # return( (inter[2]-inter[1]) / (right-left) )

  #
  inter <- embclust::intersection(a, b, interval = T)
  return( inter[2]-inter[1] )

}

inter <- lapply(1:(nrow(horas)-1), function(i) {
            a <- horas[i,2:3] %>% as.numeric()
            res <- sapply((i+1):nrow(horas), function(j) {
              b <- horas[j,2:3] %>% as.numeric()
              p <- perc_inter(a, b)
              p
            })
            res <- tibble(c1 = horas[i,] %>% pull(ciudad),
                          c2 = horas[(i+1):nrow(horas),] %>% pull(ciudad),
                          pi = res)
            res
          }) %>%
  bind_rows()

p_inter <- ggplot(inter, aes(c1, c2, fill = pi)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(pi, 3)), color = "white") +
  #scale_fill_manual(values = c("white", "black")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# mirar si está dentro ---------------------------------------------------------

is_inside <- function(a, b) {
  # true if b inside a
  return( (a[1] <= b[1]) && (b[2] <= a[2]) )
}

inside <- lapply(1:(nrow(horas)-1), function(i) {
              c1 <- horas[i,2:3] %>% as.numeric()
              res <- sapply((i+1):nrow(horas), function(j) {
                c2 <- horas[j,2:3] %>% as.numeric()
                c(is_inside(c2, c1), is_inside(c1, c2))
              })
              res <- tibble(c1 = horas[i,] %>% pull(ciudad),
                            c2 = horas[(i+1):nrow(horas),] %>% pull(ciudad),
                            c1_inside_c2 = res[1,],
                            c2_inside_c1 = res[2,])
              res
            }) %>%
              bind_rows()

inside2  <- inside %>%
  select(c1, c2, c2_inside_c1) %>%
  rename(value = c2_inside_c1) %>%
  rename(c3 = c2) %>%
  rename(c2 = c1) %>%
  rename(c1 = c3) %>%
  select(c1, c2, value)

p_inside <- ggplot(inside %>%
                     select(c1, c2, c1_inside_c2) %>%
                     rename(value = c1_inside_c2) %>%
         bind_rows(inside2),
       aes(c2, c1, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = c("white", "black")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

p_inter + p_inside

# normalize
horas <- horas %>%
  mutate(sunrise = sunrise/(24*60),
         sunset = sunset/(24*60))
horas_dist <- as.data.frame(horas[, 2:3])
rownames(horas_dist) <- horas %>% pull("ciudad")



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

  horas <- horas %>% mutate(
    ciudad = as.character(ciudad),
    ciudad = ifelse(ciudad == "Viena", "Vienna", ciudad),
    ciudad = ifelse(ciudad == "Roma", "Rome", ciudad),
    ciudad = ifelse(ciudad == "Estocolmo", "Stockholm", ciudad),
    ciudad = as.factor(ciudad)
  ) %>%
    arrange(ciudad) %>% select(ciudad, sunrise, sunset)
  #horas <- horas[2:4]
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
    geom_text(aes(label=round(value,3)), color = "white", size = 3,
              family = "Times New Roman") +#, hjust = "left") +
    # scale_color_manual(values = c("white", "black")) +
    scale_x_discrete(position = "top") +
    theme_classic() +
    labs(x = NULL, y = NULL) +
    scale_fill_gradient(low = "red",
                        high = "darkgreen",
                        na.value = NA,
                        limits = c(0.4, 1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0),
          #axis.text.y = element_text(angle = 45),
          legend.position = "none",
          text  = element_text(family = "Times New Roman"))

}

create_matrix_dist <- function() {

  h <- horas %>% arrange(ciudad) %>% pull(daylight)
  names(h) <- horas %>% arrange(ciudad) %>% pull(ciudad)
  horas_dist <- as.matrix(dist(h))
  horas_dist <- 1 - horas_dist
  horas_dist[lower.tri(horas_dist, diag = T)] <- 0
  #colnames(horas_dist) <- horas %>% arrange(ciudad) %>% pull(ciudad)
  #rownames(horas_dist) <- horas %>% arrange(ciudad) %>% pull(ciudad)
  melted_horas <- melt(horas_dist) %>%
    filter(value != 0)
  ggplot(data = melted_horas , aes(x=Var1, y=Var2, fill=value)) +
    geom_tile(color = "white") +
    geom_text(aes(label=round(value, 3)), size = 2, color = "white",
              family = "Times New Roman") +
    scale_x_discrete(position = "top") +
    theme_classic() +
    labs(x = NULL, y = NULL) +
    scale_fill_gradient(low = "red",
                        high = "darkgreen",
                        na.value = NA,
                        limits = c(0.85, 1)) +
    theme(#axis.text.x = element_text(angle = 90, hjust = 0),
          legend.position = "bottom",
          text = element_text(family = "Times New Roman"))

}

library(patchwork)
((create_matrix("dice") + ggtitle(parse(text = "S[D]"))) |
(create_matrix("jaccard") + ggtitle(parse(text = "S[J]")))) /
((create_matrix("product") + ggtitle(parse(text = "S[product]"))) |
(create_matrix("min") + ggtitle(parse(text = "S[min]")))) /
create_matrix("mean") + ggtitle(parse(text = "S[mean]")) +
  plot_layout(guides = "collect")

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


zaragoza <- horas[horas$ciudad == "Zaragoza", 2:3] %>% as.numeric()
madrid <- horas[horas$ciudad == "Madrid", 2:3] %>% as.numeric()
vitoria <- horas[horas$ciudad == "Vitoria", 2:3] %>% as.numeric()
alicante <- horas[horas$ciudad == "Alicante", 2:3] %>% as.numeric()

similarity(zaragoza, madrid, "dice")
similarity(vitoria, alicante, "dice")

similarity(zaragoza, madrid, "jaccard")
similarity(vitoria, alicante, "jaccard")

similarity(zaragoza, madrid, "mean")
similarity(vitoria, alicante, "mean")


#########

horas <- horas %>% arrange(ciudad) %>% select(ciudad, sunrise, sunset)
dice <- lapply(1:(nrow(horas)-1), function(i) {
  a <- horas[i,2:3] %>% as.numeric()
  res <- sapply((i+1):nrow(horas), function(j) {
    b <- horas[j,2:3] %>% as.numeric()
    s <- similarity(a, b, "dice")
  })
  res <- tibble(c1 = horas[i,] %>% pull(ciudad),
                c2 = horas[(i+1):nrow(horas),] %>% pull(ciudad),
                dice = res)
  res
}) %>%
  bind_rows() %>%
  pivot_wider(names_from = c2, values_from = dice)

jaccard <- lapply(1:(nrow(horas)-1), function(i) {
  a <- horas[i,2:3] %>% as.numeric()
  res <- sapply((i+1):nrow(horas), function(j) {
    b <- horas[j,2:3] %>% as.numeric()
    s <- similarity(a, b, "jaccard")
  })
  res <- tibble(c1 = horas[i,] %>% pull(ciudad),
                c2 = horas[(i+1):nrow(horas),] %>% pull(ciudad),
                jaccard = res)
  res
}) %>%
  bind_rows() %>%
  pivot_wider(names_from = c2, values_from = jaccard)

mean <- lapply(1:(nrow(horas)-1), function(i) {
  a <- horas[i,2:3] %>% as.numeric()
  res <- sapply((i+1):nrow(horas), function(j) {
    b <- horas[j,2:3] %>% as.numeric()
    s <- similarity(a, b, "mean")
  })
  res <- tibble(c1 = horas[i,] %>% pull(ciudad),
                c2 = horas[(i+1):nrow(horas),] %>% pull(ciudad),
                mean = res)
  res
}) %>%
  bind_rows() %>%
  pivot_wider(names_from = c2, values_from = mean)

min <- lapply(1:(nrow(horas)-1), function(i) {
  a <- horas[i,2:3] %>% as.numeric()
  res <- sapply((i+1):nrow(horas), function(j) {
    b <- horas[j,2:3] %>% as.numeric()
    s <- similarity(a, b, "min")
  })
  res <- tibble(c1 = horas[i,] %>% pull(ciudad),
                c2 = horas[(i+1):nrow(horas),] %>% pull(ciudad),
                min = res)
  res
}) %>%
  bind_rows() %>%
  pivot_wider(names_from = c2, values_from = min)



######

mean <- lapply(1:(nrow(horas)-1), function(i) {
  a <- horas[i,2:3] %>% as.numeric()
  res <- sapply((i+1):nrow(horas), function(j) {
    b <- horas[j,2:3] %>% as.numeric()
    s <- similarity(a, b, "mean")
  })
  res <- tibble(c1 = horas[i,] %>% pull(ciudad),
                c2 = horas[(i+1):nrow(horas),] %>% pull(ciudad),
                value = res)
  res
}) %>%
  bind_rows() %>%
  mutate(pair = paste(c1, "-", c2),
         c1 = NULL, c2 = NULL) %>%
  select(pair, value) %>%
  deframe()
mean <- melt(outer(round(mean, 3), round(mean, 3), "==")) %>% mutate(sim = "mean")

jaccard <- lapply(1:(nrow(horas)-1), function(i) {
  a <- horas[i,2:3] %>% as.numeric()
  res <- sapply((i+1):nrow(horas), function(j) {
    b <- horas[j,2:3] %>% as.numeric()
    s <- similarity(a, b, "jaccard")
  })
  res <- tibble(c1 = horas[i,] %>% pull(ciudad),
                c2 = horas[(i+1):nrow(horas),] %>% pull(ciudad),
                value = res)
  res
}) %>%
  bind_rows() %>%
  mutate(pair = paste(c1, "-", c2),
         c1 = NULL, c2 = NULL) %>%
  select(pair, value) %>%
  deframe()
jaccard <- melt(outer(round(jaccard, 3), round(jaccard, 3), "==")) %>% mutate(sim = "jaccard")


dice <- lapply(1:(nrow(horas)-1), function(i) {
  a <- horas[i,2:3] %>% as.numeric()
  res <- sapply((i+1):nrow(horas), function(j) {
    b <- horas[j,2:3] %>% as.numeric()
    s <- similarity(a, b, "dice")
  })
  res <- tibble(c1 = horas[i,] %>% pull(ciudad),
                c2 = horas[(i+1):nrow(horas),] %>% pull(ciudad),
                value = res)
  res
}) %>%
  bind_rows() %>%
  mutate(pair = paste(c1, "-", c2),
         c1 = NULL, c2 = NULL) %>%
  select(pair, value) %>%
  deframe()
dice <- melt(outer(round(dice, 3), round(dice, 3), "==")) %>% mutate(sim = "dice")

min <- lapply(1:(nrow(horas)-1), function(i) {
  a <- horas[i,2:3] %>% as.numeric()
  res <- sapply((i+1):nrow(horas), function(j) {
    b <- horas[j,2:3] %>% as.numeric()
    s <- similarity(a, b, "min")
  })
  res <- tibble(c1 = horas[i,] %>% pull(ciudad),
                c2 = horas[(i+1):nrow(horas),] %>% pull(ciudad),
                value = res)
  res
}) %>%
  bind_rows() %>%
  mutate(pair = paste(c1, "-", c2),
         c1 = NULL, c2 = NULL) %>%
  select(pair, value) %>%
  deframe()
min <- melt(outer(round(min, 3), round(min, 3), "==")) %>% mutate(sim = "min")


comparative <- bind_rows(mean, min) %>%
  as_tibble() %>%
  filter(Var1 != Var2) %>%
  pivot_wider(names_from = sim, values_from = value)

comparative %>% filter(min, !mean)

get_values <- function(comparative) {
r <- tibble()
  for(i in 1:nrow(comparative)) {
  x <- comparative %>% slice(i)
  v1 <- x %>% pull(Var1)
  v2 <- x %>% pull(Var2)
  v11 <- str_split(v1, " - ", simplify = T)[1]
  v21 <- str_split(v2, " - ", simplify = T)[1]
  v12 <- str_split(v1, " - ", simplify = T)[2]
  v22 <- str_split(v2, " - ", simplify = T)[2]
  r <- bind_rows(r,
         bind_rows(
         # dice1 = dice %>% filter(c1 == v11 & c2 == v12),
         # dice2 = dice %>% filter(c1 == v21 & c2 == v22),
         # jaccard1 = jaccard %>% filter(c1 == v11 & c2 == v12),
         # jaccard2 = jaccard %>% filter(c1 == v21 & c2 == v22),
         mean1 = mean %>% filter(c1 == v11 & c2 == v12),
         mean2 = mean %>% filter(c1 == v21 & c2 == v22),
         min1 = min %>% filter(c1 == v11 & c2 == v12),
         min2 = min %>% filter(c1 == v21 & c2 == v22),
         )
  )
  }
r
}
r <- get_values(comparative %>% filter(min, !mean))
r <- r %>% pivot_longer(-c("c1", "c2")) %>% drop_na() %>% distinct() %>% pivot_wider(names_from = name, values_from = value)
r <- r %>% mutate(name = paste(c1, "-", c2), c1 = NULL, c2 = NULL)
r <- comparative %>%
  filter(min, !mean) %>%
  select(Var1, Var2) %>%
  inner_join(r, by = c("Var1" = "name")) %>%
  # rename(dice1 = dice) %>%
  # rename(jaccard1 = jaccard) %>%
  rename(mean1 = mean) %>%
  rename(min1 = min) %>%
  inner_join(r, by = c("Var2" = "name")) %>%
  # rename(dice2 = dice) %>%
  # rename(jaccard2 = jaccard) %>%
  rename(mean2 = mean) %>%
  rename(min2 = min)
