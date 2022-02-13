compute_similarities <- function(a, b) {
  tibble(aL = a[1],
         aR = a[2],
         bL = b[1],
         bR = b[2],
         sDice = similarity(a, b, "dice"),
         sJaccard = similarity(a, b, "jaccard"),
         sMean = similarity(a, b, "mean"),
         sMin = similarity(a, b, "min"),
         sProd = similarity(a, b, "product"),
         perc = (b[2]-a[1])/.5)
}

a <- c(0.5, 1)
ej_sim <- lapply(seq(0, 0.5, 0.05), function(i) {
  b <- c(0+i, 0.5+i)
  compute_similarities(a, b)
}) %>% bind_rows()
ej_sim <- ej_sim %>%
  pivot_longer(cols = starts_with("s"), names_to = "sim")

tema <- theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 12),
        plot.title = element_text(size = 11),
        plot.margin = unit(c(.5,.5,.5,.5), "cm"),
        axis.title = element_text(size = 11),
        axis.title.x = element_text(vjust=-2),
        axis.title.y = element_text(vjust=2))

ggplot(ej_sim, aes(perc, value, color = sim, shape = sim)) +
  geom_line(size = 2, alpha = .3) +
  geom_point(size = 5) +
  labs(x = "Overlapping", y = "Similarity", shape = "Similarity", color = "Similarity") +
  scale_shape_manual(values = c(3, 5, 1, 4, 2)) +
  scale_x_continuous(labels = scales::percent) +
  tema


