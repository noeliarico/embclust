compute_similarities <- function(a, b) {
  tibble(aL = a[1],
         aR = a[2],
         bL = b[1],
         bR = b[2],
         sJaccard = similarity(a, b, "jaccard"),
         sDice = similarity(a, b, "dice"),
         sProd = similarity(a, b, "product"),
         sMin = similarity(a, b, "min"),
         sMean = similarity(a, b, "mean"),
         sgMean = similarity(a, b, "gmean"),
         perc = (b[2]-a[1])/.5)
}

a <- c(0.5, 1)
ej_sim <- lapply(seq(0, 0.5, 0.05), function(i) {
  b <- c(0+i, 0.5+i)
  compute_similarities(a, b)
}) %>% bind_rows()
ej_sim <- ej_sim %>%
  pivot_longer(cols = starts_with("s"), names_to = "sim") %>%
  mutate(sim = ifelse(sim == "sDice", "S[D]", sim),
         sim = ifelse(sim == "sJaccard", "S[J]", sim),
         sim = ifelse(sim == "sMin", "S[min]", sim),
         sim = ifelse(sim == "sMean", "S[mean]", sim),
         sim = ifelse(sim == "sProd", "S[prod]", sim),
         sim = ifelse(sim == "sgMean", "S[g-mean]", sim),
         sim = ordered(sim, levels = c("S[J]",
                                       "S[D]",
                                       "S[prod]",
                                       "S[min]",
                                       "S[mean]",
                                       "S[g-mean]")))

tema <- theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 12),
        plot.title = element_text(size = 11),
        #plot.margin = unit(c(.5,.5,.5,.5), "cm"),
        axis.title = element_text(size = 11),
        axis.title.x = element_text(vjust=-2),
        axis.title.y = element_text(vjust=2),
        panel.border = element_rect(fill = "transparent"))

ggplot(ej_sim, aes(perc, value, color = sim, shape = sim)) +
  geom_line(size = 2, alpha = .3) +
  geom_point(size = 3) +
  labs(x = "Overlapping", y = "Similarity", shape = "Similarity", color = "Similarity", parse = TRUE) +
  scale_shape_manual(values = c(3, 5, 1, 4, 2, 6), labels = scales::parse_format()) +
  scale_color_manual(values = paleta, labels = scales::parse_format()) +
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) +
  tema +
  coord_fixed()

ggplot(ej_sim, aes(perc, value, color = sim, shape = sim)) +
  geom_line(size = 2, alpha = .3) +
  geom_point(size = 3) +
  facet_wrap(~sim, ncol = 3)+
  labs(x = "Overlapping", y = "Similarity", shape = "Similarity", color = "Similarity", parse = TRUE) +
  scale_shape_manual(values = c(3, 5, 1, 4, 2, 6), labels = scales::parse_format()) +
  scale_color_manual(values = paleta, labels = scales::parse_format()) +
  scale_x_continuous(labels = scales::percent) +
  tema +
  coord_fixed()


