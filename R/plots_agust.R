library(tibble)
x <-  tibble(min_a = sample(1:5, 10, T),
             max_a = sample(5:10, 10, T),
             min_b = sample(1:5, 10, T),
             max_b = sample(5:10, 10, T),
             id = as.factor(as.numeric(t(replicate(2, 1:5)))),
             y = as.numeric(replicate(5, c(0,0.5))))

ggplot(x) +
  geom_segment(aes(x = min_a, xend = max_a, y = y, yend = y), size = 3) +
  geom_segment(aes(x = min_b, xend = max_b, y = y, yend = y), size = 3) +
  facet_grid(id~.) +
  theme_classic() +
  theme(panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(),
        axis.line.y = element_blank(),

        axis.title.y.right = element_blank(),                # hide right axis title
        axis.text.y.right = element_blank(),                 # hide right axis labels
        axis.ticks.y = element_blank(),                      # hide left/right axis ticks
        axis.text.y = element_text(margin = margin(r = 0)),  # move left axis labels closer to axis
        panel.spacing = unit(20, "mm"),                       # remove spacing between facets
        strip.background = element_rect(size = 0.5))


