######################

library(stringr)

to_plot <- ej %>%
  # mutate(interval1 = paste0("[", round(aL, 2), ",", round(aR, 2), "]"),
  #        interval2 = paste0("[", round(bL, 2), ",", round(bR, 2), "]")) %>%
  group_by(ejemplo) %>%
  mutate(i = factor(row_number(ejemplo))) %>%
  ungroup() %>%
  pivot_longer(starts_with("s"), names_to = "sim", values_to = "value")

to_plot_intervals <- to_plot %>%
  pivot_longer(cols = c("aL", "bL"), names_to = "left", values_to = "left_value") %>%
  pivot_longer(cols = c("aR", "bR"), names_to = "right", values_to = "right_value") %>%
  filter((left == "aL" & right == "aR") |
           (left == "bL" & right == "bR")) %>%
  mutate(interval = paste0("[", round(left_value, 2), ",", round(right_value, 2), "]"),
         y = ifelse(str_detect(left, "a"), 1, 2)) %>%
  select(ejemplo, i, left_value, right_value, left, right, interval, y) %>%
  distinct()

# Bar plot for each comparison where each bar is a similarity measure
sims <- ggplot(to_plot %>% filter(ejemplo == 6),
               aes(sim, value, fill = sim)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = str_pad(round(value, 3), width = 5, pad = "0", side = "right"))) +
  facet_grid(i ~ .) +
  scale_y_continuous(position = "right", limits = c(0,1)) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),

        axis.title.y = element_blank(),
        #axis.ticks.y = element_blank(),
        #axis.text.y.left =element_blank(),

        axis.text.y = element_text(margin = margin(r = 0)),  # move left axis labels closer to axis
        panel.spacing = unit(5, "mm"),                       # remove spacing between facets
        strip.background = element_rect(size = 0.5),
        legend.position = "none",
        strip.text.y = element_blank()) +
  xlab("Similarity")

intervals <- ggplot(to_plot_intervals %>%
                      filter(ejemplo == 6)) +
  geom_segment(aes(x = left_value, xend = right_value, y = y, yend = y), size = 3) +
  geom_segment(aes(x = left_value, xend = right_value, y = y, yend = y), size = 3) +
  geom_text(aes(label = interval,
                x = ((right_value-left_value)/2)+left_value,
                y = ifelse(y == 1, y-.75, y+.75)),
            check_overlap = T,
            nudge_x = 0) +
  scale_y_continuous(limits = c(-1.5,3.5)) +
  scale_x_continuous(n.breaks = 10, limits = c(0,1)) +
  facet_grid(i~., switch = "y") +
  theme_classic() +
  theme(panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(),
        axis.line.y = element_blank(),


        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y.left =element_blank(),

        axis.text.y = element_text(margin = margin(r = 0)),  # move left axis labels closer to axis
        panel.spacing = unit(5, "mm"),                       # remove spacing between facets
        strip.background = element_rect(size = 0.5)) +
  xlab("Intervals")

intervals + sims



