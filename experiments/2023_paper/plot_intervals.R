######################

library(stringr)
library(patchwork)

#' this creates a dataframe with the data for each example with the colums
#' ejemplo: el número del ejemplo
#' aL: extremo inferior del intervalo a
#' aR: extremo superior del intervalo a
#' bL: extremo inferior del intervalo b
#' bR: extremo superior del intervalo b
#' alpha: el valor del parámetro alpha
#' i:
#' sim: el nombre de la función de similitud
#' value: el valor de la similitud entre a y b
to_plot <- ej %>%
  # mutate(interval1 = paste0("[", round(aL, 2), ",", round(aR, 2), "]"),
  #        interval2 = paste0("[", round(bL, 2), ",", round(bR, 2), "]")) %>%
  group_by(ejemplo) %>%
  mutate(i = factor(row_number(ejemplo))) %>%
  ungroup() %>%
  pivot_longer(starts_with("s"), names_to = "sim", values_to = "value") %>%
  mutate(sim = ifelse(sim == "sDice", "S[D]", sim),
         sim = ifelse(sim == "sJaccard", "S[J]", sim),
         sim = ifelse(sim == "sMin", "S[min]", sim),
         sim = ifelse(sim == "sMean", "S[mean]", sim),
         sim = ifelse(sim == "sProd", "S[prod]", sim),
         sim = ifelse(sim == "sgMean", "S[gMean]", sim))

#' pivotar las columnas aL, bL, aR, bL
#' ahora tenemos left_value, right_value y left, right además del
#' valor interval que tiene la forma impresa del intervalo
#' El valor de y indica la altura dentro del plot de ejemplo
to_plot_intervals <- to_plot %>%
  pivot_longer(cols = c("aL", "bL"), names_to = "left", values_to = "left_value") %>%
  pivot_longer(cols = c("aR", "bR"), names_to = "right", values_to = "right_value") %>%
  filter((left == "aL" & right == "aR") |
           (left == "bL" & right == "bR")) %>%
  mutate(interval = paste0("[", round(left_value, 2), ",", round(right_value, 2), "]"),
         y = ifelse(str_detect(left, "a"), 1, 2)) %>%
  select(ejemplo, i, left_value, right_value, left, right, interval, y) %>%
  distinct()

ejN <- 2

# Bar plot for each comparison where each bar is a similarity measure
sims <- ggplot(to_plot %>%
                 filter(ejemplo == ejN) %>%
                 # para forzar el orden en el que aparecen
                 mutate(sim = factor(sim, level = c("S[J]", "S[D]", "S[prod]", "S[min]", "S[mean]", "S[gMean]"))),
               aes(sim, value, fill = sim)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(value != 0, str_pad(round(value, 3), width = 5, pad = "0", side = "right"), 0)),
            family = "Times New Roman",
            nudge_y = 0.1, size = 3) +
  facet_grid(i ~ .) +
  scale_y_continuous(position = "right", limits = c(0,1.2), breaks = seq(0,1,0.25)) +
  scale_x_discrete(labels = scales::parse_format()) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),

        axis.title.y = element_blank(),
        #axis.ticks.y = element_blank(),
        #axis.text.y.left =element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),  # move left axis labels closer to axis
        panel.spacing = unit(5, "mm"),                       # remove spacing between facets
        strip.background = element_rect(linewidth = 0.5),
        legend.position = "none",
        strip.text.y = element_blank()) +
  xlab("Similarity")

intervals <- ggplot(to_plot_intervals %>%
                      filter(ejemplo == ejN)) +
  geom_segment(aes(x = left_value, xend = right_value, y = y, yend = y), size = 1.5) +
  geom_segment(aes(x = left_value, xend = right_value, y = y, yend = y), size = 1.5) +
  geom_text(aes(label = interval,
                x = ((right_value-left_value)/2)+left_value,
                y = ifelse(y == 1, y-1, y+1.2)),
            check_overlap = T,
            nudge_x = 0,
            family = "Times New Roman", size = 3) +
  scale_y_continuous(limits = c(-1.5,3.5)) +
  scale_x_continuous(n.breaks = 10, limits = c(0,1)) +
  facet_grid(i~., switch = "y") +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        panel.grid.major.x = element_line(),
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


#######################################################

ejN <- 1 # 8.59 x 2.19
# Bar plot for each comparison where each bar is a similarity measure
sims <- ggplot(to_plot %>% filter(ejemplo == ejN) %>%
                 mutate(sim = factor(sim, level = c("S[J]", "S[D]", "S[prod]", "S[min]", "S[mean]", "S[gMean]"))),
               aes(sim, value, fill = sim)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(value != 0, str_pad(round(value, 3), width = 5, pad = "0", side = "right"), 0)),
            family = "Times New Roman",
            nudge_y = 0.22, size = 3) +
  facet_grid(. ~ i) +
  scale_y_continuous(position = "right", limits = c(0,1)) +
  scale_x_discrete(labels = scales::parse_format()) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(),

        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        #axis.text.y.left =element_blank(),

        axis.text.y = element_text(margin = margin(r = 0)),  # move left axis labels closer to axis
        panel.spacing = unit(5, "mm"),                       # remove spacing between facets
        # strip.background = element_rect(size = 0.5),
        legend.position = "none",

        strip.text.y = element_blank(),

        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  xlab("Similarity") + ylab("") +
  coord_flip()

intervals <- ggplot(to_plot_intervals %>%
                      filter(ejemplo == ejN)) +
  geom_segment(aes(x = left_value, xend = right_value, y = y, yend = y), size = 1.5) +
  geom_segment(aes(x = left_value, xend = right_value, y = y, yend = y), size = 1.5) +
  geom_text(aes(label = interval,
                x = ((right_value-left_value)/2)+left_value,
                y = ifelse(y == 1, y-1, y+1.3)),
            check_overlap = T,
            nudge_x = 0,
            family = "Times New Roman", size = 3.1) +
  scale_y_continuous(limits = c(-1.5,4)) +
  scale_x_continuous(n.breaks = 10, limits = c(-0.2,1)) +
  facet_grid(.~i, switch = "y") +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y.left =element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),  # move left axis labels closer to axis
        panel.spacing = unit(5, "mm"),                       # remove spacing between facets
        #strip.background = element_rect(size = 0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  xlab("Intervals")

sims / intervals + plot_layout(heights = c(1.1,0.6))

################################################

ejN <- 2 # 8.59 x 2.19
# Bar plot for each comparison where each bar is a similarity measure
sims <- ggplot(to_plot %>% filter(ejemplo == ejN) %>%
                 mutate(sim = factor(sim, level = c("S[J]", "S[D]", "S[prod]", "S[min]", "S[mean]", "S[gMean]"))),
               aes(sim, value, fill = sim)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = 0.2, label = ifelse(value != 0, str_pad(round(value, 3), width = 5, pad = "0", side = "right"), 0)),
            family = "Times New Roman",
            size = 3, face = "bold") +
  facet_grid(. ~ i) +
  scale_y_continuous(position = "right", limits = c(0,1)) +
  scale_x_discrete(labels = scales::parse_format()) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(),

        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        #axis.text.y.left =element_blank(),

        axis.text.y = element_text(margin = margin(r = 0)),  # move left axis labels closer to axis
        panel.spacing = unit(5, "mm"),                       # remove spacing between facets
        # strip.background = element_rect(size = 0.5),
        legend.position = "none",

        strip.text.y = element_blank(),

        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  xlab("Similarity") + ylab("") +
  coord_flip()

intervals <- ggplot(to_plot_intervals %>%
                      filter(ejemplo == ejN)) +
  geom_segment(aes(x = left_value, xend = right_value, y = y, yend = y), size = 1.5) +
  geom_segment(aes(x = left_value, xend = right_value, y = y, yend = y), size = 1.5) +
  geom_text(aes(label = interval,
                x = ((right_value-left_value)/2)+left_value,
                y = ifelse(y == 1, y-1, y+1.3)),
            check_overlap = T,
            nudge_x = 0,
            family = "Times New Roman", size = 3.1) +
  scale_y_continuous(limits = c(-1.5,4)) +
  scale_x_continuous(n.breaks = 10, limits = c(-0.2,1)) +
  facet_grid(.~i, switch = "y") +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y.left =element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),  # move left axis labels closer to axis
        panel.spacing = unit(5, "mm"),                       # remove spacing between facets
        #strip.background = element_rect(size = 0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  xlab("Intervals")

sims+intervals
sims / intervals + plot_layout(heights = c(1.1,0.6))



################################################

ejN <- 3 # 8.59 x 2.19
# Bar plot for each comparison where each bar is a similarity measure
sims <- ggplot(to_plot %>% filter(ejemplo == ejN) %>%
                 mutate(sim = factor(sim, level = c("S[J]", "S[D]", "S[prod]", "S[min]", "S[mean]", "S[gMean]"))),
               aes(sim, value, fill = sim)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = 0.75, label = ifelse(value != 0, str_pad(round(value, 3), width = 5, pad = "0", side = "right"), 0)),
            family = "Times New Roman",
            size = 3) +
  facet_grid(. ~ i) +
  scale_y_continuous(position = "right", limits = c(0,1)) +
  scale_x_discrete(labels = scales::parse_format()) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(),

        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        #axis.text.y.left =element_blank(),

        axis.text.y = element_text(margin = margin(r = 0)),  # move left axis labels closer to axis
        panel.spacing = unit(5, "mm"),                       # remove spacing between facets
        # strip.background = element_rect(size = 0.5),
        legend.position = "none",

        strip.text.y = element_blank(),

        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  xlab("Similarity") + ylab("") +
  coord_flip()

intervals <- ggplot(to_plot_intervals %>%
                      filter(ejemplo == ejN)) +
  geom_segment(aes(x = left_value, xend = right_value, y = y, yend = y), size = 1.5) +
  geom_segment(aes(x = left_value, xend = right_value, y = y, yend = y), size = 1.5) +
  geom_text(aes(label = interval,
                x = ((right_value-left_value)/2)+left_value,
                y = ifelse(y == 1, y-1, y+1.3)),
            check_overlap = T,
            nudge_x = 0,
            family = "Times New Roman", size = 3.1) +
  scale_y_continuous(limits = c(-1.5,4)) +
  scale_x_continuous(n.breaks = 10, limits = c(-0.2,1)) +
  facet_grid(.~i, switch = "y") +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y.left =element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),  # move left axis labels closer to axis
        panel.spacing = unit(5, "mm"),                       # remove spacing between facets
        #strip.background = element_rect(size = 0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  xlab("Intervals")

sims / intervals + plot_layout(heights = c(1.1,0.6))


################################################

ejN <- 4 # 8.59 x 2.19
# Bar plot for each comparison where each bar is a similarity measure
sims <- ggplot(to_plot %>% filter(ejemplo == ejN) %>%
                 mutate(sim = factor(sim, level = c("S[J]", "S[D]", "S[prod]", "S[min]", "S[mean]", "S[gMean]"))),
               aes(sim, value, fill = sim)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = 0.85, label = ifelse(value != 0, str_pad(round(value, 3), width = 5, pad = "0", side = "right"), 0)),
            family = "Times New Roman",
            size = 3) +
  facet_grid(. ~ i) +
  scale_y_continuous(position = "right", limits = c(0,1)) +
  scale_x_discrete(labels = scales::parse_format()) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(),

        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        #axis.text.y.left =element_blank(),

        axis.text.y = element_text(margin = margin(r = 0)),  # move left axis labels closer to axis
        panel.spacing = unit(5, "mm"),                       # remove spacing between facets
        # strip.background = element_rect(size = 0.5),
        legend.position = "none",

        strip.text.y = element_blank(),

        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  xlab("Similarity") + ylab("") +
  coord_flip()

intervals <- ggplot(to_plot_intervals %>%
                      filter(ejemplo == ejN)) +
  geom_segment(aes(x = left_value, xend = right_value, y = y, yend = y), size = 1.5) +
  geom_segment(aes(x = left_value, xend = right_value, y = y, yend = y), size = 1.5) +
  geom_text(aes(label = interval,
                x = ((right_value-left_value)/2)+left_value,
                y = ifelse(y == 1, y-1, y+1.3)),
            check_overlap = T,
            nudge_x = 0,
            family = "Times New Roman", size = 3.1) +
  scale_y_continuous(limits = c(-1.5,4)) +
  scale_x_continuous(n.breaks = 10, limits = c(-0.2,1)) +
  facet_grid(.~i, switch = "y") +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y.left =element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),  # move left axis labels closer to axis
        panel.spacing = unit(5, "mm"),                       # remove spacing between facets
        #strip.background = element_rect(size = 0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  xlab("Intervals")

sims / intervals + plot_layout(heights = c(1.1,0.6))

####################################################

ejN <- 5 # 8.59 x 2.19
# Bar plot for each comparison where each bar is a similarity measure
sims <- ggplot(to_plot %>% filter(ejemplo == ejN) %>%
                 mutate(sim = factor(sim, level = c("S[J]", "S[D]", "S[prod]", "S[min]", "S[mean]", "S[gMean]"))),
               aes(sim, value, fill = sim)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = 0.85, label = ifelse(value != 1, str_pad(round(value, 3), width = 5, pad = "0", side = "right"), 1)),
            family = "Times New Roman",
            size = 3) +
  facet_grid(. ~ i) +
  scale_y_continuous(position = "right", limits = c(0,1)) +
  scale_x_discrete(labels = scales::parse_format()) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(),

        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        #axis.text.y.left =element_blank(),

        axis.text.y = element_text(margin = margin(r = 0)),  # move left axis labels closer to axis
        panel.spacing = unit(5, "mm"),                       # remove spacing between facets
        # strip.background = element_rect(size = 0.5),
        legend.position = "none",

        strip.text.y = element_blank(),

        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  xlab("Similarity") + ylab("") +
  coord_flip()

intervals <- ggplot(to_plot_intervals %>%
                      filter(ejemplo == ejN)) +
  geom_segment(aes(x = left_value, xend = right_value, y = y, yend = y), size = 1.5) +
  geom_segment(aes(x = left_value, xend = right_value, y = y, yend = y), size = 1.5) +
  geom_text(aes(label = interval,
                x = ((right_value-left_value)/2)+left_value,
                y = ifelse(y == 1, y-1, y+1.2)),
            check_overlap = T,
            nudge_x = 0,
            family = "Times New Roman", size = 3.1) +
  scale_y_continuous(limits = c(-1.5,4)) +
  scale_x_continuous(n.breaks = 10, limits = c(0,1), expand = c(0.1, 0.1)) +
  facet_grid(.~i, switch = "y") +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y.left =element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),  # move left axis labels closer to axis
        panel.spacing = unit(5, "mm"),                       # remove spacing between facets
        #strip.background = element_rect(size = 0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  xlab("Intervals")

sims / intervals + plot_layout(heights = c(1.1,0.6))

####################################################

ejN <- 6 # 8.59 x 2.19
# Bar plot for each comparison where each bar is a similarity measure
sims <- ggplot(to_plot %>% filter(ejemplo == ejN) %>%
                 mutate(sim = factor(sim, level = c("S[J]", "S[D]", "S[prod]", "S[min]", "S[mean]", "S[gMean]"))),
               aes(sim, value, fill = sim)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(value != 0, str_pad(round(value, 3), width = 5, pad = "0", side = "right"), 0)),
            family = "Times New Roman",
            nudge_y = 0.12, size = 3) +
  facet_grid(. ~ i) +
  scale_y_continuous(position = "right", limits = c(0,1)) +
  scale_x_discrete(labels = scales::parse_format()) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(),

        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        #axis.text.y.left =element_blank(),

        axis.text.y = element_text(margin = margin(r = 0)),  # move left axis labels closer to axis
        panel.spacing = unit(5, "mm"),                       # remove spacing between facets
        # strip.background = element_rect(size = 0.5),
        legend.position = "none",

        strip.text.y = element_blank(),

        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  xlab("Similarity") + ylab("") +
  coord_flip()

intervals <- ggplot(to_plot_intervals %>%
                      filter(ejemplo == ejN)) +
  geom_segment(aes(x = left_value, xend = right_value, y = y, yend = y), size = 1.5) +
  geom_segment(aes(x = left_value, xend = right_value, y = y, yend = y), size = 1.5) +
  geom_text(aes(label = interval,
                x = ((right_value-left_value)/2)+left_value,
                y = ifelse(y == 1, y-1, y+1.2)),
            check_overlap = T,
            nudge_x = 0,
            family = "Times New Roman", size = 3.1) +
  scale_y_continuous(limits = c(-1.5,4)) +
  scale_x_continuous(n.breaks = 10, limits = c(0,1), expand = c(0.1, 0.1)) +
  facet_grid(.~i, switch = "y") +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y.left =element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),  # move left axis labels closer to axis
        panel.spacing = unit(5, "mm"),                       # remove spacing between facets
        #strip.background = element_rect(size = 0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  xlab("Intervals")

sims / intervals + plot_layout(heights = c(1.1,0.6))
