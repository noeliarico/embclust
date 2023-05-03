######################

paleta <- c("#B3BFB8", "#E83F6F",  "#32936F", "#EECFD4", "#2274A5",
                     "#FFBF00", "#60463B", "#CCE8CC", "#1A1D1A", "#FFFAFF")

paleta <- c(
  "#003f5c",
  "#444e86",
  "#955196",
  "#dd5182",
  "#ff6e54",
  "#ffa600")

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
         sim = ifelse(sim == "sgMean", "S[g-mean]", sim))

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

#######################################################

# ejN <- 1 # 6.3x5
# ejN <- 2 # 6.3x5
# Bar plot for each comparison where each bar is a similarity measure
sims <- ggplot(to_plot %>% filter(ejemplo == ejN) %>%
                 mutate(sim = factor(sim, level = c("S[J]", "S[D]", "S[prod]", "S[min]", "S[mean]", "S[g-mean]"))),
               aes(y = sim, x = value, fill = sim)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_text(aes(label = ifelse(value != 0, str_pad(round(value, 3), width = 5, pad = "0", side = "right"), 0)),
            family = "Times New Roman",
            nudge_x = 0.05, size = 2) +
  facet_grid(i~.) +
  #scale_x_reverse(limits = c(1,0)) +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_discrete(position = "right", labels = scales::parse_format()) +
  scale_fill_manual(values = paleta) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(),
        panel.grid.major.x = element_line(),

        axis.line.x = element_blank(),
        axis.line.y = element_blank(),

        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        #axis.text.y.left =element_blank(),

        axis.text.x.bottom = element_text(),
        #axis.text.y = element_text(margin = margin(r = 0), size = 6),  # move left axis labels closer to axis
        axis.text.y = element_blank(),
        panel.spacing = unit(5, "mm"),                       # remove spacing between facets
        # strip.background = element_rect(size = 0.5),
        legend.position = "bottom",

        strip.text.y = element_blank(),

        strip.background = element_blank(),
        strip.text.x = element_blank(),
        text = element_text(family = "Times New Roman", size = 8)) +
  xlab("Similarity") + ylab("")

intervals <- ggplot(to_plot_intervals %>%
                      filter(ejemplo == ejN)) +
  geom_segment(aes(x = left_value, xend = right_value, y = y, yend = y), size = 1.5) +
  geom_segment(aes(x = left_value, xend = right_value, y = y, yend = y), size = 1.5) +
  geom_text(aes(label = interval,
                x = ((right_value-left_value)/2)+left_value,
                y = ifelse(y == 1, y-1, y+1.3)),
            nudge_x = 0,
            family = "Times New Roman", size = 3) +
  scale_y_continuous(limits = c(-1.5,4)) +
  scale_x_continuous(n.breaks = 10, limits = c(-0.2,1)) +
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
        #strip.background = element_rect(size = 0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(family = "Times New Roman", size = 8))+
  xlab("Intervals")

#sims / intervals + plot_layout(heights = c(1.1,0.6))
intervals + sims + plot_spacer() + intervals + sims + plot_layout(widths = c(1,1.5,.25,1,1.5), guides = "collect") & theme(legend.position = 'bottom')


################################################################################

plot_grande_sims <- function(ejN) {
# Bar plot for each comparison where each bar is a similarity measure
sims <- ggplot(to_plot %>% filter(ejemplo == ejN) %>%
                 mutate(sim = factor(sim, level = c("S[J]", "S[D]", "S[prod]", "S[min]", "S[mean]", "S[g-mean]"))),
               aes(y = sim, x = value, fill = sim)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_text(aes(label = ifelse(value != 0 & value != 1, str_pad(round(value, 3), width = 5, pad = "0", side = "right"), value)),
            family = "Times New Roman",
            nudge_x = 0.1, size = 2) +
  facet_grid(i~.) +
  #scale_x_reverse(limits = c(1,0)) +
  scale_x_continuous(limits = c(0,1.25), breaks = seq(0,1,.25), labels = seq(0,1,.25)) +
  scale_y_discrete(position = "right", labels = scales::parse_format()) +
  scale_fill_manual(values = paleta, name = "Similarity",
                    labels = scales::parse_format(),) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_classic() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),

        axis.line.x = element_blank(),
        axis.line.y = element_blank(),

        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        #axis.text.y.left =element_blank(),

        axis.text.x.bottom = element_text(),
        #axis.text.y = element_text(margin = margin(r = 0), size = 6),  # move left axis labels closer to axis
        axis.text.y = element_blank(),
        panel.spacing = unit(5, "mm"),                       # remove spacing between facets
        # strip.background = element_rect(size = 0.5),
        legend.position = "bottom",

        strip.text.y = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        strip.text.x = element_blank(),
        text = element_text(family = "Times New Roman", size = 7)) +
  xlab("Similarity") + ylab("") + guides(fill=guide_legend(nrow=1,byrow=TRUE))

sims

}

plot_grande_intervals <- function(ejN) {
  intervals <- ggplot(to_plot_intervals %>%
                        filter(ejemplo == ejN)) +
    geom_segment(aes(x = left_value, xend = right_value, y = y, yend = y), size = 1) +
    geom_segment(aes(x = left_value, xend = right_value, y = y, yend = y), size = 1) +
    geom_text(aes(label = interval,
                  x = ((right_value-left_value)/2)+left_value,
                  y = ifelse(y == 1, y-1, y+1.3)),
              nudge_x = 0,
              family = "Times New Roman", size = 2) +
    scale_y_continuous(limits = c(-1.5,4)) +
    scale_x_continuous(breaks = seq(0,1,0.1), limits = c(-0.2,1.2)) +
    facet_grid(i~., switch = "y") +
    theme_classic() +
    theme(panel.grid.major.x = element_line(),
          panel.grid.minor.x = element_blank(),
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
          axis.text.x = element_blank(),
          text = element_text(family = "Times New Roman", size = 7),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          strip.text.y.left = element_text(angle = 0))+
    xlab("Intervals")
  intervals
}

################################################################################
# FIGURA AXIOM4

intervals2 <- plot_grande_intervals(2)
sims2 <- plot_grande_sims(2)
intervals3 <- plot_grande_intervals(3)
sims3 <- plot_grande_sims(3)
#sims / intervals + plot_layout(heights = c(1.1,0.6))
intervals2 + sims2 + plot_spacer() + intervals3 + sims3 + plot_layout(widths = c(1,1.5,.075,1,1.5), guides = "collect") & theme(legend.position = 'bottom', legend.key.size = unit(0.75,"line"))

ggsave("fig_axiom4.pdf", width = 12, height = 17, units = "cm")

################################################################################
# FIGURA PROP3

intervals5 <- plot_grande_intervals(5)
sims5 <- plot_grande_sims(5)
intervals5 + sims5 + plot_layout(widths = c(1,1.5), guides = "collect") & theme(legend.position = 'bottom', legend.key.size = unit(0.75,"line"))

ggsave("fig_prop3.pdf", width = 9, height = 10, units = "cm")

################################################################################
# FIGURA AXIOM6

intervals <- plot_grande_intervals(1)
sims <- plot_grande_sims(1)
intervals + sims + plot_layout(widths = c(1,1.5), guides = "collect") & theme(legend.position = 'bottom', legend.key.size = unit(0.75,"line"))

ggsave("fig_axiom6.pdf", width = 9, height = 15, units = "cm")

################################################################################
# FIGURA AXIOM8

intervals <- plot_grande_intervals(4)
sims <- plot_grande_sims(4)
intervals + sims + plot_layout(widths = c(1,1.5), guides = "collect") & theme(legend.position = 'bottom', legend.key.size = unit(0.75,"line"))

ggsave("fig_axiom8.pdf", width = 9, height = 15, units = "cm")


################################################################################
# FIGURA AXIOM9

intervals <- plot_grande_intervals(6)
sims <- plot_grande_sims(6)
intervals + sims + plot_layout(widths = c(1,1.5), guides = "collect") & theme(legend.position = 'bottom', legend.key.size = unit(0.75,"line"))

ggsave("fig_axiom9.pdf", width = 9, height = 12, units = "cm")


################################################################################
# PROP5 Overlapping

compute_similarities <- function(a, b) {
  tibble(aL = a[1],
         aR = a[2],
         bL = b[1],
         bR = b[2],
         sDice = similarity(a, b, "dice"),
         sJaccard = similarity(a, b, "jaccard"),
         sMean = similarity(a, b, "mean"),
         sgMean = similarity(a, b, "gmean"),
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
  pivot_longer(cols = starts_with("s"), names_to = "sim") %>%
  mutate(sim = ifelse(sim == "sDice", "S[D]", sim),
         sim = ifelse(sim == "sJaccard", "S[J]", sim),
         sim = ifelse(sim == "sMin", "S[min]", sim),
         sim = ifelse(sim == "sMean", "S[mean]", sim),
         sim = ifelse(sim == "sgMean", "S[g-mean]", sim),
         sim = ifelse(sim == "sProd", "S[prod]", sim))

tema <- theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 8),
        plot.title = element_text(size = 11),
        plot.margin = unit(c(.5,.5,.5,.5), "cm"),
        axis.title = element_text(size = 8),
        axis.title.x = element_text(vjust=-2),
        axis.title.y = element_text(vjust=2))

todos <- ggplot(ej_sim, aes(perc, value, color = sim, shape = sim)) +
  geom_line(size = 1, alpha = .3) +
  geom_point(size = 1) +
  labs(x = "Overlapping", y = "Similarity", shape = "Similarity", color = "Similarity", parse = TRUE) +
  scale_shape_manual(values = c(3, 5, 1, 4, 2, 0), labels = scales::parse_format()) +
  scale_color_discrete(labels = scales::parse_format()) +
  scale_x_continuous(labels = scales::percent) +
  coord_fixed() +
  tema +
  theme(legend.position = "bottom")

individuales <- ggplot(ej_sim, aes(perc, value, color = sim, shape = sim)) +
  geom_line(size = 1, alpha = .3) +
  geom_point(size = 1) +
  labs(x = "", y = "", shape = "Similarity", color = "Similarity", parse = TRUE) +
  scale_shape_manual(values = c(3, 5, 1, 4, 2, 0), labels = scales::parse_format()) +
  scale_color_discrete(labels = scales::parse_format()) +
  scale_x_continuous(labels = scales::percent) +
  facet_wrap(~sim, labeller = label_parsed)+
  coord_fixed() +
  tema +
  theme(legend.position = "none")

individuales+todos+plot_layout(widths = c(3, 1), guides = "collect")  & guides(fill=guide_legend(ncol=6), shape=guide_legend(ncol=6)) & theme(legend.position = "bottom")

ggsave("fig_prop5.pdf", width = 18, height = 10, units = "cm")
