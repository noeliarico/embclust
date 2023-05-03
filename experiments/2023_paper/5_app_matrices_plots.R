badajoz <- sim_por_par_de_meses[[7]]
badajoz <- bind_cols(all_months, badajoz) %>%
  mutate(m1 = as.factor(m1),
         m2 = as.factor(m2))

badajoz %>%
  select(m1, aL, aR) %>%
  distinct() %>%
  apply(1, function(x) {paste0("[", x["aL"], ", ", x["aR"], "]")}) %>%
  enframe() %>%
  xtable()

distancias <- c("sDice", "sJaccard", "sProd", "sMin", "sMean", "sgMean")
var <- distancias[1]

un_plot_por_distancia <- lapply(distancias, function(var) {
  ggplot(badajoz, aes(m1, m2)) +
    geom_tile(aes(fill = .data[[var]]), color = "white") +
    # scale_x_discrete(position = "top") +
    scale_y_discrete(limits=rev) +
    scale_fill_gradient2(midpoint = 0.5,
                         low = "white",
                         mid = "gray88",
                         high = "black",
                         limits = c(0,1),
                         guide = guide_colorbar(label = TRUE,
                                                draw.ulim = TRUE,
                                                draw.llim = TRUE,
                                                # here comes the code change:
                                                frame.colour = "black",
                                                ticks = TRUE,
                                                nbin = 5,
                                                #label.position = "bottom",
                                                barwidth = 0.5
                                                #barheight = 1.3,
                                                #direction = 'vertical')
                                                )) +
    xlab("") + ylab("") + ggtitle(parse(text = paste0(str_replace(var, "s", "S["), "]"))) +
    theme_minimal() +
   theme(legend.title = element_blank(),
         axis.line.x = element_blank(),
         axis.line.y = element_blank(),
         panel.grid.major.x = element_blank(),
         panel.grid.major.y = element_blank(),
         text = element_text(size = 9, family = "Times New Roman")
         #title = .data[[var]]
         )
})
# EXPORTAR: 4x7 inches
patchwork::wrap_plots(un_plot_por_distancia) + plot_layout(guides = "collect") &
  theme(legend.justification="right",
        legend.margin = margin(0, 0, 0, 10)
        )

badajoz %>%
  filter((m1 == 3 & m2 == 10) | m1 == 5 & m2 == 8)


intervalos <- ggplot(meteo_is_intervals_sin_normalizar %>%
         filter(provincia == "badajoz"), aes(y=month)) +
  geom_segment(aes(y = month, yend = month, x = min, xend = max)) +
  geom_point(aes(min, month)) +
  geom_point(aes(max, month)) +
  geom_text(aes(x = max, label = max), nudge_y = 0.5, size = 2.5) +
  geom_text(aes(x = min, label = min), nudge_y = 0.5, size = 2.5) +
  ylab("Month") +
  xlab("Temperature range") +
  scale_y_discrete(limits = rev, expand = c(0, 1, 0.1, 0)) +
  theme_bw() +
  theme(text = element_text(size = 9),
        # axis.text.x = element_text(angle = 90, size = 6),
        axis.title.x = element_text(margin = margin(t=10)),
        axis.title.y = element_text(margin = margin(r=10)))
intervalos



