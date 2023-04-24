badajoz <- sim_por_par_de_meses[[7]]
badajoz <- bind_cols(all_months, badajoz) %>%
  mutate(m1 = as.factor(m1),
         m2 = as.factor(m2))

distancias <- c("sDice", "sJaccard", "sProd", "sMin", "sMean", "sgMean")
var <- distancias[1]

un_plot_por_distancia <- lapply(distancias, function(var) {
  ggplot(badajoz, aes(m1, m2)) +
    geom_tile(aes(fill = .data[[var]]), color = "white") +
    scale_fill_continuous(limits = c(0,1))
})

patchwork::wrap_plots(un_plot_por_distancia)

badajoz %>%
  filter((m1 == 3 & m2 == 10) | m1 == 5 & m2 == 8)
