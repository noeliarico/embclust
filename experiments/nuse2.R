colfunc <- colorRampPalette(c("pink", "black"))
col <- colfunc(20)
ggplot(provincias) +
  #geom_sf(aes(fill = codauto), color = "#887e6a") +
  geom_sf( color = "#887e6a") +
  # canarias caja
  geom_sf(data = can, color = "grey70") +
  geom_sf_label(aes(label = id),
                fill = "white", alpha = 0.5,
                size = 2,
                label.size = 0
  ) +
  labs(title = "Spain provinces", x = " ", y = " ") +
  scale_fill_manual(values = col) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "#887e6a",
      fill = NA,
    ),
    text = element_text(
      family = "Gulliver",
      face = "bold",
      size = 8
    )
  )


