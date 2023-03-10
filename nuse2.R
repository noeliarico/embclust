library(mapSpain)

country <- esp_get_country()
provincias <- esp_get_prov() %>%
  mutate(prov.shortname.en = ifelse(prov.shortname.en == "Balearic Islands", "Islas Baleares", prov.shortname.en),
         prov.shortname.en = ifelse(prov.shortname.en == "A Coruña", "La Coruña", prov.shortname.en),
         prov.shortname.en = ifelse(prov.shortname.en == "Gipuzkoa", "Guipuzcoa", prov.shortname.en),
         prov.shortname.en = ifelse(prov.shortname.en == "Biscay", "Vizcaya", prov.shortname.en))
can <- esp_get_can_box()
lines <- esp_get_can_box()
provincias <- provincias %>% arrange(prov.shortname.en) %>% mutate(id = 1:nrow(.))

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

ids_names <- codigos <- print(provincias %>%
  as_tibble() %>%
  select(prov.shortname.es, id) %>%
  inner_join(
meteo_is %>%
  mutate(provincia = as.character(provincia)) %>%
  distinct(provincia) %>%
  arrange(provincia) %>%
  mutate(id = 1:nrow(.))), n = 52)
