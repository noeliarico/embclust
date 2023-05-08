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

########### GRANADA

granada <- sim_por_par_de_meses[[which(nombres_provincias == "granada")]]
granada <- bind_cols(all_months, granada) %>%
  mutate(m1 = as.factor(m1),
         m2 = as.factor(m2))

granada %>% mutate(sMin = round(sMin, 4)) %>% filter(sMin == 0.2968)

intervalos <- ggplot(meteo_is_intervals_sin_normalizar %>%
                       filter(provincia == "granada") %>%
                       mutate(color = month %in% c(1,8,12)), aes(y=month)) +
  geom_segment(aes(y = month, yend = month, x = min, xend = max, color = color)) +
  geom_point(aes(min, month)) +
  geom_point(aes(max, month)) +
  geom_text(aes(x = max, label = max), nudge_y = 0.5, size = 2.5) +
  geom_text(aes(x = min, label = min), nudge_y = 0.5, size = 2.5) +
  scale_color_manual(values = c("black", "red")) +
  ylab("Month") +
  xlab("Temperature range") +
  scale_y_discrete(limits = rev, expand = c(0, 1, 0.1, 0)) +
  theme_bw() +
  theme(text = element_text(size = 9),
        axis.title.x = element_text(margin = margin(t=10)),
        axis.title.y = element_text(margin = margin(r=10)),
        legend.position = "none")
intervalos

granada %>%
  select(m1, aL, aR) %>%
  distinct() %>%
  apply(1, function(x) {paste0("[", x["aL"], ", ", x["aR"], "]")}) %>%
  enframe() %>%
  xtable()

bind_cols(all_months, sim_por_par_de_meses$valencia) %>%
  select(-c(aL, aR, bL, bR)) %>%
  mutate(across(starts_with("s"), ~ num(., digits = 7))) %>%
  #filter(m1 == 2 & m2 == 9)
  filter(m1 == 10 & m2 == 12)


bind_cols(all_months, sim_por_par_de_meses$granada) %>%
  select(-c(aL, aR, bL, bR)) %>%
  mutate(across(starts_with("s"), ~ num(., digits = 7))) %>%
  filter(m1 == 8 & m2 == 12)
  #filter(m1 == 1 & m2 == 8)

################################################################################

valencia <- sim_por_par_de_meses[[which(nombres_provincias == "valencia")]]
valencia <- bind_cols(all_months, valencia) %>%
  mutate(m1 = as.factor(m1),
         m2 = as.factor(m2))


intervalos <- ggplot(meteo_is_intervals_sin_normalizar %>%
                       filter(provincia == "valencia") %>%
                       mutate(color = month %in% c(2,9,10,12)), aes(y=month)) +
  geom_segment(aes(y = month, yend = month, x = min, xend = max, color = color)) +
  geom_point(aes(min, month)) +
  geom_point(aes(max, month)) +
  geom_text(aes(x = max, label = max), nudge_y = 0.5, size = 2.5) +
  geom_text(aes(x = min, label = min), nudge_y = 0.5, size = 2.5) +
  scale_color_manual(values = c("black", "red")) +
  ylab("Month") +
  xlab("Temperature range") +
  scale_y_discrete(limits = rev, expand = c(0, 1, 0.1, 0)) +
  theme_bw() +
  theme(text = element_text(size = 9),
        axis.title.x = element_text(margin = margin(t=10)),
        axis.title.y = element_text(margin = margin(r=10)),
        legend.position = "none")
intervalos

valencia %>%
  select(m2, aL, aR) %>%
  distinct() %>%
  apply(1, function(x) {paste0("[", x["aL"], ", ", x["aR"], "]")}) %>%
  enframe() %>%
  xtable()

################################################################################

anchos <- meteo_is_intervals_sin_normalizar %>%
  mutate(ancho = max-min)

# Find rows with equal "ancho" values
equal_ancho <- duplicated(anchos$ancho) | duplicated(anchos$ancho, fromLast = TRUE)

# Subset data with equal "ancho" values
equal_ancho_data <- subset(anchos, equal_ancho)

# Print the results
equal_ancho_data %>% filter(ancho == 24.5)

todas_las_sim <- lapply(sim_por_par_de_meses, function(x) {bind_cols(all_months, x)}) %>% bind_rows()

equal_ancho_data %>% filter(ancho == 24.5)

equal_ancho_data %>% select(provincia, ancho) %>% group_by(provincia) %>% n_distinct()

# Mirar las provincias que tienen anchos duplicados

my_data <- anchos
# Find the duplicated values in "ancho" within each value of "provincia"
duplicated_ancho_within_provincia <- duplicated(my_data[, c("provincia", "ancho")]) | duplicated(my_data[, c("provincia", "ancho")], fromLast = FALSE)

# Subset data with duplicated values in "ancho" within each value of "provincia"
duplicated_ancho_within_provincia_data <- my_data[duplicated_ancho_within_provincia, ]

# Print the results
duplicated_ancho_within_provincia_data

anchos %>% filter(provincia == "valencia")
