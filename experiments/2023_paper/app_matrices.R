meteo_intervals_by_provincia <- meteo_is_intervals %>%
  group_split(provincia)

all_months <- t(combn(1:12, 2))
colnames(all_months) <- c("m1", "m2")
all_months <- all_months%>%
  as_tibble()

# Ejemplo para uno de los elementos de la lista (i.e. una provincia)
un_conjunto <- meteo_intervals_by_provincia[[1]]
lapply(1:66, function(x) {
  mes1 <- all_months[x, "m1"] %>% as.numeric()
  mes2 <- all_months[x, "m2"] %>% as.numeric()
  a <- un_conjunto[mes1, c("min", "max")] %>% as.numeric()
  b <- un_conjunto[mes2, c("min", "max")] %>% as.numeric()
  compute_similarities(a, b, 1)
}) %>% bind_rows()

sim_por_par_de_meses <- lapply(
  1:length(meteo_intervals_by_provincia), function(i) {
    un_conjunto <- meteo_intervals_by_provincia[[i]]
    provincia <- un_conjunto$provincia %>% unique()
    lapply(1:66, function(x) {
      mes1 <- all_months[x, "m1"] %>% as.numeric()
      mes2 <- all_months[x, "m2"] %>% as.numeric()
      a <- un_conjunto[mes1, c("min", "max")] %>% as.numeric()
      b <- un_conjunto[mes2, c("min", "max")] %>% as.numeric()
      compute_similarities(a, b, provincia)
    }) %>% bind_rows()
})

# prueba para una provincia
una_provincia <- sim_por_par_de_meses[[1]]

results <- lapply(1:length(sim_por_par_de_meses), function(i) {
  una_provincia <- sim_por_par_de_meses[[i]]
  valores_dice <- una_provincia %>% count(sDice) %>% filter(n > 1)
  if(nrow(valores_dice)) {
    print(i)
    print(valores_dice)
  }
})

# sDice, sJaccard
nombres_provincias <- meteo_is_intervals$provincia %>% unique()
coincide_dice <- lapply(results, function(x) !is.null(x)) %>% unlist() %>% which()
sim_por_par_de_meses[coincide_dice]

sim_por_par_de_meses[[51]] %>% filter(round(sDice,3) == 0.837)
