meteo_intervals_by_provincia <- meteo_is_intervals %>%
  group_split(provincia)

all_months <- t(combn(1:12, 2))
colnames(all_months) <- c("m1", "m2")
all_months <- all_months%>%
  as_tibble()

# Ejemplo para uno de los elementos de la lista (i.e. una provincia)
un_conjunto <- meteo_intervals_by_provincia[[11]]
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



# Objetivo: mirar si tiene similitudes repetidas con sDice
tienen_similitudes_repes_dice <- lapply(1:length(sim_por_par_de_meses), function(i) {
  una_provincia <- sim_por_par_de_meses[[i]]
  valores_dice <- una_provincia %>% count(sDice) %>% filter(n > 1)
  if(nrow(valores_dice)) return(valores_dice)
  else return(NULL)
})

nombres_provincias <- meteo_is_intervals$provincia %>% unique()
coincide_dice <- lapply(tienen_similitudes_repes_dice, function(x) !is.null(x)) %>% unlist() %>% which()
sim_por_par_de_meses[coincide_dice]
valores_repes_dice <- lapply(tienen_similitudes_repes_dice, function(x) if(!is.null(x)) x %>% pull(sDice)) %>% compact() %>% unlist()

# Se repiten similitudes en las provincias
nombres_provincias[coincide_dice]
# Concretamente los valores son
valores_repes_dice

lapply(sim_por_par_de_meses[coincide_dice], function(x) {
    bind_cols(all_months, x) %>% rename(provincia = ejemplo)
  }) %>%
  bind_rows() %>%
  DT::datatable(
    options = list(pageLength = 66),
    filter = list(position = 'top', clear = FALSE)
    )

# Objetivo: mirar si tiene similitudes repetidas con sJaccard
tienen_similitudes_repes_jaccard <- lapply(1:length(sim_por_par_de_meses), function(i) {
  una_provincia <- sim_por_par_de_meses[[4]]
  valores_jaccard <- una_provincia %>% count(sJaccard) %>% filter(n > 1)
  if(nrow(valores_jaccard)) return(valores_jaccard)
  else return(NULL)
})

nombres_provincias <- meteo_is_intervals$provincia %>% unique()
coincide_jaccard <- lapply(tienen_similitudes_repes_jaccard, function(x) !is.null(x)) %>% unlist() %>% which()
sim_por_par_de_meses[coincide_jaccard]
valores_repes_jaccard <- lapply(tienen_similitudes_repes_jaccard, function(x) if(!is.null(x)) x %>% pull(sJaccard)) %>% compact() %>% unlist()

# Se repiten similitudes en las provincias
nombres_provincias[coincide_jaccard]
# Concretamente los valores son
valores_repes_jaccard

lapply(sim_por_par_de_meses[coincide_jaccard], function(x) {
  bind_cols(all_months, x) %>% rename(provincia = ejemplo)
}) %>%
  bind_rows() %>%
  DT::datatable(
    options = list(pageLength = 66),
    filter = list(position = 'top', clear = FALSE)
  )
