library(jsonlite)
library(tidyverse)
library(lubridate)

path <- "experiments/weather_spain2021/aemet2021"
files <- list.files(path, full.names = T)
# Give the input file name to the function.
results <- lapply(files, fromJSON)
results <- bind_rows(results)

lluvias <- results %>%
  as_tibble() %>%
  select(fecha, nombre, provincia, prec) %>%
  mutate(fecha = as_date(fecha),
         year = year(fecha),
         day = day(fecha),
         month = month(fecha),
         fecha = NULL,
         prec = as.numeric(str_replace(prec, ",", "."))
  ) %>%
  rename(estacion = nombre) %>%
  mutate(provincia = tolower(provincia),
         provincia = str_replace(provincia, "araba/alava", "alava"),
         provincia = str_replace(provincia, "illes balears", "islas baleares"),
         provincia = str_replace(provincia, "a coru.a", "la coruna"),
         provincia = str_replace(provincia, "gipuzkoa", "guipuzcoa"),
         provincia = str_replace(provincia, "girona", "gerona"),
         provincia = str_replace(provincia, "bizkaia", "vizcaya"),
         provincia = str_replace(provincia, "ourense", "orense"),
         provincia = str_replace(provincia, "sta. cruz de tenerife", "santa cruz de tenerife")
  )

# Exploration

print(lluvias %>% group_by(provincia, estacion) %>% count(), n = 52)
# Por qué hay más de 365 en algunas?
# Comprobas si preco hay del año 2021
print(weather %>% group_by(year) %>% count(), n = 52) # si
# Mirar por estación y mes.
lluvias %>% group_by(provincia, estacion, month) %>% count() # algunas 32
lluvias %>% group_by(provincia, estacion, month, day) %>% count() %>%
  filter(n!=1)
lluvias %>% filter(provincia == "alava",
                    estacion == "FORONDA-TXOKIZA",
                    month == 5,#3,
                    day == 1)
# es el mismo dato
table(complete.cases(lluvias)) # 4376 missing
lluvias <- lluvias %>% drop_na() # drop 4376
print(lluvias %>% group_by(provincia, estacion) %>% count(), n = 52)

# Min and max temperature for normalization
minS <- min(lluvias$prec)
maxS <- max(lluvias$prec)
n <- function(x) {
  return((x-minS)/(maxS-minS))
}
lluvias <- lluvias %>% mutate(
  prec = n(prec),
) %>% filter(year == 2021) %>% mutate(year = NULL)

print(lluvias %>%
        group_by(provincia, month) %>% count() %>%
        pivot_wider(names_from = month, values_from = n), n = 52)

# Check range of temperatures
range(lluvias$prec)

lluvias_by_month <- lluvias %>%
  select(provincia, month, prec) %>%
  mutate(month = factor(month)) %>%
  group_by(provincia, month) %>%
  summarise(min = min(prec),
            max = max(prec),
            .groups = "drop")

lluvias_by_month_plot <- lluvias %>%
  select(provincia, month, prec) %>%
  mutate(month = factor(month)) %>%
  group_by(provincia, month) %>%
  summarise(min = min(prec),
            max = max(prec),
            .groups = "drop")



ggplot(lluvias_by_month_plot, aes(x=month)) +
  geom_segment(aes(xend = month, y = min, yend = max)) +
  geom_point(aes(month, min)) +
  geom_point(aes(month, max)) +
  # scale_x_discrete(n.breaks = 12) +
  facet_wrap(~provincia)+
  xlab("Month") +
  ylab("Sun range") +
  theme_bw()

mins <- lluvias_by_month_plot %>%
  pivot_wider(-max, names_from = month, names_prefix = "min",
              values_from = min)

maxs <- lluvias_by_month_plot %>%
  pivot_wider(-min, names_from = month, names_prefix = "max",
              values_from = max)

lluvias_data <- inner_join(mins, maxs)
lluvias_data <- lluvias_data[c(1, order(rep(1:12, 2)) + 1)]
print(lluvias_data, n = 52)

lluvias_data_variables <- lluvias_data[,-1]
lluvias_data_provinces <- lluvias_data %>% pull(provincia)

s <- sim_emb_matrix(lluvias_data_variables, emb_lk, mean, dist = T)
attr(s, "Labels") <- lluvias_data_provinces
d <- 1-s

plot(hclust(d, method = "average"))

plot_hclust_tiles(hclust(d, method = "average"))

hc <- hclust(d, method = "average")
clusters <- data.frame(cluster = factor(cutree(hc, 4))) %>%
  rownames_to_column("provincia")

plot_hclust_tiles <- function(hc, log = F) {
  merge <- vector(mode="numeric", n-1)
  plot_log <- matrix(0, nrow = n, ncol = n, byrow = T)
  print(plot_log)
  plot_log[n, ] <- 1:n
  for (i in 1:(n-1)) {
    l <- n-i
    plot_log[l, ] <- plot_log[l+1, ]
    objs <- hc$merge[i,]
    print(objs)
    if (objs[1] < 0) { # First is an object
      if (objs[2] < 0) { # Merge two objects
        merge[i] <- (-1*objs[1])
        plot_log[l, (-1*objs[2])] <- (-1*objs[1])
      } else { # Merge object and cluster
        use <- sort(c((-1*objs[1]),merge[objs[2]]))
        merge[i] <- use[1]
        update <- plot_log[l,] == use[2]
        plot_log[l, update] <- use[1]
      }
    } else { # First is a cluster
      use <- sort(c(merge[objs[1]],merge[objs[2]]))
      merge[i] <- use[1]
      update <- plot_log[l,] == use[2]
      plot_log[l, update] <- use[1]
    }
  }
  if (log) {
    return(plot_log)
  }
  else {
    colnames(plot_log) <- 1:ncol(plot_log)
    plot_log <- plot_log %>%
      tibble::as_tibble() %>%
      dplyr::mutate(level = factor(1:nrow(plot_log))) %>%
      tidyr::pivot_longer(-level, names_to = "object") %>%
      dplyr::mutate(object = as.factor(as.numeric(object)),
                    cluster = as.factor(value),
                    value = NULL)
    ggplot(plot_log,
           aes(object, level, fill = cluster)) +
      geom_tile(color = "black") +
      # xlim(levels(mchc$object)) +
      ylim(rev(levels(plot_log$level))) +
      guides(fill = guide_legend(title="Cluster")) +
      xlab("Object") + ylab("Level") +
      theme_minimal() + theme(legend.position = "none")
  }
}

weather_min_by_month_variables <- weather_min_by_month[,-1]
weather_min_by_month_provinces <- weather_min_by_month %>% pull(provincia)
