library(jsonlite)
library(tidyverse)
library(lubridate)

path <- "experiments/weather_spain2021/capitales2021/data"
files <- list.files(path, full.names = T)
# Give the input file name to the function.
results <- lapply(files, fromJSON)
results <- bind_rows(results)

weather <- results %>%
  as_tibble() %>%
  select(fecha, nombre, provincia, presMin, presMax) %>%
  mutate(fecha = as_date(fecha),
         year = year(fecha),
         day = day(fecha),
         month = month(fecha),
         fecha = NULL,
         presMin = as.numeric(str_replace(presMin, ",", ".")),
         presMax = as.numeric(str_replace(presMax, ",", "."))
  ) %>%
  rename(estacion = nombre) %>%
  mutate(provincia = tolower(provincia),
         provincia = str_replace(provincia, "araba/alava", "alava"),
         provincia = str_replace(provincia, "illes balears", "islas baleares"),
         provincia = str_replace(provincia, "a coruna", "la coruna"),
         provincia = str_replace(provincia, "gipuzkoa", "guipuzcoa"),
         provincia = str_replace(provincia, "girona", "gerona"),
         provincia = str_replace(provincia, "bizkaia", "vizcaya"),
         provincia = str_replace(provincia, "ourense", "orense"),
         provincia = str_replace(provincia, "sta. cruz de tenerife", "santa cruz de tenerife")
  )

# Exploration

print(weather %>% group_by(provincia, estacion) %>% count(), n = 52)
# Por qué hay mas de 365 en algunas?
print(weather %>% group_by(year) %>% count(), n = 52)
table(complete.cases(weather)) # 3094 missing
print(weather[!complete.cases(weather),], n = 3094)
weather <- weather %>% drop_na() # drop 3094
print(weather %>% group_by(provincia, estacion) %>% count(), n = 52)

# Min and max temperature for normalization
minT <- min(weather$presMin)
maxT <- max(weather$presMax)
n <- function(x) {
  return((x-minT)/(maxT-minT))
}
weather <- weather %>% mutate(
  maxN = n(presMax),
  minN = n(presMin)
) %>% filter(year == 2021) %>% mutate(year = NULL)

print(weather %>%
        group_by(provincia, month) %>% count() %>%
        pivot_wider(names_from = month, values_from = n), n = 52)

# Check range of temperatures
range(weather$presMin)
range(weather$presMax)
range(weather$maxN)
range(weather$minN)

weather_by_month <- weather %>%
  select(provincia, month, maxN, minN) %>%
  mutate(month = factor(month)) %>%
  group_by(provincia, month) %>%
  summarise(min = min(minN),
            max = max(maxN),
            .groups = "drop")

weather_by_month_plot <- weather %>%
  select(provincia, month, presMin, presMax) %>%
  mutate(month = factor(month)) %>%
  group_by(provincia, month) %>%
  summarise(min = min(presMin),
            max = max(presMax),
            .groups = "drop")



ggplot(weather_by_month_plot, aes(x=month)) +
  geom_segment(aes(xend = month, y = min, yend = max)) +
  geom_point(aes(month, min)) +
  geom_point(aes(month, max)) +
  # scale_x_discrete(n.breaks = 12) +
  facet_wrap(~provincia)+
  xlab("Month") +
  ylab("Temperature range") +
  theme_bw()

mins <- weather_by_month %>%
  pivot_wider(-max, names_from = month, names_prefix = "min",
              values_from = min)

maxs <- weather_by_month %>%
  pivot_wider(-min, names_from = month, names_prefix = "max",
              values_from = max)

weather_data <- inner_join(mins, maxs)
weather_data <- weather_data[c(1, order(rep(1:12, 2)) + 1)]
print(weather_data, n = 52)

weather_data_variables <- weather_data[,-1]
weather_data_provinces <- weather_data %>% pull(provincia)

plot_mapa_clusters()

plot_hclust_tiles <- function(hc, log = F) {

  n <- nrow(hc$merge)+1
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
plot_hclust_tiles(hclust(d, method = "average"))
