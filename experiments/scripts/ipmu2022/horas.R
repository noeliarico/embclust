library(lubridate)

horas <- read_delim("experiments/scripts/ipmu2022/horas", delim = ";",
                    skip = 1,
                    col_names = c("ciudad", "sunrise", "sunset", "daylight"))
horas <- horas %>%
  mutate(ciudad = str_remove_all(ciudad, "\t| "),
         ciudad = str_replace_all(ciudad, "_", " "),
         ciudad = as.factor(ciudad)) %>%
  mutate(provincia = c("islas baleares",
                       "barcelona",
                       "barcelona",
                       "valencia",
                       "alicante",
                       "alicante",
                       "zaragoza",
                       "murcia",
                       "vitoria",
                       "alava",
                       "granada",
                       "madrid",
                       "malaga",
                       "valladolid",
                       "cordoba",
                       "asturias",
                       "sevilla",
                       "a coruna",
                       "vigo",
                       "las palmas"))

to_minutes <- function(time) {
  return(hour(time)*60 + minute(time))
}

horas <- horas %>%
  mutate(sunrise = to_minutes(sunrise),
         sunset = to_minutes(sunset),
         daylight = to_minutes(daylight))

ggplot(horas, aes(x = 0, y = ciudad)) +
  geom_segment(aes(x = sunrise, xend = sunset, yend = ciudad)) +
  geom_point(aes(x = sunrise)) +
  geom_point(aes(x = sunset)) +
  theme_bw()

# normalize
horas <- horas %>%
  mutate(sunrise = sunrise/(24*60),
         sunset = sunset/(24*60),
         daylight = daylight/(24*60))
horas_dist <- as.data.frame(horas[, 2:3])
rownames(horas_dist) <- horas %>% pull("ciudad")
horas_dist[,3] <- 0
horas_dist[,4] <- 0


horas_w <- sim_emb_matrix(horas_dist, sim_w, mean, dist = T)
attr(horas_w, "Labels") <- horas %>% pull("ciudad")

library(qgraph)
# jpeg('example_forcedraw.jpg', width=1000, height=1000, unit='px')
qgraph(horas_w, layout='spring', vsize=6)
# dev.off()

# horas_w <- sim_emb_matrix(horas_dist, sim_w, mean, dist = F)
# dim <- ncol(horas_w)
#
# image(1:dim, 1:dim, horas_w, axes = FALSE, xlab="", ylab="")
#
# axis(1, 1:dim, colnames(horas_w), cex.axis = 0.5, las=3)
# axis(2, 1:dim, colnames(horas_w), cex.axis = 0.5, las=1)
# text(expand.grid(1:dim, 1:dim), sprintf("%0.5f", horas_w), cex=0.6)
