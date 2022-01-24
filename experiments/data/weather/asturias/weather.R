library(readxl)
library(lubridate)

path <- "data/weather/weather.xlsx"
weather <- lapply(excel_sheets(path),
                  read_excel,
                  path = path,
                  col_names = c("date",
                                "max",
                                "min"),
                  skip = 1,
                  col_types = c("date", "numeric", "numeric"))
names(weather) <- excel_sheets(path)
weather <- enframe(weather) %>%
              unnest(value) %>%
  mutate(name = factor(name))

nrow(weather) # 3650 = 365*10ciudades
table(complete.cases(weather))
weather <- weather %>% drop_na() # drop 109
minT <- min(weather$min)
maxT <- max(weather$max)
n <- function(x) {
  return((x-minT)/(maxT-minT))
}
weather <- weather %>% mutate(
  day = day(date),
  month = month(date),
  maxN = n(max),
  minN = n(min)
)
range(weather$maxN)
range(weather$minN)

weather_by_month <- weather %>%
  mutate(month = factor(month)) %>%
  group_by(name, month) %>%
  summarise(min = min(minN),
            max = max(maxN),
            .groups = "drop")

ggplot(weather_by_month, aes(x=month)) +
  geom_segment(aes(xend = month, y = min, yend = max)) +
  geom_point(aes(month, min)) +
  geom_point(aes(month, max)) +
  # scale_x_discrete(n.breaks = 12) +
  facet_grid(~name)+
  xlab("Month") +
  ylab("Temperature range") +
  theme_bw()

mins <- weather_by_month %>%
  pivot_wider(-max, names_from = month, names_prefix = "min",
              values_from = min)

maxs <- weather_by_month %>%
  pivot_wider(-min, names_from = month, names_prefix = "max",
              values_from = max)

for_distance <- inner_join(mins, maxs)
for_distance <- for_distance[c(1, order(rep(1:12, 2)) + 1)]
