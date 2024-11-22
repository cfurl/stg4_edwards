# This graph creates a hyetograph for a basin of interest (in this case using entire basin) with a ribbon around it and a median line

library (tidyverse)
library (lubridate)

# read in csv with daily averages for a given basin

#basins <- list.files("C:\\texas_mpe\\arrow\\metrics\\norms", pattern = "\\.csv$", full.names = TRUE) # locally
basins <- list.files (".//radar_stats//cumulative_precip", pattern = "\\.csv$", full.names = TRUE)

# you can do this for any basin, currently just doing it for 'usgs_dissolved.shp' which is entire EA recharge zone
boi <- read_csv(basins[10])

###### Let's look at calculating values for each month_day combo
# this is essentially the same code used in 'subbasin_cum_precip_and_colors.R'
# should combine and make single script....

env <- boi |>
  group_by(month, day) |>
  summarise (min_rain = min(avg_mm_rainfall, na.rm=TRUE),
             max_rain = max(avg_mm_rainfall, na.rm=TRUE),
             perc10 = quantile(avg_mm_rainfall, 0.10),
             perc25 = quantile(avg_mm_rainfall, 0.25),
             perc75 = quantile(avg_mm_rainfall, 0.75),
             perc90 = quantile(avg_mm_rainfall, 0.90),
             median_rain = median(avg_mm_rainfall, na.rm=TRUE)) |>
  ungroup()|>
  mutate(year = 2023)|>
  mutate (date = make_date(year, month, day))

# hyetograph with some radar derived norms on the ribbon

ggplot () +
  geom_ribbon(data = env, aes(x = date, ymin=perc25,ymax=perc75)) +  
  geom_line(data = env, aes(x = date, y = median_rain)) +
  geom_line(data = boi, aes(x = make_date(year, month, day), y = avg_mm_rainfall), color = "red")+ # actual rainfall
  scale_x_date(limits=as.Date(c("2023-01-01","2024-01-01")), breaks=as.Date(c("2023-01-01","2023-04-01","2023-07-01","2023-10-01","2024-01-01"),format="%Y-%m-%d"), date_labels = "%b %Y",expand = c(0, 0))


