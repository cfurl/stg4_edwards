library (tidyverse)
library (lubridate)

# read in csv with daily averages for a given basin
basins <- list.files("C:\\texas_mpe\\arrow\\metrics\\norms", pattern = "\\.csv$", full.names = TRUE)

# you can do this for any basin, currently just doing it for 'usgs_dissolved.shp'
boi <- read_csv(basins[10])

# Let's look at annual totals first

#  tot <- boi |>
#    mutate (month_day = paste0(month,"_",day)) |>
#    filter (month_day == "12_31") |>
#    mutate (ymd = ymd(paste0(year,month,day)))

#ggplot (data = tot, aes(x=ymd,y=(avg_mm_rainfall/25.4),label=sprintf("%0.2f", round((avg_mm_rainfall/25.4), digits = 2)))) +  
#  geom_line()+
#  geom_point()+
#  geom_label()

###### Let's look at calculating values for each month_day combo

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
  geom_line(data = boi, aes(x = make_date(year, month, day), y = avg_mm_rainfall), color = "red")+
  scale_x_date(limits=as.Date(c("2023-01-01","2024-01-01")), breaks=as.Date(c("2023-01-01","2023-04-01","2023-07-01","2023-10-01","2024-01-01"),format="%Y-%m-%d"), date_labels = "%b %Y",expand = c(0, 0))


