library (tidyverse)
library (lubridate)

# read in csv with daily averages for a given basin
basins <- list.files("C:\\texas_mpe\\arrow\\metrics\\norms", pattern = "\\.csv$", full.names = TRUE)

boi <- read_csv(basins[10])

# Let's look at annual totals first

tot <- boi |>
  mutate (month_day = paste0(month,"_",day)) |>
  filter (month_day == "12_31") |>
  mutate (ymd = ymd(paste0(year,month,day)))

# annual accumulations graph
ggplot (data = tot, aes(x=ymd,y=(avg_mm_rainfall/25.4),label=sprintf("%0.2f", round((avg_mm_rainfall/25.4), digits = 2)))) +  
  geom_line()+
  geom_point()+
  geom_label()

# Let's look at calculating values for each month_day combo

env <- boi |>
  group_by(month, day) |>
  summarise (min_rain = min(avg_mm_rainfall, na.rm=TRUE),
             max_rain = max(avg_mm_rainfall, na.rm=TRUE),
             median_rain = median(avg_mm_rainfall, na.rm=TRUE)) |>
  ungroup()|>
  mutate(year = 2028)|>
  mutate (date = make_date(year, month, day))


ggplot (data = env, aes(date)) +
  geom_ribbon(aes(ymin=min_rain,ymax=max_rain)) +
  geom_line(aes(y = median_rain))





