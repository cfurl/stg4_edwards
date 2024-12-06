# This graph creates a hyetograph for a basin of interest (in this case using entire basin) with a ribbon around it and a median line

library (tidyverse)
library (lubridate)

# read in csv with daily averages for a given basin
basin_spark <- list.files (".//radar_stats//cumulative_precip", pattern = "\\.csv$")

# initialize a tibble to loop through
spark_tibble <- tibble()

# calc ribbon stats for year = 2023; loop through sub basins
for (b in basin_spark) {
  
#b <- "Bexar Basin_daily_averages_2002_2023.csv"
    
  boi <- read_csv(paste0(".//radar_stats//cumulative_precip//",b))

###### Let's look at calculating values for each month_day combo
# this is essentially the same code used in 'subbasin_cum_precip_and_colors.R'
# should combine and make single script....

# this block calculates percentiles  
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
  mutate (date = make_date(year, month, day))|>
  mutate (basin = str_remove(b,"_daily_averages_2002_2023.csv"))

# This grabs recorded cumulative rainfall
cum_rain <- boi |>
  select(month, day, year, avg_mm_rainfall)

# combine recorded cumulative rainfall with your percentiles

rain_with_percentiles <- env |>
  left_join(cum_rain, by = c("day","month","year"))


spark_tibble <- bind_rows(spark_tibble, rain_with_percentiles)
}


# line graphs

ggplot () +
  geom_ribbon(data = spark_tibble |> filter (basin == "usgs_dissolved"), aes(x = date, ymin=perc25,ymax=perc75)) +  
  geom_line(data = spark_tibble |> filter (basin == "usgs_dissolved"), aes(x = date, y = median_rain)) +
  geom_line(data = spark_tibble |> filter (basin == "usgs_dissolved"), aes(x = date, y = avg_mm_rainfall), color = "red")+ # actual rainfall
  scale_x_date(limits=as.Date(c("2023-01-01","2024-01-01")), breaks=as.Date(c("2023-01-01","2023-04-01","2023-07-01","2023-10-01","2024-01-01"),format="%Y-%m-%d"), date_labels = "%b %Y",expand = c(0, 0))


