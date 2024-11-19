library(tidyverse)
# Pretending like all of my automation is figured out and I'm creating this figure on 12/31/2023


basin_avg_precip <- list.files (".//radar_stats//cumulative_precip", pattern = "\\.csv$")

final <- tibble()
date_end <- as.Date("2023-12-31")
date_begin <- as.Date("2023-01-01")

for (b in basin_avg_precip) {
  
  b<-"Bexar_daily_averages_2002_2023.csv"
  
  ddd<- read_csv (paste0(".//radar_stats//cumulative_precip//",b)) |>
    mutate(date = make_date(year, month, day))
  
  cum_total_date_end <- ddd|>
    filter(date==date_end) |>
    select(6:7) |>
    set_names (c("cum_basin_avg_precip_mm", "date")) |>
    mutate (basin = str_remove(b,"_daily_averages_2002_2023.csv"))
 
   stats_date_end <- ddd |>
    group_by(month, day) |>
    summarise (min_rain = min(avg_mm_rainfall, na.rm=TRUE),
               perc10 = quantile(avg_mm_rainfall, 0.10),
               perc25 = quantile(avg_mm_rainfall, 0.25),
               perc40 = quantile(avg_mm_rainfall, 0.40),
               median_rain = median(avg_mm_rainfall, na.rm=TRUE),
               perc60 = quantile(avg_mm_rainfall, 0.60),
               perc75 = quantile(avg_mm_rainfall, 0.75),
               perc90 = quantile(avg_mm_rainfall, 0.90),
               max_rain = max(avg_mm_rainfall, na.rm=TRUE)) |>
    ungroup()|>
    mutate(year = year(date_end))|>
    mutate (date = make_date(year, month, day)) |>
     filter(date==date_end)
  
   ccc <- bind_cols(cum_total_date_end,stats_date_end, by = "date") 
   
  pick_color
  
}