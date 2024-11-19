library(tidyverse)
# Pretending like all of my automation is figured out and I'm creating this figure on 12/31/2023


basin_avg_precip <- list.files (".//radar_stats//cumulative_precip", pattern = "\\.csv$")

final <- tibble()
date_end <- as.Date("2023-12-31")
date_begin <- as.Date("2023-01-01")

for (b in basin_avg_precip) {
  
 # b<-"Bexar_daily_averages_2002_2023.csv"
  
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
   
  ccc<- bind_cols(cum_total_date_end, stats_date_end) 

   
     
 pick_color <- ccc |>
    mutate(
      card_color = case_when 
      (cum_basin_avg_precip_mm < perc10 ~ "#A92B26",
       cum_basin_avg_precip_mm >= perc10 & cum_basin_avg_precip_mm < perc25  ~ "#FFC348",
       cum_basin_avg_precip_mm >= perc25 & cum_basin_avg_precip_mm < perc40  ~ "#F6FB07",
       cum_basin_avg_precip_mm >= perc40 & cum_basin_avg_precip_mm < median_rain  ~ "#22FE05",
       cum_basin_avg_precip_mm >= median_rain & cum_basin_avg_precip_mm < perc60  ~ "#22FE05",
       cum_basin_avg_precip_mm >= perc60 & cum_basin_avg_precip_mm < perc75  ~ "#2CAC1B",
       cum_basin_avg_precip_mm >= perc75 & cum_basin_avg_precip_mm < perc90  ~ "#248418",
       cum_basin_avg_precip_mm > perc90  ~ "#0826A2",
       .default = "other")) |>
    mutate(
      card_cat = case_when (
      cum_basin_avg_precip_mm < perc10 ~ "below 10%",
      cum_basin_avg_precip_mm >= perc10 & cum_basin_avg_precip_mm < perc25  ~ "above 10% below 25%",
      cum_basin_avg_precip_mm >= perc25 & cum_basin_avg_precip_mm < perc40  ~ "above 25% below 40%",
      cum_basin_avg_precip_mm >= perc40 & cum_basin_avg_precip_mm < median_rain  ~ "below median above 40%",
      cum_basin_avg_precip_mm >= median_rain & cum_basin_avg_precip_mm < perc60  ~ "above median below 60%",
      cum_basin_avg_precip_mm >= perc60 & cum_basin_avg_precip_mm < perc75  ~ "above 60% below 75%",
      cum_basin_avg_precip_mm >= perc75 & cum_basin_avg_precip_mm < perc90  ~ "above 75% below 90%",
      cum_basin_avg_precip_mm > perc90  ~ "above 90%",
      .default = "other")) 
  
  ttt<- pick_color |>
    select(cum_basin_avg_precip_mm, date...2, basin, card_color, card_cat)
    
  
  final <- bind_rows (final,ttt)
  
  
  
}