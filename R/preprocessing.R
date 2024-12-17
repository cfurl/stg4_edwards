library(tidyverse)

file_list <- list.files("./data/cumulative_precip", pattern = "\\.csv$", full.names = TRUE)

cumulative_subbasin <- file_list |> 
  # Read all files and add a column for basin name
  purrr::map_df(function(file) {
    # Read data
    boi <- read_csv(file)
    
    # Extract basin name
    basin_name <- str_remove(basename(file), "_daily_averages_2002_2023.csv")
    
    # Calculate percentiles
    env <- boi |> 
      group_by(month, day) |> 
      summarise(
        min_rain = min(avg_mm_rainfall, na.rm = TRUE),
        max_rain = max(avg_mm_rainfall, na.rm = TRUE),
        perc10 = quantile(avg_mm_rainfall, 0.10, na.rm = TRUE),
        perc25 = quantile(avg_mm_rainfall, 0.25, na.rm = TRUE),
        perc75 = quantile(avg_mm_rainfall, 0.75, na.rm = TRUE),
        perc90 = quantile(avg_mm_rainfall, 0.90, na.rm = TRUE),
        median_rain = median(avg_mm_rainfall, na.rm = TRUE),
        .groups = "drop"
      ) |> 
      mutate(
        year = 2023,
        date = make_date(year, month, day),
        basin = basin_name
      )
    
    # Extract cumulative rainfall
    cum_rain <- boi |> 
      select(month, day, year, avg_mm_rainfall)
    
    # Combine with percentiles
    rain_with_percentiles <- env |> 
      left_join(cum_rain, by = c("day", "month", "year"))
    
    return(rain_with_percentiles)
  })

cumulative_subbasin<-cumulative_subbasin|>
  mutate(basin = str_replace(basin, " Basin",""),
                            name = "Median")

write.csv(cumulative_subbasin, "./data/processed/cumulative_subbasin.csv", row.names=F)
