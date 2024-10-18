# my goal here is to develop median/avg/percentile rainfall amounts basin averaged for each day of the year.
# I am going to do this for the whole record although there is some early stuff I don't fully trust, I don't have
# a great reason to discard it until I do a forma analysis.

library(arrow)
library(dplyr)
library(tictoc)
library(sf)
library(units)
library(tidyverse)

# start with your arrow query:

# identify the root folder of your parquet files
eaa_parq <- "C:\\texas_mpe\\arrow\\st4_parq_eaa"

# establish your connection, this is the time to see your file system schema and make any adjustments
eaa_rain <- open_dataset(eaa_parq)


aoi <- list.files ("C:\\texas_mpe\\arrow\\gis\\eaa_recharge_basins\\clipped_hrap", pattern = "\\.shp$")

#outer loop
for (a in aoi) {
  
  # set your shapefile of interest and solve for basin area
  map <- read_sf(paste0("C:\\texas_mpe\\arrow\\gis\\eaa_recharge_basins\\clipped_hrap\\",a)) |>
    st_drop_geometry()
  basin_area <- sum(map$bin_area)
  
  # initilalize tibble
  final <- tibble()
  
  # inner loop
  for (y in 2002:2023) {
    
    eaa_query <- eaa_rain %>%
      filter(year == y) %>%
      group_by(grib_id,month, day, year) %>%
      summarize(
        sum_rain = sum(rain_mm, na.rm=TRUE)
      )
    
    #tic() 
    eaa_collect <- collect (eaa_query)   
    #toc()
    
    cp <- eaa_collect |>
      group_by(grib_id) |>
      arrange(month,day) |>
      mutate (cum_sum_rain = cumsum(sum_rain))|>
      arrange(grib_id) 
    
    map_math <- left_join(map,cp, by = "grib_id" )|> #this keeps all grib_id's in map
      mutate(cubic_m_precip = bin_area * sum_rain * .001)|>
      mutate(cum_cubic_m_precip = bin_area * cum_sum_rain * .001)
    
    # at this point for each bin I know: area, daily precip (mm), cumulative daily precip (mm), daily cubic precip (m3), cumulative daily cubic precip (m3)
    
    # finding averages
    
    yo <- map_math |>
      group_by(month, day) |>
      mutate(daily_cum_cubic_m_across_basin = sum(cum_cubic_m_precip)) |>
      select(month, day, year, daily_cum_cubic_m_across_basin) |>
      distinct () |>
      mutate(basin_area = basin_area) |>
      mutate(avg_mm_rainfall = daily_cum_cubic_m_across_basin/basin_area*1000)
    
    final <- bind_rows (final,yo)
    
  }
  
  write_csv(final,paste0("C:\\texas_mpe\\arrow\\metrics\\norms\\",a,"_daily_averages_2002_2023.csv"))
}
