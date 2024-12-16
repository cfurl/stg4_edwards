# This script creates a tibble (in the environment) that contains cumulative precipitation across each sub-basin for a historical date.
# It assigns it a 'card_color' and a 'card_category' based on where the cumulative precipitation for a date lies within it's historical
# context for that month_day combination.  All 9 subbasins + entire recharge zone are included in loop.

# Pretending like all of my automation in "/radar_stats/cumulative_precip/' is figured out and I'm creating this figure on 12/31/2023.

library(tidyverse)
library(sf)
library("ggmap")

#each of the csv's listed here contain:
  # timestamp
  # cumulative precipitation in cubic meters from Jan 1 -> current date
  # sub basin area
  # cumulative precipitation in mm from Jan 1 -> current date
# The data go from 1/1/2002 -> 12/31/2023.  
# Whenever you figure out your automation backend life you could potentially rewrite these .csv's daily
basin_avg_precip <- list.files (".//radar_stats//cumulative_precip", pattern = "\\.csv$")

# initialize a tibble to loop through
final <- tibble()

# date_end is the 'current date' or date of interest
date_end <- as.Date("2023-12-31")
date_begin <- as.Date("2023-01-01") # where do i use this?

for (b in basin_avg_precip) {
  
 # b<-"Bexar_daily_averages_2002_2023.csv"
  
  # read in 22 year record
  ddd<- read_csv (paste0(".//radar_stats//cumulative_precip//",b)) |>
    mutate(date = make_date(year, month, day))
  
  # grab "date_end", cumulative linear rainfall (mm), and a basin label
  cum_total_date_end <- ddd|>
    filter(date==date_end) |>
    select(6:7) |>
    set_names (c("cum_basin_avg_precip_mm", "date")) |>
    mutate (basin = str_remove(b,"_daily_averages_2002_2023.csv"))
 
  # group by 'month_day' combination and calculate some different percentiles.
  # then select row that corresponds with your 'month_day' combination in 'date_end'
  # in this case the result is different percentiles for 12_31 across 22 year record
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
  
  # combine your actual precip for date end, with the percentiles  
  # big ugly tibble you could clean this up 
  ccc<- bind_cols(cum_total_date_end, stats_date_end) 

   
  # use case_when to set some colors and card categories based on where the precipitation for date end falls
  # within the context of the percentiles based on 22 years previous data
  # colors and categories are flexible, not a lot of thought went in here
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
  
 # combine cumulative precip, quantiles, card color, and card message
  ttt<- pick_color |>
    select(cum_basin_avg_precip_mm, date...2, basin, card_color, card_cat)
    
  
  final <- bind_rows (final,ttt)
  
}


# map indiviudal sub basins, joint with the basin
sub_map <- read_sf(".\\gis\\boundaries_features\\usgs_basins.shp") |>
  st_cast( "MULTIPOLYGON")|>
  st_transform(4326) |> # had to make a transformation here; had difficulty with getting ggmap and sf object cooperating
  set_names(c("basin","geometry"))|>
  left_join(final, by="basin" )|>
  relocate(geometry, .after = last_col())

# grab your tiles from Stadia.  Your API key is hard written in your .Renviron.
ear <- get_stadiamap(bbox = c(left = -100.85, bottom = 29.0, 
                              right = -97.75, top = 30.4), 
                     zoom = 9,
                     maptype = "alidade_smooth_dark",
                     crop = TRUE)

# set categories and colors
cols <- c("below 10%" = "#A92B26", "above 10% below 25%" = "#FFC348", "above 25% below 40%" = "#F6FB07", "below median above 40%" = "#22FE05","above median below 60%" = "#22FE05","above 60% below 75%" = "#2CAC1B", "above 75% below 90%" = "#248418", "above 90%" = "#0826A2")

s1 <- ggmap(ear) +
#ggplot()+
  geom_sf(data = sub_map, aes(fill= card_cat), color = "black",linewidth = .05,alpha = 0.5, show.legend=TRUE,   inherit.aes = FALSE)+ 
  annotate(geom="text",x= -100.75,y=30.28,label="Edwards Aquifer Recharge Sub-basins",size=4,hjust=0, color = "#F1F4FC")+
  annotate(geom="text",x= -100.4,y=30,label="1",size=3,hjust=1, color = "#F1F4FC")+
  annotate(geom="text",x= -99.78,y=29.92,label="2",size=3,hjust=1, color = "#F1F4FC")+
  annotate(geom="text",x= -99.55,y=29.8,label="3",size=3,hjust=1, color = "#F1F4FC")+
  annotate(geom="text",x= -99.38,y=29.68,label="4",size=3,hjust=1, color = "#F1F4FC")+
  annotate(geom="text",x= -99.45,y=29.85,label="5",size=3,hjust=1, color = "#F1F4FC")+
  annotate(geom="text",x= -98.77,y=29.65,label="6",size=3,hjust=1, color = "#F1F4FC")+
  annotate(geom="text",x= -98.75,y=29.79,label="7",size=3,hjust=1, color = "#F1F4FC")+
  scale_color_discrete(cols,drop = FALSE)


#ggsave(filename = "sub_basin.png", device = "png", path = ".\\plots", plot = s1, width = 6.5, height = 4.5, units = "in")
