library(gt)
library(gtExtras)

source("./R/branded_style.R")

cumulative_subbasin<-read.csv("./data/processed/cumulative_subbasin.csv")|>
  mutate(date = as.Date(date))


df_table<-cumulative_subbasin|>
  filter(basin != 'usgs_dissolved')|>
  select(date, basin, median_rain, avg_mm_rainfall)|>
  pivot_longer(cols=c(median_rain,avg_mm_rainfall))|>
  mutate(name = case_when(name=='median_rain' ~ 'Median', TRUE ~ 'Actual'))|>
  filter(name == 'Actual')|>
  group_by(basin)|>
  summarise(cumulative = round(max(value, na.rm=T),2),
            trend = list(value))


gt(df_table)|>
  cols_label(
    basin = "Basin",
    cumulative = "Cumulative (mm)",
    trend = "YTD"
  )|>
  cols_width(
    trend ~ px(50)
  )|>
  gtExtras::gt_plt_sparkline(
    trend, 
    fig_dim=c(5,20),
    palette = c("grey", "black", "transparent", style$pal$orange, "black")
  )|>
  opt_table_font(
    font = list(
      google_font(name = style$font)
    )
  )