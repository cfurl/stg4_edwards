library(tidyverse)
library(ggiraph)

#facet plot ideal for multiple basins
plot_ribbon_facet_interactive<-function(
    data, 
    basins, 
    legend = T,
    title = "Cumulative Precpitation by Subbasin 2023",
    subtitle = "Rainfall measured in milimeters",
    caption = "Source: Edwards Aquifer Authority",
    pal_title = "#00859B",
    pal_subtitle = "black", 
    pal_caption = "grey50",
    pal_avg = '#00859B', pal_median = "#747474"){
  
  sysfonts::font_add_google("Open Sans", family = "Open Sans")
  showtext::showtext_auto()
  font = "Open Sans"
  
  if (!is.na(title)) {
    plot_title <- element_text(face='bold', color=pal_title)
  }
  else {
    plot_title<-element_blank()
  }
  
  if (!is.na(subtitle)) {
    plot_subtitle <- element_text(color=pal_subtitle)
  }
  else {
    plot_subtitle<-element_blank()
  }
  
  if (!is.na(caption)) {
    plot_caption <- element_text(color=pal_caption, hjust=0)
  }
  else {
    plot_caption<-element_blank()
  }
  
  if (legend) {
    plot_legend_position <- "top"
  }
  else {
    plot_legend_position = "none"
  }
  
  
  #get subset of data
  df<-data|>filter(basin %in% basins)
  
  df_pivoted<-df|>
    select(date, basin, median_rain, avg_mm_rainfall)|>
    pivot_longer(cols=c(median_rain,avg_mm_rainfall))|>
    mutate(name = case_when(name=='median_rain' ~ 'Median', TRUE ~ 'Actual'))|>
    left_join(
      df|>select(date, basin, name, perc25, perc75), by=c("date"="date", "name"="name", "basin"="basin")
    )
  
  #factor make sure Actual line has the higher z-index (plots over Median)
  df_pivoted$name<-factor(df_pivoted$name, levels=c("Median", "Actual"))
  
  plot<-ggplot(data=df_pivoted) +
    geom_ribbon(mapping=aes(x =date, ymin=perc25,ymax=perc75, fill=name), alpha=0.2) +  
    geom_line_interactive(mapping=aes(x = date, y = value, color=name), linewidth=0.6)+
    facet_wrap(~basin)+
    scale_x_date(
      date_labels = "%b",   
      #date_breaks = "3 months",
      expand = c(0,0)
    ) +
    scale_y_continuous(
      expand = c(0,0)
    )+
    scale_color_manual(
      values = c("Actual" = pal_avg, "Median" = pal_median),
      breaks = c("Actual", "Median")
    ) +
    scale_fill_manual(
      values = c("Actual" = "transparent", "Median" = pal_median),
      breaks = c("Actual", "Median")
    ) +
    labs(
      color = '',
      fill = '',
      x= '')+
    theme_minimal()+
    theme(
      legend.position = plot_legend_position,
      legend.location = "plot",
      legend.title = element_text(size=9),
      legend.margin = margin(l=0),
      legend.box.margin = margin(rep(0,4)),
      plot.title = plot_title, 
      plot.subtitle = plot_subtitle,
      plot.caption.position = "plot",
      plot.caption = plot_caption,
      plot.margin = margin(t=15, b=15, l=15, r=15),
      plot.title.position = "plot",
      legend.justification = "left",
      panel.spacing = unit(1.5, "lines"),
      axis.title.y = element_blank(),
      axis.line.x = element_line(color='black'),
      text = element_text(family=font),
      strip.text = element_text(hjust=0),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    )
  
  if(!is.na(title)){
    plot<-plot + labs(title =title)
  }
  
  if(!is.na(subtitle)){
    plot<-plot + labs(subtitle = subtitle)
  }
  
  if(!is.na(caption)){
    plot<-plot + labs(caption = caption)
  }
  
  return(plot)
}



#facet plot ideal for multiple basins
plot_ribbon_interactive<-function(
    data, 
    select_basin, 
    title = "Cumulative Precpitation by Subbasin 2023",
    subtitle = "Rainfall measured in milimeters",
    caption = "Source: Edwards Aquifer Authority",
    legend = T,
    pal_title = "#00859B",
    pal_subtitle = "black", 
    pal_caption = "grey50",
    pal_avg = '#00859B', pal_median = "#747474"){
  
  sysfonts::font_add_google("Open Sans", family = "Open Sans")
  showtext::showtext_auto()
  font = "Open Sans"
  
  if (!is.na(title)) {
    plot_title <- element_text(face='bold', color=pal_title)
  }
  else {
    plot_title<-element_blank()
  }
  
  if (!is.na(subtitle)) {
    plot_subtitle <- element_text(color=pal_subtitle)
  }
  else {
    plot_subtitle<-element_blank()
  }
  
  if (!is.na(caption)) {
    plot_caption <- element_text(color=pal_caption, hjust=0)
  }
  else {
    plot_caption<-element_blank()
  }
  
  if (legend) {
    plot_legend_position <- "top"
  }
  else {
    plot_legend_position = "none"
  }
  
  #get subset of data
  df<-data|>filter(basin == select_basin)
  
  df_pivoted<-df|>
    select(date, basin, median_rain, avg_mm_rainfall)|>
    pivot_longer(cols=c(median_rain,avg_mm_rainfall))|>
    mutate(name = case_when(name=='median_rain' ~ 'Median', TRUE ~ 'Actual'))|>
    left_join(
      df|>select(date, basin, name, perc25, perc75), by=c("date"="date", "name"="name", "basin"="basin")
    )
  
  #factor make sure Actual line has the higher z-index (plots over Median)
  df_pivoted$name<-factor(df_pivoted$name, levels=c("Median", "Actual"))
  
  plot<-ggplot(data=df_pivoted) +
    geom_ribbon(mapping=aes(x =date, ymin=perc25,ymax=perc75, fill=name), alpha=0.2, show.legend = T) +  
    geom_line_interactive(mapping=aes(x = date, y = value, color=name, data_id = name), linewidth=0.6)+
    scale_x_date(
      date_labels = "%b",   
      # date_breaks = "3 months",
      expand = c(0,0)
    ) +
    scale_y_continuous(
      expand = c(0,0)
    )+
    scale_color_manual(
      values = c("Actual" = pal_avg, "Median" = pal_median),
      breaks = c("Actual", "Median")
    ) +
    scale_fill_manual(
      values = c("Actual" = "transparent", "Median" = pal_median),
      breaks = c("Actual", "Median")
    ) +
    labs(
      color = '',
      fill = '',
      x= '')+
    theme_minimal()+
    theme(
      legend.position = plot_legend_position,
      legend.location = "plot",
      legend.title = element_text(size=9),
      legend.margin = margin(l=0),
      legend.box.margin = margin(rep(0,4)),
      plot.caption.position = "plot",
      plot.margin = margin(t=15, b=15, l=15, r=15),
      plot.caption = plot_caption,
      plot.title.position = "plot",
      legend.justification = "left",
      axis.title.y = element_blank(),
      axis.line.x = element_line(color='black'),
      text = element_text(family=font),
      plot.title = plot_title,
      plot.subtitle = plot_subtitle,
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    )
  
  if(!is.na(title)){
    plot<-plot + labs(title =title)
  }
  
  if(!is.na(subtitle)){
    plot<-plot + labs(subtitle = subtitle)
  }
  
  if(!is.na(caption)){
    plot<-plot + labs(caption = caption)
  }
  
  return(plot)
}


# cumulative_subbasin<-read.csv("./data/processed/cumulative_subbasin.csv")|>
#   mutate(date = as.Date(date))
# 
# basins<-cumulative_subbasin|>filter(basin!='usgs_dissolved')|>distinct(basin)|>pull()

#for bin map 
# map <- sf::read_sf("./data/gis/clipped_hrap/usgs_recharge_basins/usgs_dissolved.shp")
# streams <- read_sf("./data/gis/boundaries_features/streams_recharge.shp")
# lakes <- read_sf("./data/gis/boundaries_features/reservoirs.shp")
# tx_rain <- arrow::open_dataset("./data/parquet/parquet_files")
# 
# sum_rain_query <- tx_rain %>%
#   filter(year==2015) %>%
#   group_by (grib_id) %>%
#   summarize(
#     sum_rain = sum(rain_mm, na.rm=TRUE)) %>%
#   arrange(desc(sum_rain))
# 
# tic()
# sum_rain_collect <- collect(sum_rain_query)
# toc()
# 
# map_rain <- map|>
#   left_join(sum_rain_collect, by = "grib_id")|>
#   mutate(cubic_m_precip = bin_area * sum_rain * 0.001)

###PLOT - Regular Hyetograph####
# girafe(ggobj =plot_ribbon_interactive(
#   data = cumulative_subbasin, 
#   select_basin = 'usgs_dissolved', 
#   pal_avg = style$pal$orange,
#   pal_title = "black",
#   title = "Cumulative Precipitation 2023",
#   subtitle = "Comparison of average and median rainfall (mm) YTD",
#   caption = "Source: Edwards Aquifer Authority",
#   legend = T
# ))
