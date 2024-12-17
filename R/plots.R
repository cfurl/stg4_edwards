library(tidyverse) 
library(patchwork)

###SETUP####
#import functions and braidning
source("./R/function_ribbon.R")
source("./R/branded_style.R")
source("./R/function_bin_map.R")

#useful for text renderin if font is not available locally
sysfonts::font_add_google("Open Sans", family = "Open Sans")
showtext::showtext_auto()
showtext::showtext_opts(dpi=300)

###DATA####
#for ribbon charts
cumulative_subbasin<-read.csv("./data/processed/cumulative_subbasin.csv")|>
  mutate(date = as.Date(date))

basins<-cumulative_subbasin|>filter(basin!='usgs_dissolved')|>distinct(basin)|>pull()

#for bin map 
map <- sf::read_sf("./data/gis/clipped_hrap/usgs_recharge_basins/usgs_dissolved.shp")
streams <- read_sf("./data/gis/boundaries_features/streams_recharge.shp")
lakes <- read_sf("./data/gis/boundaries_features/reservoirs.shp")
tx_rain <- arrow::open_dataset("./data/parquet/parquet_files")

sum_rain_query <- tx_rain %>%
  filter(year==2015) %>%
  group_by (grib_id) %>%
  summarize(
    sum_rain = sum(rain_mm, na.rm=TRUE)) %>%
  arrange(desc(sum_rain))

tic()
sum_rain_collect <- collect(sum_rain_query)
toc()

map_rain <- map|>
  left_join(sum_rain_collect, by = "grib_id")|>
  mutate(cubic_m_precip = bin_area * sum_rain * 0.001)

###PLOT - Regular Hyetograph####
plot_ribbon(
  data = cumulative_subbasin, 
  select_basin = 'usgs_dissolved', 
  pal_avg = style$pal$orange,
  pal_title = "black",
  title = "Cumulative Precipitation 2023",
  subtitle = "Comparison of average and median rainfall (mm) YTD",
  caption = "Source: Edwards Aquifer Authority",
  legend = T
)

#ggsave("./outputs/ribbon_basin.png",  bg="white", width = 6, height = 6, units="in", dpi=300)

###PLOT - Facetted Hyetograph####
plot_ribbon_facet(
  data = cumulative_subbasin, 
  basins = basins, 
  pal_avg = style$pal$orange,
  pal_title = "black",
  title = "Cumulative Precipitation by Subbasin",
  subtitle = "Comparison of average and median rainfall (mm) YTD",
  caption = "Source: Edwards Aquifer Authority",
  legend = T
)

#ggsave("./outputs/ribbon_subbasin_facet.png",  bg="white", width = 6, height = 6, units="in", dpi=300)


###PLOT - Combo####
#uses patchwork to combine regular hyetograph with facetted one
precip_facet<-plot_ribbon_facet(
  data = cumulative_subbasin, 
  basins = basins, 
  pal_avg = style$pal$orange,
  pal_title = "black",
  title = NA,
  subtitle = "Subbasin Level",
  caption = NA,
  legend = T
  )

precip_total<-plot_ribbon(
  data = cumulative_subbasin, 
  select_basin = "usgs_dissolved", 
  pal_avg = style$pal$orange,
  pal_title = "black",
  title = NA,
  subtitle = "Basin Level",
  caption = NA)


precip_combo<-(guide_area() + 
    (
      precip_total + 
      (precip_facet & theme(axis.text = element_text(size=7), strip.text = element_text(size=7))) +
      plot_layout(widths=c(1.25,1.5)))
      ) +
  plot_layout(
    guides = "collect", 
    heights = unit(c(0.1, 0.1), c("cm", "null")),
  ) + 
  plot_annotation(
    title = "Cumulative Precipitation Year-to-Date",
    subtitle = "Comparison of average and median rainfall (mm) year to date for basins and subbasins.",
    caption = "Source: Edwards Aquifer Authority"
  ) & 
  theme(
    text = element_text(family = style$font, size = 12),
    plot.title = element_text(face = 'bold'),
    plot.caption = element_text(hjust = 0, color="grey60"),
    legend.justification = c(-0.05, 0),
    legend.title = element_blank(),
    legend.margin = margin(0, 0, 0, 0),
    plot.margin = margin(rep(10,4)) 
  )

#ggsave("./outputs/ribbon_combo.png",precip_combo,  bg="white", width = 9, height = 6, units="in", dpi=300)

#dark mode
bin_map_dark<-plot_bin_map(title = 'Edwards Aquifer Recharge Zone',
                   subtitle= "September 2015 Precipitation. STG4 QPE BIN 4km.",
                   font = "Open Sans",
                   map_rain = map_rain,
                   map_streams = streams,
                   map_lakes = lakes,
                   pal_water='black',
                   pal_title='white',
                   pal_subtitle='white',
                   pal_outline='black',
                   pal_legend = 'YlGnBu',
                   pal_bin_outline='black',
                   pal_legend_text='white',
                   map_type='cartodark')

bin_map_dark

ggsave("./outputs/bin_map_dark.png",bin_map_dark, width = 12, height = 9, units="in", dpi=300)


#light mode
bin_map_light<-plot_bin_map(subtitle= "September 2015 Precipitation. STG4 QPE BIN 4km.",
               map_rain = map_rain,
               map_streams = streams,
               map_lakes = lakes,
               pal_water='#697984',
               #pal_water = '#2C6690',
               pal_title='black',
              # pal_legend = 'YlOrRd',
               bin_alpha = 0.9,
               pal_subtitle='black',
               pal_outline="#697984",
               pal_bin_outline='white',
               pal_legend_text='black',
               map_type='cartolight')
 
 
 
 bin_map_light
 
 ggsave("./outputs/bin_map_light.png",bin_map_light, width = 12, height = 9, units="in", dpi=300)


