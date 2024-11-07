library(arrow)
library(dplyr)
library(tictoc)
library(sf)
library(janitor)
library(magick)
library(ggplot2)
library(units)
library(ggplot2)


# identify the root folder of your parquet files
#month_parq <- "C:\\texas_mpe\\arrow\\st4_parq"
month_parq <- ".\\parquet\\parquet_files"

# establish your connection, this is the time to see your file system schema and make any adjustments
tx_rain <- open_dataset(month_parq)
nrow(tx_rain)

# have a query space and don't collect yet
sum_rain_query <- tx_rain %>%
  filter(year==2015) %>%
  group_by (grib_id) %>%
  summarize(
    sum_rain = sum(rain_mm, na.rm=TRUE)
  ) %>%
  arrange(desc(sum_rain))

# collect the query when you are ready (grouped and summed over 120 million rows in under three minutes)
tic()
sum_rain_collect <- collect(sum_rain_query)
toc()


# read in the shape file of the ST4 bins clipped to State of Texas
# Join the just completed query with ST4 bins using 'grib_id'
bin_outline <- read_sf(".\\gis\\clipped_hrap\\usgs_recharge_basins\\usgs_dissolved.shp") 
map <- read_sf(".\\gis\\clipped_hrap\\usgs_recharge_basins\\usgs_dissolved.shp") |>
  #map <- read_sf("C:\\texas_mpe\\arrow\\gis\\texas_grib_bins_clipped.shp") |>
  clean_names() |>
  left_join(sum_rain_collect,
            by = "grib_id" )

map_math <- map |> st_drop_geometry() |>
  mutate(cubic_m_precip = bin_area * sum_rain * .001)

sum(map_math$cubic_m_precip) * 39.37/sum(map_math$bin_area) # 39.37 is meter to inch

# read in png of your logo
logo <- image_read(path='.\\gis\\logo\\EAHCP_color_vertical logo.png')
#logo <- image_read(path='C:\\texas_mpe\\arrow\\logo\\mazari-newfel.jpg')
# create a plot box to help you position your logo
plot_box <- tibble(xmin = st_bbox(map)[1],
                   xmax = st_bbox(map)[3],
                   ymin = st_bbox(map)[2],
                   ymax = st_bbox(map)[4],
                   xrange = xmax - xmin,
                   yrange = ymax - ymin) 








p1 <- ggplot() +
  
  geom_sf(data = map, aes(fill = sum_rain), color = NA)+
  
  scale_fill_stepsn(name = "mm",
                    #colours=c("#0826A2","#22FE05","#2CAC1B","#248418", "#F6FB07", "#FFE890", "#FFC348","#E01E17", "#A92B26","#8C302C","#CC17DA", "#AE6DB3","#8798C6" ),
                    colours=c(colours),
                    # this is n = colors - 1
                    #breaks = c(150, 200, 250, 375, 500, 625, 750, 1000, 1250, 1500, 2000, 2500),
                    breaks = c(breaks),
                    #limits=c(0,3000),
                    limits = c (min,max),
                    # this is n = colors; and midpoint of break
                    #values = scales::rescale(c(75, 175, 225, 312.5, 437.5, 562.5, 687.5, 875, 1125, 1375, 1750, 2250, 2750)),
                    values = scales::rescale(c(values)), 
                    show.limits = FALSE,
                    na.value = "#F1F4FC")+
  
  ggtitle("2015 Precipitation") +
  geom_sf(data = bin_outline, fill=NA) +
  annotation_raster(logo, 
                    # Position adjustments here using plot_box$max/min/range
                    ymin = plot_box$ymin + 1,
                    ymax = plot_box$ymin + 1 + plot_box$yrange*.1, 
                    xmin = plot_box$xmin + 2,
                    xmax = plot_box$xmin + 2 + plot_box$xrange * .1) +
  theme(legend.key.height = unit(1.4, "cm"),
        plot.margin=unit(c(0,0,0,0), 'cm'),
        axis.text.y = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.ticks = element_blank(),
        plot.background = element_rect(color = "#F1F4FC", fill = "#F1F4FC"),
        panel.grid.major = element_blank(),
        legend.background= element_rect(fill = "#F1F4FC"),
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 7),
        panel.background = element_rect(fill = "#F1F4FC"))


ggsave(filename = "eaa_test_recharg3.png", device = "png", path = ".\\plots", plot = p1, width = 6.5, height = 4.5, units = "in")








