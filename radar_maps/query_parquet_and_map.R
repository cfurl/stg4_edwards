library(arrow)
library(dplyr)
library(tictoc)
library(sf)
library(janitor)
library(magick)
library(ggplot2)
library(units)
library(ggplot2)
library(zoo)


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


# read in the shape file of the ST4 bins clipped to the EAA recharge area
# Join the just completed query with ST4 bins using 'grib_id'

map <- read_sf(".\\gis\\clipped_hrap\\usgs_recharge_basins\\usgs_dissolved.shp") |>
  clean_names() |>
  left_join(sum_rain_collect, by = "grib_id" )

map_math <- map |> st_drop_geometry() |>
  mutate(cubic_m_precip = bin_area * sum_rain * .001)

bap <- sum(map_math$cubic_m_precip) * 39.37/sum(map_math$bin_area) # 39.37 is meter to inch

# read in other items to map - dress up figure
streams <- read_sf(".\\gis\\boundaries_features\\streams_recharge.shp")
ws_outline <- read_sf(".\\gis\\boundaries_features\\usgs_basins.shp") 


logo <- image_read(path='.\\gis\\logo\\EAHCP_color_vertical logo.png')
# create a plot box to help you position your logo
plot_box <- tibble(xmin = st_bbox(map)[1],
                   xmax = st_bbox(map)[3],
                   ymin = st_bbox(map)[2],
                   ymax = st_bbox(map)[4],
                   xrange = xmax - xmin,
                   yrange = ymax - ymin) 

linear_radar_scale_two_break <- function (min, max, bin_number_minus_2, first_break, second_break) {
  
  # min <- 0
  #  max <- 200
  # bin_number <- 10
  #  first_break <- 2 # less than 2 I want NA for color
  # second_break <- 6 # from 2-6 I want trace rain color
  
  colours <- as_tibble(c(NA,"#0826A2","#22FE05","#2CAC1B","#248418", "#F6FB07", "#FFE890", "#FFC348","#E01E17", "#A92B26","#8C302C","#CC17DA", "#AE6DB3","#8798C6" )) |>
    slice(1:(bin_number_minus_2+2))
  
  breaks <- as_tibble(seq(min,max,max/bin_number_minus_2)) |>
    slice(2:n()) |>
    slice(1:(n()-1))|>
    add_row(value = second_break, .before = 1)|>
    add_row(value = first_break, .before = 1)
  
  breaks_w_lims <- as_tibble(seq(min,max,max/bin_number_minus_2))  |>
    add_row(value = second_break, .before = 2)|>
    add_row(value = first_break, .before = 2)  
  
  values <- rollmean(breaks_w_lims,2)
  breaks <- as.numeric(unlist(breaks))  
  
  output <- list (colours, min, max, breaks, values)
  
}

a <- linear_radar_scale_two_break (0,200,8,2,6)

colours <- unlist(a[1])
min <- unlist(a[2])
max <- unlist(a[3])
breaks <- unlist(a[4])
values <-  unlist(a[5])





p1 <- ggplot() +
  
  geom_sf(data = map, aes(fill = sum_rain), linewidth = .05)+
  scale_fill_stepsn(name = "mm",
                    colours=c(colours), 
                    breaks = c(breaks),
                    limits=c(min,max),
                    values = scales::rescale(c(values), from = c(min, max)), 
                    show.limits = FALSE,
                    na.value = "#F1F4FC")  +
  
  #ggtitle("2015 Precipitation") +
  geom_sf(data = streams, fill=NA, color="blue", linewidth = .1) +
 
  annotation_raster(logo, # native logo is 796 x 1168
                    # Position adjustments here using plot_box$max/min/range
                    ymin = plot_box$ymin ,
                    ymax = plot_box$ymin + .27 , 
                    xmin = plot_box$xmin ,
                    xmax = plot_box$xmin + .18) +
  annotate(geom="text",x= -99,y=29.28,label="September 2015 Precipitation",size=3,hjust=0)+
  annotate(geom="text",x= -99,y=29.2,label= paste0("Basin average of ", sprintf("%0.2f", bap), " in"),size=3,hjust=0)+
  
  theme(legend.key.height = unit(1.0, "cm"),
        legend.margin = margin(0,0,0,0),
        #legend.position = "left",
        legend.position = c(-.01,.5),
        legend.box.margin=margin(0,2,0,5),
        plot.margin=unit(c(0,0,0,0), 'cm'),
        axis.title = element_blank(),
        #axis.text.y = element_blank(),
        #axis.text.x = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.ticks = element_blank(),
        plot.background = element_rect(color = "#F1F4FC", fill = "#F1F4FC"),
        panel.grid.major = element_blank(),
        legend.background= element_rect(fill = "#F1F4FC"),
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 7),
        panel.background = element_rect(fill = "#F1F4FC"))


ggsave(filename = "eaa_test_recharg3.png", device = "png", path = ".\\plots", plot = p1, width = 6.5, height = 4.5, units = "in")








