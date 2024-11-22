packages<- c("arrow", "dplyr", "tictoc", "sf", "janitor", "magick", "ggplot2", "units", "zoo", "ggmap")
lapply(packages, require, character.only = TRUE)

# identify the root folder of your parquet files
#month_parq <- "C:\\texas_mpe\\arrow\\st4_parq"  # full repository here
month_parq <- ".\\parquet\\parquet_files"        # September 2015 on github

# establish your connection, this is the time to see your file system schema and make any adjustments
tx_rain <- open_dataset(month_parq)
nrow(tx_rain)

# have a query space and don't collect yet
sum_rain_query <- tx_rain %>%
  filter(year==2015) %>%
  group_by (grib_id) %>%
  summarize(
    sum_rain = sum(rain_mm, na.rm=TRUE)) %>%
  arrange(desc(sum_rain))

# collect the query when you are ready 
tic()
sum_rain_collect <- collect(sum_rain_query)
toc()


# read in the shape file of the ST4 bins clipped to the EAA recharge area
# Join the just completed query with ST4 bins using 'grib_id'

map <- read_sf(".\\gis\\clipped_hrap\\usgs_recharge_basins\\usgs_dissolved.shp") |>
  clean_names() |>
  left_join(sum_rain_collect, by = "grib_id" )

# drop your geometry and make some volume and linear rainfall calculations
map_math <- map |> st_drop_geometry() |>
  mutate(cubic_m_precip = bin_area * sum_rain * .001)

bap <- sum(map_math$cubic_m_precip) * 39.37/sum(map_math$bin_area) # 39.37 is meter to inch

# read in other items to map - dress up figure
streams <- read_sf(".\\gis\\boundaries_features\\streams_recharge.shp")
ws_outline <- read_sf(".\\gis\\boundaries_features\\usgs_basins.shp") 
lakes <- read_sf(".\\gis\\boundaries_features\\reservoirs.shp")

# raster images
radar_tag <- image_read(path='.\\gis\\logo\\radar_tag.png')
logo <- image_read(path='.\\gis\\logo\\EAHCP_color_vertical logo.png')

# function to generate radar scale with 2 breaks
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

# run function and unlist values to populate 'scale_fill_stepsn'
a <- linear_radar_scale_two_break (0,200,8,2,6)
          colours <- unlist(a[1])
          min <- unlist(a[2])
          max <- unlist(a[3])
          breaks <- unlist(a[4])
          values <-  unlist(a[5])

# grab your tiles from Stadia.  Your API key is hard written in your .Renviron.
ear <- get_stadiamap(bbox = c(left = -100.85, bottom = 29.0, 
                              right = -97.75, top = 30.4), 
                     zoom = 9,
                     maptype = "alidade_smooth_dark",
                     crop = TRUE)

p1 <- ggmap(ear) +
  
  geom_sf(data = map, aes(fill = sum_rain), alpha = .6, linewidth = .05,  inherit.aes = FALSE)+ 
              scale_fill_stepsn(name = "mm",
                    colours=c(colours), 
                    breaks = c(breaks),
                    limits=c(min,max),
                    values = scales::rescale(c(values), from = c(min, max)), 
                    show.limits = FALSE,
                    na.value = "#F1F4FC")  +
  
  guides(colour = guide_legend(override.aes = list(alpha = 0.6)))+ # trying to apply alpha to legend. Doesn't work...
  
  geom_sf(data = streams, fill=NA, color="blue", linewidth = .1,   inherit.aes = FALSE) +
  geom_sf(data = lakes,  fill = "blue", linewidth = .1,   inherit.aes = FALSE) +
  #geom_sf(data = ws_outline, fill=NA, color="black", linewidth = .1,   inherit.aes = FALSE) +
  
  annotation_raster(logo, # native logo is 796 (wide) x 1168 (tall)
                    # Position adjustments here using plot_box$max/min/range
                    ymin = 29.05 ,
                    ymax = 29.05 + .27 , 
                    xmin = -97.80 ,
                    xmax = -97.80 - .18) +
  annotation_raster(radar_tag, # native logo is 796 (wide) x 1168 (tall)
                    # Position adjustments here using plot_box$max/min/range
                    # https://onlinepngtools.com/convert-text-to-png
                    ymin = 29.02 ,
                    ymax = 29.05 + .12 , 
                    xmin = -98.00 ,
                    xmax = -98.00 - .08) +
  annotate(geom="text",x= -100.75,y=30.28,label="Edwards Aquifer Recharge Zone",size=4,hjust=0, color = "#F1F4FC")+
  annotate(geom="text",x= -99,y=29.28,label="September 2015 Precipitation",size=3,hjust=0, color = "#F1F4FC")+
  annotate(geom="text",x= -99,y=29.2,label= paste0("Basin average of ", sprintf("%0.2f", bap), " in"),size=3,hjust=0, color = "#F1F4FC")+
  
  theme(legend.key.height = unit(1.0, "cm"),
        legend.margin = margin(0,0,0,0),
        #legend.position = "left",
        legend.position = c(.05,.5),
        legend.box.margin=margin(0,2,0,5),
        plot.margin=unit(c(0,0,0,0), 'cm'),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.ticks = element_blank(),
        plot.background = element_rect(color = "#F1F4FC", fill = "#F1F4FC"),
        panel.grid.major = element_blank(),
        #legend.background= element_rect(fill = "#F1F4FC"),
        legend.background= element_rect(fill = NA),
        legend.text = element_text(size = 5, color = "#F1F4FC"),
        legend.title = element_text(size = 7, color = "#F1F4FC"),
        panel.background = element_rect(fill = "#F1F4FC"))


ggsave(filename = "eaa_test_recharg3.png", device = "png", path = ".\\plots", plot = p1, width = 6.5, height = 4.5, units = "in")








