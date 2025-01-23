library(tidyverse)
library(sf)
library(ggspatial)
library(ggtext)
library(patchwork)


RColorBrewer::brewer.pal(6, "YlOrRd")

plot_subbasin_map <- function(
    data_rain,          
    map_subbasin,            
    title = "Subbasin Map", 
    title_size = 14,
    subtitle_size = 12,
    subtitle = "Percentile of Avg Rainfall",  
    legend_title = "Percentile Category", 
    legend_title_size = 9,
    map_type = "cartolight",   
    font = "Open Sans",   
    pal_legend = c("#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#F03B20", "#BD0026"),
    padding_x = 0.1,          
    padding_y = 0.3,          
    fill_alpha = 0.8,          
    pal_line = '#3D3D3D',      
    pal_title = "black",       
    pal_subtitle = "black",    
    pal_legend_text = "black"  
) {
  # Helper function to calculate brightness
  calculate_brightness <- function(hex_color) {
    rgb <- col2rgb(hex_color)
    brightness <- 0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]
    return(brightness)
  }
  
  
  coord_sys = 3857
  
  fill_palette <- c(
    '<10%' = pal_legend[1],
    '10-24%' = pal_legend[2],
    '25-49%' = pal_legend[3],
    '50-74%' = pal_legend[4],
    '75-89%' = pal_legend[5],
    '90%+' = pal_legend[6]
  )
  
  # Prepare stats with brightness and text color
  stats <- data_rain |>
    mutate(
      category = case_when(
        avg_mm_rainfall < perc10 ~ '<10%',
        avg_mm_rainfall < perc25 ~ '10-24%',
        avg_mm_rainfall < median_rain ~ '25-49%',
        avg_mm_rainfall < perc75 ~ '50-74%',
        avg_mm_rainfall >= perc75 ~ '75-89%',
        avg_mm_rainfall >= perc90 ~ '90%+',
        TRUE ~ NA_character_
      ),
      brightness = sapply(category, function(cat) {
        calculate_brightness(fill_palette[[cat]])
      }),
      text_color = ifelse(brightness < 128, "white", "black")
    )
  
  # Factor categories
  stats$category <- factor(stats$category, levels = names(fill_palette))
  
  # Load and prepare shapefile
  map_subbasin <- map_subbasin|>
    st_cast("MULTIPOLYGON") |>
    st_transform(coord_sys) |>
    set_names(c("basin", "geometry")) |>
    mutate(basin = str_remove(basin, " Basin")) |>
    left_join(stats, by = "basin") |>
    relocate(geometry, .after = last_col())
  
  # Calculate centroids for subbasin labels
  map_centroids <- map_subbasin |>
    group_by(basin, text_color) |>
    summarise(geometry = st_union(geometry)) |>  
    mutate(centroid = st_centroid(geometry)) |>
    ungroup() |>
    mutate(
      centroid_x = st_coordinates(centroid)[, 1], 
      centroid_y = st_coordinates(centroid)[, 2],
      label = case_when(
        basin == 'Cibolo-Dry Comal' ~ 'Cibolo-Dry\nComal',
        basin == "Frio-Dry Frio" ~ "Frio-Dry\nFrio",
        TRUE ~ basin
      ),
      label_size = case_when(
        basin %in% c('Cibolo-Dry Comal', 'Sabinal') ~ 1.75,
        basin == 'Frio-Dry Frio' ~ 2.5,
        TRUE ~ 3
      ),
      #slightly adjust centroid x
      centroid_x = case_when(
        basin == 'Frio-Dry Frio' ~ centroid_x * 1.0003,
        TRUE ~ centroid_x
      ),
      #slightly adjust centroi x
      centroid_y = case_when(
        basin == 'Sabinal' ~ centroid_y * 1.001,
        TRUE ~ centroid_y
      )
    )
  
  # Calculate bounding box
  bbox <- st_bbox(map_subbasin)
  xlim <- c(
    bbox["xmin"] - (bbox["xmax"] - bbox["xmin"]) * padding_x, 
    bbox["xmax"] + (bbox["xmax"] - bbox["xmin"]) * padding_x
  )
  ylim <- c(
    bbox["ymin"] - (bbox["ymax"] - bbox["ymin"]) * padding_y, 
    bbox["ymax"] + (bbox["ymax"] - bbox["ymin"]) * padding_y
  )
  
  # Create mask (cookie) to increase plot padding
  encl_rect <- list(cbind(
    c(xlim[1], xlim[2], xlim[2], xlim[1], xlim[1]),
    c(ylim[1], ylim[1], ylim[2], ylim[2], ylim[1])
  )) %>%
    st_polygon() %>%
    st_sfc(crs = st_crs(map_subbasin))
  cookie <- st_difference(encl_rect, st_union(map_subbasin))
  
  
  
  # Plot
  main_plot<-ggplot() +
    ggspatial::annotation_map_tile(
      type = map_type,
      zoom = 9
    ) +
    geom_sf(data = map_subbasin, 
            aes(fill = category), 
            alpha = fill_alpha, 
            color = pal_line, 
            show.legend = TRUE) +
    geom_text(
      data = map_centroids,
      aes(
        x = centroid_x, 
        y = centroid_y, 
        label = label, 
        color = I(text_color), 
        size = I(label_size)
      ),
      hjust = 0.5,
      family = font
    #  fill = NA
     # label.color = NA,
     # label.padding = grid::unit(rep(0, 4), "pt") 
      
    ) +
    geom_sf(data = cookie, fill = "transparent", color = "transparent") +
    labs(fill = legend_title) +
    scale_fill_manual(
      values = fill_palette,
      drop = FALSE,
      na.value = 'grey80',
      guide = guide_legend(
        nrow = 1,
        label.position = "bottom",
        title.position = "top",
        override.aes = c(linewidth = 0.2)
      )
    )+
    theme_void() +
    theme(
      text = element_text(family = font),
      plot.title = element_text(face='bold', family=font, color = pal_title),
      legend.position = "inside", 
      legend.position.inside = c(0.72, 0.15),
      legend.direction = "horizontal", 
      legend.key.width = unit(1, 'cm'),
      legend.key.height = unit(0.5, 'cm'),
      legend.title = element_text(color = pal_legend_text, face = 'bold', size = legend_title_size),
      legend.spacing.x = unit(1, "mm"),
      legend.text = element_text(color = pal_legend_text, size = 8)
    ) 
  
  #use patchwork to add in title and subtitle in plot, avoids using coordinates to manually adjust
  main_plot +
    inset_element(
      ggplot() +
        labs(title = title, subtitle = subtitle) +
        theme_void() +
        theme(
          plot.title = element_text(size = title_size, color = pal_title, face = "bold", family=font),   
          plot.subtitle = element_text(size = subtitle_size, color = pal_subtitle, family=font)          
        ),
      #adjust positoning
      left = 0.05, bottom = 0.9, right = 0.95, top = 0.91
    )
}


#EXAMPLES


# date_end <- as.Date("2023-12-31")
# basin_avg_precip <- read.csv("data/processed/cumulative_subbasin.csv")|>filter(date == date_end)
# map<-read_sf("./data/gis/boundaries_features/usgs_basins.shp")

# plot_subbasin_map(
#   data_rain = basin_avg_precip,
#   map_subbasin = map,
#   font = "Open Sans",
#   map_type = "cartolight",
#   title = "EAA Sub-basin Precipitation",
#   subtitle = "This is an example output of the sub-basin map",
#   legend_title = "Percentile Category",
#   title_size = 16, 
#   subtitle_size = 12, 
#   pal_legend = RColorBrewer::brewer.pal(6, "YlGnBu")
# )
# 
# plot_subbasin_map(
#   data_rain = basin_avg_precip,
#   map_subbasin = map,
#   font = "Open Sans",
#   map_type = "cartodark",
#   title = "EAA Sub-basin Precipitation",
#   subtitle = "This is an example output of the sub-basin map",
#   legend_title = "Percentile Category",
#   title_size = 16, 
#   subtitle_size = 12, 
#   pal_title = "white",
#   pal_subtitle = "white",
#   pal_legend_text = "white",
#   pal_legend = RColorBrewer::brewer.pal(6, "YlOrRd")
# )


