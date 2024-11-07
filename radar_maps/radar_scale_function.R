
linear_radar_scale <- function (min, max, bin_number) {
  
  min = 0
  max = 120
  bin_number = 11
  
  colours=c("#0826A2","#22FE05","#2CAC1B","#248418", "#F6FB07", "#FFE890", "#FFC348","#E01E17", "#A92B26","#8C302C","#CC17DA", "#AE6DB3","#8798C6" )
  colours=colours[0:bin_number+1]
  
  breaks <- as_tibble(seq(min,max,max/bin_number)) |>
    slice(2:n()) |>
    slice(1:n()-1)
  
  breaks_w_lims <- breaks |>
    add_row(value = min, .before = 1) |>
    add_row(value = max, .after = bin_number + 2) 
  
  values <- rollmean(breaks_w_lims,2)
  breaks <- as.numeric(unlist(breaks))  
  
 output <- list (colours, min, max, breaks, values)
 
}

#############################################################

linear_radar_scale <- function (min, max, bin_number) {
  
  #min = 0
  #max = 100
  #bin_number = 10
  
  colours <- as_tibble(c("pink","#22FE05","#2CAC1B","#248418", "#F6FB07", "#FFE890", "#FFC348","#E01E17", "#A92B26","#8C302C","#CC17DA", "#AE6DB3","#8798C6" )) |>
    slice(1:(bin_number))
    
 breaks <- as_tibble(seq(min,max,max/bin_number)) |>
    #slice(2:n()) |>
    slice(1:n()-1)
  
 # breaks_w_lims <- breaks |>
#    add_row(value = min, .before = 1) |>
 #   add_row(value = max, .after = bin_number + 2) 
  
#  values <- rollmean(breaks_w_lims,2)
  breaks <- as.numeric(unlist(breaks))  
  
  output <- list (colours, min, max, breaks)
  
}

################################################################

a <- linear_radar_scale(0,100,10)

colours <- unlist(a[1])
min <- unlist(a[2])
max <- unlist(a[3])
breaks <- unlist(a[4])
#values <-  unlist(a[5])

colors(n) = breaks + 1; 14 colors need 13 breaks
as_tibble(dd) |>arrange(desc(dd)) # have to find the correct max value to set up breaks

ggplot() +
  
  geom_sf(data = map, aes(fill = sum_rain))+
  #geom_sf(data = map, aes(fill = sum_rain), color = NA)+
  
 # scale_fill_stepsn(colors = c("black","red","#22FE05","#2CAC1B","#248418"), 
#                    breaks = c(25,50,75,100),
#                    limits=c(0,125),
#                    values = scales::rescale(c(12.5, 37.5, 62.5, 87.5, 112.5), 
                                          #   from = c(0, 125)))
  
scale_fill_stepsn(colours=c("pink","#0826A2","#22FE05","#2CAC1B","#248418", "#F6FB07", "#FFE890", "#FFC348","#E01E17", "#A92B26","#8C302C","#CC17DA", "#AE6DB3","#8798C6" ), 
                  breaks = c(2,10,20,30,40,50,60,70,80,90,100,110,120),
                  limits=c(0,130),
                  values = scales::rescale(c(1,5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115, 125), 
                                           from = c(0, 130)))  

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



