#############################################################
# This scale_fill_stepsn is giving me a lot of grief.  Writing a function here to generate the colors, breaks, and values, but I'm guessing
# that I'll still need som manual QA on it for awhile.  One of the primary issues i see is that if you have a value that isn't represented on the
# scale it throws a fit.  For example, if I have a 125-130 represented on the scale, but no rain falls in that range, it abandons my color scheme.

# First thing I want to do is make sure I'm aware of the max/min in my dataset I'm trying to plot
# need to have a bin that covers the max

dd<-map$sum_rain

as_tibble(dd) |> 
  arrange(desc(dd)) |>
  head(1) 

as_tibble(dd) |> 
  arrange(desc(dd)) |>
  tail(1) 



linear_radar_scale <- function (min, max, bin_number) {
 
 colours <- as_tibble(c("#0826A2","#22FE05","#2CAC1B","#248418", "#F6FB07", "#FFE890", "#FFC348","#E01E17", "#A92B26","#8C302C","#CC17DA", "#AE6DB3","#8798C6" )) |>
    slice(1:(bin_number))
    
 breaks <- as_tibble(seq(min,max,max/bin_number)) |>
    slice(2:n()) |>
    slice(1:(n()-1))
  
 breaks_w_lims <- as_tibble(seq(min,max,max/bin_number)) 
  
  values <- rollmean(breaks_w_lims,2)
  breaks <- as.numeric(unlist(breaks))  
  
  output <- list (colours, min, max, breaks, values)
  }

a <- linear_radar_scale(0,130,5)

colours <- unlist(a[1])
min <- unlist(a[2])
max <- unlist(a[3])
breaks <- unlist(a[4])
values <-  unlist(a[5])

ggplot() +
  geom_sf(data = map, aes(fill = sum_rain))+
scale_fill_stepsn(colours=c(colours), 
                  breaks = c(breaks),
                  limits=c(min,max),
                  values = scales::rescale(c(values), 
                                           from = c(min, max)))



##############################################################
# add a cut
# sometimes you will want a very low value to strip away the visuals from trace amounts
# of rainfall.  In particular if you're looking at a single storm event or rainfall over
# a couple of weeks

linear_radar_scale_low_break <- function (min, max, bin_number, first_break) {
 
  colours <- as_tibble(c(NA,"#0826A2","#22FE05","#2CAC1B","#248418", "#F6FB07", "#FFE890", "#FFC348","#E01E17", "#A92B26","#8C302C","#CC17DA", "#AE6DB3","#8798C6" )) |>
    slice(1:(bin_number))
  
  breaks <- as_tibble(seq(min,max,max/bin_number)) |>
    slice(2:n()) |>
    slice(1:(n()-1))|>
    add_row(value = first_break, .before = 1)
  
  breaks_w_lims <- as_tibble(seq(min,max,max/bin_number))  |>
  add_row(value = first_break, .before = 2) 
  
  values <- rollmean(breaks_w_lims,2)
  breaks <- as.numeric(unlist(breaks))  
  
  output <- list (colours, min, max, breaks, values)
  
}

a <- linear_radar_scale_low_break (0,150,10,5)

colours <- unlist(a[1])
min <- unlist(a[2])
max <- unlist(a[3])
breaks <- unlist(a[4])
values <-  unlist(a[5])

ggplot() +
  geom_sf(data = map, aes(fill = sum_rain))+
  scale_fill_stepsn(colours=c(colours), 
                    breaks = c(breaks),
                    limits=c(min,max),
                    values = scales::rescale(c(values), 
                                             from = c(min, max)))


###### Really need to add two cuts

linear_radar_scale_two_break <- function (min, max, bin_number_minus_2, first_break, second_break) {
  
 # min <- 0
#  max <- 200
 # bin_number <- 10
#  first_break <- 2 # less than 2 I want NA for color
 # second_break <- 6 # from 2-6 I want trace rain color
  
  colours <- as_tibble(c(NA,"#0826A2","#22FE05","#2CAC1B","#248418", "#F6FB07", "#FFE890", "#FFC348","#E01E17", "#A92B26","#8C302C","#CC17DA", "#AE6DB3","#8798C6" )) |>
    slice(1:(bin_number_minus_2+2))
  
  breaks <- as_tibble(seq(min,max,max/bin_number)) |>
    slice(2:n()) |>
    slice(1:(n()-1))|>
    add_row(value = second_break, .before = 1)|>
    add_row(value = first_break, .before = 1)
  
  breaks_w_lims <- as_tibble(seq(min,max,max/bin_number))  |>
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



##############################################################
# HERE IS WHAT IT LOOKS LIKE TYPED OUT

scale_fill_stepsn(colours=c("pink",NA,"#22FE05","#2CAC1B","#248418", "#F6FB07", "#FFE890", "#FFC348","#E01E17", "#A92B26","#8C302C","#CC17DA", "#AE6DB3","orange","black" ), 
                  breaks = c(2,10,20,30,40,50,60,70,80,90,100,110,120,125),
                  limits=c(0,130),
                  values = scales::rescale(c(1,5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115, 125, 127.5), 
                                           from = c(0, 130)))  +

 