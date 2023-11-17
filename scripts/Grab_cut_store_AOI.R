x<-c('readr', 'dplyr','stringr')
lapply(x, require, character.only = TRUE)

## 1 - Grab that hourly text drop - cut to aoi - run QA tests

#make character string of system time, this will be the label of tibble you create with updated data
cc<-format(as.POSIXct(Sys.time(),tz="GMT"), "%Y-%m-%d %H:%M:%S %Z")
paste0("pull_",str_replace_all(cc, " ", "")) <- read_csv(paste0('data_drop//',data_drop_content[1]))

#make character string of file in "data_drop" - currently I only want a single file in there at a time
data_drop_content<-list.files("data_drop")


most_recent_conus_drop<-read_csv(paste0('data_drop//',data_drop_content[1]))


eaa_hrap_bins<-read_csv("gis//hrap_bins_clipped2EAA_buffer.csv")
#2110 bins - this will be one of the QA tests run





# Grab the file.
# For now, I'm assuming it is called 'st4_conus.2020072000.01h.txt'

# let's create some mock data for the moment




# 2 - we want to store the AOI file is some longer storage


# 3 - we want to store the AOI in some rolling ephemeral storage