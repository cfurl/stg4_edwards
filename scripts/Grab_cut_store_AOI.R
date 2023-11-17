x<-c('readr', 'dplyr','stringr')
lapply(x, require, character.only = TRUE)

## 1 - Grab that hourly text drop - cut to aoi - run QA tests
# how will you time stamp this? How will you make sure it't the correct file that is in there?


#make character string of system time, this will be the label of tibble you create with updated data
#cc<-format(as.POSIXct(Sys.time(),tz="GMT"), "%Y-%m-%d %H:%M:%S %Z")
#paste0("pull_",str_replace_all(cc, " ", "")) <- read_csv(paste0('data_drop//',data_drop_content[1]))

#make character string of file in "data_drop" - currently I only want a single file in there at a time


data_drop_content<-list.files("data_drop")
texas_bins<-read_csv("gis//texas_buffer_spatial_join.csv")
#49264 Texas bins - this will be one of the QA tests run


#processes and examines and individual hour before merging with larger group
most_recent_conus_drop<-read_csv(paste0("data_drop\\",data_drop_content),
           col_names=c("x1","x2","x3","x4","center_lon","center_lat",data_drop_content)) %>%
           select(-x1,-x2,-x3,-x4)
          
cut_most_recent_drop<-left_join(texas_bins,most_recent_conus_drop,by=NULL)  # this is your grib2 rain cut to AOI
  
# Do some QA on the cut AOI [This should be written to a log somewhere]
  nas<- cut_most_recent_drop %>% summarise(nacount=sum(is.na(.)))
  rowcount  <- cut_most_recent_drop %>%  summarise(n=n())
  qa<-bind_cols(nas,rowcount,data_drop_content) |>
    setNames(c("nas","rowcount","timestamp"))

# 2 - we want to store the AOI file is some longer storage


# 3 - we want to store the AOI in some rolling ephemeral storage
  
 # final<- left_join(final,setNames(rain, c("grib_id","hrap_x","hrap_y","center_lon","center_lat",a)))