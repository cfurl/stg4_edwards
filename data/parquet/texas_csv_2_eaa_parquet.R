library(tidyverse)

# currently I keep .csv's around because this is how Hatim wanted them deliverd
# this script cuts down the Texas .csv's to area of interest (Edwards) and writes a parquet dataset

# Step 1 - transform texas csvs to eaa csv's.  Could go straight to parquet, but this
# will allow me to share with others easier.

# This buffered .csv should hold all grib_ids for any EAA analysis I want to do
eaa_grib_ids <- read_csv("C:\\texas_mpe\\arrow\\csv_rain\\eaa_grib_ids_buffered.csv")
rain<-list.files("C:\\texas_mpe\\arrow\\csv_rain\\texas_stg4_qpe_hourly_2002_2023", full.names = FALSE)


for (i in rain) {
  
  csv_tex <- paste0("C:\\texas_mpe\\arrow\\csv_rain\\texas_stg4_qpe_hourly_2002_2023\\",i)
  
  ddd <- read_csv(csv_tex)
  
  f<-left_join(eaa_grib_ids,ddd, by="grib_id") |>
    relocate(grib_id, .after = center_lat)
  
  write_csv(f, file = paste0("C:\\texas_mpe\\arrow\\csv_rain\\eaa_stg4_qpe_hourly_2002_2023\\",i))
  
}


# write csvs to parquet
library(arrow)

rain<-list.files("C:\\texas_mpe\\arrow\\csv_rain\\eaa_stg4_qpe_hourly_2002_2023", full.names = TRUE)

for (i in rain) {
  
  # i =   "C:\\texas_mpe\\arrow\\csv_rain\\eaa_stg4_qpe_hourly_2002_2023\\st4_1hr_2002010101_2002070100_eaa.txt"
  
  ddd <- read_csv(i) %>%
    pivot_longer(!1:5, names_to = "time", values_to = "rain_mm") %>%
    mutate(time = ymd_h(str_sub(time,2,11))) %>%
    mutate (year = year(time), month = month(time), day = day(time), hour = hour(time)) %>%
    mutate(month_day = paste0(month,"_",day)) %>%
    relocate(rain_mm, .after = last_col()) 
  
  month_parq <- "C:\\texas_mpe\\arrow\\st4_parq_eaa"
  
  ddd |>
    group_by(year,month) |>
    write_dataset(path = month_parq,
                  format = "parquet")
  
  rm(list=ls())
  gc()
}
