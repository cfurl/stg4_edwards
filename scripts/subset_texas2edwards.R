library(tidyverse)

# I did some manual work to clean up the QGIS clip. renamed columns, got rid of 15 decimal points.
hrap_clip_eaa_buffer<-read_csv("gis\\hrap_bins_clipped2EAA_buffer.csv")

texas_mpe<-list.files("F:\\state_clips")

for (r in texas_mpe) {

t1<-read_csv(paste0("F:\\state_clips\\",r)) 

join<-left_join(hrap_clip_eaa_buffer,t1)

write_csv(join,paste0("F:\\g2_test\\testing_state_file_output\\",r))
rm(join)
gc()
}