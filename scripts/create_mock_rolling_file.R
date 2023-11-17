x<-c('readr', 'dplyr')

lapply(x, require, character.only = TRUE)

ss<-read_csv("rolling_storage//st4_1hr_2022010101_2022070100_EAA.txt",col_names = TRUE,) %>%
  select(1:173)

write_csv(ss,"rolling_storage//st4_1hr_2022010101_2022010800_EAA.txt")


