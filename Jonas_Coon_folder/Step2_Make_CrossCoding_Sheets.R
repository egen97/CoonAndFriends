
library(tidyverse)


##### VALIDATION SAMPLE SUBDIVIDED #####

telegrams_joined_sample <- readRDS("./Data/subsample_run.rds")

amalie <- bind_rows(telegrams_joined_sample[1:25,], telegrams_joined_sample[76:96,])
eric <- bind_rows(telegrams_joined_sample[26:50,], telegrams_joined_sample[76:96,])
jonas <- bind_rows(telegrams_joined_sample[51:75,], telegrams_joined_sample[76:96,])

common <- telegrams_joined_sample[76:96,]

amalie <- amalie %>%
  select(source, id, message) %>%
  mutate(ukraine = "",
         west = "",
         putin = "",
         support = "",
         sentiment = "",
         trust = "",
         competent = "",
         state_of_war = "",
         reponsibility = "",
         response = "")

eric <- eric %>%
  select(source, id, message) %>%
  mutate(ukraine = "",
         west = "",
         putin = "",
         support = "",
         sentiment = "",
         trust = "",
         competent = "",
         state_of_war = "",
         reponsibility = "",
         response = "")

jonas <- jonas %>%
  select(source, id, message) %>%
  mutate(ukraine = "",
         west = "",
         putin = "",
         support = "",
         sentiment = "",
         trust = "",
         competent = "",
         state_of_war = "",
         reponsibility = "",
         response = "")

write_excel_csv(amalie, "./Data/CrossCoding/Subsample_Amalie.csv")
write_excel_csv(eric, "./Data/CrossCoding/Subsample_Eric.csv")
write_excel_csv(jonas, "./Data/CrossCoding/Subsample_Jonas.csv")
