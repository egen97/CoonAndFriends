
library(tidyverse)
library(stringr)
library(httr)

set.seed(42)

#### Create validation sample with the translated posts as well ####

files <- list.files("./Data/Channel_Translated/", full.names = TRUE)

telegrams_english <- lapply(files, read_rds)

telegrams_english <- map(telegrams_english, .f = list(. %>% dplyr::select(source, date, id, message, detectedSourceLanguage, translatedText)))

telegrams_english <- do.call(rbind, telegrams_english)

telegrams <- readRDS("./Data/Telegrams/telegrams_cleaned_wartime_pasted_putin.rds")

telegrams_joined <- telegrams %>%
  mutate(date = as.Date(date)) %>%
  rename(ru_message = message) %>%
  #filter(date %in% c(telegrams_english$date)) %>%
  inner_join(telegrams_english %>% rename(en_message = message), join_by(source, date, id))

telegrams_joined_sample <- telegrams_joined %>%
  mutate(year = substr(date, 1, 4),
         month = substr(date, 6, 7),
         year_month = paste0(year, "_", month)) %>%
  filter(year_month != "2023_05") %>%
  group_by(year_month) %>%
  sample_n(10) %>%
  ungroup()

saveRDS(telegrams_joined_sample, "./Data/Validation_Samples/Validation_Run_2/subsample_run2.rds")

##### VALIDATION SAMPLE 10-30 posts #####

validation_sample_vars <- sample_n(telegrams_joined, 10)

validation_sample <- validation_sample_vars %>%
 select(rowid, source, date, id, message, translatedText)

saveRDS(validation_sample, file = "./Data/Validation_Samples/Validation_Sample_4.RDS")


##### VALIDATION SAMPLE 100 posts #####

# -----

### 100 posts. Everyone codes 50 posts. 25 overlap towards two others.

# -----


validation_sample_vars <- sample_n(telegrams_joined, 100)

validation_sample <- validation_sample_vars %>%
  dplyr::select(rowid, source, date, id, message, translatedText)

# saveRDS(validation_sample, file = "./Data/Validation_Samples/Validation_Sample_100.RDS")

validation_sample <- readRDS("./Data/Validation_Samples/CrossCoding/Validation_Sample_100.RDS")

## Humans ##

subsample1 <- validation_sample[1:50,]
subsample2 <- validation_sample[51:100,]
subsample3_1 <- subsample1[1:25,]
subsample3_2 <- subsample2[1:25,]
subsample3 <- rbind(subsample3_1, subsample3_2)
subsample4_1 <- subsample1[26:50,]
subsample4_2 <- subsample2[26:50,]
subsample4 <- rbind(subsample4_1, subsample4_2)

# Checking that it worked, rowids should only occur twice
# test <- tibble(rowid = c(subsample1$rowid, subsample2$rowid, subsample3$rowid, subsample4$rowid))
# test %>%
#   filter(rowid == "254284")

subsample1 <- subsample1 %>%
  select(rowid, translatedText) %>%
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

subsample2 <- subsample2 %>%
  select(rowid, translatedText) %>%
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

subsample3 <- subsample3 %>%
  select(rowid, translatedText) %>%
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

subsample4 <- subsample4 %>%
  select(rowid, translatedText) %>%
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

saveRDS(subsample1, file = "./Data/Validation_Samples/CrossCoding/Subsample_Jonas.rds")
saveRDS(subsample2, file = "./Data/Validation_Samples/CrossCoding/Subsample_Eric.rds")
saveRDS(subsample3, file = "./Data/Validation_Samples/CrossCoding/Subsample_Amalie.rds")
saveRDS(subsample4, file = "./Data/Validation_Samples/CrossCoding/Subsample_Solveig.rds")

openxlsx::write.xlsx(subsample1, "./Data/Validation_Samples/CrossCoding/Subsample_Jonas.xlsx")
openxlsx::write.xlsx(subsample2, "./Data/Validation_Samples/CrossCoding/Subsample_Eric.xlsx")
openxlsx::write.xlsx(subsample3, "./Data/Validation_Samples/CrossCoding/Subsample_Amalie.xlsx")
openxlsx::write.xlsx(subsample4, "./Data/Validation_Samples/CrossCoding/Subsample_Solveig.xlsx")

