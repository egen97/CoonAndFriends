
library(tidyverse)
library(stringr)
library(httr)

telegrams_cleaned <- readRDS("./Data/telegrams_cleaned.rds")

#### Create validation sample with the translated posts as well ####

files <- list.files("./Data/Channel_Translated/", full.names = TRUE)

telegrams_english <- lapply(files, read_rds)

telegrams_english <- map(telegrams_english, .f = list(. %>% dplyr::select(source, date, id, message, detectedSourceLanguage, translatedText)))

telegrams_english <- do.call(rbind, telegrams_english)

telegrams_joined <- telegrams_english %>%
  left_join(telegrams_cleaned, join_by(source, date, id, message)) %>%
  drop_na(rowid) %>%
  mutate(message = ifelse(message == "", NA, message)) %>%
  filter(str_detect(message, "(?i)путин[а-я]*|владимир\\s*владимирович\\s*путин|владимир\\s*путин")) %>%
  drop_na(message)


#### How many times is "Putin" mentioned in the overall data? ####

telegrams_cleaned %>%
  mutate(message = ifelse(message == "", NA, message)) %>%
  filter(str_detect(message, "(?i)путин[а-я]*|владимир\\s*владимирович\\s*путин|владимир\\s*путин")) %>%
  drop_na(message) %>%
  nrow()

# 30618


##### VALIDATION SAMPLE 15-30 posts #####

validation_sample_vars <- sample_n(telegrams_joined, 15)

validation_sample <- validation_sample_vars %>%
 select(rowid, source, date, id, message, translatedText)

saveRDS(validation_sample, file = "./Data/Validation_Samples/Validation_Sample_3.RDS")


##### VALIDATION SAMPLE 200 posts #####

validation_sample_vars <- sample_n(telegrams_joined, 200)

validation_sample <- validation_sample_vars %>%
  dplyr::select(rowid, source, date, id, message, translatedText)

saveRDS(validation_sample, file = "./Data/Validation_Samples/Validation_Sample_200.RDS")
