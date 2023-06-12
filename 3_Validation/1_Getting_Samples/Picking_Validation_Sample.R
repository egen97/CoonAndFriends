
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

test <- telegrams_joined %>% filter(date == "2022-05-05") %>%
  dplyr::select(rowid, source, date, id, message, translatedText)

#### How many times is "Putin" mentioned in the overall data? ####

# https://cooljugator.com/run/%D0%BF%D1%83%D1%82%D0%B8%D0%BD

post <- telegrams_cleaned %>%
  mutate(message = ifelse(message == "", NA, message)) %>%
  filter(str_detect(message, "(?i)путин[а-я]*|владимир\\s*владимирович\\s*путин|владимир\\s*путин")) %>%
  drop_na(message) %>%
  nrow()

# 30618

putina <- post %>%
  filter(str_detect(message, "путина"))

lateposts <- telegrams_cleaned %>%
  mutate(message = ifelse(message == "", NA, message)) %>%
  filter(str_detect(message, "(?i)путин[а-я]*|владимир\\s*владимирович\\s*путин|владимир\\s*путин")) %>%
  drop_na(message) %>%
  filter(date >= "01-01-2023") %>%
  select(rowid, source, date, id, message)


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

