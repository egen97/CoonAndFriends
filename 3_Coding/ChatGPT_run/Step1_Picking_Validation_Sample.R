
library(tidyverse)

set.seed(42)

#### Create validation sample with the translated posts as well ####

# files <- list.files("./Data/Channel_Translated/", full.names = TRUE)
#
# telegrams_english <- lapply(files, read_rds)
#
# telegrams_english <- map(telegrams_english, .f = list(. %>% dplyr::select(source, date, id, message, detectedSourceLanguage, translatedText)))
#
# telegrams_english <- do.call(rbind, telegrams_english)
#
# telegrams <- readRDS("./Data/Telegrams/telegrams_cleaned_wartime_pasted_putin.rds")
#
# telegrams_joined <- telegrams %>%
#   mutate(date = as.Date(date)) %>%
#   rename(ru_message = message) %>%
#   #filter(date %in% c(telegrams_english$date)) %>%
#   inner_join(telegrams_english %>% rename(en_message = translatedText), join_by(source, date, id)) %>%
#   select(source, id, date, ru_message, en_message)
#
# telegrams_joined_sample <- telegrams_joined %>%
#   mutate(year = substr(date, 1, 4),
#          month = substr(date, 6, 7),
#          year_month = paste0(year, "_", month)) %>%
#   filter(year_month != "2023_05") %>%
#   group_by(year_month) %>%
#   sample_n(10) %>%
#   ungroup() %>%
#   select(-month, -year, -year_month)


#### Create validation sample without translated posts ####

telegrams <- readRDS("./Data/Telegrams/telegrams_cleaned_wartime_pasted_putin.rds")
completions_df_1 <- readRDS("./Data/Validation_Samples/Winter_2023/completions_df_1.rds")

telegrams2 <- telegrams %>%
  filter(!rowid %in% completions_df_1$rowid)

telegrams_joined_sample2 <- telegrams2 %>%
  mutate(nchar = nchar(message)) %>%
  #filter(nchar <= 2000) %>%
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
  # 23.0   207.8   524.0   859.3  1076.0 12375.0
  mutate(year = substr(date, 1, 4),
         month = substr(date, 6, 7),
         year_month = paste0(year, "_", month)) %>%
  filter(year_month != "2023_11") %>%
  group_by(year_month) %>%
  sample_n(400) %>%
  ungroup() %>%
  select(rowid, source, id, date, message)

saveRDS(telegrams_joined_sample2, "./Data/Validation_Samples/Winter_2023/Subsample_Fall2023_2.rds")

