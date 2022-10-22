library(peacePHDs)
library(tidyverse)
library(ggmap)

#### Add cities (data is on google drive) ####
translated_files <- list.files("telegram_translated/")

translations <- lapply(paste0("telegram_translated/", translated_files)[c(1,3:16)], readRDS)
geocoded <- lapply(translations, city_mutate)

geocoded2 <- lapply(geocoded, oblast_mutate)

saveRDS(geocoded2, "geocoded_Translations_2.rds")

### Add coordinates ####

TeleGramData <- readRDS("Data/geocoded_Translations_2.rds")



