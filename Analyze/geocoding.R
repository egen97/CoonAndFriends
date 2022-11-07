library(peacePHDs)
library(tidyverse)
library(ggmap)

#### Add cities ####
translated_files <- list.files("telegram_translated/")

translations <- lapply(paste0("telegram_translated/", translated_files)[c(1,3:16)], readRDS) #File 2. was corrupted for unkown reasons
geocoded <- lapply(translations, city_mutate)

geocoded <- lapply(geocoded, oblast_mutate)

saveRDS(geocoded, "geocoded_Translations.rds")

### Add coordinates ####

TeleGramData <- readRDS("Data/geocoded_Translations.rds")

#Find all cities

cities <- unique(unlist(lapply(TeleGramData, '[[', "cities")))
oblast <- unique(unlist(lapply(TeleGramData, '[[', "oblast")))

cities <- as_tibble(cities) %>%
  filter(value != "")


cities <- cities %>%
  separate(value, into = c("city_a","city_b","city_c", "city_d",
                           "city_e", "city_f", "city_g", "city_h",
                           "city_i", "city_j", "city_k", "city_l", "city_m",
                           "city_n", "city_o"),
           fill = "right",  sep = ",")


cityMatrix <- as.vector(as.matrix(cities))
cityMatrix <- unique(cityMatrix)

city_tibble <- as_tibble(city_tibble)

city_tibble <- city_tibble %>%
  mutate(cityName = paste(value, ", Ukraine"))




city_tibble <- city_tibble %>%
  mutate_geocode(cityName, output = "more", source = "google")






SUSFU <- snafu %>% #Finner ikke hva snafu var
  select(cities, source, date) %>%
  group_by(source, date) %>%
  distinct(cities, .keep_all = TRUE) %>%
  separate(cities, into = c("city_a","city_b","city_c", "city_d",
                           "city_e", "city_f", "city_g", "city_h",
                           "city_i", "city_j", "city_k", "city_l", "city_m",
                           "city_n", "city_o"),
           fill = "right",  sep = ",")




SUSFU2 <- SUSFU %>%
  pivot_longer(1:15, values_to = "cities") %>%
  filter(!is.na(cities)) %>%
  filter(cities != "")




SUSFU3 <- SUSFU2 %>%
  full_join(please, by = c("cities" = "value"))


saveRDS(SUSFU3, "Data/map_data.rds")
