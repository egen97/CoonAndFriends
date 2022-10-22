library(rvest)
library(tidyverse)
Ukr_cities <- read_html("https://en.wikipedia.org/wiki/List_of_cities_in_Ukraine") %>%
  html_element(xpath = "//*[@id='mw-content-text']/div[1]/table") %>%
  html_table()



View(Ukr_cities)

Ukr_cities_clean <- Ukr_cities %>%
  select(city = `City name`, city_ukr = `City name(in Ukrainian)`, oblast = Oblast)



saveRDS(Ukr_cities_clean, "ukr_cities.rds")
