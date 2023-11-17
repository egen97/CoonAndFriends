pacman::p_load(tidyverse)

orynx <- readRDS("Data/orynx/2023-09-07.rds")
posts <- readRDS("Data/Coded_posts_13_11_2023.rds")
losses <- readRDS("Data/russian_losses.rds")
occupied_area <- readRDS("Data/occupied_area.rds")


losses <- losses %>%
  select(date, causalties)

posts <- posts %>%
  mutate(date = as.Date(date))



modData <- posts %>%
  full_join(orynx, by = "date") %>%
  full_join(occupied_area, by = "date") %>%
  full_join(losses, by = "date")

modData <- modData %>%
  ungroup() %>%
  arrange(date) %>%
  fill(starts_with("russ"), .direction = "down")



modData <- modData %>%
  mutate(area_change = actuall_area-lag(actuall_area)) %>%
  mutate(days_since_invasion = date-as.Date("2022-02-24")) %>%
  mutate(caus_1000 = causalties/1000)


modData %>%


