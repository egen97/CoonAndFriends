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
  mutate(caus_1000 = causalties/1000) %>%
  mutate(
    weeks = floor(days_since_invasion/7),
    months = floor((days_since_invasion/365)*12)
  )


modData %>%
  ggplot() +
  geom_line(aes(days_since_invasion, weeks), linewidth = 1) +
  geom_line(aes(days_since_invasion, months), linewidth = 1) +
  geom_vline(aes(xintercept = 0)) +
  ThemePark::theme_barbie()


mentioned <- readRDS("Data/Putin_mentioned_16_11_2023.rds")

reactions <- readRDS("Data/Reactions_data_16_11_23.rds")



mentioned <- mentioned %>%
  select(date, putin)


reactions <- reactions %>%
  select(date, putin, 6:27)


mentioned <- mentioned %>%
  mutate(date = as.Date(date))

mentioned <- mentioned %>%
  full_join(orynx, by = "date") %>%
  full_join(occupied_area, by = "date") %>%
  full_join(losses, by = "date")


mentioned <- mentioned %>%
  ungroup() %>%
  arrange(date) %>%
  fill(starts_with("russ"), .direction = "down")



reactions <- reactions %>%
  filter(putin == 1) %>%
  full_join(orynx, by = "date") %>%
  full_join(occupied_area, by = "date") %>%
  full_join(losses, by = "date") %>%
  ungroup() %>%
  arrange(date) %>%
  fill(starts_with("russ"), .direction = "down")


mentioned <- mentioned %>%
  mutate(area_change = actuall_area-lag(actuall_area)) %>%
  mutate(days_since_invasion = date-as.Date("2022-02-24")) %>%
  mutate(caus_1000 = causalties/1000) %>%
  mutate(
    weeks = floor(days_since_invasion/7),
    months = floor((days_since_invasion/365)*12)
  )



reactions <- reactions %>%
  mutate(area_change = actuall_area-lag(actuall_area)) %>%
  mutate(days_since_invasion = date-as.Date("2022-02-24")) %>%
  mutate(caus_1000 = causalties/1000) %>%
  mutate(
    weeks = floor(days_since_invasion/7),
    months = floor((days_since_invasion/365)*12)
  )




saveRDS(modData, "Data/mod_data_17112023.rds")
saveRDS(reactions, "Data/reactions_17112023.rds")
saveRDS(mentioned, "Data/mentioned_17112023.rds")
