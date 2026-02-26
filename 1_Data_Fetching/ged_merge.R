#### Pre amble ####

pacman::p_load(tidyverse, lubridate, zoo)

candidate_2025 <- read.csv("Data/ged_event_2025.csv")
ged_data <- read.csv("Data/GEDEvent_v25_1.csv")

depvars <- readRDS("Data/coded_posts_2025.rds")

area_data <- readRDS("Data/area_2025.rds")

Russia_Ukraine_Equipment_Losses_Original <- read_csv("Data/Russia-Ukraine Equipment Losses - Original.csv")
#### Wrangle Dangle ###

ged_war <- ged_data %>%
  filter(
    str_detect(conflict_name, "Ukraine"),
    year >= 2021
  )


candidate_war <- candidate_2025 %>%
  filter(
    str_detect(conflict_name, "Ukraine"),
    year >= 2021
  )


ged_war <- ged_war %>%
  select(
    year,
    death_russia = deaths_a,
    deaths_ukraine = deaths_b,
    date_start
  )

candidate_2025 <- candidate_2025 %>%
  select(
    year,
    death_russia = deaths_a,
    deaths_ukraine = deaths_b,
    date_start
  )



war_data <- bind_rows(ged_war, candidate_2025)


equipment_losses <- Russia_Ukraine_Equipment_Losses_Original %>%
  select(Russia_Total, Ukraine_Total, eqipment_ler = `Ratio RU/UA`, Date)

equipment_losses <- equipment_losses %>%
  mutate(
    Date = as.Date(Date)
  )

# Calculate ratio
# Simple problem for daily data, many divisions by 0. Add 1 to Russia if 0, created to many missing in later analysis..



war_data <- war_data %>%
  mutate(
    death_russia = ifelse(death_russia == 0, 1, death_russia)
  )

war_data <- war_data %>%
  mutate(
    date_start = ymd_hms(date_start),
    week = floor_date(date_start, "week", week_start = 1),
    russian_ler = deaths_ukraine/death_russia,
    ln_russian_ler = log(russian_ler)
  )


week_data <- war_data %>%
  group_by(week) %>%
  summarise(
    week_death_russia = sum(death_russia),
    week_deaths_ukraine = sum(deaths_ukraine)
    )

week_data <- week_data %>%
  mutate(
    week_russian_ler = deaths_ukraine/death_russia,
    week_ln_russian_ler = log(week_russian_ler)

  )

area_data$date <- as.Date(area_data$date)

depvars$date <- as.Date(depvars$date)


area_data$two_week <- rollmean(area_data$occupied, 14, fill = "extend")

complete <- depvars %>%
  full_join(area_data, by = "date")

complete <- complete %>%
  full_join(
    equipment_losses,
    by = c("date" = "Date")
  )

day_data <- complete %>%
  full_join(
    war_data,
    by = c("date" = "date_start")
    )



day_data %>%
  ggplot(aes(date, ln_russian_ler)) +
  geom_line()


day_data <- day_data %>%
  select(war_mention, impression_putin, support_putin, criticism_putin, occupied, two_week, death_russia, deaths_ukraine, russian_ler, ln_russian_ler, week)


week_data <- day_data %>%
  filter(week > as.Date("2021-02-01")) %>%
  group_by(week) %>%
  mutate(across(1:10, ~as.numeric(.x))) %>%
  summarise(
    war_mention = sum(war_mention, na.rm = TRUE),
    impression_putin = mean(impression_putin, na.rm = TRUE),
    support_putin = mean(support_putin, na.rm = TRUE),
    criticism_putin = mean(criticism_putin, na.rm = TRUE),
    occupied = mean(occupied, na.rm = TRUE),
    death_russia = sum(death_russia, na.rm = TRUE),
    deaths_ukraine = sum(deaths_ukraine, na.rm = TRUE),
    russian_ler = deaths_ukraine/death_russia,
    ln_russian_ler = log(russian_ler)
  )


week_data %>%
  ggplot(aes(russian_ler)) +
  geom_density()

week_data %>%
  ggplot(aes(ln_russian_ler)) +
  geom_density()

saveRDS(week_data, "Data/week_data.rds")

saveRDS(day_data, "Data/day_data.rds")















