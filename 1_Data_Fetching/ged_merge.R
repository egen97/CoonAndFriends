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
    date_start = ymd_hms(date_start),
    week = floor_date(date_start, "week", week_start = 1),
    russian_ler = (deaths_ukraine + 0.5) / (death_russia + 0.5),
    log_russian_ler = log(deaths_ukraine + 0.5) - log(death_russia + 0.5)
  )


week_data <- war_data %>%
  group_by(week) %>%
  summarise(
    week_death_russia = sum(death_russia),
    week_deaths_ukraine = sum(deaths_ukraine)
    )

week_data <- week_data %>%
  mutate(
    week_russian_ler = (week_deaths_ukraine + 0.5) / (week_death_russia + 0.5),
    week_log_russian_ler = log(week_deaths_ukraine + 0.5) - log(week_death_russia + 0.5)

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
  ggplot(aes(date, log_russian_ler)) +
  geom_line()


day_data <- day_data %>%
  select(war_mention, impression_putin, support_putin, criticism_putin, occupied, two_week, death_russia, deaths_ukraine, russian_ler, log_russian_ler, week, Russia_Total, Ukraine_Total, eqipment_ler)


day_data <- day_data %>%
  full_join(
    week_data, by = "week"
  )






saveRDS(day_data, "Data/complete_data.rds")















