#### Pre amble ####

pacman::p_load(tidyverse, lubridate)

candidate_2025 <- read.csv("Data/ged_event_2025.csv")
ged_data <- read.csv("Data/GEDEvent_v25_1.csv")


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


# Calculate ratio
# Simple problem for daily data, many divisions by 0

# One option: Calculate by week, and hope that it smooths? Or, put in a stand-in value

war_data <- war_data %>%
  mutate(
    date_start = ymd_hms(date_start),
    week = floor_date(date_start, "week", week_start = 1)
  )


week_data <- war_data %>%
  group_by(week) %>%
  summarise(
    death_russia = sum(death_russia),
    deaths_ukraine = sum(deaths_ukraine)
  )

week_data <- week_data %>%
  mutate(
    russian_ler = death_russia/deaths_ukraine,
    ln_russian_ler = log(russian_ler)

  )


week_data %>%
  ggplot(aes(week, ln_russian_ler, group = 1)) +
  geom_line() +
  geom_smooth()

saveRDS(week_data, "Data/causalties_ler_data.rds")















