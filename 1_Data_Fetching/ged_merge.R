#### Pre amble ####

pacman::p_load(tidyverse, lubridate, zoo)

candidate_2025 <- read.csv("Data/ged_event_2025.csv")
ged_data <- read.csv("Data/GEDEvent_v25_1.csv")


depvars <- readRDS("Data/coded_posts_2025.rds")

area_data <- readRDS("Data/area_2025.rds")

Russia_Ukraine_Equipment_Losses_Original <- read_csv("Data/Russia-Ukraine Equipment Losses - Original.csv")


#### Wrangle Dangle ###

ged_war <- ged_data %>%
  filter(str_detect(conflict_name, "Ukraine")) %>%
  mutate(date_start = as.Date(date_start)) %>%
  filter(date_start >= as.Date("2022-02-24")) %>%
  select(
    year,
    deaths_russia = deaths_a,
    deaths_ukraine = deaths_b,
    date_start
  )

candidate_2025 <- candidate_2025 %>%
  filter(str_detect(conflict_name, "Ukraine")) %>%
  mutate(date_start = as.Date(date_start)) %>%
  filter(date_start >= as.Date("2022-02-24")) %>%
  select(
    year,
    deaths_russia = deaths_a,
    deaths_ukraine = deaths_b,
    date_start
  )

war_data <- bind_rows(ged_war, candidate_2025)

war_data <- war_data %>%
  group_by(date_start) %>%
  summarise(
    across(starts_with("deaths"), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

equipment_losses <- Russia_Ukraine_Equipment_Losses_Original %>%
  select(Russia_Total, Ukraine_Total, eqipment_ler = `Ratio RU/UA`, date =  Date)

equipment_losses <- equipment_losses %>%
  mutate(
    date = as.Date(date)
  )

war_data <- war_data %>%
  mutate(
    week = floor_date(date_start, "week", week_start = 1),
    russian_ler = (deaths_ukraine + 0.5) / (deaths_russia + 0.5),
    log_russian_ler = log(russian_ler)
  )



area_data$date <- as.Date(area_data$date)

depvars$date <- as.Date(depvars$date)


war_data <- war_data %>%
  rename("date" = "date_start") %>%
  full_join(area_data, by = "date") %>%
  full_join(equipment_losses, by = "date")

vars <- c("occupied",
          "deaths_russia",
          "deaths_ukraine",
          "russian_ler",
          "log_russian_ler",
          "Russia_Total",
          "Ukraine_Total",
          "eqipment_ler")

war_data <- war_data %>%
  arrange(date) %>%

  #  Rolling mean
  mutate(
    across(all_of(vars),
           ~ rollmean(.x, 14, fill = "extend"),
           .names = "{.col}_rollmean")
  ) %>%

  # 2 Rolling sd
  mutate(
    across(all_of(vars),
           ~ rollapply(.x, 14, sd, fill = "extend", align = "right"),
           .names = "{.col}_rollsd")
  ) %>%

  # 3 Deviation
  mutate(
    across(all_of(vars),
           ~ .x - get(paste0(cur_column(), "_rollmean")),
           .names = "{.col}_deviation")
  ) %>%

  # ±1 SD flag
  mutate(
    across(all_of(vars),
           ~ case_when(
             (.x - get(paste0(cur_column(), "_rollmean"))) >
               get(paste0(cur_column(), "_rollsd"))  ~  1,
             (.x - get(paste0(cur_column(), "_rollmean"))) <
               -get(paste0(cur_column(), "_rollsd")) ~ -1,
             TRUE ~ 0
           ),
           .names = "{.col}_sd_flag")
  )

day_data <- war_data %>%
  full_join(depvars, by = "date")



saveRDS(war_data, "Data/war_data.rds")
saveRDS(day_data, "Data/complete_data.rds")















