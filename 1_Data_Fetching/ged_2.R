pacman::p_load(tidyverse, lubridate)

ged <- read_csv("Data/GEDEvent_v25_0_7.csv")

ged <- ged %>%
  filter(country == "Ukraine", type_of_violence == 1, date_prec <= 3) %>%
  select(country, date_start ,deaths_a, deaths_b, side_a, side_b) %>%
  group_by(date_start) %>%
  summarise(
    deaths_a = sum(deaths_a, na.rm = TRUE),
    deaths_b = sum(deaths_b, na.rm = TRUE),
    side_a = first(side_a),
    side_b = first(side_b)
  ) %>%
  mutate(
    side_a = "Russia",
    side_b = "Ukraine"
  )

ged <- ged %>%
  mutate(loss_exchange_ratio = deaths_b/deaths_a)

ged %>%
  ggplot(aes(date_start, loss_exchange_ratio)) +
  geom_line() +
  theme_barbie()
