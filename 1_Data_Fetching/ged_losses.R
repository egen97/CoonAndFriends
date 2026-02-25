pacman::p_load(tidyverse, lubridate, ThemePark)

ged <- read_csv("Data/GEDEvent_v25_0_7.csv", col_names = TRUE)

ged %>%
  filter(country == "Ukraine", type_of_violence == 1, filter()) %>%
  select(country, date_start ,deaths_a, deaths_b, side_a, side_b) %>%
  group_by(date_start) %>%
  summarise(
    deaths_a = sum(deaths_a, na.rm = TRUE),
    deaths_b = sum(deaths_b, na.rm = TRUE),
    side_a = first(side_a),
    side_b = first(side_b)
  ) %>%
  View()

ged %>%
  filter(country == "Ukraine", str_detect(side_a, "Russia")) %>%
  filter(date_start == as.Date("2023-01-01")) %>%
  View()

ler <- ged %>%
  filter(country == "Ukraine", type_of_violence == 1, date_prec <=3) %>%
  select(country, date_start ,deaths_a, deaths_b, side_a, side_b) %>%
  mutate(week_nr = week(date_start)) %>%
  group_by(week_nr) %>%
  summarise(
    country = first(country),
    deaths_a = sum(deaths_a, na.rm = TRUE),
    deaths_b = sum(deaths_b, na.rm = TRUE),
    side_a = first(side_a),
    side_b = first(side_b)
  )

ler %>%
  mutate(ler_r = deaths_b/deaths_a) %>%
  ggplot(aes(week_nr, ler_r)) +
  geom_line(colour = godfather_theme_colors["light"], linewidth = 1.4) +
  theme_godfather() +
  labs(
    title = "Loss-Exchange Ratio: Ukraine/Russia",
    x = "week",
    y = "",
    caption = "Source: UCDP Candidate Events Dataset, Jan-Dec 2023"
    )



