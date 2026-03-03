#### PRe amble ####

pacman::p_load(tidyverse, ggthemes)

complete_data <- readRDS("Data/complete_data.rds")

war_data <- readRDS("Data/war_data.rds")


#### War descriptives ####

# Russian LER over time

war_data %>%
  ggplot(aes(date, log_russian_ler_rollmean)) +
  geom_line(linewidth = 1.3) +
  geom_hline(aes(yintercept = 0), linewidth = 1.3) +
  theme_minimal() +
  labs(
    y = "Ln Russian Loss/Exchange ratio: 14-day rolling mean",
    x = ""
  ) +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )



# Russian Equipment LER over time

war_data %>%
  ggplot(aes(date, log_equipment_ler_rollmean)) +
  geom_line(linewidth = 1.3) +
  geom_hline(aes(yintercept = 0), linewidth = 1.3) +
  theme_minimal() +
  labs(
    y = "Russian Equipment Loss/Exchange ratio: 14-day rolling mean",
    x = ""
  ) +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )


# Occupied territory over time



war_data %>%
  ggplot(aes(date, occupied_rollmean)) +
  geom_line(linewidth = 1.3) +
  theme_minimal() +
  labs(
    y = "Russian Occupied territory: 14-day rolling mean",
    x = ""
  ) +
  scale_y_continuous(labels = scales::label_percent(scale = 1), limits = c(NA,20))
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )





