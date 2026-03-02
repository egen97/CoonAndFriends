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
    axis.title = element_text(size = 14)
  )
