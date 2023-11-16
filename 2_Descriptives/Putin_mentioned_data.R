
library(tidyverse)

cleaned_1 <- read_rds("./Data/Telegrams/telegrams_cleaned_wartime_pasted.rds")
cleaned_2 <- read_rds("./Data/Telegrams/telegrams_cleaned_wartime_pasted_putin.rds")

putin_mentioned <- cleaned_1 %>%
  mutate(putin = ifelse(rowid %in% cleaned_2$rowid, 1, 0))

# saveRDS(putin_mentioned, file = "./Data/Validation_Samples/Winter_2023/Putin_mentioned_16_11_2023.rds")

p1 <- putin_mentioned %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  filter(date >= "2022-01-01") %>%
  filter(date <= "2023-10-31") %>%
  # mutate(year = substr(date, 1, 4),
  #        month = substr(date, 6, 7),
  #        year_month = paste0(year, "_", month)) %>%
  group_by(date) %>%
  summarise(n = sum(putin, na.rm = TRUE)) %>%
  rename(`Number of posts where Putin is mentioned` = n) %>% # ifelse(n >= 1, "mentioned", "not mentioned")
  ggplot(aes(date, `Number of posts where Putin is mentioned`, group = 1)) +
  geom_line(color = "steelblue") + # aes(color =  `Number of posts where Putin is`, linetype = `Number of posts where Putin is`)
  #scale_color_manual(values = c("steelblue")) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 20))

ggsave(p1, file = "./Figures/Mentioning_Putin_1.pdf", width = 12, height = 10, dpi = 150)

p2 <- putin_mentioned %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  filter(date >= "2022-01-01") %>%
  filter(date <= "2023-10-31") %>%
  # mutate(year = substr(date, 1, 4),
  #        month = substr(date, 6, 7),
  #        year_month = paste0(year, "_", month)) %>%
  group_by(date, putin) %>%
  count() %>%
  spread(putin, n) %>%
  mutate(putin_share = `1`/(`0`+`1`)) %>%
  mutate(Share = "",
         `Share of posts mentioning Putin` = as.factor(Share)) %>%
  ggplot(aes(date, putin_share,
             group = `Share of posts mentioning Putin`,
             color = `Share of posts mentioning Putin`)) +
  geom_line() +
  scale_color_manual(values = c("steelblue", "steelblue")) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 20))

ggsave(p1, file = "./Figures/Mentioning_Putin_2.pdf", width = 12, height = 10, dpi = 150)

cowplot::plot_grid(p1, p2,
                   labels = c("A", "B"))



