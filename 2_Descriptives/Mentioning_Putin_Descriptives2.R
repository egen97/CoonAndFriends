
library(tidyverse)

p1 <- putin_mentioned %>%
  filter(date >= "2022-01-01") %>%
  filter(date <= "2023-04-30") %>%
  mutate(year = substr(date, 1, 4),
         month = substr(date, 6, 7),
         year_month = paste0(year, "_", month)) %>%
  group_by(date, putin) %>%
  count() %>%
  mutate(`Number of posts where Putin is` = ifelse(putin == 1, "mentioned", "not mentioned")) %>%
  ggplot(aes(date, n, group = `Number of posts where Putin is`)) +
  geom_line(aes(color =  `Number of posts where Putin is`, linetype = `Number of posts where Putin is`)) +
  scale_color_manual(values = c("steelblue", "black")) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 20))

p2 <- putin_mentioned %>%
  filter(date >= "2022-01-01") %>%
  filter(date <= "2023-04-30") %>%
  mutate(year = substr(date, 1, 4),
         month = substr(date, 6, 7),
         year_month = paste0(year, "_", month)) %>%
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

cowplot::plot_grid(p1, p2,
                   labels = c("A", "B"))
