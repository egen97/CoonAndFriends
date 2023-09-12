

library(tidyverse)

plotposts <- coded_posts %>%
  select(rowid, source, date, year_month, support, sentiment, trust, competence) %>%
  filter(year_month != "2023_05") %>%
  mutate(year = substr(year_month, 3, 4),
         month = substr(year_month, 6, 8),
         year_month = paste0(month, "/", year)) %>%
  mutate(year_month = factor(year_month, levels = c("01/22", "02/22", "03/22", "04/22", "05/22", "06/22", "07/22", "08/22", "09/22",
                                                    "10/22", "11/22", "12/22", "01/23", "02/23", "03/23", "04/23")))

support_p <- plotposts %>%
  mutate(support = as.numeric(support)) %>%
  group_by(year_month) %>%
  summarise(Support = mean(support)) %>%
  ggplot(aes(year_month, Support, group = 1)) +
  ylim(1.5,3) +
  geom_line(linewidth = 1, linetype = "dashed") +
  labs(x = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

sentiment_p <- plotposts %>%
  mutate(sentiment = as.numeric(sentiment)) %>%
  group_by(year_month) %>%
  summarise(Sentiment = mean(sentiment)) %>%
  ggplot(aes(year_month, Sentiment, group = 1)) +
  ylim(1.5,3) +
  labs(x = "") +
  geom_line(linewidth = 1, linetype = "dashed") +
  theme_minimal() +
  theme(legend.position = "bottom")

trust_p <- plotposts %>%
  mutate(trust = as.numeric(trust)) %>%
  group_by(year_month) %>%
  summarise(Trust = mean(trust)) %>%
  ggplot(aes(year_month, Trust, group = 1)) +
  ylim(1.5,3) +
  labs(x = "") +
  geom_line(linewidth = 1, linetype = "dashed") +
  theme_minimal() +
  theme(legend.position = "bottom")

competence_p <- plotposts %>%
  mutate(competence = as.numeric(competence)) %>%
  group_by(year_month) %>%
  summarise(Competence = mean(competence)) %>%
  ggplot(aes(year_month, Competence, group = 1)) +
  ylim(1.5,3) +
  geom_line(linewidth = 1, linetype = "dashed") +
  labs(x = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

cowplot::plot_grid(support_p, sentiment_p, competence_p, trust_p,
                   labels = c("Support", "Sentiment",
                              "Competence", "Trust"))

coded_posts %>%
  select(rowid, source, date, year_month, state_of_war, responsibility, response) %>%
  gather(state_of_war, responsibility, response,
         key = "Variable", value = "Value") %>%
  group_by(year_month, Variable, Value) %>%
  count() %>%
  spread(Value, n) %>%
  mutate(`3` = ifelse(is.na(`3`), 0, `3`)) %>%
  mutate(share_high = `3`/(`1`+`2`+`3`)*100) %>%
  mutate(share_high2 = `2`/(`1`+`2`)*100) %>%
  ggplot(aes(year_month, share_high2, group = Variable, color = Variable)) +
  geom_line() +
  facet_wrap(~ Variable, scales = "free_y") +
  theme_minimal()
