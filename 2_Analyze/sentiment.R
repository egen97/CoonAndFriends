
library(tidyverse)
library(tidytext)
library(vader)

locs <- read_rds("./geocoded_Translations.rds")
locs <- bind_rows(locs)

locs_sub <- locs %>% select(translatedText, id, source, cities, oblast)

vaders <- list()

for (i in 1:nrow(locs_sub)) {

  vaders[[i]] <- vader::get_vader(locs_sub$translatedText[i])

  message("Finishing up ", i)
}

saveRDS(vaders, file = "./vaders.rds")
save(vaders, file = "./vaders.rda")

bind_rows(vaders) -> vaders_df

locs %>%
  cbind(vaders_df) -> vader_sentiments

saveRDS(vader_sentiments, file = "./sentiment/vader_sentiments.rds")


# MEAN SENTIMENT PER DATE AND LOCATION

dat <- left_join(all_posts, sentiment_posts)

saveRDS(sentiment_posts, file = "./sentiment_vader.rds")


left_join(vader_sentiments, google_sentiment) %>% rename(g_sentiment = score,
                                                         g_magnitude = magnitude,
                                                         v_sentiment = compound,
                                                         v_pos = pos,
                                                         v_neg = neg,
                                                         v_neu = neu) -> all_sentiments

saveRDS(all_sentiments, file = "./sentiment/all_sentiments.rds")

library(zoo)

plot_sentiments <- all_sentiments %>%
  #filter(source == "anna_news") %>%
  filter(str_detect(translatedText, "[PuTtIiNn]")) %>%
  select(translatedText, source, date, v_sentiment, g_sentiment) %>%
  group_by(source, date) %>%
  summarise(v_sum = mean(as.numeric(v_sentiment), na.rm = TRUE),
            g_sum = mean(as.numeric(g_sentiment), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(v_sum = rollmean(v_sum, k = 30, fill = NA),
         g_sum = rollmean(g_sum, k = 30, fill = NA)) %>%
  gather(v_sum, g_sum,
         key = "sentiment_type", value = "sentiment") %>%
  mutate(sentiment_type = ifelse(sentiment_type == "v_sum", "Vader",
                                   ifelse(sentiment_type == "g_sum", "Google",
                                          NA)))

saveRDS(plot_sentiments, file = "./prototype/plot_sentiments.rds")

plot_sentiments %>%
  ggplot(aes(date, sentiment, group = sentiment_type, color = sentiment_type)) +
  geom_line(alpha = 0.3)
