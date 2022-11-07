
library(tidyverse)
library(tidytext)
library(googleLanguageR)
googleLanguageR::gl_auth()

locs <- read_rds("./geocoded_Translations.rds")
locs <- bind_rows(locs)

locs_sub <- locs %>% select(translatedText, id, source, cities, oblast)

all_texts <- locs_sub$translatedText %>% list()

g_sentiments <- lapply(all_texts, gl_nlp)

g_sentiment_df <- locs_sub %>%
  cbind(TEST2[[1]]$documentSentiment)

save(g_sentiments, file = "./g_sentiments.rda")
saveRDS(g_sentiments, file = "./g_sentiments.rds")


all_sents <- g_sentiments[[1]]$documentSentiment

locs %>%
  cbind(all_sents) -> google_sentiment

saveRDS(google_sentiment, file = "./sentiment/google_sentiment.rds")
