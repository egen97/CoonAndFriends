 #Google
sentiment <- readRDS("Data/Sentiments/google_sentiment.rds")



cities <- unique(sentiment$cities)
oblast <- unique(sentiment$oblast)


sentiment <- as_tibble(sentiment) %>%
  filter(value != "")


sentiment2 <- sentiment %>%
  separate(cities, into = c("city_a","city_b","city_c", "city_d",
                           "city_e", "city_f", "city_g", "city_h",
                           "city_i", "city_j", "city_k", "city_l", "city_m",
                           "city_n", "city_o"),
           fill = "right",  sep = ",")


df2 <- as.vector(as.matrix(sentiment2))
please <- unique(df2)

please <- as_tibble(please)

google_sentiment <- please %>%
  select(starts_with("city"), source, date, magnitude, score) %>%
  pivot_longer(starts_with("city"), values_to = "city") %>%
  select(-name) %>%
  filter(city != "") %>%
  filter(!is.na(city)) %>%
  mutate(across(everything(), ~ifelse(.x == "NA", NA, .x)))

saveRDS(google_sentiment, "Data/google_sentiment_agg.rds")

## Vader


sentiment <- readRDS("Data/Sentiments/vader_sentiments.rds")



cities <- unique(sentiment$cities)
oblast <- unique(sentiment$oblast)


sentiment <- as_tibble(sentiment) %>%
  filter(value != "")


sentiment2 <- sentiment %>%
  separate(cities, into = c("city_a","city_b","city_c", "city_d",
                            "city_e", "city_f", "city_g", "city_h",
                            "city_i", "city_j", "city_k", "city_l", "city_m",
                            "city_n", "city_o"),
           fill = "right",  sep = ",")


df2 <- as.vector(as.matrix(sentiment2))
please <- unique(df2)

please <- as_tibble(please)

vader_sentiment <- please %>%
  select(starts_with("city"), source, date, compound) %>%
  pivot_longer(starts_with("city"), values_to = "city") %>%
  select(-name) %>%
  filter(city != "") %>%
  filter(!is.na(city)) %>%
  mutate(across(everything(), ~ifelse(.x == "NA", NA, .x)))



saveRDS(vader_sentiment, "Data/vader_sentiment_agg.rds")
