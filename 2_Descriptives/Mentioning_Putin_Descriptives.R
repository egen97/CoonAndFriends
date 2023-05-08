
library(tidyverse)
library(tidytext)
library(stopwords)
library(ggcorrplot)

telegrams_cleaned <- readRDS("../Data/telegrams_cleaned.rds")

tokens <- telegrams_cleaned %>%
  filter(date >= "2022-01-01") %>%
  unnest_tokens(input = message, 
                output = token, 
                token = "words") 

#### Times Putin is mentioned ####

## Count of words of posts on each day being Putin, per source

stopwords <- tibble(token = stopwords("ru", source = "snowball"))

tokens_preprocessed <- tokens %>% 
  select(source, date, token) %>%
  mutate(token = str_remove_all(token, ","),
         token = str_remove_all(token, "\\.")) %>%
  filter(!str_detect(token, "[0-9]+?")) %>%
  na.omit() %>%
  anti_join(stopwords, by = join_by(token)) 

putin_mentioned_count <- tokens_preprocessed %>%
  mutate(putin = ifelse(str_detect(token, "путин"), 1, 0)) %>%
  group_by(source, date) %>%
  summarise(putin = sum(putin)) %>%
  ungroup() %>%
  ggplot(aes(date, putin, fill = source)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~source, scales = "free") +
  labs(title = "Comparison over time for each channel:",
       subtitle = "Count of the word Putin ('путин') per channel per day",
       x = "", y = "") + 
  theme_minimal() +
  theme(legend.position = "bottom")

## Looks to me like bolshiepushki mostly has either photos or videos (many of him videoblogging).
## The lack of text therefore naturally means less mentioning of Putin in text.

ggsave(putin_mentioned_count, file = "./Plots/Putin_Mentioned_Count.png",
       width = 12, height = 10, dpi = 150, units = "in", device= "png")


## Percentage words of posts on each day being Putin, per source

putin_mentioned <- tokens_preprocessed %>%
  mutate(putin = ifelse(str_detect(token, "путин"), 1, 0)) %>%
  group_by(source, date) %>%
  summarise(putin = sum(putin)) %>%
  ungroup()

putin_percentage <- tokens_preprocessed %>%
  group_by(source, date) %>%
  count(token) %>%
  summarise(tokens = sum(n)) %>%
  ungroup() %>%
  left_join(putin_mentioned, by = join_by(source, date)) %>%
  mutate(putin_percentage = (putin/tokens)*100)

putin_mentioned_percentage <- putin_percentage %>%
  ggplot(aes(date, putin_percentage, color = source)) +
  geom_line() + 
  facet_wrap(~source) +
  labs(title = "Comparison between channels over time:",
       subtitle = "Percentage of words per channel per day containing the word Putin ('путин')",
       x = "", y = "") + 
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(putin_mentioned_percentage, file = "./Plots/Putin_Mentioned_Percentage.png",
       width = 12, height = 10, dpi = 150, units = "in", device= "png")


#### Reactions and Putin-mentioning correlation #####

## Overview of reactions

top_reactions <- telegrams_cleaned %>%
  filter(date >= "2022-01-01") %>% # Using posts after the 1st of January 2022
  select(id, reaction) %>%
  unnest(cols = c(reaction)) %>% # Getting reactions
  mutate(count = as.numeric(count)) %>%
  group_by(reaction, id) %>%
  summarise(sum = sum(count, na.rm = T)) %>% # Summing up the number of reactions per post
  ungroup()

# Overview of all reactions that exists in the data
all_reactions <- top_reactions %>%
  count(reaction) %>%
  na.omit() %>%
  arrange(desc(n))

## Finding mentionings of Putin

stopwords <- tibble(token = stopwords("ru", source = "snowball"))

putin_times_per_post <- tokens %>% 
  select(id, source, token) %>%
  # Preprocessing
  mutate(token = str_remove_all(token, "[[:punct:]]+?")) %>% # Removing commas and dots and other punctuation
  filter(!str_detect(token, "[0-9]+?")) %>% # Removing numbers 
  na.omit() %>% # Removing places with missing tokens
  anti_join(stopwords, by = join_by(token)) %>% # Removing Russian stopwords
  # Finding tokens of Putin
  mutate(putin = ifelse(str_detect(token, "путин"), 1, 0)) %>%
  # Counting number of times token occurs per post
  group_by(id) %>%
  summarise(putin = sum(putin)) %>%
  ungroup()

telegrams_putin <- telegrams_cleaned %>% # Starting with full dataset to make sure all rows are included
  filter(date >= "2022-01-01") %>% # Only picking posts after 1st of January 2022
  select(id, source, reaction) %>%
  # Joining with mentionings of Putin before unnesting so not to create duplicate ids
  left_join(putin_times_per_post, by = join_by(id)) %>%
  # If there is NA, it means there were no mentionings of Putin to count, so make 0
  mutate(putin = ifelse(is.na(putin), 0, putin)) %>% 
  # Unnest reactions
  unnest(cols = c(reaction)) %>%
  # Removing NAs from count in order to remove all sources that didn't have reactions in the API
  filter(reaction != "no_reaction_in_api") %>%
  select(-chosen) %>% # Not sure what this variable is beyond being a logical TRUE/FALSE
  # If count is NA, it means there were no reactions to the post
  mutate(count = ifelse(is.na(count), 0, count)) 

nrow(telegrams_putin)

reaction_filter <- telegrams_putin %>%
  filter(reaction %in% c(all_reactions$reaction[1:20]))%>%
  pivot_wider(names_from = "reaction", values_from = "count", values_fill = 0)

data_cor <- round(cor(reaction_filter %>% select(4:length(reaction_filter)),
                      reaction_filter$putin,
                      use = "pairwise.complete.obs"), 3)

reaction_putin_correlation <- ggcorrplot(data_cor, lab = TRUE)

ggsave(reaction_putin_correlation, file = "./Plots/Reactions_Putin_Correlation.png",
       width = 12, height = 10, dpi = 150, units = "in", device= "png")

# Correlation between all reactions
corr <- round(cor(reaction_filter %>% select(4:length(reaction_filter)), 
                  use = "pairwise.complete.obs"), 1)

ggcorrplot(corr, method = "circle", 
           type = "lower",
           colors = c("#6D9EC1", "white", "#E46726"))

