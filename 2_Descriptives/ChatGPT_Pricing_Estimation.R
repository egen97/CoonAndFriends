
library(tidyverse)
library(tidytext)

telegrams_cleaned <- readRDS("../Data/telegrams_cleaned.rds")

## Keywords for mentioning Putin:

# Путин - Putin
# 
# 
# 

#### Estimating price using ChatGPT API ####

telegrams_putin <- telegrams_cleaned %>%
  filter(date >= "2022-01-01") %>%
  mutate(putin = ifelse(str_detect(message, "Путин"), 1, 0)) %>%
  filter(putin == 1) %>%
  select(-putin)

telegrams_putin %>% 
  nrow()
# 17032 posts in our sample mentions Putin

tokens <- telegrams_putin %>%
  unnest_tokens(input = message, output = token, token = "words") 

tokens %>% 
  select(id, token) %>%
  na.omit() %>%
  count(id) %>%
  summarise(mean_tokens = mean(n))
# These posts have an average of 97 tokens


## Using the open.ai chatgpt API pricing calculator
## https://gptforwork.com/tools/openai-chatgpt-api-pricing-calculator 
## The estimated cost of using ChatGPT on these posts is:

# $3.41

