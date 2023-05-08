
library(tidyverse)

telegrams_cleaned <- readRDS("../Data/telegrams_cleaned.rds")

#### URLs that have been embedded in the post ####
urls <- telegrams_cleaned %>%
  select(source, entity) %>%
  unnest() %>% 
  na.omit() %>%
  group_by(MessageEntityTextUrl) %>%
  count(sort = TRUE)


#### Channels that have been forwarded ####
forwarded_channels <- telegrams_cleaned %>%
  select(source, fwd_from.from_id.channel_id) %>%
  na.omit() %>%
  group_by(fwd_from.from_id.channel_id) %>%
  count(sort = TRUE)

current_channels <- telegrams_cleaned %>% 
  select(peer_id.channel_id) %>%
  unique() %>%
  pull()

forwarded_channels <- forwarded_channels %>%
  mutate(in_sample = ifelse(fwd_from.from_id.channel_id %in% c(current_channels), 1 ,0)) %>%
  arrange(desc(in_sample))


### HOW TO GO FROM CHANNEL ID TO CHANNEL NAME? ###

## 1. This work occasionally, most often not:

# How to look at a channel by the channel-id?
# Type this into your browser: https://web.telegram.org/k/#
# Then add -XXXXXXXXXX behind the #, where each X represents a number in the channel-id
# For example like so: https://web.telegram.org/k/#-1109403194 

## 2. You cannot provide the channel-id directly to the API scraper without first providing the channel name
# https://arabic-telethon.readthedocs.io/en/stable/extra/basic/entities.html


