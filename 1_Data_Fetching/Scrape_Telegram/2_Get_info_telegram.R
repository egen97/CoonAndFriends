
library(tidyverse)
library(stringr)
library(jsonlite)

#### Scrape the channels using python ####

# system('scrape_telegram.py')

#### Get information on dates and number of posts ####

post <- fromJSON("./1_Data_Fetching/Scrape_Telegram/jsons/anna_news.json", flatten = TRUE) %>%
  mutate(date = as.Date(date))

post$date %>% min()
post$date %>% max()

post %>%
  mutate(message = ifelse(message == "", NA, message)) %>%
  drop_na(message) %>%
  nrow()


# fromJSON("./wehearfromyanina.json", flatten = TRUE) %>%
#  mutate(source = name) %>%
#   mutate(date = as.Date(date)) %>%
#   filter(date >= "2022-01-01")

############

# for(i in 1:length(telegram_ru)){
#   source <- telegram_ru[[i]]$source %>% unique()
#   print(paste(i, source))
# }




#telegram_ru[[22]]$date %>% min()
