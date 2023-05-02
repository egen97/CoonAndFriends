
#### TELEGRAM BOT ####

library(tidyverse)
library(jsonlite)
library(httr)

bottoken <- "5619805180:AAGF4fK4NhmWuKtaR2LF1En9xu5C9V5ks_8"

files <- list.files("./telegram_translated/", full.names = TRUE)

posts <- list()

for (i in 1:length(files)) {

  posts[[i]] <- readRDS(files[i]) %>%
    filter(!message == "") %>%
    select(source, date, translatedText) %>%
    group_by(source, date) %>%
    summarise(english_text = str_c(translatedText, collapse = "\n\n")) %>%
    ungroup()

}

all_posts <- bind_rows(posts) %>%
  mutate(bot_message = paste0("*", "On ",
                              date,
                              " @", source,
                              " wrote this:", "*",
                              "\n\n",
                              english_text)) %>%
  mutate(bot_message = URLencode(bot_message))


# posts_split <- all_posts %>%
#   mutate(text1 = ifelse(str_length(english_text) >= 0 | str_length(english_text) <= 4000, substr(english_text, 1, 4000), ""),
#          text2 = ifelse(str_length(english_text) >= 4001 | str_length(english_text) <= 8000, substr(english_text, 4001, 8000), ""),
#          text3 = ifelse(str_length(english_text) >= 8001 | str_length(english_text) <= 12000, substr(english_text, 8001, 12000), ""),
#          text4 = ifelse(str_length(english_text) >= 12001 | str_length(english_text) <= 16000, substr(english_text, 12001, 16000), ""),
#          text5 = ifelse(str_length(english_text) >= 16001 | str_length(english_text) <= 20000, substr(english_text, 16001, 20000), "")) %>%
#   gather(text1, text2, text3, text4, text5,
#          key = "split", value = "text") %>%
#   select(-english_text) %>%
#   mutate(split = as.numeric(str_remove(split, "text"))) %>%
#   group_by(source, date) %>%
#   mutate(split = sort(split, decreasing = FALSE)) %>%
#   ungroup()


for (i in 1:nrow(all_posts)) {

  try(
    POST(paste0("https://api.telegram.org/bot",
              bottoken,
              "/sendMessage?",
              "chat_id=@russian_milbloggers_english",
              "&text=", all_posts$bot_message[i],
              "&parse_mode=markdown"))
    )

  Sys.sleep(2)

}



# POST(paste0("https://api.telegram.org/bot",
#             bottoken,
#             "/sendMessage?",
#             "chat_id=@russian_milbloggers_english",
#             "&text=", all_posts$bot_message[11],
#             "&parse_mode=markdown"))




