
library(tidyverse)

# Path in WSL to avoid encoding issues: /home/sunny369/osint/OSINT Telegram/telegrams/telegram_rds_filtered

# https://platform.openai.com/docs/guides/embeddings/limitations-risks - classification?

# https://openai.com/pricing#language-models - prrrricing

# chat for R https://irudnyts.github.io/openai/index.html


ru_files <- list.files("../../telegrams/telegram_rds_filtered/", full.names = T)
eng_files <- list.files("../../telegrams/telegram_translated/", full.names = T)

telegram_ru <- lapply(ru_files, read_rds)
telegram_eng <- lapply(eng_files, read_rds)


russian_df <- tibble(id = telegram_ru[[1]]$id,
                     date = telegram_ru[[1]]$date,
                     text = telegram_ru[[1]]$message)


english_df <- tibble(id = telegram_eng[[1]]$id,
                     date = telegram_eng[[1]]$date,
                     text = telegram_eng[[1]]$translatedText)


# https://www.r-bloggers.com/2023/03/call-chatgpt-or-really-any-other-api-from-r/

# How to call the new (as of 2023-03-01) ChatGPT API from R
# Get your API key over here: https://platform.openai.com/

api_key <- read_lines("../../Credentials/api_key_chatgpt")

library(httr)
library(stringr)

# Calls the ChatGPT API with the given prompt and returns the answer
ask_chatgpt <- function(prompt) {
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions", ## ADD TEMPERATURE
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      messages = list(list(
        role = "user",
        content = prompt
      ))
    )
  )
  str_trim(content(response)$choices[[1]]$message$content)
}


#answer <- ask_chatgpt("Can you help me find the sentiment of some Telegram posts in Russian?")
cat(answer)

# Yes, as an AI language model, I can help you analyze the sentiment of Telegram posts in Russian.
# However, I will need access to the text of the posts you want me to analyze.
# Please provide me with the text, and I will do my best to determine the sentiment.


#answer2 <- ask_chatgpt(paste0("What is the sentiment of this Telegram post: ' ", russian_df$text[1], "' ?"))
cat(answer2)

# The sentiment of this Telegram post is neutral, as it simply reports a military event without expressing any positive or negative emotions towards it.

russian_df$text[1]
english_df$text[1]


#answer3 <- ask_chatgpt(paste0("What is the sentiment of this Telegram post: ' ", russian_df$text[2], "' ?"))
cat(answer3)

# Negative sentiment.

russian_df$text[2]
english_df$text[2]


#answer4 <- ask_chatgpt(paste0("What is the sentiment of this Telegram post: ' ", russian_df$text[4], "' ?"))
cat(answer4)

# The sentiment of this Telegram post is negative or critical.



#answer5 <- ask_chatgpt(paste0("From a Russian point of view, what is the sentiment of this Telegram post: ' ", russian_df$text[4], "' ?"))
cat(answer5)

# From a Russian point of view, the sentiment of this Telegram post is likely to be neutral or slightly positive.
# The post reports on the destruction of Ukrainian energy infrastructure, which could be seen as a positive development for Russia in the context of tensions between the two countries.
# The use of the Russian flag emojis also suggests a sense of national pride or support for actions taken by Russian forces in Ukraine.
# However, the language used in the post itself is relatively neutral and factual, without any overt expressions of joy or celebration.

russian_df$text[4]
english_df$text[4]


#### COMMENTS ####

## DV1: How likely is it that Putin is mentioned?
# Russian president / our president / Putin / maybe ask marina
# Probability that Putin is mentioned

## DV2:
# Filter out posts where Putin is mentioned
# Anchor the API - "imagine that..." , tell it where the data is from, say that it's a war, say that Putin is mentioned,
# something like "how is putin portrayed?" - make a scale / dimensions
# Supportive, satisfied, think is competent? 5-scale (somewhat, neutral, etc.)
# Something about a Russian point of view (perspective)

## Other:
# Subjective ideas about how the war is going

