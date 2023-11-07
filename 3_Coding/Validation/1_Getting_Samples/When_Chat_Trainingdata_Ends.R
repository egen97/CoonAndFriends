
library(httr)
library(stringr)
library(dplyr)


#### Checking when training data for ChatGPT starts ####

api_key <- read_lines("./Credentials/api_key_chatgpt")

# Calls the ChatGPT API with the given prompt and returns the answer
ask_chatgpt <- function(prompt) {
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      temperature = 0.0,
      messages = list(list(
        role = "user",
        content = prompt
      ))
    )
  )
  str_trim(content(response)$choices[[1]]$message$content)
}

q1 <- ask_chatgpt("Hi. Do you have access to the internet?")
cat(q1)
# As an AI language model, I do not have direct access to the internet. However, I can provide answers based on the information and knowledge stored in my database.

q2 <- ask_chatgpt("When does your training data end? What is the latest date on the information you have?")
cat(q2)
# As an AI language model, I am fed with a vast amount of information from various sources, and it is challenging to pinpoint a specific end date for my training data.
# However, my training data is continuously updated and expanded to provide the latest and most relevant information to users.
# Since my training data is continually evolving, I do not have a specific latest date for the information I have.

q3 <- ask_chatgpt("Has there been a large scale invasion of Ukraine by Russia?")
cat(q3)
# As an AI language model, my training data only goes up to October 2021. At that time, the conflict in Ukraine between Russian-backed separatists and
# Ukrainian government forces was ongoing, but there had not been a large scale invasion of Ukraine by Russia.
# However, there have been reports of Russian troop buildup near the Ukrainian border. The situation remains tense, and the
# possibility of a large scale invasion cannot be ruled out entirely.
