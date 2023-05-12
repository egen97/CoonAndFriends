
library(tidyverse)
library(stringr)
library(httr)

telegrams_cleaned <- readRDS("./Data/telegrams_cleaned.rds")

#### Create validation sample with the translated posts as well ####

files <- list.files("./Data/Channel_Translated/", full.names = TRUE)

telegrams_english <- lapply(files, read_rds)

telegrams_english <- map(telegrams_english, .f = list(. %>% dplyr::select(source, date, id, message, detectedSourceLanguage, translatedText)))

telegrams_english <- do.call(rbind, telegrams_english)

telegrams_joined <- telegrams_english %>%
  left_join(telegrams_cleaned, join_by(source, date, id, message)) %>%
  mutate(message = ifelse(message == "", NA, message)) %>%
  filter(str_detect(message, "(?i)путин[а-я]*|владимир\\s*владимирович\\s*путин|владимир\\s*путин")) %>%
  drop_na(message)


#### How many times is "Putin" mentioned in the overall data? ####

telegrams_cleaned %>%
  mutate(message = ifelse(message == "", NA, message)) %>%
  filter(str_detect(message, "(?i)путин[а-я]*|владимир\\s*владимирович\\s*путин|владимир\\s*путин")) %>%
  drop_na(message) %>%
  nrow()

# 30618


##### CHAT #####

# validation_sample_vars <- sample_n(telegrams_joined, 30)
#
# validation_sample <- validation_sample_vars %>%
#  select(rowid, source, date, id, message, translatedText)
#
# saveRDS(validation_sample, file = "../Data/Validation_Sample.RDS")


validation_sample <- read_rds("./Data/Validation_Sample.RDS")


create_prompt <- function(validation_sample){
  prompts <- purrr::map2(validation_sample$translatedText, validation_sample$rowid,

                         ~list(

                           list(
                             "role" = "user",
                             "content" = stringr::str_c(

                               "On February 24th 2022 Russia began a large-scale invasion of Ukraine.",
                               "1,5 years into the war, a group of researchers wants to investigate how different Russian warbloggers",
                               "that is individuals who are generally aligned with the Russian side in the war and post about the war on Telegram",
                               "talk about the war. Specifically, they are interested in analysing warbloggers’s posts on Telegram with respect",
                               "to how the bloggers perceive and talk about Wladimir Putin, the President of Russia.",
                               "You are a political scientist with in-depth knowledge about Russia, and have been hired by this group",
                               "of researchers to code the content of Telegram posts.",

                             "The text in the Telegram post reads: '", .x, "' \n\n Your task is to answer the following questions: ",

                             "Opposition: Is the post opposed or supportive of Putin? Pick one of the following categories: ",
                             "1 (strongly opposed), 2 (somewhat opposed), 3 (neither opposed nor supportive), 4 (somewhat supportive) or 5 (strongly supportive). ",

                             "Competence: Does the post portray Putin as incompetent or competent? Pick one of the following categories: ",
                             "1 (highly incompetent), 2 (somewhat incompetent), 3 (neither competent nor incompetent), 4 (somewhat competent) or 5 (highly competent). ",

                             "Responsibility: Does the post assign responsibility for how the war is going to Putin? Pick one of the following categories: ",
                             "1 (not responsible at all), 2 (somewhat responsible), 3 (fully responsible) or 0 (not applicable). ",

                             "State of the war: How does the post describe the current state of the war for Russia? Pick one of the following categories: ",
                             "1 (bad), 2 (neutral), 3 (good) or 0 (not applicable). ",

                             "Response: How does the post want Putin to respond to the current state of the war? Pick one of the following categories: ",
                             "1 (escalate the war), 2 (continue as it is), 3 (negotiate peace) or 0 (not applicable). ",

                             "Use this template to answer the questions and justify your answers. Please follow the template exactly.",
                             "Include PostID number and separate answers using punctuation and line shifts (\n\n) in the template: ",
                             "PostID ", .y, " \n\n ",
                             "Opposition: number from 1 to 5, justification \n\n",
                             "Competence: number from 1 to 5, justification \n\n",
                             "Responsibility: number from 0 to 3, justification \n\n",
                             "State of the war: number from 0 to 3, justification \n\n",
                             "Response: number from 0 to 3, justification \n\n",
                             "Other remarks: ")
                             )
                         )
  )
  prompts
}

#validation_sample2 <- validation_sample[18,]

prompts <- create_prompt(validation_sample)
prompts

api_key <- read_lines("./Credentials/api_key_chatgpt")

submit_prompt <- function(prompt, temperature = 0.0, n = 1) {
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      temperature = temperature,
      messages = prompt,
      n = n))
  Sys.sleep(10)
  message("Finished post.")
  str_trim(content(response)$choices[[1]]$message$content)
}

openai_completions_en <- prompts %>%
  purrr::map(submit_prompt) # It didn't catch post number 445399, so run that separately.

openai_completions_en

completions_en <- do.call(rbind, openai_completions_en)

completions_df_en <- as_tibble(completions_en, .name_repair = "universal") %>%
  rename(post = `...1`) %>%
  separate(post, into = c("rowid", "opposition", "competence", "responsibility", "state_of_the_war", "response", "other"), "\n\n") %>%
  mutate(rowid = str_remove_all(rowid, "PostID ")) %>%
  mutate(opposition = str_remove(opposition, "Opposition: "),
         opposition_justification = str_remove_all(opposition, "[0-9], "),
         opposition = str_extract(opposition, "[0-9]")) %>%
  mutate(competence = str_remove(competence, "Competence: "),
         competence_justification = str_remove_all(competence, "[0-9], "),
         competence = str_extract(competence, "[0-9]")) %>%
  mutate(responsibility = str_remove(responsibility, "Responsibility: "),
         responsibility_justification = str_remove_all(responsibility, "[0-9], "),
         responsibility = str_extract(responsibility, "[0-9]")) %>%
  mutate(state_of_the_war = str_remove(state_of_the_war, "State of the war: "),
         state_of_the_war_justification = str_remove_all(state_of_the_war, "[0-9], "),
         state_of_the_war = str_extract(state_of_the_war, "[0-9]")) %>%
  mutate(response = str_remove(response, "Response: "),
         response_justification = str_remove_all(response, "[0-9], "),
         response = str_extract(response, "[0-9]")) %>%
  mutate(other = str_remove_all(other, "Other remarks: ")) %>%
  select(rowid,
         opposition, opposition_justification,
         competence, competence_justification,
         responsibility, responsibility_justification,
         state_of_the_war, state_of_the_war_justification,
         response, response_justification,
         other)

completions_df_en_full <- completions_df_en %>%
  mutate(rowid = as.numeric(rowid)) %>%
  left_join(telegrams_joined %>%
              select(rowid, source, date, message, translatedText),
            join_by(rowid))

completions_df_en_final <- completions_df_en_full %>%
  select(source, date, rowid,
         message, translatedText,
         opposition, opposition_justification,
         competence, competence_justification,
         responsibility, responsibility_justification,
         state_of_the_war, state_of_the_war_justification,
         response, response_justification,
         other)

# saveRDS(completions_df_en_final, file = "./Data/Chat testruns/chat_testrun_english.rds")
# openxlsx::write.xlsx(completions_df_en_final, "./Data/Chat testruns/chat_testrun_english.xlsx")
