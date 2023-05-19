
##### CHAT GPT TEMPLATE RUN #####

library(tidyverse)
library(stringr)
library(httr)

validation_sample <- read_rds("./Data/Validation_Samples/Validation_Sample_3.RDS")

validation_sample2 <- validation_sample[1:3,]

create_prompt <- function(validation_sample2){
  prompts <- purrr::map2(validation_sample2$message, validation_sample2$rowid,

                         ~list(

                           list(
                             "role" = "system",
                             "content" = stringr::str_c(

                               "On February 24th, 2022, Russia began a large-scale military invasion of Ukraine. 1,5 years into the war, a group of researchers wants to ",
                               "investigate different Russian warbloggers, that is pro-Russian anti-western individuals who post about the war on Telegram. Specifically, ",
                               "they would like to know how warbloggers’ posts on Telegram perceive and talk about Vladimir Putin, the President of Russia. ",
                               "The researchers collected a sample of all posts that mention Putin. ",
                               "You are a political scientist with in-depth knowledge about Russia and have been hired by this group of researchers ",
                               "to code the content of Telegram posts utilizing this knowledge.")
                           ),

                           list(
                             "role" = "user",
                             "content" = stringr::str_c(

                               "The text in the Telegram post reads: '", .x, "' \n\n Your task is to answer the following questions: ",

                               ## FOCUS ##

                               "Ukraine: Is the post negative or positive towards Ukraine? Pick one of the following categories: 1 (negative), 2 (neutral), 3 (positive), 0 (not applicable). ",

                               "West: Is the post negative or positive towards the West, the US, EU, NATO or European countries? Pick one of the following categories: 1 (negative), 2 (neutral), 3 (positive), 0 (not applicable). ",

                               "Putin: Just because Putin is mentioned in all posts, it does not mean that he is necessarily the main focus of the post. Is Putin the main focus of this post, or is something else the main focus? Pick one of the following categories: 1 (Putin), 2 (something else), 0 (unclear). ",

                               ## SUPPORT ##

                               "Support: Is the post supportive or opposed to Putin specifically? Pick one of the following categories: 1 (opposed), 2 (neither opposed nor supportive), 3 (supportive). ",

                               "Sentiment: Is the post positive or negative towards Putin specifically? Pick one of the following categories: 1 (negative), 2 (neutral), 3 (positive). ",

                               #"Praise: Does the post praise or critique Putin specifically? Pick one of the following categories: 1 (critique), 2 (neither critique nor praise), 3 (praise).",

                               ## TRUST ##

                               "Trust: Does the post express distrust or trust towards Putin as a leader? Pick one of the following categories: 1 (distrust), 2 (neither distrust nor trust), 3 (trust). ",

                               #"Approve: Does the post express skepticism or approval towards Putin specifically? Pick one of the following categories: 1 (skepticism), 2 (neither skepticism nor approval), 3 (approval). ",

                               "Favorable: Is the post hostile or favorable towards Putin specifically? Pick one of the following categories: 1 (hostile), 2 (indifferent), 3 (favorable). ",

                               "Competence: Does the post portray Putin specifically as competent or incompetent? Pick one of the following categories: 1 (incompetent), 2 (neither competent nor incompetent), 3 (competent). ",

                               ## WAR PROGRESS ##

                               "State_of_war: How does the post describe the current state of the war for Russia? Pick one of the following categories: 1 (bad), 2 (neutral), 3 (good) or 0 (not applicable). ",

                               "Responsibilty: Does the post explicitly assign responsibility for how the war is going to Putin? Pick one of the following categories: 1 (not responsible at all), 2 (somewhat responsible), 3 (fully responsible) or 0 (not applicable). ",

                               "Response: How does the post think Russia should continue the war? Pick one of the following categories: 1 (escalate the war), 2 (continue as it is), 3 (reduce the war effort) or 0 (not applicable). ",

                               ## TEMPLATE ##

                               "Use this template to answer the questions and justify your answers. Please follow the template exactly.",
                               "Include PostID number and separate answers using punctuation and line shifts (\n\n) in the template: ",

                               "PostID ", .y, " \n\n ",
                               ## Focus ##
                               "Ukraine: number from 0 to 3 | category | justification \n\n",
                               "West: number from 0 to 3 | category | justification \n\n",
                               "Putin: number from 0 to 2 | category | justification \n\n",
                               ## Support ##
                               "Support: number from 1 to 3 | category | justification \n\n",
                               "Sentiment: number from 1 to 3 | category | justification \n\n",
                               #"Praise: number from 1 to 3 | category | justification \n\n",
                               ## Trust ##
                               "Trust: number from 1 to 3 | category | justification \n\n",
                               #"Approve: number from 1 to 3 | category | justification \n\n",
                               "Favorable: number from 1 to 3 | category | justification \n\n",
                               "Competence: number from 1 to 3 | category | justification \n\n",
                               ## War progress ##
                               "State_of_war: number from 0 to 3 | category | justification \n\n",
                               "Responsibility: number from 0 to 3 | category | justification \n\n",
                               "Response: number from 0 to 3 | category | justification \n\n",
                               ## Other ##
                               "Other remarks: ")
                           )
                         )
  )
  prompts
}


prompts <- create_prompt(validation_sample2)
prompts

api_key <- read_lines("./Credentials/api_key_chatgpt")

# submit_prompt <- function(prompt, temperature = 0.0, n = 1) {
#   response <- POST(
#     url = "https://api.openai.com/v1/chat/completions",
#     add_headers(Authorization = paste("Bearer", api_key)),
#     content_type_json(),
#     encode = "json",
#     body = list(
#       model = "gpt-3.5-turbo",
#       temperature = temperature,
#       messages = prompt,
#       n = n))
#   Sys.sleep(10)
#   message(paste0("Finished post."))
#   str_trim(content(response)$choices[[1]]$message$content)
# }
#
# openai_completions <- prompts %>%
#   purrr::map(submit_prompt)

openai_completions <- list()
openai_prompt_tokens <- list()
openai_completion_tokens <- list()
openai_total_tokens <- list()

completion <- "start"
i <- 1

while(!identical(completion, character(0)) & i < nrow(validation_sample2)){

  for(i in 1:nrow(validation_sample2)){

    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", api_key)),
      content_type_json(),
      encode = "json",
      body = list(
        model = "gpt-3.5-turbo",
        temperature = 0.0,
        messages = prompts[[i]],
        n = 1))

    completion <<- str_trim(content(response)$choices[[1]]$message$content)

    openai_completions[[i]] <-  completion
    openai_prompt_tokens[[i]] <- str_trim(content(response)$usage$prompt_tokens)
    openai_completion_tokens[[i]] <- str_trim(content(response)$usage$completion_tokens)
    openai_total_tokens[[i]] <- str_trim(content(response)$usage$total_tokens)

    Sys.sleep(5)

    message(paste0("Finished post no. ", i))

  }
}

completions2 <- do.call(rbind, openai_completions)

tokenuse2 <- tibble(prompt_tokens = unlist(openai_prompt_tokens),
                   completion_tokens = unlist(openai_completion_tokens),
                   total_tokens = unlist(openai_total_tokens))


completions_df2 <- as_tibble(completions2, .name_repair = "universal") %>%
  rename(post = `...1`) %>%
  separate(post, into = c("rowid",
                          "ukraine", "west",
                          "putin",
                          "support", "sentiment", #"praise",
                          "trust", "favorable", "competence", #"approve",
                          "state_of_war", "responsibility", "response",
                          "other"), "\n\n") %>%
  mutate(rowid = str_remove_all(rowid, "PostID ")) %>%
  mutate(ukraine = str_remove(ukraine, "Ukraine: "),
         ukraine_category = str_remove_all(str_trim(str_remove_all(str_extract(ukraine, "\\|.*\\|"), "\\|")), "\\)|\\("),
         ukraine_justification = str_trim(str_remove_all(ukraine, "[0-9] \\|.*\\| ")),
         ukraine = str_extract(ukraine, "[0-9]")) %>%
  mutate(west = str_remove(west, "West: "),
         west_category = str_remove_all(str_trim(str_remove_all(str_extract(west, "\\|.*\\|"), "\\|")), "\\)|\\("),
         west_justification = str_trim(str_remove_all(west, "[0-9] \\|.*\\| ")),
         west = str_extract(west, "[0-9]")) %>%
  mutate(putin = str_remove(putin, "Putin: "),
         putin_category = str_remove_all(str_trim(str_remove_all(str_extract(putin, "\\|.*\\|"), "\\|")), "\\)|\\("),
         putin_justification = str_trim(str_remove_all(putin, "[0-9] \\|.*\\| ")),
         putin = str_extract(putin, "[0-9]")) %>%
  mutate(support = str_remove(support, "Support: "),
         support_category = str_remove_all(str_trim(str_remove_all(str_extract(support, "\\|.*\\|"), "\\|")), "\\)|\\("),
         support_justification = str_trim(str_remove_all(support, "[0-9] \\|.*\\| ")),
         support = str_extract(support, "[0-9]")) %>%
  mutate(sentiment = str_remove(sentiment, "Sentiment: "),
         sentiment_category = str_remove_all(str_trim(str_remove_all(str_extract(sentiment, "\\|.*\\|"), "\\|")), "\\)|\\("),
         sentiment_justification = str_trim(str_remove_all(sentiment, "[0-9] \\|.*\\| ")),
         sentiment = str_extract(sentiment, "[0-9]")) %>%
  # mutate(praise = str_remove(praise, "Praise: "),
  #        praise_category = str_remove_all(str_trim(str_remove_all(str_extract(praise, "\\|.*\\|"), "\\|")), "\\)|\\("),
  #        praise_justification = str_trim(str_remove_all(praise, "[0-9] \\|.*\\| ")),
  #        praise = str_extract(praise, "[0-9]")) %>%
  mutate(trust = str_remove(trust, "Trust: "),
         trust_category = str_remove_all(str_trim(str_remove_all(str_extract(trust, "\\|.*\\|"), "\\|")), "\\)|\\("),
         trust_justification = str_trim(str_remove_all(trust, "[0-9] \\|.*\\| ")),
         trust = str_extract(trust, "[0-9]")) %>%
  # mutate(approve = str_remove(approve, "Approve: "),
  #        approve_category = str_remove_all(str_trim(str_remove_all(str_extract(approve, "\\|.*\\|"), "\\|")), "\\)|\\("),
  #        approve_justification = str_trim(str_remove_all(approve, "[0-9] \\|.*\\| ")),
  #        approve = str_extract(approve, "[0-9]")) %>%
  mutate(favorable = str_remove(favorable, "Favorable: "),
         favorable_category = str_remove_all(str_trim(str_remove_all(str_extract(favorable, "\\|.*\\|"), "\\|")), "\\)|\\("),
         favorable_justification = str_trim(str_remove_all(favorable, "[0-9] \\|.*\\| ")),
         favorable = str_extract(favorable, "[0-9]")) %>%
  mutate(competence = str_remove(competence, "Competence: "),
         competence_category = str_remove_all(str_trim(str_remove_all(str_extract(competence, "\\|.*\\|"), "\\|")), "\\)|\\("),
         competence_justification = str_trim(str_remove_all(competence, "[0-9] \\|.*\\| ")),
         competence = str_extract(competence, "[0-9]")) %>%
  mutate(state_of_war = str_remove(state_of_war, "State_of_war: "),
         state_of_war_category = str_remove_all(str_trim(str_remove_all(str_extract(state_of_war, "\\|.*\\|"), "\\|")), "\\)|\\("),
         state_of_war_justification = str_trim(str_remove_all(state_of_war, "[0-9] \\|.*\\| ")),
         state_of_war = str_extract(state_of_war, "[0-9]")) %>%
  mutate(responsibility = str_remove(responsibility, "Responsibility: "),
         responsibility_category = str_remove_all(str_trim(str_remove_all(str_extract(responsibility, "\\|.*\\|"), "\\|")), "\\)|\\("),
         responsibility_justification = str_trim(str_remove_all(responsibility, "[0-9] \\|.*\\| ")),
         responsibility = str_extract(responsibility, "[0-9]")) %>%
  mutate(response = str_remove(response, "Response: "),
         response_category = str_remove_all(str_trim(str_remove_all(str_extract(response, "\\|.*\\|"), "\\|")), "\\)|\\("),
         response_justification = str_trim(str_remove_all(response, "[0-9] \\|.*\\| ")),
         response = str_extract(response, "[0-9]")) %>%
  mutate(other = str_remove_all(other, "Other remarks: ")) %>%
  dplyr::select(rowid,
                ukraine, ukraine_category, ukraine_justification,
                west, west_category, west_justification,
                putin, putin_category, putin_justification,
                support, support_category, support_justification,
                sentiment, sentiment_category, sentiment_justification,
                #praise, praise_category, praise_justification,
                trust, trust_category, trust_justification,
                #approve, approve_category, approve_justification,
                favorable, favorable_category, favorable_justification,
                competence, competence_category, competence_justification,
                state_of_war, state_of_war_category, state_of_war_justification,
                responsibility, responsibility_category, responsibility_justification,
                response, response_category, response_justification,
                other)

#saveRDS(completions_df, file = "./Data/Chat testruns/completions_df.rds")
#openxlsx::write.xlsx(completions_df, "./Data/Chat testruns/completions_df.xlsx")


#### MERGING WITH ENGLISH AND RUSSIAN POSTS ####

# telegrams_cleaned <- readRDS("./Data/telegrams_cleaned.rds")
#
# #### Create validation sample with the translated posts as well ####
#
# files <- list.files("./Data/Channel_Translated/", full.names = TRUE)
#
# telegrams_english <- lapply(files, read_rds)
#
# telegrams_english <- map(telegrams_english, .f = list(. %>% dplyr::select(source, date, id, message, detectedSourceLanguage, translatedText)))
#
# telegrams_english <- do.call(rbind, telegrams_english)
#
# telegrams_joined <- telegrams_english %>%
#   left_join(telegrams_cleaned, join_by(source, date, id, message)) %>%
#   mutate(message = ifelse(message == "", NA, message)) %>%
#   filter(str_detect(message, "(?i)путин[а-я]*|владимир\\s*владимирович\\s*путин|владимир\\s*путин")) %>%
#   drop_na(message)

# completions_df_full <- completions_df %>%
#   mutate(rowid = as.numeric(rowid)) %>%
#   left_join(telegrams_joined %>%
#               select(rowid, source, date, message, translatedText),
#             join_by(rowid))
#
# completions_df_en_final <- completions_df_en_full %>%
#   select(source, date, rowid,
#          message, translatedText,
#          opposition, opposition_justification,
#          competence, competence_justification,
#          responsibility, responsibility_justification,
#          state_of_the_war, state_of_the_war_justification,
#          response, response_justification,
#          other)

