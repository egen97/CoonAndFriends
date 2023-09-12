
##### CHAT GPT TEMPLATE RUN #####

library(tidyverse)
library(stringr)
library(httr)

# Remembering history? Comes with a lot of extra tokens, does not seem worth it.
# https://blog.devgenius.io/how-to-maintain-conversation-flow-in-with-chatgpts-api-in-r-part-17-of-r-for-applied-d010cca1326a

# Use assistant function to train some answers? Would also require a lot of more tokens.
# https://www.programmingelectronics.com/chatgpt-api/


set.seed(42)

telegrams <- readRDS("./Data/subsample_run.rds")


create_prompt <- function(telegrams_sample_month){
  prompts <- purrr::map2(telegrams_sample_month$message, telegrams_sample_month$rowid,

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

                               "Ukraine: Is the post negative or positive towards Ukraine? Pick one of the following categories: 1 (negative), 2 (neutral), 3 (positive), 0 (no statement about Ukraine). ",

                               "West: Is the post negative or positive towards the West, the US, EU, NATO or European countries? Pick one of the following categories: 1 (negative), 2 (neutral), 3 (positive), 0 (no statement about the West). ",

                               "Putin: Just because Putin is mentioned in all posts, it does not mean that he is necessarily the main focus of the post. Is Putin the main focus of this post, or is something else the main focus? Pick one of the following categories: 1 (Putin), 2 (something else), 0 (unclear). ",

                               ## SUPPORT ##

                               "Support: Is the post supportive or opposed to Putin specifically? Pick one of the following categories: 1 (opposed), 2 (neither opposed nor supportive), 3 (supportive). ",

                               "Sentiment: What is the general tone of the post? Pick one of the following categories: 1 (negative), 2 (neutral), 3 (positive). ",

                               ## TRUST ##

                               "Trust: Does the post express distrust or trust towards Putin specifically? Pick one of the following categories: 1 (distrust), 2 (neither distrust nor trust), 3 (trust). ",

                               "Competence: Does the post portray Putin specifically as competent or incompetent? Pick one of the following categories: 1 (incompetent), 2 (neither competent nor incompetent), 3 (competent). ",

                               ## WAR PROGRESS ##

                               "State_of_war: How does the post describe the current state of the war for Russia? Pick one of the following categories: 1 (bad), 2 (neutral), 3 (good), 0 (no description of the war). ",

                               "Responsibility: Does the post explicitly assign responsibility for how the war is going to Putin personally? Pick one of the following categories: 0 (Putin is not explicitly assigned responsibility), 1 (Putin is explicitly assigned responsibility).",

                               "Response: How does the post think Russia should continue the war? Pick one of the following categories: 1 (escalate the war), 2 (de-escalate the war), 0 (no statement). ",

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
                               ## Trust ##
                               "Trust: number from 1 to 3 | category | justification \n\n",
                               "Competence: number from 1 to 3 | category | justification \n\n",
                               ## War progress ##
                               "State_of_war: number from 0 to 3 | category | justification \n\n",
                               "Responsibility: number from 0 to 3 | category | justification \n\n",
                               "Response: number from 0 to 2 | category | justification \n\n",
                               ## Other ##
                               "Other remarks: ")
                           )
                         )
  )
  prompts
}


prompts <- create_prompt(telegrams_sample_month)
# prompts

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

for(i in 1:3){ #nrow(telegrams_month)){

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

    completion <- tibble(completion = openai_completions[[i]])

    tokenuse <- tibble(prompt_tokens = openai_prompt_tokens[[i]],
                       completion_tokens = openai_completion_tokens[[i]],
                       total_tokens = openai_total_tokens[[i]])

    write.table(completion, file = paste0("./Data/ChatGPT_output/completion_", i, ".txt"))
    write_csv(tokenuse, file = paste0("./Data/ChatGPT_output/tokenuse_", i, ".csv"))

    if(file.size(paste0("./Data/ChatGPT_output/completion_", i, ".txt")) == 0L | is.na(paste0("./Data/ChatGPT_output/completion_", i, ".txt"))){

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

      completion <- tibble(completion = openai_completions[[i]])

      tokenuse <- tibble(prompt_tokens = openai_prompt_tokens[[i]],
                         completion_tokens = openai_completion_tokens[[i]],
                         total_tokens = openai_total_tokens[[i]])

      write.table(completion, file = paste0("./Data/ChatGPT_output/completion_", i, ".txt"))
      write_csv(tokenuse, file = paste0("./Data/ChatGPT_output/tokenuse_", i, ".csv"))

    } else {

      message(paste0("Finished post no. ", i))

    }

  }

completions <- do.call(rbind, openai_completions)

tokenuse <- tibble(prompt_tokens = unlist(openai_prompt_tokens),
                   completion_tokens = unlist(openai_completion_tokens),
                   total_tokens = unlist(openai_total_tokens))


completions_df_all <- as_tibble(completions, .name_repair = "universal") %>%
  rename(post = `...1`) %>%
  separate(post, into = c("rowid",
                          "ukraine", "west",
                          "putin",
                          "support", "sentiment",
                          "trust", "competence",
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
  mutate(trust = str_remove(trust, "Trust: "),
         trust_category = str_remove_all(str_trim(str_remove_all(str_extract(trust, "\\|.*\\|"), "\\|")), "\\)|\\("),
         trust_justification = str_trim(str_remove_all(trust, "[0-9] \\|.*\\| ")),
         trust = str_extract(trust, "[0-9]")) %>%
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
                trust, trust_category, trust_justification,
                competence, competence_category, competence_justification,
                state_of_war, state_of_war_category, state_of_war_justification,
                responsibility, responsibility_category, responsibility_justification,
                response, response_category, response_justification,
                other)

saveRDS(completions_df_all, file = "./Data/completions_df_limited_sample_all.rds")
#openxlsx::write.xlsx(completions_df_all, "./Data/completions_df.xlsx")
saveRDS(tokenuse_all, file = "./Data/tokenuse_all.rds")

coded_posts <- left_join(coded_posts %>% mutate(rowid = as.numeric(rowid)), telegrams_month,
                         by = join_by(rowid))

saveRDS(coded_posts, file = "./Data/EPSA data/coded_posts.rds")
