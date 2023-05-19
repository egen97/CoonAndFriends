
##### CHAT GPT TEMPLATE RUN #####

library(tidyverse)
library(stringr)
library(httr)

validation_sample <- read_rds("./Data/Validation_Samples/Validation_Sample_200.RDS")

#### Human coders ####
validation_sample_humans <- validation_sample %>%
  select(rowid, translatedText) %>%
  mutate(putin = "",
         support = "",
         trust = "",
         state_of_war = "")

openxlsx::write.xlsx(validation_sample_humans, file = "./3_Validation/3_Intercoder_Reliability/Validation_Sample_200_Posts.xlsx")


#### AI coder ####

create_prompt <- function(validation_sample){
  prompts <- purrr::map2(validation_sample$message, validation_sample$rowid,

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

                               ## TRUST ##

                               "Trust: Does the post express distrust or trust towards Putin as a leader? Pick one of the following categories: 1 (distrust), 2 (neither distrust nor trust), 3 (trust). ",

                               ## WAR PROGRESS ##

                               "State_of_war: How does the post describe the current state of the war for Russia? Pick one of the following categories: 1 (bad), 2 (neutral), 3 (good) or 0 (not applicable). ",

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
                               ## Trust ##
                               "Trust: number from 1 to 3 | category | justification \n\n",
                               ## War progress ##
                               "State_of_war: number from 0 to 3 | category | justification \n\n",
                               ## Other ##
                               "Other remarks: ")
                           )
                         )
  )
  prompts
}


prompts <- create_prompt(validation_sample)

api_key <- read_lines("./Credentials/api_key_chatgpt")


openai_completions <- list()
openai_prompt_tokens <- list()
openai_completion_tokens <- list()
openai_total_tokens <- list()

completion <- "start"
i <- 1

while(!identical(completion, character(0)) & i < nrow(validation_sample)){

  for(i in 1:nrow(validation_sample)){

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

completions <- do.call(rbind, openai_completions)

tokenuse <- tibble(prompt_tokens = unlist(openai_prompt_tokens),
                    completion_tokens = unlist(openai_completion_tokens),
                    total_tokens = unlist(openai_total_tokens))


completions_df <- as_tibble(completions, .name_repair = "universal") %>%
  rename(post = `...1`) %>%
  separate(post, into = c("rowid",
                          "ukraine", "west",
                          "putin",
                          "support",
                          "trust",
                          "state_of_war",
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
  mutate(trust = str_remove(trust, "Trust: "),
         trust_category = str_remove_all(str_trim(str_remove_all(str_extract(trust, "\\|.*\\|"), "\\|")), "\\)|\\("),
         trust_justification = str_trim(str_remove_all(trust, "[0-9] \\|.*\\| ")),
         trust = str_extract(trust, "[0-9]")) %>%
  mutate(state_of_war = str_remove(state_of_war, "State_of_war: "),
         state_of_war_category = str_remove_all(str_trim(str_remove_all(str_extract(state_of_war, "\\|.*\\|"), "\\|")), "\\)|\\("),
         state_of_war_justification = str_trim(str_remove_all(state_of_war, "[0-9] \\|.*\\| ")),
         state_of_war = str_extract(state_of_war, "[0-9]")) %>%
  mutate(other = str_remove_all(other, "Other remarks: ")) %>%
  dplyr::select(rowid,
                ukraine, ukraine_category, ukraine_justification,
                west, west_category, west_justification,
                putin, putin_category, putin_justification,
                support, support_category, support_justification,
                trust, trust_category, trust_justification,
                state_of_war, state_of_war_category, state_of_war_justification,
                other)

#### SAVE ####

saveRDS(tokenuse, file = "./3_Validation/3_Intercoder_Reliability/tokenuse.rds")
saveRDS(completions_df, file = "./3_Validation/3_Intercoder_Reliability/completions_df.rds")
openxlsx::write.xlsx(completions_df, file = "./3_Validation/3_Intercoder_Reliability/ChatGPT_codings.xlsx")
