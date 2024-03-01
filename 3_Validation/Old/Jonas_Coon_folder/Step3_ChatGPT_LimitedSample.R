
##### CHAT GPT TEMPLATE RUN #####

library(tidyverse)
library(stringr)
library(httr)

# Remembering history? Comes with a lot of extra tokens, does not seem worth it.
# https://blog.devgenius.io/how-to-maintain-conversation-flow-in-with-chatgpts-api-in-r-part-17-of-r-for-applied-d010cca1326a

# Use assistant function to train some answers? Would also require a lot of more tokens.
# https://www.programmingelectronics.com/chatgpt-api/


set.seed(42)

telegrams <- readRDS("./Data/Validation_Samples/Fall_2023/subsample_run.rds")

create_prompt <- function(telegrams){
  prompts <- purrr::map2(telegrams$message, telegrams$rowid,

                         ~list(

                           list(
                             "role" = "system",
                             "content" = stringr::str_c(

                               "On February 24th 2022, Russia initiated a large-scale military invasion of Ukraine. A group of researchers seeks to investigate how different Russian warbloggers, ",
                               "who post about the war on their Telegram channels, write about Vladimir Putin, the President of Russia. They gathered all posts that mention Putin. Imagine you are a ",
                               "political scientist with in-depth knowledge about Russia and have been hired to code the content of Telegram posts, utilizing your knowledge. \n",

                               "The warbloggers frequently refer to the war as a special operation. They hold anti-Western views, exhibit pro-Russian sentiments (though not unconditional ",
                               "support for the Russian government), and generally endorse the ongoing war. ",
                               "Answer based on explicit clues in the post. Use N/A if the question is not applicable to the post.")
                           ),

                           # list(
                           #   "role" = "assistant",
                           #   "content" = stringr::str_c(
                           #
                           #     "PostID 143565 \n\n
                           #
                           #     War_mention: 0 | (no) | The post talks about the progress of military personell in Ukraine. \n\n
                           #
                           #     Post_type: 1 | (news/factual description) | This post contains actual descriptions of the war against Ukraine. \n\n")
                           #
                           #   ),

                           list(
                             "role" = "user",
                             "content" = stringr::str_c(

                               "The text in the Telegram post reads: '",
                               .x,
                               "' \n\n Answer the following questions: ",

                               ## FOCUS ##

                               "War_mention: Does the post mention the war against Ukraine? 0 (no), 1 (yes). ",

                               "Putin_focus: All the post mention Putin, but not all posts are about Putin mainly. Is Putin the main focus of this post? 0 (no), 1 (yes), 2 (unclear). ",

                               "Post_type: What kind of post is this? 0 (news/factual description), 1 (opinion). ",

                               "Opinion_intensity: On a scale of 1-10, how opinionated is this post? ",

                               "Sentiment: What is the tone of the post? 1 (negative), 2 (neutral), 3 (positive). ",

                               ## SUPPORT ##

                               "Support_for_Putin: Is the post supportive or opposed to Putin personally? 1 (opposed), 2 (neither opposed nor supportive), 3 (supportive). ",

                               "Criticism_of_Putin Does the post explicitly criticize Putin personally? 0 (no), 1 (yes). ",

                               ## TRUST ##

                               "Trust_in_Putin: Does the post express distrust or trust in Putin as a leader? 1 (distrust), 2 (neither distrust nor trust), 3 (trust). ",

                               "Competence_of_Putin: Does the post portray Putin as a competent or incompetent leader? 1 (incompetent), 2 (neither competent nor incompetent), 3 (competent). ",

                               ## WAR PROGRESS ##

                               "State_of_war_for_Russia: How does the post describe the current state of the war for Russia? 1 (bad), 2 (neutral), 3 (good), 0 (no statement). ",

                               "Responsibility_for_the_war: Does the post explicitly assign responsibility for the war’s progress to Putin personally? 0 (no), 1 (yes). ",

                               "Course_of_action_for_Russia: How does the post suggest Russia should continue the war? 1 (escalate the war), 2 (de-escalate the war), 0 (no statement). ",

                               ## TEMPLATE ##

                               "Use this template to answer the questions and justify your answers. Please follow the template exactly. ",

                               "Include PostID number and separate answers using punctuation and line shifts (\n\n) in the template: ",

                               "PostID ", .y, " \n\n ",

                               "Variable: number | (category) | justification \n\n ",

                               "For example: ",

                               "War_mention: 0 | (no) | The post does not mention the war in Ukraine. \n\n")


                           )
                         )
  )
  prompts
}


prompts <- create_prompt(telegrams)
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

output_folder <- "ChatGPT_output_2"

for(i in 1:nrow(telegrams)){

  post_id <- telegrams$rowid[i]

  destfile <- paste0("./Data/Validation_Samples/Fall_2023/", output_folder, "/completion_", post_id, ".txt")

  if(!file.exists(destfile)){ # Do not download if file exists in folder already

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

    write.table(completion, file = paste0("./Data/Validation_Samples/Fall_2023/", output_folder, "/completion_", post_id, ".txt"))
    write_csv(tokenuse, file = paste0("./Data/Validation_Samples/Fall_2023/", output_folder, "/tokenuse_", post_id, ".csv"))

  } else {

    message(paste0("File ", post_id, " already exists in folder."))

  }

    if(file.size(paste0("./Data/Validation_Samples/Fall_2023/", output_folder, "/completion_", post_id, ".txt")) == 0L | is.na(paste0("./Data/Validation_Samples/Fall_2023/", output_folder, "/completion_", post_id, ".txt"))){

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

      write.table(completion, file = paste0("./Data/Validation_Samples/Fall_2023/", output_folder, "/completion_", post_id, ".txt"))
      write_csv(tokenuse, file = paste0("./Data/Validation_Samples/Fall_2023/", output_folder, "/tokenuse_", post_id, ".csv"))

    } else {

      message(paste0("Finished post no. ", i))

    }

}

completions <- lapply(list.files(paste0("./Data/Validation_Samples/Fall_2023/", output_folder), full.names = TRUE, pattern = ".txt"), read.table) %>%
  bind_rows()

tokenuse <- lapply(list.files(paste0("./Data/Validation_Samples/Fall_2023/", output_folder), full.names = TRUE, pattern = ".csv"), read.csv) %>%
  bind_rows()

(sum(tokenuse$prompt_tokens)/1000)*0.0015 + (sum(tokenuse$completion_tokens)/1000)*0.002
# 0.2087965 dollar $$ spent


completions_df <- as_tibble(completions, .name_repair = "universal") %>%
  separate(completion, into = c("rowid", "War_mention", "Putin_focus", "Post_type", "Opinion_intensity", "Sentiment", "Support_for_Putin", "Criticism_of_Putin", "Trust_in_Putin", "Competence_of_Putin",
                          "State_of_war_for_Russia", "Responsibility_for_the_war", "Course_of_action_for_Russia"), sep = "\n(\n)?") %>%
  mutate(rowid = str_remove_all(rowid, "PostID ")) %>%
  mutate(War_mention = str_remove(War_mention, "War_mention: "),
         War_mention_category = str_remove_all(str_trim(str_remove_all(str_extract(War_mention, "\\|.*\\|"), "\\|")), "\\)|\\("),
         War_mention_justification = str_trim(str_remove_all(War_mention, "([0-9])?(N/A)?(\\s+)?\\|.*\\| ")),
         War_mention = str_extract(War_mention, "[0-9]|N/A")) %>%
  mutate(Putin_focus = str_remove(Putin_focus, "Putin_focus: "),
         Putin_focus_category = str_remove_all(str_trim(str_remove_all(str_extract(Putin_focus, "\\|.*\\|"), "\\|")), "\\)|\\("),
         Putin_focus_justification = str_trim(str_remove_all(Putin_focus, "([0-9])?(N/A)?(\\s+)?\\|.*\\| ")),
         Putin_focus = str_extract(Putin_focus, "[0-9]|N/A")) %>%
  mutate(Post_type = str_remove(Post_type, "Post_type: "),
         Post_type_category = str_remove_all(str_trim(str_remove_all(str_extract(Post_type, "\\|.*\\|"), "\\|")), "\\)|\\("),
         Post_type_justification = str_trim(str_remove_all(Post_type, "([0-9])?(N/A)?(\\s+)?\\|.*\\|")),
         Post_type = str_extract(Post_type, "[0-9]|N/A")) %>%
  mutate(Opinion_intensity = str_remove(Opinion_intensity, "Opinion_intensity: "),
         Opinion_intensity_category = str_remove_all(str_trim(str_remove_all(str_extract(Opinion_intensity, "\\|.*\\|"), "\\|")), "\\)|\\("),
         Opinion_intensity_justification = str_trim(str_remove_all(Opinion_intensity, "([0-9])?(N/A)?(\\s+)?\\|.*\\| ")),
         Opinion_intensity = str_extract(Opinion_intensity, "[0-9]|N/A")) %>%
  mutate(Sentiment = str_remove(Sentiment, "Sentiment: "),
         Sentiment_category = str_remove_all(str_trim(str_remove_all(str_extract(Sentiment, "\\|.*\\|"), "\\|")), "\\)|\\("),
         Sentiment_justification = str_trim(str_remove_all(Sentiment, "([0-9])?(N/A)?(\\s+)?\\|.*\\| ")),
         Sentiment = str_extract(Sentiment, "[0-9]|N/A")) %>%
  mutate(Support_for_Putin = str_remove(Support_for_Putin, "Support_for_Putin: "),
         Support_for_Putin_category = str_remove_all(str_trim(str_remove_all(str_extract(Support_for_Putin, "\\|.*\\|"), "\\|")), "\\)|\\("),
         Support_for_Putin_justification = str_trim(str_remove_all(Support_for_Putin, "([0-9])?(N/A)?(\\s+)?\\|.*\\| ")),
         Support_for_Putin = str_extract(Support_for_Putin, "[0-9]|N/A")) %>%
  mutate(Criticism_of_Putin = str_remove(Criticism_of_Putin, "Criticism_of_Putin: "),
         Criticism_of_Putin_category = str_remove_all(str_trim(str_remove_all(str_extract(Criticism_of_Putin, "\\|.*\\|"), "\\|")), "\\)|\\("),
         Criticism_of_Putin_justification = str_trim(str_remove_all(Criticism_of_Putin, "([0-9])?(N/A)?(\\s+)?\\|.*\\| ")),
         Criticism_of_Putin = str_extract(Criticism_of_Putin, "[0-9]|N/A")) %>%
  mutate(Trust_in_Putin = str_remove(Trust_in_Putin, "Trust_in_Putin: "),
         Trust_in_Putin_category = str_remove_all(str_trim(str_remove_all(str_extract(Trust_in_Putin, "\\|.*\\|"), "\\|")), "\\)|\\("),
         Trust_in_Putin_justification = str_trim(str_remove_all(Trust_in_Putin, "([0-9])?(N/A)?(\\s+)?\\|.*\\| ")),
         Trust_in_Putin = str_extract(Trust_in_Putin, "[0-9]|N/A")) %>%
  mutate(Competence_of_Putin = str_remove(Competence_of_Putin, "Competence_of_Putin: "),
         Competence_of_Putin_category = str_remove_all(str_trim(str_remove_all(str_extract(Competence_of_Putin, "\\|.*\\|"), "\\|")), "\\)|\\("),
         Competence_of_Putin_justification = str_trim(str_remove_all(Competence_of_Putin, "([0-9])?(N/A)?(\\s+)?\\|.*\\| ")),
         Competence_of_Putin = str_extract(Competence_of_Putin, "[0-9]|N/A")) %>%
  mutate(State_of_war_for_Russia = str_remove(State_of_war_for_Russia, "State_of_war_for_Russia: "),
         State_of_war_for_Russia_category = str_remove_all(str_trim(str_remove_all(str_extract(State_of_war_for_Russia, "\\|.*\\|"), "\\|")), "\\)|\\("),
         State_of_war_for_Russia_justification = str_trim(str_remove_all(State_of_war_for_Russia, "([0-9])?(N/A)?(\\s+)?\\|.*\\| ")),
         State_of_war_for_Russia = str_extract(State_of_war_for_Russia, "[0-9]|N/A")) %>%
  mutate(Responsibility_for_the_war = str_remove(Responsibility_for_the_war, "Responsibility_for_the_war: "),
         Responsibility_for_the_war_category = str_remove_all(str_trim(str_remove_all(str_extract(Responsibility_for_the_war, "\\|.*\\|"), "\\|")), "\\)|\\("),
         Responsibility_for_the_war_justification = str_trim(str_remove_all(Responsibility_for_the_war, "([0-9])?(N/A)?(\\s+)?\\|.*\\| ")),
         Responsibility_for_the_war = str_extract(Responsibility_for_the_war, "[0-9]|N/A")) %>%
  mutate(Course_of_action_for_Russia = str_remove(Course_of_action_for_Russia, "Course_of_action_for_Russia: "),
         Course_of_action_for_Russia_category = str_remove_all(str_trim(str_remove_all(str_extract(Course_of_action_for_Russia, "\\|.*\\|"), "\\|")), "\\)|\\("),
         Course_of_action_for_Russia_justification = str_trim(str_remove_all(Course_of_action_for_Russia, "([0-9])?(N/A)?(\\s+)?\\|.*\\| ")),
         Course_of_action_for_Russia = str_extract(Course_of_action_for_Russia, "[0-9]|N/A")) %>%
  dplyr::select(rowid,
                War_mention, War_mention_category, War_mention_justification,
                Putin_focus, Putin_focus_category, Putin_focus_justification,
                Post_type, Post_type_category, Post_type_justification,
                Opinion_intensity, Opinion_intensity_category, Opinion_intensity_justification,
                Sentiment, Sentiment_category, Sentiment_justification,
                Support_for_Putin, Support_for_Putin_category, Support_for_Putin_justification,
                Criticism_of_Putin, Criticism_of_Putin_category, Criticism_of_Putin_justification,
                Trust_in_Putin, Trust_in_Putin_category, Trust_in_Putin_justification,
                Competence_of_Putin, Competence_of_Putin_category, Competence_of_Putin_justification,
                State_of_war_for_Russia, State_of_war_for_Russia_category, State_of_war_for_Russia_justification,
                Responsibility_for_the_war, Responsibility_for_the_war_category, Responsibility_for_the_war_justification,
                Course_of_action_for_Russia, Course_of_action_for_Russia_category, Course_of_action_for_Russia_justification)

saveRDS(completions_df, file = "./Data/Validation_Samples/Fall_2023/completions_chatgpt_2.rds")
saveRDS(tokenuse, file = "./Data/Validation_Samples/Fall_2023/tokenuse_chatgpt_2.rds")

