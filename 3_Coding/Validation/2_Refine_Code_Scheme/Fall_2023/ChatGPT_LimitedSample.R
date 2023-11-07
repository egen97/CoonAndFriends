
library(tidyverse)
library(tidytext)
library(DBI)
library(RSQLite)

set.seed(42)

con <- dbConnect(RSQLite::SQLite(), "../iso-standards/iso-app/data/iso_standards.sqlite")
standards <- dbReadTable(con, "standards_status")
dbDisconnect(con)

abstracts <- standards %>%
  rename(text = abstract) %>%
  mutate(year = str_extract(publication_date, "[0-9]{4}")) %>%
  #filter(year != "2023") %>%
  select(stdno, text, year, ics_number) %>%
  mutate(text = str_replace_all(text, "[0-9]+?", "")) %>%
  filter(text != "") %>%
  na.omit() %>%
  unique()

create_prompt <- function(abstracts){
  prompts <- purrr::map2(abstracts$text, abstracts$stdno,

                         ~list(

                           list(
                             "role" = "system",
                             "content" = stringr::str_c(

                               "Imagine you are the owner of a large and well-established manufacturing company and you have decided to adopt an international standard.")

                             ),

                           list(
                             "role" = "user",
                             "content" = stringr::str_c(

                               "The abstract of the standard reads: '", .x, "'",

                               "What would be your reason for adopting this standard? ",

                               "Pick one of the following: (1) technical considerations, (2) social/ethical considerations, (3) a combination of both. ",

                               "Use this template to answer the questions and explain your reasoning. ",

                               "Include STDNO number and separate answers using punctuation (|) and line shifts (\n\n) as shown in the template: ",

                               "STDNO ", .y, " \n\n ",

                               "(number) | category | reasoning \n\n ",

                               "For example: ",

                               "(2) | social/ethical | Because the standard addresses environmental hazards. \n\n")

                           )
                         )
  )
  prompts
}


prompts <- create_prompt(abstracts)

api_key <- read_lines("../../OSINT Telegram/CoonAndFriends/Credentials/api_key_chatgpt")

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

output_folder <- "chatgpt_output"

for(i in 1:nrow(abstracts)){

  stdno <- abstract$stdno[i]

  destfile <- paste0(output_folder, "/completion_", stdno, ".txt")

  if(!file.exists(destfile)){

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

    write.table(completion, file = paste0(output_folder, "/completion_", stdno, ".txt"))
    write_csv(tokenuse, file = paste0(output_folder, "/tokenuse_", stdno, ".csv"))

  } else {

    message(paste0("File ", stdno, " already exists in folder."))

  }

    if(file.size(paste0(output_folder, "/completion_", stdno, ".txt")) <= 15L | is.na(paste0(output_folder, "/completion_", stdno, ".txt"))){

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

saveRDS(completions_df, file = paste0("./Data/Validation_Samples/Fall_2023/completions_chatgpt_", str_extract(output_folder, "[0-9]+"), ".rds"))
saveRDS(tokenuse, file = paste0("./Data/Validation_Samples/Fall_2023/tokenuse_chatgpt_", str_extract(output_folder, "[0-9]+"), ".rds"))

