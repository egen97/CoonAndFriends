
##### CHAT GPT TEMPLATE RUN #####

library(tidyverse)
library(stringr)
library(httr)

# Remembering history? Comes with a lot of extra tokens, does not seem worth it.
# https://blog.devgenius.io/how-to-maintain-conversation-flow-in-with-chatgpts-api-in-r-part-17-of-r-for-applied-d010cca1326a

# Use assistant function to train some answers? Would also require a lot of more tokens.
# https://www.programmingelectronics.com/chatgpt-api/

set.seed(42)

telegrams <- readRDS("./Data/Validation_Samples/Winter_2023/Subsample_Fall2023_2.rds") %>%
  mutate(message = str_remove(message, "_"))

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

                               "Use this template to answer the questions. Follow the template exactly. ",

                               "Include PostID number and separate answers using punctuation and line shifts (\n\n) in the template: ",

                               "PostID ", .y, " \n\n ",

                               "Variable: number | (category) | \n\n ",

                               "For example: ",

                               "War_mention: 0 | (no) | \n\n")


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

  destfile <- paste0("./Data/Validation_Samples/Winter_2023/", output_folder, "/completion_", post_id, ".txt")

  if(!file.exists(destfile)){ # Do not download if file exists in folder already

      response <- RETRY(
        "POST",
        times = 3,
        url = "https://api.openai.com/v1/chat/completions",
        add_headers(Authorization = paste("Bearer", api_key)),
        content_type_json(),
        encode = "json",
        #timeout(10),
        body = list(
          model = "gpt-3.5-turbo",
          temperature = 0.0,
          messages = prompts[[i]],
          n = 1)
      )

    completion <<- str_trim(content(response)$choices[[1]]$message$content)

    openai_completions[[i]] <-  completion
    openai_prompt_tokens[[i]] <- str_trim(content(response)$usage$prompt_tokens)
    openai_completion_tokens[[i]] <- str_trim(content(response)$usage$completion_tokens)
    openai_total_tokens[[i]] <- str_trim(content(response)$usage$total_tokens)

    Sys.sleep(1.5)

    completion <- tibble(completion = openai_completions[[i]])

    tokenuse <- tibble(prompt_tokens = openai_prompt_tokens[[i]],
                       completion_tokens = openai_completion_tokens[[i]],
                       total_tokens = openai_total_tokens[[i]])

    write.table(completion, file = paste0("./Data/Validation_Samples/Winter_2023/", output_folder, "/completion_", post_id, ".txt"))
    write_csv(tokenuse, file = paste0("./Data/Validation_Samples/Winter_2023/", output_folder, "/tokenuse_", post_id, ".csv"))

    gc()

  } else {

    message(paste0("File ", post_id, " already exists in folder."))

  }

    if(file.size(paste0("./Data/Validation_Samples/Winter_2023/", output_folder, "/completion_", post_id, ".txt")) < 15L | is.na(paste0("./Data/Validation_Samples/Winter_2023/", output_folder, "/completion_", post_id, ".txt"))){

      response <- RETRY(
        "POST",
        times = 3,
          url = "https://api.openai.com/v1/chat/completions",
          add_headers(Authorization = paste("Bearer", api_key)),
          content_type_json(),
          #progress(),
          encode = "json",
          #timeout(10),
          body = list(
            model = "gpt-3.5-turbo",
            temperature = 0.0,
            messages = prompts[[i]],
            n = 1)
        )

      completion <<- str_trim(content(response)$choices[[1]]$message$content)

      openai_completions[[i]] <-  completion
      openai_prompt_tokens[[i]] <- str_trim(content(response)$usage$prompt_tokens)
      openai_completion_tokens[[i]] <- str_trim(content(response)$usage$completion_tokens)
      openai_total_tokens[[i]] <- str_trim(content(response)$usage$total_tokens)

      Sys.sleep(1.5)

      completion <- tibble(completion = openai_completions[[i]])

      tokenuse <- tibble(prompt_tokens = openai_prompt_tokens[[i]],
                         completion_tokens = openai_completion_tokens[[i]],
                         total_tokens = openai_total_tokens[[i]])

      write.table(completion, file = paste0("./Data/Validation_Samples/Winter_2023/", output_folder, "/completion_", post_id, ".txt"))
      write_csv(tokenuse, file = paste0("./Data/Validation_Samples/Winter_2023/", output_folder, "/tokenuse_", post_id, ".csv"))

      gc()

    } else {

      message(paste0("Finished post no. ", i))

    }

}
# output_folder <- "ChatGPT_output_2"

completions <- lapply(list.files(paste0("./Data/Validation_Samples/Winter_2023/", output_folder), full.names = TRUE, pattern = ".txt"), read.table) %>%
  bind_rows()

tokenuse <- lapply(list.files(paste0("./Data/Validation_Samples/Winter_2023/", output_folder), full.names = TRUE, pattern = ".csv"), read.csv) %>%
  bind_rows()

postids <- str_remove(str_extract(list.files(paste0("./Data/Validation_Samples/Winter_2023/", output_folder), full.names = TRUE, pattern = ".txt"), "[0-9]+\\.txt"), ".txt")

completions_df <- as_tibble(completions, .name_repair = "universal") %>%
  separate(completion, into = c("rowid",
                                "War_mention", "Putin_focus", "Post_type", "Opinion_intensity", "Sentiment",
                                "Support_for_Putin", "Criticism_of_Putin", "Trust_in_Putin", "Competence_of_Putin",
                                "State_of_war_for_Russia", "Responsibility_for_the_war", "Course_of_action_for_Russia"), sep = "\n(\n)?") %>%
  mutate(rowid = as.numeric(str_remove_all(rowid, "PostID(:)? "))) %>%
  mutate(postid = as.numeric(postids)) %>%
  mutate(War_mention = str_extract(War_mention, "[0-9]|N/A")) %>%
  mutate(Putin_focus = str_extract(Putin_focus, "[0-9]|N/A")) %>%
  mutate(Post_type = str_extract(Post_type, "[0-9]+|N/A")) %>%
  mutate(Opinion_intensity = str_extract(Opinion_intensity, "[0-9]+|N/A")) %>%
  mutate(Sentiment = str_extract(Sentiment, "[0-9]+|N/A")) %>%
  mutate(Support_for_Putin = str_extract(Support_for_Putin, "[0-9]+|N/A")) %>%
  mutate(Criticism_of_Putin = str_extract(Criticism_of_Putin, "[0-9]+|N/A")) %>%
  mutate(Trust_in_Putin = str_extract(Trust_in_Putin, "[0-9]+|N/A")) %>%
  mutate(Competence_of_Putin = str_extract(Competence_of_Putin, "[0-9]+|N/A")) %>%
  mutate(State_of_war_for_Russia = str_extract(State_of_war_for_Russia, "[0-9]+|N/A")) %>%
  mutate(Responsibility_for_the_war = str_extract(Responsibility_for_the_war, "[0-9]+|N/A")) %>%
  mutate(Course_of_action_for_Russia = str_extract(Course_of_action_for_Russia, "[0-9]+|N/A")) %>%
  dplyr::select(rowid, postid,
                War_mention, Putin_focus, Post_type, Opinion_intensity, Sentiment,
                Support_for_Putin, Criticism_of_Putin, Trust_in_Putin, Competence_of_Putin,
                State_of_war_for_Russia, Responsibility_for_the_war, Course_of_action_for_Russia) %>%
  mutate_all(~ifelse(. == "N/A", NA, .))

completions_df %>%
  filter(is.na(rowid))

# saveRDS(completions_df, file = "./Data/Validation_Samples/Winter_2023/completions_df_1.rds")
# saveRDS(tokenuse, file = "./Data/Validation_Samples/Winter_2023/Tokenuse_1.rds")

##### PLOT ######

completions1 <- read_rds("./Data/Validation_Samples/Winter_2023/completions_df_1.rds") %>%
  bind_rows()
tokenuse1 <- lapply(list.files("./Data/Validation_Samples/Winter_2023/ChatGPT_output_1/", full.names = TRUE, pattern = ".csv"), read.csv) %>%
  bind_rows()
completions2 <- read_rds("./Data/Validation_Samples/Winter_2023/completions_df_2.rds")
tokenuse2 <- lapply(list.files("./Data/Validation_Samples/Winter_2023/ChatGPT_output_2/", full.names = TRUE, pattern = ".csv"), read.csv) %>%
  bind_rows()

completions_all <- bind_rows(completions1, completions2)
tokenuse_all <- bind_rows(tokenuse1, tokenuse2)

(sum(tokenuse_all$prompt_tokens)/1000)*0.0015 + (sum(tokenuse_all$completion_tokens)/1000)*0.002
# 18.26015 dollar $$ spent

telegrams1 <- readRDS("./Data/Validation_Samples/Winter_2023/Subsample_Fall2023_1.rds") %>%
  mutate(message = str_remove(message, "_"))
telegrams2 <- readRDS("./Data/Validation_Samples/Winter_2023/Subsample_Fall2023_2.rds") %>%
  mutate(message = str_remove(message, "_"))

completions_all <- completions_all %>%
  select(-postid) %>%
  left_join(telegrams1 %>% bind_rows(telegrams2), by = join_by(rowid))

# saveRDS(completions_all, file = "./Data/Coded_posts_13_11_2023.rds")

completions_all %>%
  group_by(Post_type) %>%
  count()

#### PLOTS ####

plotposts <- completions_all %>%
  left_join(telegrams1 %>% bind_rows(telegrams2) %>% select(date, rowid)) %>%
  mutate(year_month = paste0( substr(date, 6, 7), "/", substr(date, 3, 4))) %>%
  select(rowid, year_month, Sentiment, Support_for_Putin, Criticism_of_Putin, Trust_in_Putin, Competence_of_Putin) %>%
  filter(year_month != "11/23") %>%
  mutate(year_month = factor(year_month, levels = c("01/22", "02/22", "03/22", "04/22", "05/22", "06/22", "07/22", "08/22", "09/22",
                                                    "10/22", "11/22", "12/22", "01/23", "02/23", "03/23", "04/23", "05/23", "06/23",
                                                    "07/23", "08/23", "09/23", "10/23")))

support_p <- plotposts %>%
  drop_na(rowid, year_month) %>%
  mutate(support = as.numeric(Support_for_Putin)) %>%
  group_by(year_month) %>%
  summarise(Support = mean(support, na.rm = TRUE)) %>%
  ggplot(aes(year_month, Support, group = 1)) +
  ylim(1,3) +
  geom_line(linewidth = 1) +
  labs(x = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

sentiment_p <- plotposts %>%
  drop_na(rowid, year_month) %>%
  mutate(sentiment = as.numeric(Sentiment)) %>%
  group_by(year_month) %>%
  summarise(Sentiment = mean(sentiment, na.rm = TRUE)) %>%
  ggplot(aes(year_month, Sentiment, group = 1)) +
  ylim(1,3) +
  labs(x = "") +
  geom_line(linewidth = 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

critique_p <- plotposts %>%
  drop_na(rowid, year_month) %>%
  mutate(critique = as.numeric(Criticism_of_Putin)) %>%
  group_by(year_month) %>%
  summarise(Critique = mean(critique, na.rm = TRUE)) %>%
  ggplot(aes(year_month, Critique, group = 1)) +
  ylim(1,3) +
  labs(x = "") +
  geom_line(linewidth = 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

trust_p <- plotposts %>%
  drop_na(rowid, year_month) %>%
  mutate(trust = as.numeric(Trust_in_Putin)) %>%
  group_by(year_month) %>%
  summarise(Trust = mean(trust, na.rm = TRUE)) %>%
  ggplot(aes(year_month, Trust, group = 1)) +
  ylim(1,3) +
  labs(x = "") +
  geom_line(linewidth = 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

competence_p <- plotposts %>%
  drop_na(rowid, year_month) %>%
  mutate(competence = as.numeric(Competence_of_Putin)) %>%
  group_by(year_month) %>%
  summarise(Competence = mean(competence, na.rm = TRUE)) %>%
  ggplot(aes(year_month, Competence, group = 1)) +
  ylim(1,3) +
  geom_line(linewidth = 1) +
  labs(x = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

codeplots <- cowplot::plot_grid(support_p, sentiment_p, competence_p, trust_p, # critique_p
                                labels = c("Support", "Sentiment", #"Criticism",
                                           "Competence", "Trust"), ncol = 2)

ggsave(codeplots, file = "./Figures/Coded_plots.pdf", width = 18, height = 10, dpi = 150)
