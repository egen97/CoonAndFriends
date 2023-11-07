
library(tidyverse)

##### VALIDATION SAMPLE SUBDIVIDED #####

telegrams_joined_sample <- readRDS("./Data/subsample_run.rds")

amalie <- bind_rows(telegrams_joined_sample[1:25,], telegrams_joined_sample[76:96,])
eric <- bind_rows(telegrams_joined_sample[26:50,], telegrams_joined_sample[76:96,])
jonas <- bind_rows(telegrams_joined_sample[51:75,], telegrams_joined_sample[76:96,])

common <- telegrams_joined_sample[76:96,]

amalie <- amalie %>%
  select(source, rowid, message) %>%
  mutate(War_mention = "",
         Putin_focus = "",
         Post_type = "",
         Sentiment = "",
         Support_for_Putin = "",
         Criticism_of_Putin = "",
         Trust_in_Putin = "",
         Competence_of_Putin = "",
         State_of_war_for_Russia = "",
         Responsibility_for_the_war = "",
         Course_of_action_for_Russia = "")

eric <- eric %>%
  select(source, rowid, message) %>%
  mutate(War_mention = "",
         Putin_focus = "",
         Post_type = "",
         Sentiment = "",
         Support_for_Putin = "",
         Criticism_of_Putin = "",
         Trust_in_Putin = "",
         Competence_of_Putin = "",
         State_of_war_for_Russia = "",
         Responsibility_for_the_war = "",
         Course_of_action_for_Russia = "")

jonas <- jonas %>%
  select(source, rowid, message) %>%
  mutate(War_mention = "",
         Putin_focus = "",
         Post_type = "",
         Sentiment = "",
         Support_for_Putin = "",
         Criticism_of_Putin = "",
         Trust_in_Putin = "",
         Competence_of_Putin = "",
         State_of_war_for_Russia = "",
         Responsibility_for_the_war = "",
         Course_of_action_for_Russia = "")

write_excel_csv(amalie, "./Data/CrossCoding/Subsample_Amalie.csv")
write_excel_csv(eric, "./Data/CrossCoding/Subsample_Eric.csv")
write_excel_csv(jonas, "./Data/CrossCoding/Subsample_Jonas.csv")
