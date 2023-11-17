
# completions_df <- as_tibble(completions, .name_repair = "universal") %>%
#   separate(completion, into = c("rowid",
#                                 "War_mention", "Putin_focus", "Post_type", "Opinion_intensity", "Sentiment",
#                                 "Support_for_Putin", "Criticism_of_Putin", "Trust_in_Putin", "Competence_of_Putin",
#                                 "State_of_war_for_Russia", "Responsibility_for_the_war", "Course_of_action_for_Russia"), sep = "\n(\n)?") %>%
#   mutate(rowid = str_remove_all(rowid, "PostID ")) %>%
#   mutate(War_mention = str_remove(War_mention, "War_mention: "),
#          War_mention_category = str_remove_all(str_trim(str_remove_all(str_extract(War_mention, "\\(.*\\)"), "\\|")), "\\)|\\("),
#          #War_mention_justification = str_trim(str_remove_all(War_mention, "([0-9])?(N/A)?(\\s+)?\\|.*\\| ")),
#          War_mention = str_extract(War_mention, "[0-9]|N/A")) %>%
#   mutate(Putin_focus = str_remove(Putin_focus, "Putin_focus: "),
#          Putin_focus_category = str_remove_all(str_trim(str_remove_all(str_extract(Putin_focus, "\\(.*\\)"), "\\|")), "\\)|\\("),
#          #Putin_focus_justification = str_trim(str_remove_all(Putin_focus, "([0-9])?(N/A)?(\\s+)?\\|.*\\| ")),
#          Putin_focus = str_extract(Putin_focus, "[0-9]|N/A")) %>%
#   mutate(Post_type = str_remove(Post_type, "Post_type: "),
#          Post_type_category = str_remove_all(str_trim(str_remove_all(str_extract(Post_type, "\\(.*\\)"), "\\|")), "\\)|\\("),
#          #Post_type_justification = str_trim(str_remove_all(Post_type, "([0-9])?(N/A)?(\\s+)?\\|.*\\|")),
#          Post_type = str_extract(Post_type, "[0-9]|N/A")) %>%
#   mutate(Opinion_intensity = str_remove(Opinion_intensity, "Opinion_intensity: "),
#          Opinion_intensity_category = str_remove_all(str_trim(str_remove_all(str_extract(Opinion_intensity, "\\(.*\\)"), "\\|")), "\\)|\\("),
#          #Opinion_intensity_justification = str_trim(str_remove_all(Opinion_intensity, "([0-9])?(N/A)?(\\s+)?\\|.*\\| ")),
#          Opinion_intensity = str_extract(Opinion_intensity, "[0-9]|N/A")) %>%
#   mutate(Sentiment = str_remove(Sentiment, "Sentiment: "),
#          Sentiment_category = str_remove_all(str_trim(str_remove_all(str_extract(Sentiment, "\\(.*\\)"), "\\|")), "\\)|\\("),
#          #Sentiment_justification = str_trim(str_remove_all(Sentiment, "([0-9])?(N/A)?(\\s+)?\\|.*\\| ")),
#          Sentiment = str_extract(Sentiment, "[0-9]|N/A")) %>%
#   mutate(Support_for_Putin = str_remove(Support_for_Putin, "Support_for_Putin: "),
#          Support_for_Putin_category = str_remove_all(str_trim(str_remove_all(str_extract(Support_for_Putin, "\\(.*\\)"), "\\|")), "\\)|\\("),
#          #Support_for_Putin_justification = str_trim(str_remove_all(Support_for_Putin, "([0-9])?(N/A)?(\\s+)?\\|.*\\| ")),
#          Support_for_Putin = str_extract(Support_for_Putin, "[0-9]|N/A")) %>%
#   mutate(Criticism_of_Putin = str_remove(Criticism_of_Putin, "Criticism_of_Putin: "),
#          Criticism_of_Putin_category = str_remove_all(str_trim(str_remove_all(str_extract(Criticism_of_Putin, "\\(.*\\)"), "\\|")), "\\)|\\("),
#          #Criticism_of_Putin_justification = str_trim(str_remove_all(Criticism_of_Putin, "([0-9])?(N/A)?(\\s+)?\\|.*\\| ")),
#          Criticism_of_Putin = str_extract(Criticism_of_Putin, "[0-9]|N/A")) %>%
#   mutate(Trust_in_Putin = str_remove(Trust_in_Putin, "Trust_in_Putin: "),
#          Trust_in_Putin_category = str_remove_all(str_trim(str_remove_all(str_extract(Trust_in_Putin, "\\(.*\\)"), "\\|")), "\\)|\\("),
#          #Trust_in_Putin_justification = str_trim(str_remove_all(Trust_in_Putin, "([0-9])?(N/A)?(\\s+)?\\|.*\\| ")),
#          Trust_in_Putin = str_extract(Trust_in_Putin, "[0-9]|N/A")) %>%
#   mutate(Competence_of_Putin = str_remove(Competence_of_Putin, "Competence_of_Putin: "),
#          Competence_of_Putin_category = str_remove_all(str_trim(str_remove_all(str_extract(Competence_of_Putin, "\\(.*\\)"), "\\|")), "\\)|\\("),
#          #Competence_of_Putin_justification = str_trim(str_remove_all(Competence_of_Putin, "([0-9])?(N/A)?(\\s+)?\\|.*\\| ")),
#          Competence_of_Putin = str_extract(Competence_of_Putin, "[0-9]|N/A")) %>%
#   mutate(State_of_war_for_Russia = str_remove(State_of_war_for_Russia, "State_of_war_for_Russia: "),
#          State_of_war_for_Russia_category = str_remove_all(str_trim(str_remove_all(str_extract(State_of_war_for_Russia, "\\(.*\\)"), "\\|")), "\\)|\\("),
#          #State_of_war_for_Russia_justification = str_trim(str_remove_all(State_of_war_for_Russia, "([0-9])?(N/A)?(\\s+)?\\|.*\\| ")),
#          State_of_war_for_Russia = str_extract(State_of_war_for_Russia, "[0-9]|N/A")) %>%
#   mutate(Responsibility_for_the_war = str_remove(Responsibility_for_the_war, "Responsibility_for_the_war: "),
#          Responsibility_for_the_war_category = str_remove_all(str_trim(str_remove_all(str_extract(Responsibility_for_the_war, "\\(.*\\)"), "\\|")), "\\)|\\("),
#          #Responsibility_for_the_war_justification = str_trim(str_remove_all(Responsibility_for_the_war, "([0-9])?(N/A)?(\\s+)?\\|.*\\| ")),
#          Responsibility_for_the_war = str_extract(Responsibility_for_the_war, "[0-9]|N/A")) %>%
#   mutate(Course_of_action_for_Russia = str_remove(Course_of_action_for_Russia, "Course_of_action_for_Russia: "),
#          Course_of_action_for_Russia_category = str_remove_all(str_trim(str_remove_all(str_extract(Course_of_action_for_Russia, "\\(.*\\)"), "\\|")), "\\)|\\("),
#          #Course_of_action_for_Russia_justification = str_trim(str_remove_all(Course_of_action_for_Russia, "([0-9])?(N/A)?(\\s+)?\\|.*\\| ")),
#          Course_of_action_for_Russia = str_extract(Course_of_action_for_Russia, "[0-9]|N/A")) %>%
#   dplyr::select(rowid,
#                 War_mention, War_mention_category, #War_mention_justification,
#                 Putin_focus, Putin_focus_category, #Putin_focus_justification,
#                 Post_type, Post_type_category, #Post_type_justification,
#                 Opinion_intensity, Opinion_intensity_category, #Opinion_intensity_justification,
#                 Sentiment, Sentiment_category, #Sentiment_justification,
#                 Support_for_Putin, Support_for_Putin_category, #Support_for_Putin_justification,
#                 Criticism_of_Putin, Criticism_of_Putin_category, #Criticism_of_Putin_justification,
#                 Trust_in_Putin, Trust_in_Putin_category, #Trust_in_Putin_justification,
#                 Competence_of_Putin, Competence_of_Putin_category, #Competence_of_Putin_justification,
#                 State_of_war_for_Russia, State_of_war_for_Russia_category, #State_of_war_for_Russia_justification,
#                 Responsibility_for_the_war, Responsibility_for_the_war_category, #Responsibility_for_the_war_justification,
#                 Course_of_action_for_Russia, Course_of_action_for_Russia_category) #Course_of_action_for_Russia_justification)
