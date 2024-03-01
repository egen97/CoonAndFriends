
library(tidyverse)
#library(ltm)
library(scico)

### Read in human coding

amalie <- openxlsx::read.xlsx("./Data/CrossCoding/Subsample_Amalie.xlsx") %>%
  dplyr::select(-message) %>%
  rename_all(str_to_lower) %>%
  mutate_all(as.numeric) %>%
  mutate(coder = "amalie") %>%
  dplyr::select(-source) %>% 
  mutate(
    across(everything(), ~replace_na(.x, 9))
  ) %>% 
  mutate(
    sentiment = if_else(sentiment == 9 , 2, sentiment),
    support_for_putin = if_else(support_for_putin == 9, 2, support_for_putin),
    criticism_of_putin = if_else(criticism_of_putin == 9, 0, criticism_of_putin),
    trust_in_putin = if_else(trust_in_putin == 9, 2, trust_in_putin),
    competence_of_putin = if_else(competence_of_putin == 9, 2, competence_of_putin),
    responsibility_for_the_war = if_else(responsibility_for_the_war == 9, 0, responsibility_for_the_war),
    course_of_action_for_russia = if_else(course_of_action_for_russia == 9, 0, course_of_action_for_russia),
    state_of_war_for_russia = if_else(state_of_war_for_russia == 9, 2, state_of_war_for_russia),
    post_type = if_else(post_type == 9, 1.5, post_type),
    war_mention = if_else(war_mention == 9, 1.5, war_mention)
    
  )


eric <- openxlsx::read.xlsx("./Data/CrossCoding/Subsample_Eric.xlsx") %>%
  dplyr::select(-message) %>%
  rename_all(str_to_lower) %>%
  mutate_all(as.numeric) %>%
  mutate(coder = "eric") %>%
  dplyr::select(-source) %>% 
  mutate(
    across(everything(), ~replace_na(.x, 9))
  )%>% 
  mutate(
    sentiment = if_else(sentiment == 9 , 2, sentiment),
    support_for_putin = if_else(support_for_putin == 9, 2, support_for_putin),
    criticism_of_putin = if_else(criticism_of_putin == 9, 0, criticism_of_putin),
    trust_in_putin = if_else(trust_in_putin == 9, 2, trust_in_putin),
    competence_of_putin = if_else(competence_of_putin == 9, 2, competence_of_putin),
    responsibility_for_the_war = if_else(responsibility_for_the_war == 9, 0, responsibility_for_the_war),
    course_of_action_for_russia = if_else(course_of_action_for_russia == 9, 0, course_of_action_for_russia),
    state_of_war_for_russia = if_else(state_of_war_for_russia == 9, 2, state_of_war_for_russia),
    post_type = if_else(post_type == 9, 1.5, post_type),
    war_mention = if_else(war_mention == 9, 1.5, war_mention)
  )



jonas <- openxlsx::read.xlsx("./Data/CrossCoding/Subsample_Jonas.xlsx")  %>%
  dplyr::select(-message) %>%
  rename_all(str_to_lower) %>%
  mutate_all(as.numeric) %>%
  mutate(coder = "jonas") %>%
  dplyr::select(-source) %>% 
  mutate(
    across(everything(), ~replace_na(.x, 9))
  )%>% 
  mutate(
    sentiment = if_else(sentiment == 9 , 2, sentiment),
    support_for_putin = if_else(support_for_putin == 9, 2, support_for_putin),
    criticism_of_putin = if_else(criticism_of_putin == 9, 0, criticism_of_putin),
    trust_in_putin = if_else(trust_in_putin == 9, 2, trust_in_putin),
    competence_of_putin = if_else(competence_of_putin == 9, 2, competence_of_putin),
    responsibility_for_the_war = if_else(responsibility_for_the_war == 9, 0, responsibility_for_the_war),
    course_of_action_for_russia = if_else(course_of_action_for_russia == 9, 0, course_of_action_for_russia),
    state_of_war_for_russia = if_else(state_of_war_for_russia == 9, 2, state_of_war_for_russia),
    post_type = if_else(post_type == 9, 1.5, post_type),
    war_mention = if_else(war_mention == 9, 1.5, war_mention)
  )



### Read in ChatGPT coding. Generated from ChatGPT_LimitedSample.R

chatgpt_1 <- readRDS("./Data/completions_chatgpt_1.rds") %>%
  dplyr::select(rowid, War_mention, Putin_focus, Post_type, Sentiment, Support_for_Putin, Criticism_of_Putin, Trust_in_Putin, Competence_of_Putin,
                State_of_war_for_Russia, Responsibility_for_the_war, Course_of_action_for_Russia) %>%
  rename_all(str_to_lower) %>%
  mutate_all(as.numeric) %>%
  mutate(coder = "chatgpt_1") %>%
  mutate(rowid = as.numeric(str_squish(rowid)))%>% 
  mutate(
    across(everything(), ~replace_na(.x, 9))
  )%>% 
  mutate(
    sentiment = if_else(sentiment == 9 , 2, sentiment),
    support_for_putin = if_else(support_for_putin == 9, 2, support_for_putin),
    criticism_of_putin = if_else(criticism_of_putin == 9, 0, criticism_of_putin),
    trust_in_putin = if_else(trust_in_putin == 9, 2, trust_in_putin),
    competence_of_putin = if_else(competence_of_putin == 9, 2, competence_of_putin),
    responsibility_for_the_war = if_else(responsibility_for_the_war == 9, 0, responsibility_for_the_war),
    course_of_action_for_russia = if_else(course_of_action_for_russia == 9, 0, course_of_action_for_russia),
    state_of_war_for_russia = if_else(state_of_war_for_russia == 9, 2, state_of_war_for_russia),
    post_type = if_else(post_type == 9, 1.5, post_type),
    war_mention = if_else(war_mention == 9, 1.5, war_mention)
  )



chatgpt_2 <- readRDS("./Data/completions_chatgpt_2.rds") %>%
  dplyr::select(rowid, War_mention, Putin_focus, Post_type, Sentiment, Support_for_Putin, Criticism_of_Putin, Trust_in_Putin, Competence_of_Putin,
                State_of_war_for_Russia, Responsibility_for_the_war, Course_of_action_for_Russia) %>%
  rename_all(str_to_lower) %>%
  mutate_all(as.numeric) %>%
  mutate(coder = "chatgpt_2") %>%
  mutate(rowid = as.numeric(str_squish(rowid)))%>% 
  mutate(
    across(everything(), ~replace_na(.x, 9))
  )%>% 
  mutate(
    sentiment = if_else(sentiment == 9 , 2, sentiment),
    support_for_putin = if_else(support_for_putin == 9, 2, support_for_putin),
    criticism_of_putin = if_else(criticism_of_putin == 9, 0, criticism_of_putin),
    trust_in_putin = if_else(trust_in_putin == 9, 2, trust_in_putin),
    competence_of_putin = if_else(competence_of_putin == 9, 2, competence_of_putin),
    responsibility_for_the_war = if_else(responsibility_for_the_war == 9, 0, responsibility_for_the_war),
    course_of_action_for_russia = if_else(course_of_action_for_russia == 9, 0, course_of_action_for_russia),
    state_of_war_for_russia = if_else(state_of_war_for_russia == 9, 2, state_of_war_for_russia),
    post_type = if_else(post_type == 9, 1.5, post_type),
    war_mention = if_else(war_mention == 9, 1.5, war_mention)
  )



chatgpt_3 <- readRDS("./Data/completions_chatgpt_3.rds") %>%
  dplyr::select(rowid, War_mention, Putin_focus, Post_type, Sentiment, Support_for_Putin, Criticism_of_Putin, Trust_in_Putin, Competence_of_Putin,
                State_of_war_for_Russia, Responsibility_for_the_war, Course_of_action_for_Russia) %>%
  rename_all(str_to_lower) %>%
  mutate_all(as.numeric) %>%
  mutate(coder = "chatgpt_3") %>%
  mutate(rowid = as.numeric(str_squish(rowid)))%>% 
  mutate(
    across(everything(), ~replace_na(.x, 9))
  )%>% 
  mutate(
    sentiment = if_else(sentiment == 9 , 2, sentiment),
    support_for_putin = if_else(support_for_putin == 9, 2, support_for_putin),
    criticism_of_putin = if_else(criticism_of_putin == 9, 0, criticism_of_putin),
    trust_in_putin = if_else(trust_in_putin == 9, 2, trust_in_putin),
    competence_of_putin = if_else(competence_of_putin == 9, 2, competence_of_putin),
    responsibility_for_the_war = if_else(responsibility_for_the_war == 9, 0, responsibility_for_the_war),
    course_of_action_for_russia = if_else(course_of_action_for_russia == 9, 0, course_of_action_for_russia),
    state_of_war_for_russia = if_else(state_of_war_for_russia == 9, 2, state_of_war_for_russia),
    post_type = if_else(post_type == 9, 1.5, post_type),
    war_mention = if_else(war_mention == 9, 1.5, war_mention)
  )




df <- rbind(amalie, jonas, eric, chatgpt_1, chatgpt_2, chatgpt_3)



overlap <- function(df, name_1, name_2){
  
  df%>% 
    filter(coder %in% c(name_1, name_2)) %>%
    add_count(rowid) %>%
    filter(n > 1) %>%
    dplyr::select(!n) %>%
    pivot_longer(!c(rowid, coder), names_to = "question", values_to = "value") %>%
    group_by(rowid, question) %>%
    dplyr::select(!coder) %>%
    summarize_all(n_distinct) %>%
    mutate(value =  if_else(value == 1, 1, 0)) %>%
    group_by(question) %>%
    summarize(agreement = mean(value, na.rm = T)) %>%
    mutate(coder_1 = name_1, 
           coder_2 = name_2,
           type = case_when(grepl("chatgpt",coder_1) + grepl("chatgpt",coder_2) == 0 ~ "Human Pairs",
                            grepl("chatgpt",coder_1) + grepl("chatgpt",coder_2) == 1 ~ "Human-GPT Pairs",
                            grepl("chatgpt",coder_1) + grepl("chatgpt",coder_2) == 2 ~ "GPT Pairs"
           ))
}



means <- rbind(
 df %>% overlap("jonas", "amalie"),
 df %>% overlap("jonas", "eric"),
 df %>% overlap("jonas", "chatgpt_1"),
 df %>% overlap("jonas", "chatgpt_2"),
 df %>% overlap("jonas", "chatgpt_3"),
 df %>% overlap("amalie", "eric"),
 df %>% overlap("amalie", "chatgpt_1"),
 df %>% overlap("amalie", "chatgpt_2"),
 df %>% overlap("amalie", "chatgpt_3"),
 df %>% overlap("eric", "chatgpt_1"),
 df %>% overlap("eric", "chatgpt_2"),
 df %>% overlap("eric", "chatgpt_3"),
 df %>% overlap("chatgpt_1", "chatgpt_2"),
 df %>% overlap("chatgpt_1", "chatgpt_3"),
 df %>% overlap("chatgpt_2", "chatgpt_3")
) %>%
  group_by(type, question) %>%
  summarize(agreement = mean(agreement)) %>% 
  mutate(
    Variable = case_when(
      question == "post_type" ~ "Post Type",
      question == "sentiment" ~ "Sentiment",
      question == "support_for_putin" ~ "Support",
      question == "criticism_of_putin" ~ "Criticism",
      question == "competence_of_putin" ~ "Competence",
      question == "trust_in_putin" ~ "Trust",
      question == "war_mention" ~ "War Mention",
      question == "responsibility_for_the_war" ~ "Responsibility",
      question == "state_of_war_for_russia" ~ "State of War",
      question == "course_of_action_for_russia" ~ "Course of Action",
      question == "putin_focus" ~ "Putin Focus"),
    Variable = fct_relevel(Variable, c("War Mention", "Putin Focus", 
                                       "Post Type",  "Sentiment", 
                                       "Support", "Criticism", 
                                       "Trust", "Competence", 
                                       "State of War", "Responsibility",
                                       "Course of Action")))
 


means$type <- fct_relevel(as.factor(means$type), c( "Human Pairs","Human-GPT Pairs", "GPT Pairs"))

means %>% 
 ggplot(aes(x=Variable, y= type , fill=agreement)) + 
  geom_tile(color="black") + 
  theme_classic(base_size = 15) +
  geom_text(aes(x = Variable, y = type, label = paste( round(agreement, digits = 2)*100, "%")), color = "black", size = 4) +
  scico::scale_fill_scico( palette = "bam") +
  labs(x="",
       y="",
       fill = "Agreement") +
  coord_fixed() +
  guides(fill = guide_colourbar(label = F,
                                ticks = T)) +
  theme(axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.2))

means %>% 
filter(question %in% c("sentiment", "support_for_putin", "criticism_of_putin", "competence_of_putin", "trust_in_putin", "responsibility_for_the_war")) %>%
  ggplot(aes(x=Variable, y= type , fill=agreement)) + 
  geom_tile(color="black") + 
  theme_classic(base_size = 15) +
  geom_text(aes(x = Variable, y = type, label = paste( round(agreement, digits = 2)*100, "%")), color = "black", size = 4) +
  scico::scale_fill_scico( palette = "bam") +
  labs(x="",
       y="",
       fill = "Agreement") +
  coord_fixed() +
  guides(fill = guide_colourbar(label = F,
                                ticks = T)) +
  theme(axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.2))

ggsave("C:/Users/jonassc/Dropbox (UiO)/Apps/Overleaf/ChatGPTResearchNote/Graphics/Validation_Results.pdf", width = 10, height = 6)



means %>% 
  filter(question %in% c("sentiment", "support_for_putin", "criticism_of_putin", "competence_of_putin", "trust_in_putin", "responsibility_for_the_war")) %>%
  group_by(type) %>%
  summarize(agreement = mean(agreement))


means %>%
  group_by(type) %>%
  summarize(agreement = mean(agreement))

  