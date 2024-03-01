
library(tidyverse)
library(ltm)

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



### Replace NA ###

# NA needs to be the same across sheets

### Function to check specific variables

pick_var <- function(dat, var){
  
  var_q <- enquo(var)
  dat %>%
    dplyr::select(rowid, coder, !!var_q)
  
}

coded_vars <- c("war_mention", "putin_focus", "post_type", "sentiment", "support_for_putin", "criticism_of_putin", "trust_in_putin", "competence_of_putin",
                "state_of_war_for_russia", "responsibility_for_the_war", "course_of_action_for_russia") # Fill in which variables you want to check the alpha on.

amalie_eric_list <- list()
amalie_jonas_list <- list()
eric_jonas_list <- list()
eric_jonas_amalie_list <- list()

amalie_chatgpt_list <- list()
jonas_chatgpt_list <- list()
eric_chatgpt_list <- list()

amalie_eric_chatgpt_list <- list()
amalie_jonas_chatgpt_list <- list()
eric_jonas_chatgpt_list <- list()
eric_jonas_amalie_chatgpt_list <- list()

chatgpts_list <- list()

for (i in 1:length(coded_vars)) {
  
  amalie_var <- pick_var(amalie, coded_vars[i])
  eric_var <- pick_var(eric, coded_vars[i])
  jonas_var <- pick_var(jonas, coded_vars[i])
  
  
  chatgpt_1_var <- pick_var(chatgpt_1, coded_vars[i])
  chatgpt_2_var <- pick_var(chatgpt_2, coded_vars[i])
  chatgpt_3_var <- pick_var(chatgpt_3, coded_vars[i])
  
  
  #### HUMANS ####
  
  amalie_eric <- bind_rows(amalie_var, eric_var) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    unnest() %>%
    dplyr::select(-rowid) %>%
    na.omit()
  
  amalie_jonas <- bind_rows(amalie_var, jonas_var) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    unnest() %>%
    dplyr::select(-rowid) %>%
    na.omit()
  
  eric_jonas <- bind_rows(eric_var, jonas_var) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    unnest() %>%
    dplyr::select(-rowid) %>%
    na.omit()
  
  eric_jonas_amalie <- bind_rows(eric_var, jonas_var, amalie_var) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    unnest() %>%
    dplyr::select(-rowid) %>%
    na.omit()
  
  # #### HUMANS AND AI ####
  
  amalie_eric_chatgpt_1 <- bind_rows(amalie_var, eric_var, chatgpt_1_var) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    unnest() %>%
    dplyr::select(-rowid) %>%
    na.omit()
  
  amalie_jonas_chatgpt_1 <- bind_rows(amalie_var, jonas_var, chatgpt_1_var) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    unnest() %>%
    dplyr::select(-rowid) %>%
    na.omit()
  
  eric_jonas_chatgpt_1 <- bind_rows(eric_var, jonas_var, chatgpt_1_var) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    unnest() %>%
    dplyr::select(-rowid) %>%
    na.omit()
  
  eric_jonas_amalie_chatgpt_1 <- bind_rows(eric_var, jonas_var, amalie_var, chatgpt_1_var) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    unnest() %>%
    dplyr::select(-rowid) %>%
    na.omit()
  
  
  eric_chatgpt_1 <- bind_rows(eric_var, chatgpt_1_var) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    unnest() %>%
    dplyr::select(-rowid) %>%
    na.omit()
  
  jonas_chatgpt_1 <- bind_rows(jonas_var, chatgpt_1_var) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    unnest() %>%
    dplyr::select(-rowid) %>%
    na.omit()
  
  amalie_chatgpt_1 <- bind_rows(amalie_var, chatgpt_1_var) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    unnest() %>%
    dplyr::select(-rowid) %>%
    na.omit()
  
  
  #### AI ####
  
  chatgpts <- bind_rows(chatgpt_1_var, chatgpt_2_var, chatgpt_3_var) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    dplyr::select(-rowid) %>%
    mutate_all(~ ifelse(is.na(.), 99, .))
  
  #### CALCULATE ####
  
  print(coded_vars[i])
  
  amalie_eric_list[[i]] <- cronbach.alpha(amalie_eric)
  amalie_jonas_list[[i]] <- cronbach.alpha(amalie_jonas)
  eric_jonas_list[[i]] <- cronbach.alpha(eric_jonas)
  eric_jonas_amalie_list[[i]] <- cronbach.alpha(eric_jonas_amalie)
  
  amalie_chatgpt_list[[i]] <-  cronbach.alpha(amalie_chatgpt_1)
  eric_chatgpt_list[[i]] <-  cronbach.alpha(eric_chatgpt_1)
  jonas_chatgpt_list[[i]] <- cronbach.alpha(jonas_chatgpt_1)
  
  
  amalie_eric_chatgpt_list[[i]] <- cronbach.alpha(amalie_eric_chatgpt_1)
  amalie_jonas_chatgpt_list[[i]] <- cronbach.alpha(amalie_jonas_chatgpt_1)
  eric_jonas_chatgpt_list[[i]] <- cronbach.alpha(eric_jonas_chatgpt_1)
  eric_jonas_amalie_chatgpt_list[[i]] <- cronbach.alpha(eric_jonas_amalie_chatgpt_1)
  
  chatgpts_list[[i]] <- cronbach.alpha(chatgpts)
  
}


cronbach_df <- bind_rows(
  data.frame(do.call(rbind, amalie_eric_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, amalie_jonas_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, eric_jonas_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, eric_jonas_amalie_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, amalie_eric_chatgpt_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, amalie_jonas_chatgpt_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, eric_jonas_chatgpt_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, eric_jonas_amalie_chatgpt_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, chatgpts_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, amalie_chatgpt_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, eric_chatgpt_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, jonas_chatgpt_list)) %>% mutate(var = coded_vars)) %>%
  unnest(cols = c(alpha, n, p, standardized, name)) %>%
  spread(var, alpha) %>%
  dplyr::select(name, n,
                war_mention, putin_focus, post_type, sentiment, support_for_putin, criticism_of_putin, trust_in_putin, competence_of_putin,
                state_of_war_for_russia, responsibility_for_the_war, course_of_action_for_russia) %>%
  mutate(type = ifelse(name %in% c("amalie_eric", "amalie_jonas", "eric_jonas"), "Only humans",
                       ifelse(name %in% c("amalie_chatgpt_1", "jonas_chatgpt_1", "eric_chatgpt_1"), "Humans and AI",
                              ifelse(name %in% c("chatgpts"), "Only AI",
                                     name))))





mean_alpha <- cronbach_df %>%
  rename(`Coder combination` = type) %>%
  group_by(`Coder combination`) %>%
  gather(war_mention, putin_focus, post_type, sentiment, support_for_putin, criticism_of_putin, trust_in_putin, competence_of_putin,
         state_of_war_for_russia, responsibility_for_the_war, course_of_action_for_russia,
         key = "Variable", value = "Alpha") %>%
  summarise(`Average Cronbach's Alpha` = mean(Alpha, na.rm = TRUE))

knitr::kable(mean_alpha) %>%
  kableExtra::kable_styling()





###heatmap
cronbach_df
unique(cronbach_df$name)

heat <- cronbach_df %>%
  filter(name %in% c( "amalie_eric", "amalie_jonas", "eric_jonas", "jonas_chatgpt_1", "amalie_chatgpt_1", "eric_chatgpt_1" )) %>%
  separate(name, into = c("Coder_1", "Coder_2"), sep = "_", extra = "drop")


heat <- rbind(heat,
              heat %>%
  mutate(Coder_3 = Coder_2, 
         Coder_4 = Coder_1) %>% 
  dplyr::select(!c(Coder_1, Coder_2)) %>%
  rename(Coder_1 = Coder_3,
         Coder_2 = Coder_4)) %>%
  dplyr::select(!c(n, type)) %>%
  pivot_longer(!1:2, names_to = "Variable", values_to = "Cronbach's Alpha")

heat$Coder_1 <- factor(heat$Coder_1, levels=c('amalie', 'eric', 'jonas', 'chatgpt'))
heat$Coder_2 <- factor(heat$Coder_2, levels=c('amalie', 'eric', 'jonas', 'chatgpt'))

library(scico)

heat %>%
  ggplot(aes(x=fct_rev(Coder_1), y=fct_rev(Coder_2), fill=`Cronbach's Alpha`)) + 
  geom_tile(color="black") + 
  facet_wrap(~Variable, scales = "free") + 
  theme_classic(base_size = 15) +
  geom_text(aes(fct_rev(Coder_1), fct_rev(Coder_2), label = round(`Cronbach's Alpha`, digits = 2)), color = "black", size = 4) +
  labs(y= "Coder 2",
       x ="Coder 1") +
  scico::scale_fill_scico( palette = "lajolla")



unique(means$name)

means <- cronbach_df %>%
  filter(name %in% c( "amalie_eric", "amalie_jonas", "eric_jonas", "jonas_chatgpt_1", "amalie_chatgpt_1", "eric_chatgpt_1", "chatgpts" )) %>%
  mutate(type = if_else(name %in% c("amalie_eric", "amalie_jonas", "eric_jonas"), "Human_Pairs", "Human_AI_Pairs"),
         type = if_else(name == "chatgpts", "GPT Only", type))



heat2 <- means %>%
  group_by(type) %>% 
  dplyr::select(!c(name, n)) %>%
  summarise_all(mean, na.rm = T) %>%
  pivot_longer(!1, names_to = "Variable", values_to = "Cronbach's Alpha")

unique(heat2$Variable)
unique(heat2$type)



heat2 %>%
  filter(Variable %in% c("post_type", "sentiment", "support_for_putin", "criticism_of_putin", "competence_of_putin", "trust_in_putin")) %>%
  mutate(
    Variable = case_when(
    Variable == "post_type" ~ "Post Type",
    Variable == "sentiment" ~ "Sentiment",
    Variable == "support_for_putin" ~ "Support",
    Variable == "criticism_of_putin" ~ "Criticism",
    Variable == "competence_of_putin" ~ "Competence",
    Variable == "trust_in_putin" ~ "Trust"),
    type = case_when(
      type == "Human_Pairs" ~ "Human Pairs",
      type == "Human_AI_Pairs" ~ "Human-GPT Pairs",
      type == "GPT Only" ~ "GPT Pairs"),
    Variable = fct_relevel(Variable, c("Post Type", "Sentiment", "Support", "Criticism", "Competence", "Trust")),
           type = fct_relevel(type, c("GPT Pairs","Human-GPT Pairs","Human Pairs"))) %>%
  ggplot(aes(x=Variable, y= fct_rev(type) , fill=`Cronbach's Alpha`)) + 
  geom_tile(color="black") + 
  theme_classic(base_size = 15) +
  geom_text(aes(Variable, type, label = round(`Cronbach's Alpha`, digits = 2)), color = "black", size = 4) +
  scico::scale_fill_scico( palette = "bam") +
  labs(x="",
       y="") +
  coord_fixed() +
  guides(fill = guide_colourbar(label = T,
                                ticks = T)) +
  theme(axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.2))
  




ggsave("C:/Users/jonassc/Dropbox (UiO)/Apps/Overleaf/ChatGPTResearchNote/Graphics/Validation_Results.pdf", width = 10, height = 6)


heat2 %>%
  ggplot(aes(x=Variable, y= fct_rev(type) , fill=`Cronbach's Alpha`)) + 
  geom_tile(color="black") + 
  theme_classic(base_size = 15) +
  geom_text(aes(Variable, type, label = round(`Cronbach's Alpha`, digits = 2)), color = "black", size = 4) +
  scico::scale_fill_scico( palette = "bam") +
  labs(x="",
       y="") +
  coord_fixed() +
  guides(fill = guide_colourbar(label = T,
                                ticks = T)) +
  theme(axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0.2))



# 
# 
# #### BINDING TOGTHER ####
# 
# cronbach_df <- bind_rows(
#   data.frame(do.call(rbind, amalie_eric_list)) %>% mutate(var = coded_vars),
#   data.frame(do.call(rbind, amalie_jonas_list)) %>% mutate(var = coded_vars),
#   data.frame(do.call(rbind, eric_jonas_list)) %>% mutate(var = coded_vars),
#   data.frame(do.call(rbind, eric_jonas_amalie_list)) %>% mutate(var = coded_vars),
#   data.frame(do.call(rbind, amalie_eric_chatgpt_list)) %>% mutate(var = coded_vars),
#   data.frame(do.call(rbind, amalie_jonas_chatgpt_list)) %>% mutate(var = coded_vars),
#   data.frame(do.call(rbind, eric_jonas_chatgpt_list)) %>% mutate(var = coded_vars),
#   data.frame(do.call(rbind, eric_jonas_amalie_chatgpt_list)) %>% mutate(var = coded_vars),
#   data.frame(do.call(rbind, chatgpts_list)) %>% mutate(var = coded_vars)) %>%
#   unnest(cols = c(alpha, n, p, standardized, name)) %>%
#   spread(var, alpha) %>%
#   dplyr::select(name, n,
#                 war_mention, putin_focus, post_type, sentiment, support_for_putin, criticism_of_putin, trust_in_putin, competence_of_putin,
#                 state_of_war_for_russia, responsibility_for_the_war, course_of_action_for_russia) %>%
#   mutate(type = ifelse(name %in% c("amalie_eric", "amalie_jonas", "eric_jonas", "eric_jonas_amalie"), "Only humans",
#                        ifelse(name %in% c("amalie_eric_chatgpt_1", "amalie_jonas_chatgpt_1", "eric_jonas_chatgpt_1", "eric_jonas_amalie_chatgpt_1"), "Humans and AI",
#                               ifelse(name %in% c("chatgpts"), "Only AI",
#                                      name))))
# 
# mean_alpha <- cronbach_df %>%
#   rename(`Coder combination` = type) %>%
#   group_by(`Coder combination`) %>%
#   gather(war_mention, putin_focus, post_type, sentiment, support_for_putin, criticism_of_putin, trust_in_putin, competence_of_putin,
#          state_of_war_for_russia, responsibility_for_the_war, course_of_action_for_russia,
#          key = "Variable", value = "Alpha") %>%
#   summarise(`Average Cronbach's Alpha` = mean(Alpha, na.rm = TRUE))
# 
# knitr::kable(mean_alpha) %>%
#   kableExtra::kable_styling()
# 
# mean_alpha_pairs <- cronbach_df %>%
#   rename(`Coder combination` = type) %>%
#   gather(war_mention, putin_focus, post_type, sentiment, support_for_putin, criticism_of_putin, trust_in_putin, competence_of_putin,
#          state_of_war_for_russia, responsibility_for_the_war, course_of_action_for_russia,
#          key = "Variable", value = "Alpha") %>%
#   group_by(`Coder combination`, Variable) %>%
#   summarise(`Average Cronbach's Alpha` = mean(Alpha, na.rm = TRUE))
# 
# knitr::kable(mean_alpha_pairs) %>%
#   kableExtra::kable_styling()
# 
# mean_alpha_per_question <- cronbach_df %>%
#   gather(war_mention, putin_focus, post_type, sentiment, support_for_putin, criticism_of_putin, trust_in_putin, competence_of_putin,
#          state_of_war_for_russia, responsibility_for_the_war, course_of_action_for_russia,
#          key = "Variable", value = "Alpha") %>%
#   group_by(type, Variable) %>%
#   summarise(mean_cronbachs_alpha = mean(Alpha, na.rm = TRUE))
# 
# knitr::kable(mean_alpha_per_question) %>%
#   kableExtra::kable_styling()
# 
# 
# 
#   
# 
# 
# ### Plot ###
# 
# alpha_per_var <- cronbach_df %>%
#   mutate(type = factor(type)) %>%
#   mutate(name = factor(name, levels = c("amalie_eric", "amalie_jonas", "eric_jonas", "eric_jonas_amalie",
#                                         "amalie_eric_chatgpt_1", "amalie_jonas_chatgpt_1", "eric_jonas_chatgpt_1", "eric_jonas_amalie_chatgpt_1",
#                                         "chatgpts"))) %>%
#   gather(war_mention, putin_focus, post_type, sentiment, support_for_putin, criticism_of_putin, trust_in_putin, competence_of_putin,
#          state_of_war_for_russia, responsibility_for_the_war, course_of_action_for_russia,
#          key = "Variable", value = "Alpha") %>%
#   mutate(Variable = factor(Variable, levels = c("war_mention", "putin_focus", "post_type", "sentiment", "support_for_putin", "criticism_of_putin", "trust_in_putin", "competence_of_putin",
#                                                 "state_of_war_for_russia", "responsibility_for_the_war", "course_of_action_for_russia"))) %>%
#   ggplot(aes(Variable, Alpha, fill = type)) +
#   geom_bar(stat = "identity") +
#   coord_flip() +
#   facet_wrap(~ name, ncol = 4) +
#   labs(x = "", y = "Cronbach's Alpha")+
#   theme_minimal() +
#   theme(legend.position = "none")
# 
# 
