
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
    across(everything(), ~replace_na(.x, 99))
  )


eric <- openxlsx::read.xlsx("./Data/CrossCoding/Subsample_Eric.xlsx") %>%
  dplyr::select(-message) %>%
  rename_all(str_to_lower) %>%
  mutate_all(as.numeric) %>%
  mutate(coder = "eric") %>%
  dplyr::select(-source) %>% 
  mutate(
    across(everything(), ~replace_na(.x, 99))
  )


jonas <- openxlsx::read.xlsx("./Data/CrossCoding/Subsample_Jonas.xlsx")  %>%
  dplyr::select(-message) %>%
  rename_all(str_to_lower) %>%
  mutate_all(as.numeric) %>%
  mutate(coder = "jonas") %>%
  dplyr::select(-source) %>% 
  mutate(
    across(everything(), ~replace_na(.x, 99))
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
    across(everything(), ~replace_na(.x, 99))
  )


chatgpt_2 <- readRDS("./Data/completions_chatgpt_2.rds") %>%
  dplyr::select(rowid, War_mention, Putin_focus, Post_type, Sentiment, Support_for_Putin, Criticism_of_Putin, Trust_in_Putin, Competence_of_Putin,
                State_of_war_for_Russia, Responsibility_for_the_war, Course_of_action_for_Russia) %>%
  rename_all(str_to_lower) %>%
  mutate_all(as.numeric) %>%
  mutate(coder = "chatgpt_2") %>%
  mutate(rowid = as.numeric(str_squish(rowid)))%>% 
  mutate(
    across(everything(), ~replace_na(.x, 99))
  )


chatgpt_3 <- readRDS("./Data/completions_chatgpt_3.rds") %>%
  dplyr::select(rowid, War_mention, Putin_focus, Post_type, Sentiment, Support_for_Putin, Criticism_of_Putin, Trust_in_Putin, Competence_of_Putin,
                State_of_war_for_Russia, Responsibility_for_the_war, Course_of_action_for_Russia) %>%
  rename_all(str_to_lower) %>%
  mutate_all(as.numeric) %>%
  mutate(coder = "chatgpt_3") %>%
  mutate(rowid = as.numeric(str_squish(rowid)))%>% 
  mutate(
    across(everything(), ~replace_na(.x, 99))
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

  amalie_eric_chatgpt_list[[i]] <- cronbach.alpha(amalie_eric_chatgpt_1)
  amalie_jonas_chatgpt_list[[i]] <- cronbach.alpha(amalie_jonas_chatgpt_1)
  eric_jonas_chatgpt_list[[i]] <- cronbach.alpha(eric_jonas_chatgpt_1)
  eric_jonas_amalie_chatgpt_list[[i]] <- cronbach.alpha(eric_jonas_amalie_chatgpt_1)

  chatgpts_list[[i]] <- cronbach.alpha(chatgpts)

}


#### BINDING TOGTHER ####

cronbach_df <- bind_rows(
  data.frame(do.call(rbind, amalie_eric_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, amalie_jonas_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, eric_jonas_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, eric_jonas_amalie_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, amalie_eric_chatgpt_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, amalie_jonas_chatgpt_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, eric_jonas_chatgpt_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, eric_jonas_amalie_chatgpt_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, chatgpts_list)) %>% mutate(var = coded_vars)) %>%
  unnest(cols = c(alpha, n, p, standardized, name)) %>%
  spread(var, alpha) %>%
  dplyr::select(name, n,
                war_mention, putin_focus, post_type, sentiment, support_for_putin, criticism_of_putin, trust_in_putin, competence_of_putin,
                state_of_war_for_russia, responsibility_for_the_war, course_of_action_for_russia) %>%
  mutate(type = ifelse(name %in% c("amalie_eric", "amalie_jonas", "eric_jonas", "eric_jonas_amalie"), "Only humans",
                       ifelse(name %in% c("amalie_eric_chatgpt_1", "amalie_jonas_chatgpt_1", "eric_jonas_chatgpt_1", "eric_jonas_amalie_chatgpt_1"), "Humans and AI",
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

mean_alpha_pairs <- cronbach_df %>%
  rename(`Coder combination` = type) %>%
  gather(war_mention, putin_focus, post_type, sentiment, support_for_putin, criticism_of_putin, trust_in_putin, competence_of_putin,
         state_of_war_for_russia, responsibility_for_the_war, course_of_action_for_russia,
         key = "Variable", value = "Alpha") %>%
  group_by(`Coder combination`, Variable) %>%
  summarise(`Average Cronbach's Alpha` = mean(Alpha, na.rm = TRUE))

knitr::kable(mean_alpha_pairs) %>%
  kableExtra::kable_styling()

mean_alpha_per_question <- cronbach_df %>%
  gather(war_mention, putin_focus, post_type, sentiment, support_for_putin, criticism_of_putin, trust_in_putin, competence_of_putin,
         state_of_war_for_russia, responsibility_for_the_war, course_of_action_for_russia,
         key = "Variable", value = "Alpha") %>%
  group_by(type, Variable) %>%
  summarise(mean_cronbachs_alpha = mean(Alpha, na.rm = TRUE))

knitr::kable(mean_alpha_per_question) %>%
  kableExtra::kable_styling()


### Plot ###

alpha_per_var <- cronbach_df %>%
  mutate(type = factor(type)) %>%
  mutate(name = factor(name, levels = c("amalie_eric", "amalie_jonas", "eric_jonas", "eric_jonas_amalie",
                                        "amalie_eric_chatgpt_1", "amalie_jonas_chatgpt_1", "eric_jonas_chatgpt_1", "eric_jonas_amalie_chatgpt_1",
                                        "chatgpts"))) %>%
  gather(war_mention, putin_focus, post_type, sentiment, support_for_putin, criticism_of_putin, trust_in_putin, competence_of_putin,
         state_of_war_for_russia, responsibility_for_the_war, course_of_action_for_russia,
         key = "Variable", value = "Alpha") %>%
  mutate(Variable = factor(Variable, levels = c("war_mention", "putin_focus", "post_type", "sentiment", "support_for_putin", "criticism_of_putin", "trust_in_putin", "competence_of_putin",
                                                "state_of_war_for_russia", "responsibility_for_the_war", "course_of_action_for_russia"))) %>%
  ggplot(aes(Variable, Alpha, fill = type)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~ name, ncol = 4) +
  labs(x = "", y = "Cronbach's Alpha")+
  theme_minimal() +
  theme(legend.position = "none")


