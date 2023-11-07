
library(tidyverse)
library(ltm)

### Read in human coding

amalie <- openxlsx::read.xlsx("./Data/Validation_Samples/Fall_2023/CrossCoding/Subsample_Amalie.xlsx") %>%
  dplyr::select(-message) %>%
  rename_all(str_to_lower) %>%
  mutate_all(as.numeric) %>%
  mutate(coder = "amalie") %>%
  dplyr::select(-source)

eric <- openxlsx::read.xlsx("./Data/Validation_Samples/Fall_2023/CrossCoding/Subsample_Eric.xlsx") %>%
  dplyr::select(-message) %>%
  rename_all(str_to_lower) %>%
  mutate_all(as.numeric) %>%
  mutate(coder = "eric") %>%
  dplyr::select(-source)

jonas <- openxlsx::read.xlsx("./Data/Validation_Samples/Fall_2023/CrossCoding/Subsample_Jonas.xlsx")  %>%
  dplyr::select(-message) %>%
  rename_all(str_to_lower) %>%
  mutate_all(as.numeric) %>%
  mutate(coder = "jonas") %>%
  dplyr::select(-source)

### Read in ChatGPT coding. Generated from ChatGPT_LimitedSample.R

chatgpt <- readRDS("./Data/Validation_Samples/Fall_2023/completions.rds") %>%
  dplyr::select(rowid, War_mention, Putin_focus, Post_type, Sentiment, Support_for_Putin, Criticism_of_Putin, Trust_in_Putin, Competence_of_Putin,
                State_of_war_for_Russia, Responsibility_for_the_war, Course_of_action_for_Russia) %>%
  rename_all(str_to_lower) %>%
  mutate_all(as.numeric) %>%
  mutate(coder = "chatgpt") %>%
  mutate(rowid = as.numeric(str_squish(rowid)))

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

amalie_eric_chatgpt_list <- list()
amalie_jonas_chatgpt_list <- list()
eric_jonas_chatgpt_list <- list()

chatgpt_list <- list()

for (i in 1:length(coded_vars)) {

  amalie_1 <- pick_var(amalie, coded_vars[i])
  eric_1 <- pick_var(eric, coded_vars[i])
  jonas_1 <- pick_var(jonas, coded_vars[i])

  chatgpt_1 <- pick_var(chatgpt, coded_vars[i])

  #### HUMANS ####

  amalie_eric <- bind_rows(amalie_1, eric_1) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    dplyr::select(-rowid) %>%
    na.omit()

  amalie_jonas <- bind_rows(amalie_1, jonas_1) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    dplyr::select(-rowid) %>%
    na.omit()

  eric_jonas <- bind_rows(eric_1, jonas_1) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    dplyr::select(-rowid) %>%
    na.omit()

  eric_jonas_amalie <- bind_rows(eric_1, jonas_1, amalie_1) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    dplyr::select(-rowid) %>%
    na.omit()

  #### HUMANS AND AI ####

  amalie_eric_chatgpt <- bind_rows(amalie_1, eric_1, chatgpt_1) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    dplyr::select(-rowid) %>%
    na.omit()

  amalie_jonas_chatgpt_1 <- bind_rows(amalie_1, jonas_1, chatgpt_1) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    dplyr::select(-rowid) %>%
    na.omit()

  eric_jonas_chatgpt_1 <- bind_rows(eric_1, jonas_1, chatgpt_1) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    dplyr::select(-rowid) %>%
    na.omit()

  eric_jonas_amalie_chatgpt_1 <- bind_rows(eric_1, jonas_1, amalie_1, chatgpt_1) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    dplyr::select(-rowid) %>%
    na.omit()

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
  data.frame(do.call(rbind, eric_jonas_amalie_chatgpt_list)) %>% mutate(var = coded_vars)) %>%
  unnest(cols = c(alpha, n, p, standardized, name)) %>%
  spread(var, alpha) %>%
  dplyr::select(name, n,
                war_mention, putin_focus, post_type, sentiment, support_for_putin, criticism_of_putin, trust_in_putin, competence_of_putin,
                state_of_war_for_russia, responsibility_for_the_war, course_of_action_for_russia) %>%
  mutate(type = ifelse(name %in% c("amalie_eric", "amalie_jonas", "eric_jonas", "eric_jonas_amalie"), "Only humans",
                       ifelse(name %in% c("amalie_eric_chatgpt_1", "amalie_jonas_chatgpt_1", "eric_jonas_chatgpt_1", "eric_jonas_amalie_chatgpt_1"), "Humans and ChatGPT",
                              name)))

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
  group_by(`Coder combination`, Variable) %>% # name
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
                                        "amalie_eric_chatgpt_1", "amalie_jonas_chatgpt_1", "eric_jonas_chatgpt_1", "eric_jonas_amalie_chatgpt_1"))) %>%
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


