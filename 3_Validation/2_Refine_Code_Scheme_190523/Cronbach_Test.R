
library(tidyverse)
library(ltm)

amalie <- openxlsx::read.xlsx("./3_Validation/3_Intercoder_Reliability/human_coding/Subsample_Amalie.xlsx") %>%
  dplyr::select(-translatedText) %>%
  rename_all(str_to_lower) %>%
  mutate_all(as.numeric) %>%
  mutate(coder = "amalie") %>%
  rename(competence = competent,
         responsibility = reponsibility)

eric <- openxlsx::read.xlsx("./3_Validation/3_Intercoder_Reliability/human_coding/Subsample_Eric.xlsx") %>%
  dplyr::select(-translatedText) %>%
  rename_all(str_to_lower) %>%
  mutate_all(as.numeric) %>%
  mutate(coder = "eric") %>%
  rename(competence = competent,
         responsibility = reponsibility)

solveig <- openxlsx::read.xlsx("./3_Validation/3_Intercoder_Reliability/human_coding/Subsample_Solveig.xlsx") %>%
  dplyr::select(-translatedText) %>%
  rename_all(str_to_lower) %>%
  mutate_all(as.numeric) %>%
  mutate(coder = "solveig") %>%
  rename(competence = competent,
         responsibility = reponsibility)

jonas <- openxlsx::read.xlsx("./3_Validation/3_Intercoder_Reliability/human_coding/Subsample_Jonas.xlsx")  %>%
  dplyr::select(-translatedText) %>%
  rename_all(str_to_lower) %>%
  mutate_all(as.numeric) %>%
  mutate(coder = "jonas") %>%
  rename(competence = competent,
         responsibility = reponsibility)

chatgpt1 <- readRDS("./3_Validation/3_Intercoder_Reliability/completions_all.rds") %>%
  dplyr::select(rowid, ukraine, west, putin, support, sentiment, trust, competence, state_of_war, responsibility, response) %>%
  rename_all(str_to_lower) %>%
  mutate_all(as.numeric) %>%
  mutate(coder = "chatgpt_1") %>%
  mutate(rowid = as.numeric(str_squish(rowid)))

chatgpt2 <- readRDS("./3_Validation/3_Intercoder_Reliability/secondround/completions_df.rds") %>%
  dplyr::select(rowid, ukraine, west, putin, support, sentiment, trust, competence, state_of_war, responsibility, response) %>%
  rename_all(str_to_lower) %>%
  mutate_all(as.numeric) %>%
  mutate(coder = "chatgpt_2") %>%
  mutate(rowid = as.numeric(str_squish(rowid)))

chatgpt3 <- readRDS("./3_Validation/3_Intercoder_Reliability/thirdround/completions_df_final.rds") %>%
  dplyr::select(rowid, ukraine, west, putin, support, sentiment, trust, competence, state_of_war, responsibility, response) %>%
  rename_all(str_to_lower) %>%
  mutate_all(as.numeric) %>%
  mutate(coder = "chatgpt_3") %>%
  mutate(rowid = as.numeric(str_squish(rowid)))

chatgpt4 <- readRDS("./3_Validation/3_Intercoder_Reliability/fourthrun/completions_df_all.rds") %>%
  dplyr::select(rowid, ukraine, west, putin, support, sentiment, trust, competence, state_of_war, responsibility, response) %>%
  rename_all(str_to_lower) %>%
  mutate_all(as.numeric) %>%
  mutate(coder = "chatgpt_4") %>%
  mutate(rowid = as.numeric(str_squish(rowid)))



pick_var <- function(dat, var){

  var_q <- enquo(var)
  dat %>%
    dplyr::select(rowid, coder, !!var_q)

}

coded_vars <- c("ukraine", "west", "putin", "support", "sentiment", "trust", "competence", "state_of_war", "responsibility", "response")

amalie_eric_list <- list()
amalie_jonas_list <- list()
eric_solveig_list <- list()
jonas_solveig_list <- list()

amalie_eric_chatgpt_list <- list()
amalie_jonas_chatgpt_list <- list()
eric_solveig_chatgpt_list <- list()
jonas_solveig_chatgpt_list <- list()

chatgpts_list <- list()

for (i in 1:length(coded_vars)) {

  amalie_1 <- pick_var(amalie, coded_vars[i])
  eric_1 <- pick_var(eric, coded_vars[i])
  solveig_1 <- pick_var(solveig, coded_vars[i])
  jonas_1 <- pick_var(jonas, coded_vars[i])

  chatgpt_1 <- pick_var(chatgpt1, coded_vars[i])
  chatgpt_2 <- pick_var(chatgpt2, coded_vars[i])
  chatgpt_3 <- pick_var(chatgpt3, coded_vars[i])
  chatgpt_4 <- pick_var(chatgpt4, coded_vars[i])

  #### HUMANS ####

  amalie_eric <- bind_rows(amalie_1, eric_1) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    dplyr::select(-rowid) %>%
    na.omit()

  amalie_jonas <- bind_rows(amalie_1, jonas_1) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    dplyr::select(-rowid) %>%
    na.omit()

  eric_solveig <- bind_rows(eric_1, solveig_1) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    dplyr::select(-rowid) %>%
    na.omit()

  jonas_solveig <- bind_rows(jonas_1, solveig_1) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    dplyr::select(-rowid) %>%
    na.omit()

  #### HUMANS AND AI ####

  amalie_eric_chatgpt_1 <- bind_rows(amalie_1, eric_1, chatgpt_1) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    dplyr::select(-rowid) %>%
    na.omit()

  amalie_jonas_chatgpt_1 <- bind_rows(amalie_1, jonas_1, chatgpt_1) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    dplyr::select(-rowid) %>%
    na.omit()

  eric_solveig_chatgpt_1 <- bind_rows(eric_1, solveig_1, chatgpt_1) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    dplyr::select(-rowid) %>%
    na.omit()

  jonas_solveig_chatgpt_1 <- bind_rows(jonas_1, solveig_1, chatgpt_1) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    dplyr::select(-rowid) %>%
    na.omit()

  #### AI ####

  chatgpts <- bind_rows(chatgpt_1, chatgpt_2, chatgpt_3, chatgpt_4) %>%
    pivot_wider(names_from = "coder", values_from = !!coded_vars[i]) %>%
    dplyr::select(-rowid) %>%
    na.omit()

  #### CALCULATE ####

  print(coded_vars[i])

  amalie_eric_list[[i]] <- cronbach.alpha(amalie_eric)
  amalie_jonas_list[[i]] <- cronbach.alpha(amalie_jonas)
  eric_solveig_list[[i]] <- cronbach.alpha(eric_solveig)
  jonas_solveig_list[[i]] <- cronbach.alpha(jonas_solveig)

  amalie_eric_chatgpt_list[[i]] <- cronbach.alpha(amalie_eric_chatgpt_1)
  amalie_jonas_chatgpt_list[[i]] <- cronbach.alpha(amalie_jonas_chatgpt_1)
  eric_solveig_chatgpt_list[[i]] <- cronbach.alpha(eric_solveig_chatgpt_1)
  jonas_solveig_chatgpt_list[[i]] <- cronbach.alpha(jonas_solveig_chatgpt_1)

  chatgpts_list[[i]] <- cronbach.alpha(chatgpts)

}


#### BINDING TOGTHER ####

intercoder_df <- bind_rows(
  data.frame(do.call(rbind, amalie_eric_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, amalie_jonas_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, eric_solveig_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, jonas_solveig_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, amalie_eric_chatgpt_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, amalie_jonas_chatgpt_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, eric_solveig_chatgpt_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, jonas_solveig_chatgpt_list)) %>% mutate(var = coded_vars),
  data.frame(do.call(rbind, chatgpts_list)) %>% mutate(var = coded_vars)
) %>%
  unnest(cols = c(alpha, n, p, standardized, name)) %>%
  spread(var, alpha) %>%
  dplyr::select(name, n, ukraine, west, putin, support, sentiment, trust, competence, state_of_war, responsibility, response) %>%
  mutate(type = ifelse(name %in% c("amalie_eric", "amalie_jonas", "eric_solveig", "jonas_solveig"), 1,
                       ifelse(name %in% c("amalie_eric_chatgpt_1", "amalie_jonas_chatgpt_1", "eric_solveig_chatgpt_1", "jonas_solveig_chatgpt_1"), 2,
                              ifelse(name %in% c("chatgpts"), 3,
                                     name))))

intercoder_df %>%
  group_by(type) %>%
  gather(ukraine, west, putin, support, sentiment, trust, competence, state_of_war, responsibility, response,
         key = "Variable", value = "Alpha") %>%
  summarise(mean_cronbachs_alpha = mean(Alpha, na.rm = TRUE))

intercoder_df %>%
  group_by(type, name) %>%
  gather(ukraine, west, putin, support, sentiment, trust, competence, state_of_war, responsibility, response,
         key = "Variable", value = "Alpha") %>%
  summarise(mean_cronbachs_alpha = mean(Alpha, na.rm = TRUE))


intercoder_df %>%
  mutate(type = factor(type)) %>%
  mutate(name = factor(name, levels = c("amalie_eric", "amalie_jonas", "eric_solveig", "jonas_solveig",
                                        "amalie_eric_chatgpt_1", "amalie_jonas_chatgpt_1", "eric_solveig_chatgpt_1", "jonas_solveig_chatgpt_1",
                                        "chatgpts"))) %>%
  gather(ukraine, west, putin, support, sentiment, trust, competence, state_of_war, responsibility, response,
         key = "Variable", value = "Alpha") %>%
  mutate(Variable = factor(Variable, levels = c("ukraine", "west", "putin",
                                                "support", "sentiment",
                                                "trust", "competence",
                                                "state_of_war", "responsibility", "response"))) %>%
  ggplot(aes(Variable, Alpha, fill = type)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~ name, ncol = 4) +
  labs(x = "", y = "Cronbach's Alpha")+
  theme_minimal() +
  theme(legend.position = "none")


intercoder_df %>%
  filter(type == 1) %>%
  gather(ukraine, west, putin, support, sentiment, trust, competence, state_of_war, responsibility, response,
         key = "Variable", value = "Alpha") %>%
  ggplot(aes(Variable, Alpha, fill = name)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~ name)

intercoder_df %>%
  filter(type == 2) %>%
  gather(ukraine, west, putin, support, sentiment, trust, competence, state_of_war, responsibility, response,
         key = "Variable", value = "Alpha") %>%
  ggplot(aes(Variable, Alpha, fill = name)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~ name)

intercoder_df %>%
  filter(type == 3) %>%
  gather(ukraine, west, putin, support, sentiment, trust, competence, state_of_war, responsibility, response,
         key = "Variable", value = "Alpha") %>%
  ggplot(aes(Variable, Alpha, fill = name)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~ name)

