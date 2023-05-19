
library(tidyverse)
library(ltm)

amalie <- openxlsx::read.xlsx("./3_Validation/Coding_19_05_2023/Coding_sheet_Amalie.xlsx") %>%
  mutate(rowid = as.numeric(rowid)) %>%
  rename_all(str_to_lower)%>%
  mutate_all(as.numeric) %>% mutate(coder = "amalie")

eric <- openxlsx::read.xlsx("./3_Validation/Coding_19_05_2023/Coding_sheet_Eric.xlsx") %>%
  mutate(rowid = as.numeric(rowid)) %>%
  rename_all(str_to_lower)%>%
  mutate_all(as.numeric) %>% mutate(coder = "eric")

solveig <- openxlsx::read.xlsx("./3_Validation/Coding_19_05_2023/Coding_sheet_Solveig.xlsx") %>%
  mutate(rowid = as.numeric(rowid))  %>%
  rename_all(str_to_lower)%>%
  mutate_all(as.numeric) %>% mutate(coder = "solveig")

chatgpt <- openxlsx::read.xlsx("./3_Validation/Coding_19_05_2023/Coding_sheet_ChatGPT.xlsx")  %>%
  mutate(rowid = as.numeric(rowid)) %>%
  dplyr::select(rowid, support, sentiment, competence, praise, trust, approve, favorable, state_of_war, response, responsibility) %>%
  rename_all(str_to_lower) %>%
  mutate_all(as.numeric)%>% mutate(coder = "chatgpt")

pick_var <- function(dat, var){

  var_q <- enquo(var)
  dat %>%
    dplyr::select(rowid, coder, !!var_q)

}

vars <- colnames(amalie)[5:14]

allchecks <- list()

for (i in 1:length(vars)) {

  amalies <- pick_var(amalie, vars[i])
  erics <- pick_var(eric, vars[i])
  solveigs <- pick_var(solveig, vars[i])
  chats <- pick_var(chatgpt, vars[i])

  telegram_questions <- bind_rows(amalies, erics, solveigs, chats) %>%
    pivot_wider(names_from = "coder", values_from = !!vars[i]) %>%
    drop_na(chatgpt) %>%
    dplyr::select(-rowid)

  print(vars[i])
  print(cronbach.alpha(telegram_questions))

}

