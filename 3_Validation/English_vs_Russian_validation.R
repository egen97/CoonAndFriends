
library(tidyverse)

### English and Russian correlation

chat_ru <- read_rds("./Data/Chat testruns/chat_testrun_russian.rds") %>%
  rename_all(~str_c("ru_", .)) %>%
  select(ru_rowid, ru_opposition, ru_competence, ru_responsibility, ru_state_of_the_war, ru_response) %>%
  mutate(ru_opposition = factor(ru_opposition, level = c("1", "2", "3", "4", "5")),
         ru_competence = factor(ru_competence, level = c("1", "2", "3", "4", "5")),
         ru_responsibility = factor(ru_responsibility, level = c("0", "1", "2", "3")),
         ru_state_of_the_war = factor(ru_state_of_the_war, level = c("0", "1", "2", "3")),
         ru_response = factor(ru_response, level = c("0", "1", "2", "3"))) %>%
  rename(rowid = ru_rowid)

chat_en <- read_rds("./Data/Chat testruns/chat_testrun_english.rds") %>%
  rename_all(~str_c("en_", .)) %>%
  select(en_rowid, en_opposition, en_competence, en_responsibility, en_state_of_the_war, en_response) %>%
  mutate(en_opposition = factor(en_opposition, level = c("1", "2", "3", "4", "5")),
         en_competence = factor(en_competence, level = c("1", "2", "3", "4", "5")),
         en_responsibility = factor(en_responsibility, level = c("0", "1", "2", "3")),
         en_state_of_the_war = factor(en_state_of_the_war, level = c("0", "1", "2", "3")),
         en_response = factor(en_response, level = c("0", "1", "2", "3"))) %>%
  rename(rowid = en_rowid)


#### Confusion tables ####

confusions <- function(ru_variable, en_variable){

  caret::confusionMatrix(ru_variable,
                         en_variable,
                         dnn = c("Russian", "English"))

}

ggplot(as.data.frame(confusions(chat_ru$ru_opposition, chat_en$en_opposition)$table),
                aes(Russian, English, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  ggtitle("Opposition: Is the post opposed or supportive of Putin?") +
  scale_x_discrete(labels=c("strongly opposed","somewhat opposed","neither opposed nor supportive","somewhat supportive", "strongly supportive")) +
  scale_y_discrete(labels=c("strongly opposed","somewhat opposed","neither opposed nor supportive","somewhat supportive", "strongly supportive")) +
  theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=0.5))

ggplot(as.data.frame(confusions(chat_ru$ru_competence, chat_en$en_competence)$table),
                aes(Russian, English, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  ggtitle("Competence: Does the post portray Putin as incompetent or competent?") +
  scale_x_discrete(labels=c("strongly opposed","somewhat opposed","neither opposed nor supportive","somewhat supportive", "strongly supportive")) +
  scale_y_discrete(labels=c("strongly opposed","somewhat opposed","neither opposed nor supportive","somewhat supportive", "strongly supportive")) +
  theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=0.5))

ggplot(as.data.frame(confusions(chat_ru$ru_responsibility, chat_en$en_responsibility)$table),
       aes(Russian, English, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  ggtitle("Responsibility: Does the post assign responsibility for how the war is going to Putin?") +
  scale_x_discrete(labels=c("not applicable", "not responsible at all","somewhat responsible","fully responsible")) +
  scale_y_discrete(labels=c("not applicable", "not responsible at all","somewhat responsible","fully responsible")) +
  theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=0.5))

ggplot(as.data.frame(confusions(chat_ru$ru_state_of_the_war, chat_en$en_state_of_the_war)$table),
       aes(Russian, English, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  ggtitle("State of the war: How does the post describe the current state of the war for Russia?") +
  scale_x_discrete(labels=c("not applicable", "not responsible at all","somewhat responsible","fully responsible")) +
  scale_y_discrete(labels=c("not applicable", "not responsible at all","somewhat responsible","fully responsible")) +
  theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=0.5))

ggplot(as.data.frame(confusions(chat_ru$ru_response, chat_en$en_response)$table),
       aes(Russian, English, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  ggtitle("Response: How does the post want Putin to respond to the current state of the war?") +
  scale_x_discrete(labels=c("not applicable", "not responsible at all","somewhat responsible","fully responsible")) +
  scale_y_discrete(labels=c("not applicable", "not responsible at all","somewhat responsible","fully responsible")) +
  theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=0.5))





### Degree of difference on differently coded posts ###

chat_lang <- left_join(chat_ru, chat_en, join_by(rowid)) %>%
  filter(en_competence != ru_competence) %>%
  gather(ru_competence, en_competence,
         key = "language", value = "score")

chat_lang %>%
  ggplot(aes(score, factor(rowid), color = language)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#ba79b6", "#009194")) +
  labs(x = "", y = "") +
  geom_vline(xintercept = 3, alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "top")


