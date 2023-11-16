
library(tidyverse)
library(stopwords)
library(ggcorrplot)
library(fixest)
library(modelsummary)
library(kableExtra)

cleaned_1 <- read_rds("./Data/Telegrams/telegrams_cleaned_wartime_pasted.rds")
cleaned_2 <- read_rds("./Data/Telegrams/telegrams_cleaned_wartime_pasted_putin.rds")

putin_mentioned <- cleaned_1 %>%
  mutate(putin = ifelse(rowid %in% cleaned_2$rowid, 1, 0))

# tokens <- telegrams_cleaned %>%
#   unnest_tokens(input = message,
#                 output = token,
#                 token = "words")

#### Reactions and Putin-mentioning correlation #####

## Overview of reactions

top_reactions <- putin_mentioned %>%
  filter(date <= "2023-10-31") %>%
  select(source, id, date, putin, reaction) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  mutate(month = substr(date, 6, 7)) %>%
  unnest(cols = c(reaction)) %>% # Getting reactions
  mutate(count = as.numeric(count)) %>%
  group_by(`reaction.emoticon`, date, month, count, id, source) %>%
  summarise(putin = sum(putin, na.rm = T)) %>% # Summing up the number of reactions per putin-mention
  ungroup()

# Overview of all reactions that exists in the data
all_reactions <- top_reactions %>%
  count(`reaction.emoticon`) %>%
  na.omit() %>%
  arrange(desc(n))

## Finding mentionings of Putin

# stopwords <- tibble(token = stopwords("ru", source = "snowball"))

# putin_times_per_post <- tokens %>%
#   select(id, source, token) %>%
#   # Preprocessing
#   mutate(token = str_remove_all(token, "[[:punct:]]+?")) %>% # Removing commas and dots and other punctuation
#   filter(!str_detect(token, "[0-9]+?")) %>% # Removing numbers
#   na.omit() %>% # Removing places with missing tokens
#   anti_join(stopwords, by = join_by(token)) %>% # Removing Russian stopwords
#   # Finding tokens of Putin
#   mutate(putin = ifelse(str_detect(token, "путин"), 1, 0)) %>%
#   # Counting number of times token occurs per post
#   group_by(id) %>%
#   summarise(putin = sum(putin)) %>%
#   ungroup()

# telegrams_putin <- telegrams_cleaned %>% # Starting with full dataset to make sure all rows are included
#   filter(date <= "2023-10-31") %>%
#   select(id, source, reaction) %>%
#   # Joining with mentionings of Putin before unnesting so not to create duplicate ids
#   left_join(putin_times_per_post, by = join_by(id)) %>%
#   # If there is NA, it means there were no mentionings of Putin to count, so make 0
#   mutate(putin = ifelse(is.na(putin), 0, putin)) %>%
#   # Unnest reactions
#   unnest(cols = c(reaction)) %>%
#   # Removing NAs from count in order to remove all sources that didn't have reactions in the API
#   #filter(reaction != "no_reaction_in_api") %>%
#   select(-chosen, -reaction) %>% # Not sure what this variable is beyond being a logical TRUE/FALSE
#   # If count is NA, it means there were no reactions to the post
#   mutate(count = ifelse(is.na(count), 0, count))

reaction_filter <- top_reactions %>%
  filter(`reaction.emoticon` %in% c(all_reactions$`reaction.emoticon`[1:20])) %>%
  pivot_wider(names_from = "reaction.emoticon", values_from = "count", values_fill = 0) %>%
  mutate(putin = factor(putin))

reaction_filter <- reaction_filter %>%
  left_join(putin_mentioned %>% select(id, source, views), by = join_by(id, source)) %>%
  unnest()

# reactions_data <- reaction_filter %>%
#   full_join(putin_mentioned %>%
#               mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
#               select(-reaction, -putin),
#             by = join_by(date, id, source))
#
# saveRDS(reactions_data, file = "./Data/Validation_Samples/Winter_2023/Reactions_data_16_11_23.rds")

colnames(reaction_filter) <- c("date", "month", "id", "source", "putin",
                               "whiteheart", "heartfire", "celebrate", "thumb_up", "thumb_down", "clap", "poop", "fire",
                               "grin", "cry", "despair", "pray", "think", "clown", "lol", "stareyed", "angry", "puke",
                               "mindblown", "love", "views")

mod1 <- feols(whiteheart ~ putin + views | month + source,
              data = reaction_filter)

mod2 <- feols(heartfire ~ putin + views | month + source,
              data = reaction_filter)

mod3 <- feols(celebrate ~ putin + views | month + source,
              data = reaction_filter)

mod4 <- feols(thumb_up ~ putin + views | month + source,
              data = reaction_filter)

mod5 <- feols(thumb_down ~ putin + views | month + source,
              data = reaction_filter)

mod6 <- feols(clap ~ putin + views | month + source,
              data = reaction_filter)

mod7 <- feols(poop ~ putin + views | month + source,
              data = reaction_filter)

mod8 <- feols(fire ~ putin + views | month + source,
              data = reaction_filter)

mod9 <- feols(grin ~ putin + views | month + source,
              data = reaction_filter)

mod10 <- feols(cry ~ putin + views | month + source,
              data = reaction_filter)

mod11 <- feols(despair ~ putin + views | month + source,
              data = reaction_filter)

mod12 <- feols(pray ~ putin + views | month + source,
              data = reaction_filter)

mod13 <- feols(think ~ putin + views | month + source,
              data = reaction_filter)

mod14 <- feols(clown ~ putin + views | month + source,
              data = reaction_filter)

mod15 <- feols(lol ~ putin + views | month + source,
              data = reaction_filter)

mod16 <- feols(stareyed ~ putin + views | month + source,
               data = reaction_filter)

mod17 <- feols(angry ~ putin + views | month + source,
               data = reaction_filter)

mod18 <- feols(puke ~ putin + views | month + source,
               data = reaction_filter)

mod19 <- feols(mindblown ~ putin + views | month + source,
               data = reaction_filter)

mod20 <- feols(love ~ putin + views | month + source,
               data = reaction_filter)

models <- list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, mod13, mod14, mod15, mod16, mod17, mod18, mod19, mod20)
names(models) <- c("White heart", "Heartfire", "Celebrate", "Thumb up", "Thumb down", "Clap", "Poop", "Fire", "Grin", "Cry", "Despair",
                   "Pray", "Think", "Clown", "LOL", "Stareyed", "Angry", "Puke", "Mindblown", "Love")

tabsummary <- modelsummary(models,
                           fmt = 3,
                           #output = "latex",
                           stars = TRUE,
                           statistic = c("{std.error}"),
                           gof_omit = 'AIC|BIC|Within|Std.Errors|FE',
                           notes = c("Fixed effects: Source and month"))

tabsummary %>%
  kable_styling(font_size = 12, full_width = FALSE)

model_names <- c("White heart", "Heartfire", "Celebrate", "Thumb up", "Thumb down", "Clap", "Poop", "Fire", "Grin", "Cry", "Despair",
                 "Pray", "Think", "Clown", "LOL", "Stareyed", "Angry", "Puke", "Mindblown", "Love")

mods <- lapply(models, broom::tidy, conf.int = TRUE)
mods <- map2(mods, model_names, ~mutate(.x, Model = .y)) %>%
  bind_rows()

p1 <- mods %>%
  filter(!str_detect(term, "views")) %>%
  mutate(term = ifelse(str_detect(term, "putin1"), "Putin", term)) %>%
  ggplot(aes(x = estimate, y = Model)) +
  geom_point(aes(x = estimate, y = Model, color = term, shape = term), position = position_dodge(width = 1/2), color = "black") +
  geom_linerange(aes(y = Model,
                     xmin = conf.low,
                     xmax = conf.high,
                     color = term), position = position_dodge(width = 1/2), linewidth = 0.5, alpha = 0.8) +
  ggtitle("Reactions to post mentioning Putin") +
  scale_color_manual(values = rep("darkgrey", 13)) +
  labs(x="Estimate", y="", colour="Model") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  theme(legend.position = "none")


ggsave(p1, file = "./Figures/Reactions_Coefplot.pdf", width = 12, height = 10, dpi = 150)


#### BIVARIATE CORRELATION ####


data_cor <- round(cor(reaction_filter %>% select(4:length(reaction_filter)),
                      reaction_filter$putin,
                      use = "pairwise.complete.obs"), 3)

reaction_putin_correlation <- ggcorrplot(data_cor, lab = TRUE)

ggsave(reaction_putin_correlation, file = "./Plots/Reactions_Putin_Correlation.png",
       width = 12, height = 10, dpi = 150, units = "in", device= "png")

# Correlation between all reactions
corr <- round(cor(reaction_filter %>% select(4:length(reaction_filter)),
                  use = "pairwise.complete.obs"), 1)

ggcorrplot(corr, method = "circle",
           type = "lower",
           colors = c("#6D9EC1", "white", "#E46726"))

