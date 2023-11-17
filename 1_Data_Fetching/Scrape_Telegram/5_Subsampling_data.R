
library(tidyverse)
library(data.table)

#### 1a. Main dataset ####

telegrams_cleaned_a <- readRDS("./Data/Telegrams/telegrams_cleaned.rds")

nrow(telegrams_cleaned_a) # 2454540

#### 1b. Removing Soloviev ####

# telegrams_cleaned_b <- telegrams_cleaned_a %>%
#   filter(source != "SolovievLive")
#
# nrow(telegrams_cleaned_b) # 676014


#### 2. Removing empty posts ####
telegrams_cleaned2 <- telegrams_cleaned_a %>%
  mutate(message = ifelse(message == "", NA, message)) %>%
  drop_na(message) %>%
  distinct(.keep_all = TRUE)

# saveRDS(telegrams_cleaned2, file = "./Data/Telegrams/telegrams_cleaned2.rds")

nrow(telegrams_cleaned2) # 1947691


#### 3. Picking only the posts that occur in the wartime period ####
telegrams_cleaned_wartime <- telegrams_cleaned2 %>%
  filter(date >= "2022-01-01") %>%
  filter(date <= "2023-10-31")

# saveRDS(telegrams_cleaned_wartime, file = "./Data/Telegrams/telegrams_cleaned_wartime.rds")

nrow(telegrams_cleaned_wartime) # 1182268


#### 4. Pasting messages ####

df <- telegrams_cleaned_wartime %>%
  mutate(putin = ifelse(str_detect(message, "(?i)путин[а-я]*|владимир\\s*владимирович\\s*путин|владимир\\s*путин"), 1, 0),
         date_2 = if_else(putin == 1, date, NA),
         date_3 = if_else(putin == 1, date, NA)) %>%
  group_by(source) %>%
  arrange(desc(date))

a <- 1
b <- 1

while (a > 0 | b > 0) {

  a1 <- sum(is.na(df$date_2))
  b1 <- sum(is.na(df$date_3))

  df <- df %>%
    select(source, id, date, putin, message, date_2, date_3) %>%
    group_by(source) %>%
    mutate(date_2 = ifelse(is.na(date_2), lag(date_2, 1), date_2),
           date_3 = ifelse(is.na(date_3), lead(date_3, 1), date_3))

  a <- a1 - sum(is.na(df$date_2))
  b <- b1 - sum(is.na(df$date_3))

}

gc()

df <- df %>%
  ungroup()

bloggers <- table(df$source) %>%
  rownames()

paste_id_dfs <- list()

for (i in 1:length(bloggers)) {

  paste_id_dfs[[i]] <- df %>%
    filter(source == bloggers[i]) %>%
    arrange(desc(date)) %>%
    select(source, id, date, putin, message, date_2, date_3) %>%
    group_by(source) %>%
    mutate(timediff_lag = difftime(date, date_2),
           timediff_lead = difftime(date, date_3)) %>%
    mutate(timediff_lag_tag = ifelse(lead(putin) == 1 & timediff_lag >= -60 & !is.na(timediff_lag), 1, 0)) %>%
    mutate(timediff_lead_tag = ifelse(lag(putin) == 1 & timediff_lead <= 60 & !is.na(timediff_lead), 1, 0)) %>%
    mutate(pastetogether = ifelse(timediff_lag_tag == 1 | timediff_lead_tag == 1, 1, 0)) %>%
    ungroup()

  message("Finished blogger ", bloggers[i])

}

df2 <- bind_rows(paste_id_dfs) %>%
  mutate(count = rleid(pastetogether))

df2 %>%
  select(timediff_lag, source, putin) %>%
  rename(timediff = timediff_lag) %>%
  bind_rows(df2 %>% select(timediff_lead, source, putin) %>% rename(timediff = timediff_lead)) %>%
  ggplot(aes(timediff , fill = source))+
  geom_histogram(bins = 1000) +
  ggtitle("Full sample: Distribution of posts and time since previous and next post") +
  theme_classic() +
  theme(legend.position = "bottom")

evens <- function(x) subset(x, x %% 2 == 0)

df3 <- df2 %>%
  filter(count %in% evens(count)) %>%
  group_by(count) %>%
  mutate(postno = row_number()) %>%
  mutate(message = paste0("_", message)) %>%
  mutate(message2 = paste(message, collapse = " \n\n ")) %>% # posts, when they are pasted together, are now simply pasted using lineshift.
  ungroup()

df3 %>%
  select(timediff_lag, source, putin) %>%
  rename(timediff = timediff_lag) %>%
  bind_rows(df3 %>% select(timediff_lead, source, putin) %>% rename(timediff = timediff_lead)) %>%
  ggplot(aes(timediff , fill = source))+
  geom_histogram(bins = 1000) +
  ggtitle("Pasted sample: Distribution of posts and time since previous and next post") +
  theme_classic() +
  theme(legend.position = "bottom")

df3 <- df3 %>%
  select(source, id, date, message2)

telegrams_cleaned_wartime_pasted <- left_join(df2, df3, by = join_by(source, id, date)) %>%
  mutate(pasted = ifelse(!is.na(message2), 1, 0)) %>%
  mutate(message = ifelse(is.na(message2), message, message2)) %>%
  select(source, id, date, putin, count, message) %>%
  distinct(source, message, .keep_all = TRUE) %>%
  left_join(telegrams_cleaned_wartime %>% select(-message, -date), by = c("source", "id")) %>%
  select(-count)

gc()

## Test that it went well
# df %>%
#   mutate(nchar1 = nchar(message),
#          nchar2 = nchar(message2)) %>%
#   filter(nchar1 != nchar2)

saveRDS(telegrams_cleaned_wartime_pasted, file = "./Data/Telegrams/telegrams_cleaned_wartime_pasted.rds")

nrow(telegrams_cleaned_wartime_pasted) # 1144182


#### 5. Putin messages ####

# https://cooljugator.com/run/%D0%BF%D1%83%D1%82%D0%B8%D0%BD

telegrams_cleaned_wartime_pasted_putin <- telegrams_cleaned_wartime_pasted %>%
  filter(str_detect(message, "(?i)путин[а-я]*|владимир\\s*владимирович\\s*путин|владимир\\s*путин"))

saveRDS(telegrams_cleaned_wartime_pasted_putin, file = "./Data/Telegrams/telegrams_cleaned_wartime_pasted_putin.rds")

nrow(telegrams_cleaned_wartime_pasted_putin) # 39015

#### Plotting row reduction ####

options(scipen = 999)

# "Removed Soloviev",
sample_reduction <- tibble(type = c("Original", "Removed missing", "Wartime posts", "Pasted", "Putin mentioned"),
       number = c(nrow(telegrams_cleaned_a), nrow(telegrams_cleaned2), nrow(telegrams_cleaned_wartime), nrow(telegrams_cleaned_wartime_pasted), nrow(telegrams_cleaned_wartime_pasted_putin))) %>%
  mutate(type = factor(type, levels = c("Original", "Removed missing", "Wartime posts", "Pasted", "Putin mentioned")))

sample_reduction_plot <- sample_reduction %>%
  ggplot(aes(type, number)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Number of posts") +
  theme_classic()

ggsave(sample_reduction_plot, file = "./Figures/sample_reduction_plot.pdf", width = 8, height = 8)

sample_reduction %>%
  knitr::kable("latex") %>%
  kableExtra::kable_styling()

#### Stats ####

telegrams_cleaned_wartime_pasted_putin %>%
  group_by(source) %>%
  pull(source) %>%
  unique()

sample_reduction

telegrams_cleaned_wartime_pasted %>%
  group_by(putin) %>%
  count() %>%
  spread(putin, n) %>%
  mutate(share_putin = `1`/(`1`+`0`)*100)

# Time frame:
### 2022-01-01
### 2023-10-31

