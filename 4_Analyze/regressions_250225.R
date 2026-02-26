### PRE AMBLE  ####

pacman::p_load(tidyverse, fixest, lubridate)

day_data <- readRDS("Data/day_data.rds")
week_data <- readRDS("Data/week_data.rds")

week_data$month <- paste(month(week_data$week), year(week_data$week), sep = "-")
day_data$month <- paste(month(day_data$week), year(day_data$week), sep = "-")

day_data$impression_putin <- as.numeric(day_data$impression_putin)
### Week models ###

impression_simple <- feols(impression_putin ~ ln_russian_ler | month, data = week_data %>% filter(!if_any(everything(),~is.nan(.x))))

summary(impression_simple)


### Day models ####

impression_simple <- feols(impression_putin ~ ln_russian_ler | month, data = day_data %>% filter(!if_any(everything(),~is.nan(.x))))

summary(impression_simple)
