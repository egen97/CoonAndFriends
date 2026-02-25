### PRE AMBLE AND MERGE ####

pacman::p_load(tidyverse, fixest, lubridate, zoo)


depvars <- readRDS("Data/coded_posts_2025.rds")

area_data <- readRDS("Data/area_2025.rds")

area_data$date <- as.Date(area_data$date)

depvars$date <- as.Date(depvars$date)


area_data$two_week <- rollmean(area_data$occupied, 14, fill = "extend")

complete <- depvars %>%
  full_join(area_data, by = "date")

complete <- complete %>%
  mutate(
    time = paste(month(date), year(date), sep = "-")
  )

complete$criticism_putin <- as.numeric(complete$criticism_putin)

complete <- complete %>%
  mutate(
    time_since_start = as.numeric(date - as.Date("2022-01-01"))
  )

### Models ###

simple <- lm(support_putin ~ occupied, data = complete)

summary(simple)

criticism <- lm(criticism_putin ~ occupied, data = complete)
summary(criticism)

clustered_critisism <- feols(criticism_putin ~ occupied, data = complete, cluster = "date")

summary(clustered_critisism)

clustered_critisism_fe <- feols(criticism_putin ~ occupied|time, data = complete)

summary(clustered_critisism_fe)


## Rolling average ###

clustered_critisism_fe <- feols(criticism_putin ~ two_week|time, data = complete)

summary(clustered_critisism_fe)


