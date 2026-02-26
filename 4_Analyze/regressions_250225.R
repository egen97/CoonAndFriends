### PRE AMBLE AND MERGE ####

pacman::p_load(tidyverse, fixest, lubridate, zoo)


depvars <- readRDS("Data/coded_posts_2025.rds")

area_data <- readRDS("Data/area_2025.rds")

ler_data <- readRDS("Data/causalties_ler_data.rds")

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

complete <- complete %>%
  left_join(
    ler_data,
    by = c("date" = "week")
  )



# It is vital for further analysis that all data is aggregated to the week level. Currently, dependent variables, as well as the area under occupation data, is at the day-by-day level, making the regression models unstable.
# This may have adverse effects on the presented results.



