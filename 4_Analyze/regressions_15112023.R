#### PRE AMBLE ####
pacman::p_load(tidyverse, coefplot)

zero_log <- function(x){
  x <- ifelse(x == 0, 0, log(x))
  return(x)
}


map_lm <- function( ...){

  st_formula <- function(dep, indp, st_formula_vector = 1){
    st_formula_vector <- trimws(st_formula_vector)
    form <- as.formula(paste(dep, "~ ", paste(indp, "+"), st_formula_vector))
    return(form)
  }

  x <- lm(st_formula(...),
          data = modData
  )
  return(x)
}


#### Wrangle Tangle Dangle ####

indep <- c("ratio_ru_ua", "actuall_area", "area_change" , "russia_total", "log(russia_total+1)", "causalties", "log(causalties+1)",  "causalties*actuall_area")


orynx <- readRDS("Data/orynx/2023-09-07.rds")

posts <- readRDS("Data/Coded_posts_13_11_2023.rds")
losses <- readRDS("Data/russian_losses.rds")
occupied_area <- readRDS("Data/occupied_area.rds")

losses <- losses %>%
  select(date, causalties)

posts <- posts %>%
  mutate(date = as.Date(date))



modData <- posts %>%
  full_join(orynx, by = "date") %>%
  full_join(occupied_area, by = "date") %>%
  full_join(losses, by = "date")

modData <- modData %>%
  ungroup() %>%
  arrange(date) %>%
  fill(starts_with("russ"), .direction = "down")



modData <- modData %>%
  mutate(area_change = actuall_area-lag(actuall_area))

modData <- modData %>%
  mutate(days_since_invasion = date-as.Date("2022-02-24"))

modData <- modData %>%
  mutate(caus_1000 = causalties/1000)

modData <- modData %>%
  mutate(weeks_since_invasion = days_since_invasion/7)

modData <- modData %>%
  mutate(months_since_invasion = weeks_since_invasion/4)


#### Models ####



support_models <- list()

support_models <- map2("Support_for_Putin", indep, st_formula_vector = "months_since_invasion", map_lm)



multiplot(support_models, outerCI = 0, innerCI = 1.96, intercept = FALSE) +
  labs(title = "Support") +
  ggthemes::theme_excel_new()

summary(support_models[[1]])


stargazer::stargazer(support_models, type = "text", dep.var.caption = "Support")


critisism_models <- list()

critisism_models <- map2("Criticism_of_Putin", indep, st_formula_vector = "months_since_invasion", map_lm)



multiplot(critisism_models, outerCI = 0, innerCI = 1.96, intercept = FALSE) +
  labs(title = "Criticism") +
  ggthemes::theme_excel_new()



stargazer::stargazer(critisism_models, type = "text", dep.var.caption = "Criticism")





trust_models <- list()

trust_models <- map2("Trust_in_Putin", indep, st_formula_vector = "months_since_invasion", map_lm)



multiplot(trust_models, outerCI = 0, innerCI = 1.96, intercept = FALSE) +
  labs(title = "Trust") +
  ggthemes::theme_excel_new()



stargazer::stargazer(critisism_models, type = "text", dep.var.caption = "Criticism")




