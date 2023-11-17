#### PRE AMBLE ####

pacman::p_load(tidyverse, coefplot, ThemePark)

mentioned <- readRDS("Data/mentioned_17112023.rds")
mod_data <- readRDS("Data/mod_data_17112023.rds")
reactions <- readRDS("Data/reactions_17112023.rds")

zero_log <- function(x){
  x <- ifelse(x == 0, 0, log(x))
  return(x)
}


map_lm <- function( data, ...){

  st_formula <- function(dep, indp, st_formula_vector = 1){
    st_formula_vector <- trimws(st_formula_vector)
    form <- as.formula(paste(dep, "~ ", paste(indp, "+"), st_formula_vector))
    return(form)
  }

  x <- lm(st_formula(...),
          data = data
  )
  return(x)
}


#### MENTIONED REGRESSIONS ####


indep <- c("russia_total", "change_3", "ratio_ru_ua", "actuall_area", "causalties",
           "causalties*actuall_area", "ratio_ru_ua*actuall_area", "causalties*area_change",
           "ratio_ru_ua*area_change")

mentioned_models <- list()

mentioned_models <- map2("putin", indep, st_formula_vector = "months", map_lm, data = mentioned)

multiplot(mentioned_models[6:9], outerCI = 0, innerCI = 1.96, intercept = FALSE) +
  labs(title = "Mentioned") +
  ggthemes::theme_excel_new()


multiplot(mentioned_models[8], outerCI = 0, innerCI = 1.96, intercept = FALSE) +
  labs(title = "Mentioned") +
  ggthemes::theme_excel_new()



##### SUPPORT REGRESSIONS ####

indep <- c("russia_total", "change_3", "ratio_ru_ua", "actuall_area", "causalties",
           "causalties*actuall_area", "ratio_ru_ua*actuall_area", "causalties*area_change",
           "ratio_ru_ua*area_change")

support_models <- list()

support_models <- map2("Support_for_Putin", indep, st_formula_vector = "months", map_lm, data = mod_data)

multiplot(support_models, outerCI = 0, innerCI = 1.96, intercept = FALSE) +
  labs(title = "Support") +
  ggthemes::theme_excel_new()


multiplot(support_models[9], outerCI = 0, innerCI = 1.96, intercept = FALSE) +
  labs(title = "Support") +
  ggthemes::theme_excel_new()


#### TRUST REGRESSIONS ####

indep <- c("russia_total", "change_3", "ratio_ru_ua", "actuall_area", "causalties",
           "causalties*actuall_area", "ratio_ru_ua*actuall_area", "causalties*area_change",
           "ratio_ru_ua*area_change")

trust_models <- list()

trust_models <- map2("Trust_in_Putin", indep, st_formula_vector = "months", map_lm, data = mod_data)

multiplot(trust_models, outerCI = 0, innerCI = 1.96, intercept = FALSE) +
  labs(title = "Trust") +
  ggthemes::theme_excel_new()


multiplot(trust_models[9], outerCI = 0, innerCI = 1.96, intercept = FALSE) +
  labs(title = "Support") +
  ggthemes::theme_excel_new()


#### COMPETENCE REGRESSIONS ####

indep <- c("russia_total", "change_3", "ratio_ru_ua", "actuall_area", "causalties",
           "causalties*actuall_area", "ratio_ru_ua*actuall_area", "causalties*area_change",
           "ratio_ru_ua*area_change")

competence_models <- list()

competence_models <- map2("Competence_of_Putin", indep, st_formula_vector = "months", map_lm, data = mod_data)

multiplot(competence_models, outerCI = 0, innerCI = 1.96, intercept = FALSE) +
  labs(title = "Competence") +
  ggthemes::theme_excel_new()


multiplot(competence_models[9], outerCI = 0, innerCI = 1.96, intercept = FALSE) +
  labs(title = "Competence") +
  ggthemes::theme_excel_new()


#### SENTIMENT REGRESSIONS ####

indep <- c("russia_total", "change_3", "ratio_ru_ua", "actuall_area", "causalties",
           "causalties*actuall_area", "ratio_ru_ua*actuall_area", "causalties*area_change",
           "ratio_ru_ua*area_change")

sentiment_models <- list()

sentiment_models <- map2("Sentiment", indep, st_formula_vector = "months", map_lm, data = mod_data)

multiplot(sentiment_models, outerCI = 0, innerCI = 1.96, intercept = FALSE) +
  labs(title = "Sentiment") +
  ggthemes::theme_excel_new()


multiplot(competence_models[9], outerCI = 0, innerCI = 1.96, intercept = FALSE) +
  labs(title = "Sentiment") +
  ggthemes::theme_excel_new()


#### CRITICISM REGRESSIONS ####



indep <- c("russia_total", "change_3", "ratio_ru_ua", "actuall_area", "causalties",
           "causalties*actuall_area", "ratio_ru_ua*actuall_area", "causalties*area_change",
           "ratio_ru_ua*area_change")

criticism_models <- list()

criticism_models <- map2("Criticism_of_Putin", indep, st_formula_vector = "months", map_lm, data = mod_data)

multiplot(criticism_models, outerCI = 0, innerCI = 1.96, intercept = FALSE) +
  labs(title = "Criticism") +
  ggthemes::theme_excel_new()


multiplot(criticism_models[8], outerCI = 0, innerCI = 1.96, intercept = FALSE) +
  labs(title = "Criticism") +
  ggthemes::theme_excel_new()


#### STATE OF WAR REGRESSIONS ####



indep <- c("russia_total", "change_3", "ratio_ru_ua", "actuall_area", "causalties",
           "causalties*actuall_area", "ratio_ru_ua*actuall_area", "causalties*area_change",
           "ratio_ru_ua*area_change")

sof_models <- list()

sof_models <- map2("State_of_war_for_Russia", indep, st_formula_vector = "months", map_lm, data = mod_data)

multiplot(sof_models, outerCI = 0, innerCI = 1.96, intercept = FALSE) +
  labs(title = "State of War") +
  ggthemes::theme_excel_new()


multiplot(sof_models[9], outerCI = 0, innerCI = 1.96, intercept = FALSE) +
  labs(title = "State of War") +
  ggthemes::theme_excel_new()


#### THUMBS UP 👍 REGRESSIONS ####


indep <- c("russia_total", "change_3", "ratio_ru_ua", "actuall_area", "causalties",
           "causalties*actuall_area", "ratio_ru_ua*actuall_area", "causalties*area_change",
           "ratio_ru_ua*area_change")

names(reactions)[6] <- "tu"
tu_models <- list()

tu_models <- map2("tu", indep, st_formula_vector = "months", map_lm, data = reactions)

multiplot(tu_models, outerCI = 0, innerCI = 1.96, intercept = FALSE) +
  labs(title = "#Thumbs Up") +
  ggthemes::theme_excel_new()


multiplot(tu_models[9], outerCI = 0, innerCI = 1.96, intercept = FALSE) +
  labs(title = "#Thumbs Up") +
  ggthemes::theme_excel_new()


#### Some quick descriptions ####







mod_data %>%
  drop_na() %>%
  ggplot(aes(Support_for_Putin)) +
  geom_density(linewidth = 1) +
  theme_barbie() +
  labs(x = "", y = "", title = "Support for Putin")


mod_data %>%
  drop_na() %>%
  ggplot(aes(Competence_of_Putin)) +
  geom_bar(fill = barbie_theme_colors["medium"]) +
  theme_barbie() +
  labs(x = "", y = "", title = "Competence of Putin")


mod_data %>%
  drop_na() %>%
  ggplot(aes(Sentiment)) +
  geom_bar(fill = barbie_theme_colors["medium"]) +
  theme_barbie() +
  labs(x = "", y = "", title = "Sentiment")





