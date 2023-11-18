#### PRE AMBLE ####

pacman::p_load(tidyverse, coefplot, ThemePark, stargazer)

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

coef_names <- c("causalties" = "Causalties",
                "actuall_area" = "Occupied Area",
                "area_change" = "Occupied Area, daily change",
                "ratio_ru_ua" = "RUS/UKR LER",
                "ratio_ru_ua:area_change" = "Interaction LER, Area Change",
                "months" = "Months since invasion",
                "russia_total" = "Russian cumulative losses",
                "causalties:area_change" = "Interaction, Causalties, Area Change",
                "ratio_ru_ua:actuall_area" = "Interaction, LER, Occupied Area",
                "causalties" = "Causalties",
                "actuall_area" = "Occupied Area",
                "ratio_ru_ua" = "RUS/UKR LER",
                "change_3" = "Change (3 days)",
                "months" = "Months since invasion",
                "russia_total" = "Russian cumulative losses"
)


#### MENTIONED REGRESSIONS ####


indep <- c("russia_total", "change_3", "ratio_ru_ua", "actuall_area", "causalties",
           "causalties*actuall_area", "ratio_ru_ua*actuall_area", "causalties*area_change",
           "ratio_ru_ua*area_change")

mentioned_models <- list()

mentioned_models <- map2("putin", indep, st_formula_vector = "months", map_lm, data = mentioned)

multiplot(mentioned_models[1:5], outerCI = 0, innerCI = 1.96, intercept = FALSE,
          newNames = coef_names
            ) +
  labs(title = "Mentioned") +
  ggthemes::theme_excel_new()


multiplot(mentioned_models[6:9], outerCI = 0, innerCI = 1.96, intercept = FALSE,
          newNames = coef_names
) +
  labs(title = "Mentioned") +
  ggthemes::theme_excel_new()


stargazer(mentioned_models, type = "text", title = "Mentioned",  out = "4_Analyze/regressions/mentioned_18112023.tex")

##### SUPPORT REGRESSIONS ####

indep <- c("russia_total", "change_3", "ratio_ru_ua", "actuall_area", "causalties",
           "causalties*actuall_area", "ratio_ru_ua*actuall_area", "causalties*area_change",
           "ratio_ru_ua*area_change")

support_models <- list()

support_models <- map2("Support_for_Putin", indep, st_formula_vector = "months", map_lm, data = mod_data)

multiplot(support_models[1:5], outerCI = 0, innerCI = 1.96, intercept = FALSE, newNames = coef_names) +
  labs(title = "Support") +
  ggthemes::theme_excel_new()


multiplot(support_models[6:9], outerCI = 0, innerCI = 1.96, intercept = FALSE, newNames = coef_names) +
  labs(title = "Support") +
  ggthemes::theme_excel_new()



stargazer(support_models,type = "text", title = "Support", out =  "4_Analyze/regressions/support_18112023.tex")

#### TRUST REGRESSIONS ####

indep <- c("russia_total", "change_3", "ratio_ru_ua", "actuall_area", "causalties",
           "causalties*actuall_area", "ratio_ru_ua*actuall_area", "causalties*area_change",
           "ratio_ru_ua*area_change")

trust_models <- list()

trust_models <- map2("Trust_in_Putin", indep, st_formula_vector = "months", map_lm, data = mod_data)

multiplot(trust_models[1:5], outerCI = 0, innerCI = 1.96, intercept = FALSE, newNames = coef_names) +
  labs(title = "Trust") +
  ggthemes::theme_excel_new()

multiplot(trust_models[6:9], outerCI = 0, innerCI = 1.96, intercept = FALSE, newNames = coef_names) +
  labs(title = "Trust") +
  ggthemes::theme_excel_new()

stargazer(trust_models,type = "text", title = "Trust", out = "trust_18112023.tex")


#### COMPETENCE REGRESSIONS ####

indep <- c("russia_total", "change_3", "ratio_ru_ua", "actuall_area", "causalties",
           "causalties*actuall_area", "ratio_ru_ua*actuall_area", "causalties*area_change",
           "ratio_ru_ua*area_change")

competence_models <- list()

competence_models <- map2("Competence_of_Putin", indep, st_formula_vector = "months", map_lm, data = mod_data)

multiplot(competence_models[1:5], outerCI = 0, innerCI = 1.96, intercept = FALSE, newNames = coef_names) +
  labs(title = "Competence") +
  ggthemes::theme_excel_new()


multiplot(competence_models[6:9], outerCI = 0, innerCI = 1.96, intercept = FALSE, newNames = coef_names) +
  labs(title = "Competence") +
  ggthemes::theme_excel_new()

stargazer(competence_models, type = "text", title = "Competence" ,out = "competence_18112023.tex")

#### SENTIMENT REGRESSIONS ####

indep <- c("russia_total", "change_3", "ratio_ru_ua", "actuall_area", "causalties",
           "causalties*actuall_area", "ratio_ru_ua*actuall_area", "causalties*area_change",
           "ratio_ru_ua*area_change")

sentiment_models <- list()

sentiment_models <- map2("Sentiment", indep, st_formula_vector = "months", map_lm, data = mod_data)

multiplot(sentiment_models[1:5], outerCI = 0, innerCI = 1.96, intercept = FALSE, newNames = coef_names) +
  labs(title = "Sentiment") +
  ggthemes::theme_excel_new()

multiplot(sentiment_models[6:9], outerCI = 0, innerCI = 1.96, intercept = FALSE, newNames = coef_names) +
  labs(title = "Sentiment") +
  ggthemes::theme_excel_new()

stargazer(sentiment_models,type = "text", title = "Sentiment", out = "4_Analyze/regressions/sentiment_18112023.tex")


#### CRITICISM REGRESSIONS ####



indep <- c("russia_total", "change_3", "ratio_ru_ua", "actuall_area", "causalties",
           "causalties*actuall_area", "ratio_ru_ua*actuall_area", "causalties*area_change",
           "ratio_ru_ua*area_change")

criticism_models <- list()

criticism_models <- map2("Criticism_of_Putin", indep, st_formula_vector = "months", map_lm, data = mod_data)

multiplot(criticism_models[1:5], outerCI = 0, innerCI = 1.96, intercept = FALSE, newNames = coef_names) +
  labs(title = "Criticism") +
  ggthemes::theme_excel_new()


multiplot(criticism_models[6:9], outerCI = 0, innerCI = 1.96, intercept = FALSE, newNames = coef_names) +
  labs(title = "Criticism") +
  ggthemes::theme_excel_new()

stargazer(criticism_models,type = "text", title = "Criticism", out = "4_Analyze/regressions/critisism_18112023.tex")

#### STATE OF WAR REGRESSIONS ####



indep <- c("russia_total", "change_3", "ratio_ru_ua", "actuall_area", "causalties",
           "causalties*actuall_area", "ratio_ru_ua*actuall_area", "causalties*area_change",
           "ratio_ru_ua*area_change")

sof_models <- list()

sof_models <- map2("State_of_war_for_Russia", indep, st_formula_vector = "months", map_lm, data = mod_data)

multiplot(sof_models[1:5], outerCI = 0, innerCI = 1.96, intercept = FALSE, newNames = coef_names) +
  labs(title = "State of War") +
  ggthemes::theme_excel_new()

multiplot(sof_models[6:9], outerCI = 0, innerCI = 1.96, intercept = FALSE, newNames = coef_names) +
  labs(title = "State of War") +
  ggthemes::theme_excel_new()


stargazer(sof_models,type = "text", title = "State of War", out = "4_Analyze/regressions/sof_18112023.tex")



#### THUMBS UP 👍 REGRESSIONS ####


indep <- c("russia_total", "change_3", "ratio_ru_ua", "actuall_area", "causalties",
           "causalties*actuall_area", "ratio_ru_ua*actuall_area", "causalties*area_change",
           "ratio_ru_ua*area_change")

names(reactions)[6] <- "tu"
tu_models <- list()

tu_models <- map2("tu", indep, st_formula_vector = "months", map_lm, data = reactions)

multiplot(tu_models[1:5], outerCI = 0, innerCI = 1.96, intercept = FALSE, newNames = coef_names) +
  labs(title = "#Thumbs Up") +
  ggthemes::theme_excel_new()


multiplot(tu_models[6:9], outerCI = 0, innerCI = 1.96, intercept = FALSE, newNames = coef_names) +
  labs(title = "#Thumbs Up") +
  ggthemes::theme_excel_new()

stargazer(tu_models, type = "text", title = "Thumbs Up", out = "4_Analyze/regressions/thumbs_18112023.tex")












