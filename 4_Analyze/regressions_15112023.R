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




