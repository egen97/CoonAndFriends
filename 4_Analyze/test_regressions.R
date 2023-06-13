pacman::p_load(tidyverse, coefplot, stargazer)

zero_log <- function(x){
  x <- ifelse(x == 0, 0, log(x))
  return(x)
}

losses <- readRDS("Data/russian_losses.rds")

posts <- readRDS("Data/EPSA data/coded_posts.rds")
posts$date <- as.Date(posts$date)

putinmentioned <- readRDS("Data/EPSA data/putin_mentioned.rds")

putinmentioned <- putinmentioned %>%
  select(date, peer_id.channel_id, mentioned = putin, source) %>%
  filter(date > 2021)


posts <- posts %>%
  select(date, peer_id.channel_id, support, sentiment, state_of_war, source) %>%
  filter(date > 2021)


model_data <- posts %>%
  full_join(putinmentioned, by = c("date", "peer_id.channel_id")) %>%
  full_join(losses, by = "date")

model_data <- model_data %>% filter(year(date) > 2021)
min(model_data$date)
model_data <- model_data %>%
  group_by(date) %>%
  summarise(
    across(everything(), ~mean(as.numeric(.x), na.rm = TRUE))
  ) %>%
  mutate(
    days_since_invasion = row_number(),
    area_change = actuall_area - lag(actuall_area),
    total_material = rowSums(across(armored_veichle:uav)),
    caus_10 = causalties/100,
    supp_dum = ifelse(support == 3, 1, 0),
    mentioned_dum = ifelse(mentioned)
    )








map_lm <- function( ...){

  st_formula <- function(dep, indp, st_formula_vector = 1){
    st_formula_vector <- trimws(st_formula_vector)
    form <- as.formula(paste(dep, "~ ", paste(indp, "+"), st_formula_vector))
    return(form)
  }

  x <- lm(st_formula(...),
        data = model_data
     )
  return(x)
}


summary(map_lm(dep = "support", indp = "causalties"))




indep <- c("causalties", "actuall_area", "area_change","causalties*actuall_area",  "area_change*causalties", "total_material", "total_material*causalties",
           "total_material*actuall_area","zero_log(causalties)", "actuall_area", "area_change","zero_log(causalties)*actuall_area",  "area_change*zero_log(causalties)", "total_material", "total_material*zero_log(causalties)",
           "total_material*actuall_area")

support_models <- list()

support_models <- map2("support", indep, st_formula_vector = "days_since_invasion", map_lm)

mentioned_models <- list()
mentioned_models <- map2("mentioned", indep, st_formula_vector = "days_since_invasion", map_lm)


multiplot(mentioned_models) +
  labs(title = "Support") +
  ggthemes::theme_excel_new()


multiplot(mentioned_models, newNames = c(
  "causalties" = "Causalties",
  "actuall_area" = "Area",
  "total_material" = "Material Loss",
  "area_change" = "Area: Change",
  "zero_log(causalties)" = "Ln Causalties",
  "days_since_invasion" = "Time since invasion",
  "total_material:zero_log(causalties)" = "Ln Causalties, Material Loss",
  "area_change:zero_log(causalties)" = "Ln Causalties, Area: Change",
  "total_material:causalties" = "Causalties, Material Loss",
  "actuall_area:zero_log(causalties)" = "Ln Causalties",
  "zero_log(causalties):actuall_area" = "Ln Causalties, Area",
  "causalties:actuall_area" = "Causalties, Area",
  "total_material:actuall_area" = "Material Loss, Area",
  "area_change:causalties" = "Causalties, Are: Change"
  ),
  intercept = FALSE
) +
  labs(title = "Mentioned") +
  ggthemes::theme_excel_new()



multiplot(support_models, newNames = c(
  "causalties" = "Causalties",
  "actuall_area" = "Area",
  "total_material" = "Material Loss",
  "area_change" = "Area: Change",
  "zero_log(causalties)" = "Ln Causalties",
  "days_since_invasion" = "Time since invasion",
  "total_material:zero_log(causalties)" = "Ln Causalties, Material Loss",
  "area_change:zero_log(causalties)" = "Ln Causalties, Area: Change",
  "total_material:causalties" = "Causalties, Material Loss",
  "actuall_area:zero_log(causalties)" = "Ln Causalties",
  "zero_log(causalties):actuall_area" = "Ln Causalties, Area",
  "causalties:actuall_area" = "Causalties, Area",
  "total_material:actuall_area" = "Material Loss, Area",
  "area_change:causalties" = "Causalties, Are: Change"
),
intercept = FALSE
) +
  labs(title = "Support") +
  ggthemes::theme_excel_new()

stargazer(
  support_models,
  type = "text",
  covariate.labels = c(
    "Causalties",
    "Area",
    "Area: Change",
    "Material Loss",
    "Ln Causalties",
    "Time since invasion",
    "Causalties, Area",
    "Causalties, Area: Change",
    "Causalties, Material Loss",
    "Material Loss, Area",
    "Ln Causalties, Area",
    "Ln Causalties, Area: Change",
    "Ln Causalties, Material Loss"
    ),
  dep.var.caption = "Support of Putin",
  out = "4_Analyze/regressions/support_models_130623.tex"
)



stargazer(
  mentioned_models,
  type = "text",
  covariate.labels = c(
    "Causalties",
    "Area",
    "Area: Change",
    "Material Loss",
    "Ln Causalties",
    "Time since invasion",
    "Causalties, Area",
    "Causalties, Area: Change",
    "Causalties, Material Loss",
    "Material Loss, Area",
    "Ln Causalties, Area",
    "Ln Causalties, Area: Change",
    "Ln Causalties, Material Loss"
  ),
  dep.var.caption = "Mentioning Putin",
  out = "4_Analyze/regressions/mentioned_models_130623.tex"
)




