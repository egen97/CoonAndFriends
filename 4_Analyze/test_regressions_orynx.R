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
          data = model_data
  )
  return(x)
}


indep <- c("ratio_ru_ua", "actuall_area", "area_change","ratio_ru_ua*actuall_area",  "area_change*ratio_ru_ua", "total_material", "total_material*ratio_ru_ua",
           "total_material*actuall_area","zero_log(causalties)", "actuall_area", "area_change","zero_log(causalties)*actuall_area",  "area_change*zero_log(causalties)", "total_material", "total_material*zero_log(causalties)",
           "total_material*actuall_area")


orynx <- readRDS("Data/orynx/2023-09-07.rds")
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
  full_join(losses, by = "date") %>%
  full_join(orynx, by = "date")

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
    mentioned_dum = ifelse(mentioned, 1, 0)
  )

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
intercept = FALSE,
innerCI = 1,
outerCI = 0
) +
  labs(title = "Mentioned") +
  ggthemes::theme_excel_new()
999999e089


model_data %>%
  filter(date > "2022-01-01") %>%
  ggplot() +
  geom_line(aes(date, support), colour = barbie_theme_colors["light"]) +
  geom_smooth(aes(date, support), se = FALSE, colour = barbie_theme_colors["light"]) +
  geom_line(aes(date, ratio_ru_ua-1), colour = barbie_theme_colors["dark"]) +
  geom_smooth(aes(date, ratio_ru_ua-1), se = FALSE, colour = barbie_theme_colors["dark"]) +
  theme_barbie()




model_data %>%
  filter(date > "2022-01-01") %>%
  mutate(
    period = case_when(
      between(date, as.Date("2022-01-01"), as.Date("2022-04-01")) ~ "Initial Invasion",
      between(date, as.Date("2022-04-02"), as.Date("2022-08-31")) ~ "Donbass Offensive",
      between(date, as.Date("2023-01-01"), as.Date("2023-05-01")) ~ "Russian Winter Offensive",
      between(date, as.Date("2023-04-02"), as.Date("2023-08-31")) ~ "Donbass Offensive",
      .default = NA

    )
  ) %>%
  ggplot() +
  geom_line(aes(date, support), colour = barbie_theme_colors["light"]) +
  geom_smooth(aes(date, support), se = FALSE, colour = barbie_theme_colors["light"]) +
  geom_line(aes(date, ratio_ru_ua-1), colour = barbie_theme_colors["dark"]) +
  geom_smooth(aes(date, ratio_ru_ua-1), se = FALSE, colour = barbie_theme_colors["dark"]) +
  theme_barbie() +
  facet_wrap(~period, scales = "free")

2+2+3+6
