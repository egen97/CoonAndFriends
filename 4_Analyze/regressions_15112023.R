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
          data = model_data
  )
  return(x)
}


#### Wrangle Tangle Dangle ####

indep <- c("ratio_ru_ua", "actuall_area", "area_change","ratio_ru_ua*actuall_area",  "area_change*ratio_ru_ua", "russia_total", "russia_total*ratio_ru_ua",
           "russia_total*actuall_area", "actuall_area", "area_change", "total_material","total_material*actuall_area")


orynx <- readRDS("Data/orynx/2023-09-07.rds")

posts <- readRDS("Data/Coded_posts_13_11_2023.rds")

occupied_area <- readRDS("Data/occupied_area.rds")


posts <- posts %>%
  mutate(date = as.Date(date))



modData <- posts %>%
  full_join(orynx, by = "date") %>%
  full_join(occupied_area, by = "date")

modData <- modData %>%
  ungroup() %>%
  arrange(date) %>%
  fill(starts_with("russ"), .direction = "down")



modData <- modData %>%
  mutate(area_change = actuall_area-lag(actuall_area))


#### Models ####
