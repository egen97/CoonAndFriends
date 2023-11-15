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

indep <- c("ratio_ru_ua", "actuall_area", "area_change","ratio_ru_ua*actuall_area",  "area_change*ratio_ru_ua", "total_material", "total_material*ratio_ru_ua",
           "total_material*actuall_area","zero_log(causalties)", "actuall_area", "area_change","zero_log(causalties)*actuall_area",  "area_change*zero_log(causalties)", "total_material", "total_material*zero_log(causalties)",
           "total_material*actuall_area")


orynx <- readRDS("Data/orynx/2023-09-07.rds")
losses <- readRDS("Data/russian_losses.rds")

posts <- readRDS("Data/Coded_posts_13_11_2023.rds")

posts$date <- as.Date(posts$date)
