library(peacePHDs)
library(tidyverse)
library(rvest)


### Download Data ####



base_url <- "https://index.minfin.com.ua/en"
ukr_url <- function(x){
  return(paste0(base_url, x))
}

causalties_data <- read_html(ukr_url("/russian-invading/casualties/graph.inc.php?months=0&subj=personnel&bydays=1"))
tanks_data <- read_html(ukr_url("/russian-invading/casualties/graph.inc.php?months=0&subj=tanks&bydays=1"))
planes_data <- read_html(ukr_url("/russian-invading/casualties/graph.inc.php?months=0&subj=planes&bydays=1"))
armored_veichle_data <- read_html(ukr_url("/russian-invading/casualties/graph.inc.php?months=0&subj=armored&bydays=1"))
cannons_data <- read_html(ukr_url("/russian-invading/casualties/graph.inc.php?months=0&subj=cannons&bydays=1"))
mlrs_data <- read_html(ukr_url("/russian-invading/casualties/graph.inc.php?months=0&subj=mlrs&bydays=1"))
helicopters_data <- read_html(ukr_url("/russian-invading/casualties/graph.inc.php?months=0&subj=helicopters&bydays=1"))
uav_data <- read_html(ukr_url("/russian-invading/casualties/graph.inc.php?months=0&subj=uav&bydays=1"))
cruise_misils_data <- read_html(ukr_url("/russian-invading/casualties/graph.inc.php?months=0&subj=missiles&bydays=1"))
ships_data <- read_html(ukr_url("/russian-invading/casualties/graph.inc.php?months=0&subj=ships&bydays=1"))
cars_cisterns_data <- read_html(ukr_url("/russian-invading/casualties/graph.inc.php?months=0&subj=cars&bydays=1"))

#Did not download "special equipment" and "anti-aircraft warfare" as I am not certain of the definition


loss_data <- list(
  armored_veichle_data,
  cannons_data,
  cars_cisterns_data,
  causalties_data,
  cruise_misils_data,
  helicopters_data,
  mlrs_data,
  planes_data,
  ships_data,
  tanks_data,
  uav_data
)




#### clean ####

ukr_df <- function(html, name){

  tempdf <- data.frame("date" = rep(NA, 500), name = rep(NA, 500))

  for (i in 5:422) {
    tempdf[,1][i] <- ukr_minfin_extractor(html, i)[[1]][1]
    tempdf[,2][i] <- ukr_minfin_extractor(html, i)[[1]][[2]]

  }

  tempdf <- tempdf %>%
    drop_na() %>%
    mutate(across(.cols = c(2), ~ as.numeric(str_remove(.x, "'"))),
           date = str_remove(date, "'"),
           date = as.Date(date, tryFormats = c("%d.%m.%Y"))
    )

  names(tempdf)[2] <- name

  return(tibble(tempdf))
}


types <- c(
  "armored_veichle",
"cannons",
"cars_cisterns",
"causalties",
"cruise_misils",
"helicopters",
"mlrs",
"planes",
"ships",
"tanks",
"uav"
)


loss_df <- map2(loss_data, types, possibly(ukr_df))



purrr::possibly()

#Cruise misiles got an subscript out of bonds error
cruise_misils <- data.frame("date" = rep(NA, 500), name = rep(NA, 500))

for (i in 5:374) {
  cruise_misils[,1][i] <- ukr_minfin_extractor(cruise_misils_data, i)[[1]][1]
  cruise_misils[,2][i] <- ukr_minfin_extractor(cruise_misils_data, i)[[1]][[2]]

}

cruise_misils <- cruise_misils %>%
  drop_na() %>%
  mutate(name = as.numeric(str_remove(name, "'")),
         date = str_remove(date, "'"),
         date = as.Date(date, tryFormats = c("%d.%m.%Y"))
  ) %>%
  rename("cruise_misils" = "name")

loss_df[[5]] <- as_tibble(cruise_misils)


merged_df <- loss_df %>%
  reduce(full_join, by = "date")

merged_df <- merged_df %>%
  mutate(across(2:ncol(.), ~ifelse(is.na(.x), 0, .x)))

territory <- readRDS("Data/occupied_area.rds")

merged_df <- merged_df %>%
  full_join(territory, by  = "date")

saveRDS(merged_df, "Data/russian_losses.rds")



