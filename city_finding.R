library(tidyverse)

wargonzo <- readRDS("Data/translated/wargonzo_translated.rds")
ukr_cities <- readRDS("ukr_cities.rds")



?agrepl

hm <- agrepl(paste(ukr_cities$city, collapse = "|"), wargonzo$translatedText)

pattern <- paste(ukr_cities$city, collapse = "|")

hm <- str_extract_all(wargonzo$translatedText, pattern)
wargonzo1 <- wargonzo
wargonzo1$cities <- hm
wargonzo1$cities <- paste(unique(wargonzo1$cities), collapse = ", ")

wargonzo2 <- wargonzo1 %>%
  rowwise() %>%
  mutate(cities2 = paste(unique(cities), collapse = ", "))



paste(unique(c("Bakhmut", "Bakhmut", "Soledar")), collapse = ", ")


city_mutate <- function(df, pattern = ukr_cit_pat) {
  df$cities <- str_extract_all(df$translatedText, pattern)
  df <- df %>%
    rowwise() %>%
    mutate(cities = paste(unique(cities), collapse = ", "))
  return(df)
}

hmpf <- pattern
wargonzo2 <- city_mutate(wargonzo, hmpf)

hmpf2 <- paste(ukr_cities$oblast, collapse = "|")

oblast_mutate <- function(df, pattern = ukr_cit_pat) {
  df$oblast <- str_extract_all(df$translatedText, pattern)
  df <- df |>
    rowwise() |>
    mutate(oblast = paste(unique(oblast), collapse = ", "))
  return(df)
}

wargonzo2 <- oblast_mutate(wargonzo2, hmpf2)
