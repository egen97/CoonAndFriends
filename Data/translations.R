library(googleLanguageR)
library(tidyverse)


gl_auth("~/secure-site-328710-ada7added035.json")


translator <- function(x, text = message) {
  df <- googleLanguageR::gl_translate(
    x$message,
    target = "en"
  )
  df <- dplyr::select(df, translatedText, detectedSourceLanguage)
  df <- bind_cols(df, x)
  return(df)
}


telegram_posts <- list.files("Data/telegram_rds/")


telegram_posts <- lapply(paste0("Data/telegram_rds/", telegram_posts), readRDS)


translated_texts <- lapply(telegram_posts, translator)

test <- translator(telegram_posts[[1]])
