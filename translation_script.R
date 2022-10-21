library(googleLanguageR)
library(tidyverse)


gl_auth("~/secure-site-328710-ada7added035.json")

?gl_translate

russian_text <- "Почему у меня такие сухие глаза, меня это начинает раздражать!"

telegram_text <- readRDS("russorientalist_telegram.rds")


testData <- telegram_text[2:11,]

1

hello_world <- gl_translate(
  testData$text,
  target = "en"
)
cat(hello_world$translatedText[2])


telegram_text %>%
  filter(text != "") %>%
  View()

telegram_text[which(telegram_text$text != ""),] %>% View()



translator <- function(x, text = text) {
  tran_text <- gl_translate(
    x$text[which(x$text != "")],
    target = "en"
  )
  return(tran_text)
}

hello <- translator(telegram_text[1:20,])

hello
