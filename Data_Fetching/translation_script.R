devtools::install_github("https://github.com/egen97/peacePHDs")
library(peacePHDs)

googleLanguageR::gl_auth("MyJson!")

translated_texts <- lapply(telegram_posts, peacePHDs::translate_colBind)

