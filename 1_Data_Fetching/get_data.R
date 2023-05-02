
library(tidyverse)

# Path in WSL to avoid encoding issues: /home/sunny369/osint/OSINT Telegram/telegrams/telegram_rds_filtered

# https://platform.openai.com/docs/guides/embeddings/limitations-risks - classification?

# https://openai.com/pricing#language-models - prrrricing

# chat for R https://irudnyts.github.io/openai/index.html


ru_files <- list.files("../../telegrams/telegram_rds_filtered/", full.names = T)
eng_files <- list.files("../../telegrams/telegram_translated/", full.names = T)

telegram_ru <- lapply(ru_files, read_rds)
telegram_eng <- lapply(eng_files, read_rds)


russian_df <- tibble(id = telegram_ru[[1]]$id,
                     date = telegram_ru[[1]]$date,
                     text = telegram_ru[[1]]$message)


english_df <- tibble(id = telegram_eng[[1]]$id,
                     date = telegram_eng[[1]]$date,
                     text = telegram_eng[[1]]$translatedText)

usethis::edit_r_environ()
Sys.setenv(OPENAI_API_KEY = "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")

library(openai)

create_completion(
  model = "ada",
  prompt = "Can you help me classify some telefram texts in Russian?"
)
