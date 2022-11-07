
library(tidyverse)
library(stringr)
library(jsonlite)

#system('scrape_telegram.py')

jsons <- list.files("./channel_jsons/", full.names = TRUE)

for (i in 1:length(jsons)) {

  name <- str_remove_all(jsons[i], "./channel_jsons/channel_messages_")
  name <- str_remove_all(name, ".json")

  rds <- fromJSON(jsons[i], flatten = TRUE) %>%
    mutate(source = name) %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= "2022-01-01")

  saveRDS(rds, file = str_c("./channel_rds_filtered/", name, ".rds"))

}

