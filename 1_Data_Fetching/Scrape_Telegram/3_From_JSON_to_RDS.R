
library(stringr)
library(jsonlite)
library(dplyr)

#### Listing json-files ####

jsons <- list.files("./1_Data_Fetching/Scrape_Telegram/jsons/", full.names = TRUE)

for (i in 1:length(jsons)) {

  name <- str_remove_all(jsons[i], "./1_Data_Fetching/Scrape_Telegram/jsons/")
  name <- str_remove_all(name, ".json")
  name <- str_c(name)

  destfile <- str_c("./Data/Channels_RDS/", name, ".rds")

  if(!file.exists(destfile)){

  rds <- fromJSON(jsons[i], flatten = TRUE) %>%
    mutate(source = name)

  saveRDS(rds, file = str_c("./Data/Channels_RDS/", name, ".rds"))

  } else {

    message("Blogger already in place.")

  }

}
