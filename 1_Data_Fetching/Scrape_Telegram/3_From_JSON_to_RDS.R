
library(stringr)
library(jsonlite)
library(dplyr)

#### Listing json-files ####

jsons <- list.files("../../Data/Scraped_channels/", full.names = TRUE)

for (i in 1:length(jsons)) {

  name <- str_remove_all(jsons[i], "../../Data/Scraped_channels/")
  name <- str_remove_all(name, ".json")

  rds <- fromJSON(jsons[i], flatten = TRUE) %>%
    mutate(source = name)

  saveRDS(rds, file = str_c("../../Data/Channels_RDS/", name, ".rds"))

}
