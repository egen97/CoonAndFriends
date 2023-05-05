library(rvest)
library(tidyverse)
### Pictures ####
url <- "https://www.understandingwar.org/backgrounder/ukraine-conflict-updates-2022"
webpage <- read_html(url)

img_tags <- webpage %>% html_nodes("img")
img_urls <- html_attr(img_tags, "src")

for (i in 1:length(img_urls)) {
  tryCatch(
    {
      download.file(img_urls[i], paste0("Data/pictures/image", i, ".jpg"), method = "curl")
    },
    error = function(e) {
      cat("Error: ", e$message, "\n")
    }
  )
}

url <- "https://www.understandingwar.org/backgrounder/ukraine-conflict-updates"
webpage <- read_html(url)

img_tags <- webpage %>% html_nodes("img")
img_urls <- html_attr(img_tags, "src")

for (i in 1:length(img_urls)) {
  tryCatch(
    {
      download.file(img_urls[i], paste0("Data/pictures/img_23_", i, ".jpg"), method = "curl")
    },
    error = function(e) {
      cat("Error: ", e$message, "\n")
    }
  )
}
#Not fine grade enough, need daily not monthly

#### Video ####


library(av)

av_video_images("Data/pictures/movie/isw_interactive.mp4", destdir = "Data/pictures/movie", fps = 1)
#The pictures are from a screencapture of
#Deleted every other frame by searching for "*0.jpg OR *2.jpg OR *4.jpg OR *6.jpg OR *8.jpg  " in explorer,
# and removing those. Think it's better to set fps = 2 tbh, but who knows
#Those are now in the "corrected" folder

dates <- seq.Date(from = as.Date("2022-2-23"), to = as.Date("2023-2-28"), by = "day")

errors <- as.Date(c("2022-8-2", "2022-8-3", "2022-8-4", "2022-8-5"))

dates <- dates[-which(dates %in% errors)]

filnames <- list.files("Data/pictures/corrected/")

file.rename(
  paste0("Data/pictures/renamed/", filnames),
  paste0("Data/pictures/renamed/",as.character(dates), ".jpg")
  )


