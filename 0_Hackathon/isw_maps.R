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
