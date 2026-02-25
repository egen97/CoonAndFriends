pacman::p_load(av)
#source https://storymaps.arcgis.com/stories/733fe90805894bfc8562d90b106aa895
movies <- list.files("Data/new_area_videoes")

movies_output <- gsub(".mp4", "", movies)
movies_output <- paste0("Data/new_area_videoes/pictures/", movies_output)


movies <- paste0("Data/new_area_videoes/", movies)


av_video_images("Data/pictures/movie/isw_interactive.mp4", destdir = "Data/pictures/movie", fps = 1)

sapply(av_video_images,video =  movies, destdir = "Data/new_area_videoes/pictures", fps = 1)

for (i in 1:7) {
  av_video_images(movies[i], destdir = movies_output[i], fps = 1)
}


files <- list.files("Data/new_area_videoes/pictures/dec24/")

# extract number at the end
nums <- as.numeric(sub("^image_(\\d+)\\.jpg$", "\\1", files))

# find odd ones
odd_files <- files[nums %% 2 == 1]

odd_files <- paste0("Data/new_area_videoes/pictures/dec24/", odd_files)

# delete them
file.remove(odd_files)





###### Rename #####


dec24 <- list.files("Data/new_area_videoes/pictures/dec24/")

dec24_dates <-  seq.Date(from = as.Date("2022-2-19"), to = as.Date("2024-12-31"), by = "day")

# file.rename(
#   paste0("Data/pictures/renamed/", filnames),
#   paste0("Data/pictures/renamed/",as.character(dates), ".jpg")
# )

file.rename(
  paste0("Data/new_area_videoes/pictures/dec24/", dec24),
  paste0("Data/new_area_videoes/pictures/dec24/", as.character(dec24_dates), ".jpg")
)






