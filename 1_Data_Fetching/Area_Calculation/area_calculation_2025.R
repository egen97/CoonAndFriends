library(tidyverse)

##### Dec 24 ######

area_dec24 <- read_csv("Data/new_area_videoes/area_dec24.csv")

area_dec24$ukraine <- 350174

area_dec24$occupied <- (area_dec24$Target_pixels/area_dec24$ukraine)*100

area_dec24 <- area_dec24 %>%
  mutate(date = str_extract(Filename, "\\d*-\\d*-\\d*")) %>%
  mutate(date = as.Date(date))


### January 2025 #####

area_jan25 <- read_csv("Data/new_area_videoes/area_jan25.csv")

area_jan25 <- area_jan25[1:31,]

area_jan25$ukraine <- 141859

area_jan25$occupied <- (area_jan25$Target_pixels/area_jan25$ukraine)*100


area_jan25$date <- seq.Date(from = as.Date("2025-01-1"), to = as.Date("2025-01-31"), by = "day")


#### Feb 25 ####

area_feb25 <- read_csv("Data/new_area_videoes/area_feb25.csv")
area_feb25 <- area_feb25[1:28,]


area_feb25$ukraine <- 135029

area_feb25$occupied <- (area_feb25$Target_pixels/area_feb25$ukraine)*100

area_feb25$date <- seq.Date(from = as.Date("2025-2-01"), to = as.Date("2025-02-28"), by = "day")

#### March 25 ####

area_march25 <- read_csv("Data/new_area_videoes/area_feb25.csv")

area_march25$ukraine <- 132398

area_march25$occupied <- (area_march25$Target_pixels/area_march25$ukraine)*100

area_march25$date <- seq.Date(from = as.Date("2025-03-01"), to = as.Date("2025-03-29"), by = "day")


#### April 25 ####

area_april25 <- read_csv("Data/new_area_videoes/area_april25.csv")

area_april25$ukraine <- 189351

area_april25$occupied <- (area_april25$Target_pixels/area_april25$ukraine)*100

area_april25$date <- seq.Date(from = as.Date("2025-04-01"), to = as.Date("2025-04-30"), by = "day")

#### May 25 ####

area_may25 <- read_csv("Data/new_area_videoes/area_may25.csv")
area_may25 <- area_may25[1:31,]

area_may25$ukraine <- 79361

area_may25$occupied <- (area_may25$Target_pixels/area_may25$ukraine)*100

area_may25$date <- seq.Date(from = as.Date("2025-05-01"), to = as.Date("2025-05-31"), by = "day")

#### june ####



area_june25 <- read_csv("Data/new_area_videoes/area_june25.csv")

area_june25$ukraine <- 127161

area_june25$occupied <- (area_june25$Target_pixels/area_june25$ukraine)*100

area_june25$date <- seq.Date(from = as.Date("2025-06-01"), to = as.Date("2025-06-27"), by = "day")




##### Bind ######


final_area <- area_dec24 %>%
  bind_rows(area_jan25) %>%
  bind_rows(area_feb25) %>%
  bind_rows(area_march25) %>%
  bind_rows(area_april25) %>%
  bind_rows(area_may25) %>%
  bind_rows(area_june25)


final_area <- final_area %>%
  select(occupied, date)



final_area %>%
  ggplot(aes(date, occupied)) +
  geom_line() +
  geom_smooth(se = FALSE)


saveRDS(final_area, "Data/area_2025.rds")




