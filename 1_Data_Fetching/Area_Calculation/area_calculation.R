library(tidyverse)

area <- read.csv("Data/pictures/area.csv")
#Red/ Ukraine = Red/(Total-Ukraine)

#Reference are ukraine and ukraine_inverse



area <- area %>%
  filter(!str_detect(Filename, "ukr")) %>%
  mutate(Filename = str_remove(Filename, ".jpg"),
         date = as.Date(Filename),
         actuall_area = (Percentage/19.579834)*100
         ) %>%
  select(-Filename)

area$actuall_area <- ifelse(area$date == as.Date("2022-04-25"), 13.55226, area$actuall_area) #Sudden weird colour change


saveRDS(area, "Data/occupied_area.rds")

ggplot(area, aes(date, actuall_area)) +
  geom_line(size = 1, colour = "#F6B5B1") +
  ggthemes::theme_excel_new() +
  labs(
    title = "Russian Occupied Area in Ukraine",
    caption = "Source: Institute for the Study of War"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))
