library(magick)
library(tidyverse)

march5 <- image_read("Data/pictures/march5.jfif")


kern <- matrix(0, ncol = 3, nrow = 3)
kern[1, 2] <- 0.25
kern[2, c(1, 3)] <- 0.25
kern[3, 2] <- 0.25
kern

march5 %>%
  image_convolve("Octagon", scaling = "200%!")
