pacman::p_load(googleway, ggmap, tidyverse, sf, mapview)

source("C:/Users/jonassc/OneDrive - Universitetet i Oslo/Desktop/Hack4Peace/key.txt")

register_google(key = gmaps_key, write = TRUE)


a <- c("Donetsk", "Kyiv", "Kharkiv")


a <- tibble(town = c("Donetsk", "Kyiv", "Kharkiv")) %>%
   mutate_geocode(town)
gsg

map <- a %>%
st_as_sf(coords = c("lon",  "lat")) %>%
  st_set_crs(4326) %>% select(town)

map$bloggercount <- c(5,10,15)
map$sentiment <- c("desperate", "happy", "angry")



m <- mapview(map, zcol= c("sentiment"),  cex = "bloggercount" )

m


?st_set_crs
