pacman::p_load(mapview, sf, tidyverse)



map_data<- readRDS("vader_sentiments.rds")







map <- map_data %>% select(source,date,  compound, cities )


map <- map %>%
  group_by(source, date) %>%
  distinct(cities, .keep_all = TRUE) %>%
  separate(cities, into = c("city_a","city_b","city_c", "city_d",
                            "city_e", "city_f", "city_g", "city_h",
                            "city_i", "city_j", "city_k", "city_l", "city_m",
                            "city_n", "city_o"),
           fill = "right",  sep = ",")



names(map)
map <- map %>%
  pivot_longer(4:18, values_to = "cities") %>%
  filter(!is.na(cities)) %>%
  filter(cities != "")






a <- map %>% group_by(date, cities) %>% tally()


plotmap <- map%>%group_by(date, cities) %>% summarise(mean_sentiment = mean(as.numeric(compound, na.rm = T)),
                                            sd_sentiment = sd(as.numeric(compound, na.rm = T)))


plotmap <- left_join(plotmap, a%>% rename(Bloggercount = n), by = c("cities", "date"))
plotmap$sd_sentiment <- if_else(is.na(plotmap$sd_sentiment) & plotmap$Bloggercount == 1, 0, plotmap$sd_sentiment )


geocode<- readRDS("map_data.rds")

names(geocode)
geocode <- geocode %>% ungroup() %>% select(lon, lat, cities, cityName) %>%distinct()





plotmap2 <- left_join(plotmap, geocode, by = "cities")

plotmap2 <- plotmap2 %>%
  st_as_sf(coords = c("lon",  "lat")) %>%
  st_set_crs(4326) %>% st_jitter(factor = 0.001)



saveRDS(plotmap2, "vadermap_prepped.rds")







m <- mapview(plotmap2, zcol= c("mean_sentiment", "sd_sentiment", "Bloggercount"), legend = TRUE)
m
