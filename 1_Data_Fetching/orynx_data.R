#### PRE AMBLE ####

pacman::p_load(gsheet, tidyverse, janitor, ThemePark, ggrepel, ggthemes)

#Define functions:
 #Several empty collums, used for visual help in the google sheet

save_latest <- function(link, ...){
  not_all_na <- function(x) any(!is.na(x))

  orynx_data <<- gsheet2tbl(link, ...) %>%
    select(where(not_all_na)) %>%
    clean_names()

  stopifnot("Newest data already on disk" =
    !str_detect(list.files("Data/orynx/"),
                as.character(Sys.Date()))
  )
  saveRDS(
    orynx_data,
    paste0("Data/orynx/", Sys.Date(), ".rds")
  )
  print(paste("Data saved to", paste0("Data/orynx/", Sys.Date(), ".rds")))
}

change_points <- function(tbl){
tbl %>%
  mutate(
    label = case_when(
      date == "2022-04-01" ~ "E: Rus Invasion",
      date == "2022-08-31" ~ "E: Rus Donbass Offensive",
      date == "2022-09-01" ~ "S: Ukr Kharkiv/Kherson Offensives",
      date == "2023-01-01" ~ "S: Rus Winter Offensive",
      date == "2023-05-01" ~ "E: Rus Winter Offensive",
      date == "2023-06-01" ~ "St: Ukr Summer Offensive",
      .default = NA_character_
    ),
    of_change = ifelse(!is.na(label), date, NA_Date_)
  )
}

# Data

save_latest("https://docs.google.com/spreadsheets/d/1bngHbR0YPS7XH1oSA1VxoL4R34z60SJcR3NxguZM9GI/edit#gid=0")
# See documentation here: https://github.com/leedrake5/Russia-Ukraine
# Quite reliable source, as well reasonable recent data (updated dayly, but confirmation necessity lag)
# Contains data on period of the war, so which offensive and such

# This list only includes destroyed vehicles and equipment of which photo or videographic evidence is available.
# Therefore, the amount of equipment destroyed is significantly higher than recorded here. Small arms, munitions,
# civilian vehicles, trailers and derelict equipment (including aircraft) are not included in this list.
# All possible effort has gone into discerning the status of equipment between captured or abandoned.
# Many of the entries listed as 'abandoned' will likely end up captured or destroyed.
# Similarly, some of the captured equipment might be destroyed if it can't be recovered.
# ATGMs and MANPADS are included in the list but not included in the ultimate count.
# The Soviet flag is used when the equipment in question was produced prior to 1991


orynx_data <- orynx_data %>%
  select(where(not_all_na)) %>%
  clean_names()


#### Plots ####

Amelia::missmap(orynx_data)


orynx_data %>%
  select(date, russia_total, ukraine_total, russia_infantry, ukraine_infantry) %>%
  gtsummary::tbl_summary()




#Russian invasion 2022-04-01
#Russian Donbass Offensive 2022-08-31
#Ukrainian Kharkiv and Kherson Offensives, Start  2022-09-01
#Russian Winter Offensive 2023-01-01 - 2023-05-01
#Ukrainian Summer 2023 Offensive 2023-06-01



orynx_data %>%
  change_points() %>%
  ggplot(aes(date, ratio_ru_ua)) +
  geom_line(colour = oppenheimer_theme_colors["hotflame"]) +
  geom_smooth(method = "loess", colour = oppenheimer_theme_colors["coolflame"]) +
  geom_label_repel(aes(label = label, x = date)) +
  geom_vline(aes(xintercept = of_change), colour = oppenheimer_theme_colors["flame"]) +
  theme_excel_new() +
  labs(y = "Russia/Ukraine Losses", x = "", title = "Russo-Ukrainian Loss Ratio") +
  ylim(1,4.5)



orynx_data %>%
  change_points() %>%
  pivot_longer(contains("total"), names_to = "country") %>%
  mutate(country = ifelse(country == "russia_total", "Russia", "Ukraine")) %>%
  mutate(label = ifelse(str_detect(label, "Ukr") & country == "Russia", NA, label)) %>%
  mutate(label = ifelse(str_detect(label, "Rus") & country == "Ukraine", NA, label)) %>%
  ggplot(aes(date, value)) +
  geom_line(aes(colour = country), linewidth = 1) +
  geom_label(aes(label = label, x = date)) +
  geom_vline(aes(xintercept = of_change)) +
  theme_excel_new() +
  labs(x = "", y = "Cummulative Losses") +
  scale_y_continuous(labels = scales::label_comma())

max(orynx_data$date)








