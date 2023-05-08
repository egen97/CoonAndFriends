library(peacePHDs)
library(tidyverse)

deaths <- rvest::read_html("https://index.minfin.com.ua/en/russian-invading/casualties/graph.inc.php?months=0&subj=personnel&bydays=1")
tanks_data <- rvest::read_html("https://index.minfin.com.ua/en/russian-invading/casualties/graph.inc.php?months=0&subj=tanks&bydays=1")
planes_data <- rvest::read_html("https://index.minfin.com.ua/en/russian-invading/casualties/graph.inc.php?months=0&subj=planes&bydays=1")

ukr_minfin_extractor(deaths, 422)[[1]][1]

causalties <- data.frame("date" = rep(NA, 500), "causalties" = rep(NA, 500))

for (i in 5:422) {
  causalties$date[i] <- ukr_minfin_extractor(deaths, i)[[1]][1]
  causalties$causalties[i] <- ukr_minfin_extractor(deaths, i)[[1]][[2]]

}



causalties <- causalties %>%
  drop_na() %>%
  mutate(causalties = as.numeric(str_remove(causalties, "'")),
        date = str_remove(date, "'"),
        date = as.Date(date, tryFormats = c("%d.%m.%Y"))
        )

ggplot(causalties, aes(date, causalties)) +
  geom_line()


tanks <- data.frame("date" = rep(NA, 500), "tanks_losses" = rep(NA, 500))

for (i in 5:422) {
  tanks$date[i] <- ukr_minfin_extractor(tanks_data, i)[[1]][1]
  tanks$tanks_losses[i] <- ukr_minfin_extractor(tanks_data, i)[[1]][[2]]

}



tanks <- tanks %>%
  drop_na() %>%
  mutate(tanks_losses = as.numeric(str_remove(tanks_losses, "'")),
         date = str_remove(date, "'"),
         date = as.Date(date, tryFormats = c("%d.%m.%Y"))
  )

ggplot(tanks, aes(date, tanks_losses)) +
  geom_line()


planes <- data.frame("date" = rep(NA, 500), "planes_losses" = rep(NA, 500))

for (i in 5:422) {
  planes$date[i] <- ukr_minfin_extractor(planes_data, i)[[1]][1]
  planes$planes_losses[i] <- ukr_minfin_extractor(planes_data, i)[[1]][[2]]

}



planes <- planes %>%
  drop_na() %>%
  mutate(planes_losses = as.numeric(str_remove(planes_losses, "'")),
         date = str_remove(date, "'"),
         date = as.Date(date, tryFormats = c("%d.%m.%Y"))
  )


causalties %>%
  left_join(tanks) %>%
  left_join(planes) %>%
  pivot_longer(2:4, names_to = "type", values_to = "loss") %>%
  mutate(type = case_when(
    type == "causalties" ~ "Casualties",
    type == "tanks_losses" ~ "Tanks",
    type == "planes_losses" ~ "Planes"
    ),
    loss = ifelse(is.na(loss), 0, loss)
  ) %>%
  ggplot(aes(date, loss, colour = type)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  facet_wrap(~type, scales = "free_y") +
  ggthemes::theme_excel_new() +
  labs(
    title = "Russian Losses in Ukraine",
    subtitle = "Collected by the Ukrainin Ministry of Defence",
    caption = 'Source: Ukranian Ministry of Finance, "Casualties of the Russian troops in Ukraine", https://index.minfin.com.ua/en/russian-invading/casualties/'
  )



