library(tidyverse)

losses <- readRDS("Data/russian_losses.rds")
territory <- readRDS("Data/occupied_area.rds")

losses %>%
  pivot_longer(2:ncol(.), names_to = "type", values_to = "loss") %>%
  mutate(
    type =
      case_when(
        type == "causalties" ~ "Casualties",
        type == "tanks" ~ "Tanks",
        type == "planes" ~ "Planes",
        type == "armored_veichle" ~ "Armored Vehicle",
        type == "cannons" ~ "Cannon",
        type == "cars_cisterns" ~  "Cars and Cisterns",
        type == "cruise_misils" ~ "Cruise Missile",
        type == "helicopters" ~ "Helicopters",
        type == "mlrs" ~ "Multiple Launch Rocket System",
        type == "ships" ~ "Ships",
        type == "uav" ~ "Drone"
      )
  ) %>%
  ggplot(aes(date, loss, colour = type)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  facet_wrap(~type, scales = "free_y") +
  ggthemes::theme_excel_new() +
  labs(
    title = "Russian Losses in Ukraine",
    tag = "Note the differing Y-axis",
    subtitle = "Collected by the Ukrainin Ministry of Defence",
    caption = 'Source: Ukranian Ministry of Finance, "Casualties of the Russian troops in Ukraine", https://index.minfin.com.ua/en/russian-invading/casualties/'
  ) +
  guides(colour = "none")

loss_plot <- function(x){

  p <- losses_new %>%
    ggplot(aes(Date, !!x)) +
    geom_line() +
    geom_smooth(se = FALSE) +
    labs(title = paste("Losses:", quo_name(x) )) +
    ggthemes::theme_excel_new()
  return(p)
}

losses_new <- losses
names(losses_new) <- c("Date", "Armored Vehicle", "Cannon", "Cars and Cisterns", "Causalties", "Cruise Missile", "Helicopters", "MLRS", "Planes", "Ships", "Tanks", "UAV")
name_para <- syms(names(losses_new))



plots <- lapply(name_para, loss_plot)
plots[[1]]$labels$title

plot_saver <- function(x){
  title <- x$labels$title
  filepath <- function(x){paste0("Data/graphics/losses/",
    gsub(" ", "",
        gsub( ":","_",
          title
        )
        ),
    ".png"
    )
  }
  ggsave(filepath(title), plot = x, dpi = 300)
  print(filepath(title))

}

plot_saver(plots[[2]])
mapply(plot_saver, plots) #Aaal the sideeffects, may overwrite data
saveRDS(plots, "Data/graphics/losses/binaryplots.rds")


territory_plot <- territory %>%
  ggplot(aes(date, actuall_area)) +
  geom_line() +
  labs(title = "Occupied Area") +
  ggthemes::theme_excel_new() +
  scale_y_continuous(labels = scales::percent_format(scale = 1))


saveRDS(territory_plot, "Data/graphics/losses/occupied_area.rds")
ggsave("Data/graphics/losses/occupied_area.png", dpi = 300)
