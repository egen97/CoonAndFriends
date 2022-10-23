
###### APP ##########

library(geometries)
library(rapidjsonr)
library(tidyverse)
library(sf)
library(here)
library(mapview)
library(shiny)
library(leaflet)
library(shinydashboard)
library(shinyWidgets)
library(plainview)
library(shinymanager)
library(zoo)
library(plotly)


map_data <- readRDS("./vadermap_prepped.rds") %>%
  rename("Sentiment" = "mean_sentiment",
         "Disagreement" = "sd_sentiment",
         "Bloggers" = "Bloggercount")

plot_sentiments <- readRDS("./plot_sentiments.rds")

ui <-  dashboardPage(skin = "black",

  dashboardHeader(title = "OSINT: TELEGRAM"),

  dashboardSidebar(

    sidebarMenu(

      menuItem("Map",
               tabName = "map"),

      menuItem("Telegram translated",
               tabName = "telegram"),

      menuItem("The Putin-meter",
               tabName = "putin")

    )),

  dashboardBody(

    tabItems(

      tabItem(tabName = "map",
              h2("Where do they talk about?"),

              h5("From telegram posts starting 01-01-2022"),

              sliderInput(inputId = "date", label = "Choose day:",
                          value = max(map_data$date, na.rm = TRUE),
                          min = min(map_data$date, na.rm = TRUE),
                          max = max(map_data$date, na.rm = TRUE),
                          animate = TRUE),

              selectizeInput("mapchoice",
                             "Choose an estimator:",
                             choices = c("Sentiment", "Disagreement", "Bloggers"),
                             multiple = FALSE),

              h4("1. A sentiment score of 1 means that the telegram posts are extremely positive. A score of -1 means that they are extremely negative."),

              h4("2. A disagreement score is the standard deviation among bloggers - higher score means more disagreement."),

              h4("3. Bloggers is the number of bloggers mentioning given place on the given day."),


              column(width = 12,
                     leafletOutput("mapplot", width = "100%", height = 600))

            ),

      tabItem(tabName = "telegram",

              h2("Telegram translated"),

              h5("Please find the translated telegram posts here:"),

              h3("https://web.telegram.org/k/#@russian_milbloggers_english"),

              br(),

              img(src="qr_telegram.PNG", align = "center"),


              # tags$iframe(
              #  # seamless = "seamless",
              #   src = "https://web.telegram.org/k/#@russian_milbloggers_english",
              #   height = 800, width = 1400
              #
              #   )

              ),


      tabItem(tabName = "putin",

              h2("Putin meter"),

              # selectizeInput("sentchoice",
              #                "Choose a sentiment estimator:",
              #                choices = c("Vader", "Google"),
              #                multiple = FALSE),

              selectizeInput("blogchoice",
                             "Choose a blogger:",
                             choices = c("anna_news", "galeksandr_skif", "grey_zone", "opersvodki", "RKadyrov_95", "rsotmdivision",
                                         "rybar", "sashakots", "Sladkov_plus", "SolovievLive", "strelkovii", "vagner", "wargonzo", "wehearfromyanina"),
                             multiple = FALSE),

              h3("How positive and negative are the telegram posts when they mention Putin?"),

              h4("A sentiment score of 1 means that the telegram posts are extremely positive. A score of -1 means that they are extremely negative."),


              column(width = 12,
                     plotlyOutput("sentplot", width = "100%", height = 600))

              )

      )
    ))

server <- function(input, output, session) {

  #map_data <- read_rds("map_data.rds")

  output$mapplot <- renderLeaflet({

    map <- map_data %>%
      st_as_sf(coords = c("lon",  "lat")) %>%
      #st_set_crs(4326) %>%
      filter(date == input$date)

    m <- mapview(map,
                 zcol= input$mapchoice, legend = TRUE)

    # m <- mapview::mapview(map, zcol= c("source"), legend = TRUE,
    #                       layer.name = 'Blogger' )

    m@map

  })

  output$sentplot <- renderPlotly({

    plotsent <- plot_sentiments %>%
      filter(source %in% input$blogchoice) %>%
      #filter(sentiment_type %in% input$sentchoice) %>%
      ggplot(aes(date, sentiment, group = sentiment_type, color = sentiment_type)) +
      geom_line(alpha = 0.5) +
      theme_minimal()

    ggplotly(plotsent)

  })

}

shinyApp(ui, server)
