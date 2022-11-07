
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

      menuItem("Welcome!",
               tabName = "welcome"),

      menuItem("Map",
               tabName = "map"),

      menuItem("Telegram translated",
               tabName = "telegram"),

      menuItem("The Putin-meter",
               tabName = "putin"),

      menuItem("Who are we?",
               tabName = "whoarewe")

    )),

  dashboardBody(

    tabItems(

      tabItem(tabName = "welcome",

              h2("Welcome to OSINT: Telegram!"),

              column(12,
                     br(),
                     p("At OSINT: Telegram, we have automatically collected, translate and analysed more than 300,000 telegram posts from 19 of Russia’s biggest pro-War telegram channels to make them accessible to researchers, journalists and the interested public."),
                     p("While some of these war blogs are already used as valuable sources by a handful of analysists and journalists, the information is spread out, only available in Russian, and not easily available for systematic analysis. We want to change this, and make the data available for the interested public!"),
                     p("For the Hack4Peace, our team used the telegram API to automatically download all posts on the selected channels from 01.01.2022 – 21.10.2022, translated them to English using the Google translate API, and ran sentiment analyses using Google’s natural language processing algorithm and Vader’s unsupervised classification algorithm. Using entity recognition, we further identified all Ukrainian cities mentioned in the posts and used the Google Maps API to geolocate them."),
                     br(),
                     p("On this webpage, you can currently get a glimpse at three of the things we already did with this data:"),
                     p("On the map page, you can access an interactive map giving you a day-by-day overview of which Ukrainian cities the Russian warbloggers were talking about. Moreover, the map currently includes three key variables: First, for each city it indicates the average sentiment for each city among the bloggers who mentioned it. This indicates how the general mood related to the location is on a giveb day. Second, the standard deviation of the sentiment score. This illustrates how much internal disagreement there is among Russia’s warbloggers concerning a place at a given time. Lastly, we show the number of bloggers mentioning a location on a given day, illustrating how much attention the warbloggers are paying to a particular place. Currently, the map is only using the sentiment scores estimated by our Vader model."),
                     br(),
                     p("On the telegram translated page, you can find a link to our automated telegram channel. The channel collects all posts by a blogger on a day, and posts the English translations of all posts on this day."),
                     br(),
                     p("On the Putin-Meter page, you can – unsurprisingly – access our Putin-Meter. For the Putin-Meter, we analyse all Posts mentioning Vladimir Putin, and how positively or negatively he is talked about. Currently, we have separate Putin-Meter for each of the bloggers, that illustrates how positively or negatively they were talking about Putin and how this changed over the course of the war."),
                     br(),
                     p("In the near future, we will implement automated daily updates of the data to keep tracking the communication of Russian warbloggers. We are also planning to add more bloggers to our list, and to make the raw data available for the general public."),
              )

              ),

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

              ),

      tabItem(tabName = "whoarewe",

              h2("Who are we?"),

              column(3,
                     img(src = "us.png", heigh = "100%", width = "100%", alight = "right")),

              h4("Amalie Nilsen"),
              h4("Eric Gabo Ekeberg Nilsen"),
              h4("Jonas Willibald Schmid"),
              h4("Solveig Bjørkholt"),
              h6("PhD candidates at the University of Oslo"),

              column(12,
                     br(),
                     p(""))


              )

    )
  )
)


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
