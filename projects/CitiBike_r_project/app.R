# Intro to Data Science - HW 11
# Copyright 2023, Jeffrey Stanton, Jeffrey Saltz, Christopher Dunham, and Jasmina Tacheva.
# Please do not post online.

#### USE OF ARTIFICIAL INTELLIGENCE: 
# While you are allowed to use generative AI as a learning tool for this assignment 
# (e.g., to help with brainstorming, to see how to code a certain concept, to help identify flaws 
# in reasoning, or to spot confusing or underdeveloped code segments), it must not be used 
# to input homework questions, problems, or instructions directly, 
# and you may not copy and paste any AI-generated content into your work. 
# By submitting this assignment, you confirm that all the work is your own.

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

#### Assignment Description

# You are a data scientist working for the Citi Bike program in New York City.
# Your job is to build an interactive application to identify "hot spots" for disabled bikes.
# Your application will be used by bike maintenance personnel who have limited time and resources 
# and want to keep as much of the bike fleet in operation as possible.

# Tasks:
  
# 0) Update register_stadiamaps to use your key

# 1) Add an appropriate title to the app

# 2) Using the function HTML() in dashboardSidebar() section of the UI, 
# add a small paragraph that describes what this application does and how it should be used.

# 3) At the top of the dashboardBody() section, add a new textOutput() 
# (and corresponding renderText() in the server section)
# that shows the total number of disabled bikes in bike_data(). 
# There is a space reserved for this already, but you need to fill in the 
# missing number which should be generated from live data (i.e., not "hard-coded").

# Currently, the table and the map show all bikes.
# We want the user of the application to be able to filter 
# both the table and the map to just those stations that have disabled bikes. 

# 4) Create a filter in the UI section of the app for "All stations" or "Stations with disabled bikes." 
# Use the selectInput() function.

# 5) Update the reactive object bike_data in the server section of the app 
# to link to the filter you created in step 4, 
# filtering the data appropriately based on the user's selection.

# 6) Make a dynamic title for the map based on filter input, 

# 7) make the size of the dot relative to the number of disabled bikes at the station.

# 8) Publish your app, then upload to Blackboard your app.R file. 
# Include the URL to your published app.


library(shiny)
library(shinydashboard)
library(RCurl)
library(jsonlite)
library(tidyverse)
library(ggmap)
library(dplyr)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "Citi Bike Maintenance Dashboard"),
  dashboardSidebar(
    HTML('<p>This is a Citi Bike Maintenance Dashboard. Its goal is to provide information 
         on bike stations with disabled bikes in New York City. Use the map and table to 
         find stations that require attention.</p>'),
    selectInput(
      "filter", 
      label = "Filter Stations:", 
      choices = c("All stations", "Stations with disabled bikes"),
      selected = "All stations"
    ),
    actionButton("refresh", "Refresh Data")
  ),
  dashboardBody(
    fluidRow(box(textOutput("total_disabled_bikes"), width = 12)),
    fluidRow(box(textOutput("map_title"), width = 12)),
    fluidRow(
      box(
        width = 12,
        height = "550px",
        plotOutput('bike_map', height = "500px", width = '100%')
      )
    ),
    fluidRow(box(textOutput("map_caption"), width = 12)),
    br(),
    fluidRow(box(DTOutput('bike_table', width = '100%')))
  )
)

server <- function(input, output, session) {
  
  data_trigger <- reactiveVal(Sys.time())
  
  observeEvent(input$refresh, {
    data_trigger(Sys.time())
  })
  
  bike_data <- reactive({
    data_trigger()
    
    station_status <- (getURL('https://gbfs.citibikenyc.com/gbfs/en/station_status.json') |> fromJSON())$data$stations
    station_information <- (getURL('https://gbfs.citibikenyc.com/gbfs/en/station_information.json') |> fromJSON())$data$stations
    
    data <- station_information |> 
      inner_join(station_status, by = 'station_id') |> 
      select(
        station_name = name, lat, lon, capacity, 
        num_bikes_available, num_bikes_disabled, 
        num_docks_available, num_docks_disabled
      )
    
    if (!is.null(input$filter) && input$filter == "Stations with disabled bikes") {
      data <- data |> filter(num_bikes_disabled > 0)
    }
    
    return(data)
  })
  
  output$total_disabled_bikes <- renderText({
    paste("Total Disabled Bikes:", sum(bike_data()$num_bikes_disabled, na.rm = TRUE))
  })
  
  output$map_title <- renderText({
    if (!is.null(input$filter) && input$filter == "Stations with disabled bikes") {
      "Map of Stations with Disabled Bikes"
    } else {
      "Map of All Stations"
    }
  })
  
  output$map_caption <- renderText({
    "Circle size = # of disabled bikes. Color = # of bikes available."
  })
  
  output$bike_table <- renderDT({
    bike_data() |> 
      select(-lat, -lon) |> 
      datatable()
  })
  
  register_stadiamaps("f6db4743-2ab2-4162-ae14-af535dfca8ba")
  
  output$bike_map <- renderPlot({
    bb <- c(
      left = min(bike_data()$lon, na.rm = TRUE),
      bottom = min(bike_data()$lat, na.rm = TRUE),
      right = max(bike_data()$lon, na.rm = TRUE),
      top = max(bike_data()$lat, na.rm = TRUE)
    )
    
    base_map <- get_stadiamap(bbox = bb, zoom = 10, maptype = 'stamen_toner_lite')
    
    ggmap(base_map) +
      geom_point(
        data = bike_data(),
        aes(
          x = lon, 
          y = lat, 
          color = num_bikes_available, 
          size = num_bikes_disabled
        ),
        alpha = 0.5
      ) +
      scale_size_continuous(name = "Disabled Bikes") +
      labs(color = "Bikes Available")
  })
}

shinyApp(ui = ui, server = server)