#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(shinydashboard)
library(tmap)
library(rjson)
library(sf)
library(readxl)
library(eurostat)
library(dplyr)
library(plotly)
library(rstudioapi)  
library(httpuv)
library(shinylive)
script_path <-getSourceEditorContext()$path 

# Set the working directory to the directory of the script
setwd(dirname(script_path))


ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Immigration in Europe"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",style = "position:fixed;width:220px;",
                menuItem("Options", tabName = "Year and Country", icon = icon("bar-chart")),
                selectInput("year", "Select a Year", c("2010-01-01", "2011-01-01",
                                                       "2012-01-01", "2013-01-01", "2014-01-01",
                                                       "2015-01-01", "2016-01-01", "2017-01-01", 
                                                       "2018-01-01", "2019-01-01" )),
                selectInput("country", "Select a Country",       
                            c("All", "FR", "HR", "HU", "AT", "BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "IE", "ES", "FI", "IS",
                              "PT", "RO", "RS", "SE",
                              "SI", "SK", "TR", "UK", "NO", "IT", "LI", "LT", "LU", "LV", "ME", "MK", "MT", "NL", "PL") %>% sort() )
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot3"), width = "100%", height = 550),
      box(plotOutput("plot4"), width = "100%", height = 550)
      
    )
  )
)

server <- function(input, output) {
  raw_data <- get_eurostat("migr_imm8")
  
  # Filter data
  vars <- c("age", "sex", "agedef")
  cond <- c("TOTAL", "T", "COMPLET")
  pos1 <- position_jitter(width = 0.6, height = 0.05, seed = 2)
  
  filtered_data <- reactive({
    data <- raw_data %>%
      filter(.data[[vars[[1]]]] == cond[[1]],
             .data[[vars[[2]]]] == cond[[2]],
             .data[[vars[[3]]]] == cond[[3]])
    data <- data[data$geo != "EU27_2020",]
    if (input$country != "All") {
      data <- data[data$geo == input$country, ]
    }
    
    
    return(data)
  })
  
  output$plot3 <- renderPlot({
    
    raw_data <- get_eurostat("migr_imm8")
    
    # Filter data
    vars <- c("age", "sex", "agedef")
    cond <- c("TOTAL", "T", "COMPLET")
    pos1 <- position_jitter(width = 0.6, height = 0.05, seed = 2)
    
    filtered_data2 <- reactive({
      data <- raw_data %>%
        filter(.data[[vars[[1]]]] == cond[[1]],
               .data[[vars[[2]]]] == cond[[2]],
               .data[[vars[[3]]]] == cond[[3]])
      
      data <- data[data$geo != "EU27_2020",]
      
      if (!is.null(input$year)) {
        data <- data[data$TIME_PERIOD == input$year,]
      }
      
      europe_data <- st_read("https://raw.githubusercontent.com/eurostat/Nuts2json/master/pub/v2/2021/3035/20M/0.json")
      europe_data2 <- merge(europe_data, data, by.x = "id", by.y = "geo")
      
      if (input$country != "All") {
        europe_data2 <- europe_data2[europe_data2$id == input$country, ]
      }
      
      return(europe_data2)
    })
    
    observe({
      
      filtered_data2()
    })
    
    
    tmap_mode("plot")
    
    tm_shape(filtered_data2()) +
      tm_borders(col = "gray50") +
      tm_fill(col = "values", palette = "Reds", title = "Immigration", as.count = T) +
      tm_layout(title = "Absolute Immigration in Europe", legend.position = c("right", "top"), 
                legend.title.size = 1, legend.text.size = 0.6, bg.color = "white") +
      tm_text(text = "values", size = 0.5, root = 3) 
  }, height =  500, width = 800)
  
  output$plot4 <- renderPlot({
    raw_data <- get_eurostat("migr_imm8")
    
    # Filter data
    vars <- c("age", "sex", "agedef")
    cond <- c("TOTAL", "T", "COMPLET")
    pos1 <- position_jitter(width = 0.6, height = 0.05, seed = 2)
    
    filtered_data3 <- reactive({
      data <- raw_data %>%
        filter(.data[[vars[[1]]]] == cond[[1]],
               .data[[vars[[2]]]] == cond[[2]],
               .data[[vars[[3]]]] == cond[[3]])
      
      data <- data[data$geo != "EU27_2020",]
      
      if (!is.null(input$year)) {
        data <- data[data$TIME_PERIOD == input$year,]
      }
      
      europe_data <- st_read("https://raw.githubusercontent.com/eurostat/Nuts2json/master/pub/v2/2021/3035/20M/0.json")
      europe_data2 <- merge(europe_data, data, by.x = "id", by.y = "geo")
      
      if (input$country != "All") {
        europe_data2 <- europe_data2[europe_data2$id == input$country, ]
      }
      
      pop <- read.csv("pop.csv")
      year_index <- as.numeric(substr(names(pop[, seq(3, 16)]), 2, 5))
      position <-year_index == as.numeric(substr(input$year, 1, 4))
      pop <- pop[, c(2, which(position) + 2)]
      colnames(pop) <- c("geo", "OBS_VALUE")
      
      merged <- merge(europe_data2, pop, by.x = "id", by.y = "geo")
      merged$imm_per_capita <- round(merged$values / merged$OBS_VALUE, 4)
      merged$imm_per_capita_col <- merged$imm_per_capita *10000
      
      return(merged)
    })
    
    observe({
      
      filtered_data3()
    })
    
    
    tmap_mode("plot")
    
    tm_shape(filtered_data3()) +
      tm_borders(col = "gray50") +
      tm_fill(col = "imm_per_capita_col", palette = "Reds", title = "Immigration per Capita", as.count = T, labels = c("0.001 to 0.01", "0.011 to 0.02", "0.021 to 0.03", "0.031 to 0.04", "0.041 to 0.05", "0.051 to 0.06")) +
      tm_layout(title = "Immigration per capita in Europe", legend.position = c("right", "top"), 
                legend.title.size = 1, legend.text.size = 0.6, bg.color = "white") +
      tm_text(text = "imm_per_capita", size = 0.5, root = 2, col = "black") 
    
  }, height =  500, width = 800)
}

# Run the application 
shinyApp(ui = ui, server = server)

