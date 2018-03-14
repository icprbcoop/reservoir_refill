library(shiny)
library(shinydashboard)
library(lubridate)
library(tidyverse)
library(data.table)
library(ggplot2)


#functions###########################################################
numberOfDays <- function(date) {
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}

#Shiny user interface ###############################################

ui <- dashboardPage(
  dashboardHeader(title = "Reservoir Refill Scenarios"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 500), width = 9),
      
      
      
      box(width = 3,
          title = "Controls",
          dateInput('date_input', 'Date', value = NULL, min = NULL, max = NULL,
                    format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                    language = "en", width = NULL),
          numericInput('beginning_storage', 'Beginning Storage (BG)', 9.1),
          numericInput('capacity', 'Capacity (BG)', 10.12),
          numericInput('dead_storage', 'Dead Storage (BG)', 0),
          numericInput('min_wqrl', 'Minimum WQ Release (MGD)', 16),
          sliderInput("slider", "*Monthly withdrawals  MGD)*", 10, 100, 34)
      )
    )
  )
)

#Shiny server #######################################################

server <- function(input, output) {
  output$plot1 <- renderPlot({
    
    #inputs ########################################################
    #todayis <- as.Date("2016-10-5", "%Y-%m-%d")  #input variable (to be)
    todayis <- input$date_input
    startstorage <- input$beginning_storage                        
    deadstorage <- input$dead_storage
    minwqrel <- input$min_wqrl  #minimum water quality release
    capacity <- input$capacity
    
    inflow.df = read.csv("data/inflows.csv", header = TRUE, sep = ',')
    withdrawals.df = read.csv("data/monthly_withdrawals.csv", header = FALSE, sep = ',')
    month_order <- c(6,7,8,9,10,11,12,1,2,3,4,5)
    month_names <- c(
      "June",
      "July",
      "August",
      "September",
      "October",
      "November",
      "December",
      "January",
      "February",
      "March",
      "April",
      "May"
    )
    
    
  })
}

shinyApp(ui, server)