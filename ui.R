#Shiny user interface ###############################################
library(shiny)
library(shinydashboard)
library(lubridate)
library(tidyverse)
library(data.table)

ui <- dashboardPage(
  dashboardHeader(title = "Reservoir Refill Scenarios"),
  dashboardSidebar(),
  dashboardBody(# Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 500), width = 9),
      box(
        width = 3,
        title = "Controls",
        numericInput('beginning_storage', 'Beginning Storage', 9.1),
        dateInput(
          'date_input',
          'Date',
          value = NULL,
          min = NULL,
          max = NULL,
          format = "yyyy-mm-dd",
          startview = "month",
          weekstart = 0,
          language = "en",
          width = NULL
        ),
        sliderInput("slider", "Tbd(Monthly withdrawals)", 10, 100, 34)
      )
    ))
)