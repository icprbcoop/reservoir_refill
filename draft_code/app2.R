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
          numericInput('beginning_storage', 'Beginning Storage (BG)', 7.1),
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
    month_index <- c(6,7,8,9,10,11,12,13,14,15,16,17)
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
    quants <- c(0.05,0.50,0.9,0.95)
    
    #Processing#######################################################
    
    #daily to monthly inflows
    monthinflow.df <- rename(inflow.df, Inflow=Patuxent.Reservoir.Inflow..MGD) %>%  
      group_by(Year, Month) %>% 
      summarize_all(sum) 
    monthinflow1.df <- select(monthinflow.df, Year, Month, Inflow)
    monthinflow2.df <- monthinflow1.df %>% 
      na.omit()  %>% 
      select(Inflow, Month) %>% 
      group_by(Year) %>%
      dplyr::mutate(i1 = row_number()) %>% 
      spread(Year, Inflow) %>%  
      select(-i1)
    monthinflow3.df <- setDT(monthinflow2.df)[, lapply(.SD, function(x) first(na.omit(x))), by = Month]
    monthinflow4.df <- monthinflow3.df %>%
      slice(match(month_order, Month))
    
    #how many days in month and how many days left
    ndays_todayis <- numberOfDays(todayis)
    rdays_todayis <- numberOfDays(todayis) - day(todayis)
    ratiodays_todaysis <- rdays_todayis / ndays_todayis
    
    #Calculating traces###############################################
    b.df <- (withdrawals.df + (minwqrel*0.646))* rdays_todayis / 1000    # need to add conversion constants as inputs
    a.df <- startstorage - b.df
    c.df <- monthinflow4.df * ratiodays_todaysis / 1000
    d.df <- a.df$V2 + c.df
    d.df <- replace(d.df, d.df > capacity, capacity)
    d.df <- replace(d.df, d.df < deadstorage, deadstorage)
    d.df$Month <- monthinflow4.df$Month
    d.df$MonthIndex <- month_index
    
    percentile.df <- apply( d.df[2:81] , 1 , quantile , probs = quants , na.rm = TRUE )
    colnames(percentile.df) <- month_names
    
    
    #Plots############################################################# 
    plot(1:12,percentile.df[2,],type="l",xlim=range(1:12),ylim=range(deadstorage:capacity),xaxs="i",yaxs="i",xlab="Month",ylab="Storage (BG)")
    #polygon(c(1:12,12:1),c(percentile.df[1,],rev(percentile.df[4,])),col="skyblue",border=NA)
    #polygon(c(1:12,12:1),c(percentile.df[1,],rev(percentile.df[3,])),col="skyblue",border=NA)
    lines((percentile.df[1,]),lty=1,lwd=2, col="red")
    lines((percentile.df[2,]),lty=1,lwd=2,col="orange")
    lines((percentile.df[3,]),lty=1,lwd=2,col="yellow")
    lines((percentile.df[4,]),lty=1,lwd=2,col="green")
    legend("bottomright", inset=.04, legend=c("5th", "10th", "25th", "50th"), col=c("red", "orange", "yellow", "green"), lty=1:1, cex=0.8)
    
  })
}

shinyApp(ui, server)