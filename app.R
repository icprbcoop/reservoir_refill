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
          dateInput('date_input', 'Date', value = "2017-07-01", min = "2017-06-01", max = "2018-05-30",
                    format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                    language = "en", width = NULL),
          numericInput('beginning_storage', 'Beginning Storage (BG)', 3.0),
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
    
    #todayis <- as.Date("2018-10-5", "%Y-%m-%d")  #input variable (to be)
    #todayis <- input$date_input
    #startstorage <- 7.1                        
    #deadstorage <- 0
    #minwqrel <- 16 #minimum water quality release
    #capacity <- 10.12
    #withdrawal <- 34
    
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
    quants <- c(0.05,0.1,0.25,0.5,0.75,0.9)
    
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
    b.df <- (withdrawal + (minwqrel*0.646))* rdays_todayis / 1000    # need to add conversion constants as inputs
    a.df <- startstorage - b.df
    c.df <- monthinflow4.df * ratiodays_todaysis / 1000
    d.df <- startstorage - b.df + c.df
    
    d.df$Month <- monthinflow4.df$Month
    d.df$MonthIndex <- month_index
    
    ##################put in function###############################
    #remove previous months (put NA values)
    month_today <- c(month(todayis))
    index_month_today <- match(c(month_today),d.df$Month)
    index_prevmonth_today <- index_month_today-1
    d.df$RegIndex <- c(1:12)
    
    wide.df <- d.df
    long.df <- wide.df %>% 
      tidyr::gather(year, value, -Month, -MonthIndex, -RegIndex)
    
    
    sub.df <- long.df %>% 
      dplyr::mutate(value = dplyr::if_else(RegIndex != index_month_today, as.numeric(NA), value))
    
    wide.sub <- sub.df %>% 
      tidyr::spread(year, value)%>%
      slice(match(month_order, Month))
    
    
    test.df <- wide.sub
    test1.df <- dplyr::select(test.df, -c(Month, MonthIndex, RegIndex, 1929)) 
    
    for(j in names(test1.df)){
      for(i in index_month_today:(nrow(test1.df))) {
        #print (i)
        if ( i==index_month_today) {
          test1.df[[j]][i] <- test1.df[[j]][i]
          test1.df[[j]][i - 1] <- startstorage
        } else {
          test1.df[[j]][i] <-  test1.df[[j]][i - 1]  - b.df +  c.df[[j]][i]
        }
      }
      
    }
    
    e.df <- test1.df
    e.df <- replace(e.df, e.df > capacity, capacity)
    e.df <- replace(e.df, e.df < deadstorage, deadstorage)
    e.df$Month <- monthinflow4.df$Month
    e.df$MonthIndex <- month_index
    
    percentile.df <- apply( e.df[2:80] , 1 , quantile , probs = quants , na.rm = TRUE )
    colnames(percentile.df) <- month_names
    emergency.df <- rep(1,12)
    
    #Plots############################################################# 
    plot(index_prevmonth_today:12,percentile.df[2,index_prevmonth_today:12],type="l",xlim=range(1:12),ylim=range(deadstorage:11),xaxs="i",yaxs="i",xlab="Month",ylab="Storage (BG)",axes=FALSE,frame.plot=TRUE)
    polygon(c(1:12,12:1),c(rep(deadstorage,12),rep(1,12)),col="rosybrown1",border=NA)
    polygon(c(1:12,12:1),c(rep(1,12),rep(capacity,12)),col="lightblue",border=NA)
    polygon(c(1:12,12:1),c(rep(capacity,12),rep(11,12)),col="grey",border=NA)
    #polygon(c(1:12,12:1),c(percentile.df[1,],rev(percentile.df[6,])),col="skyblue")
    #polygon(c(1:12,12:1),c(percentile.df[1,],rev(percentile.df[3,])),col="skyblue",border=NA)
    lines(index_prevmonth_today:12,(c(rep(capacity*0.9,12))),lty=2,lwd=1, cex=1.2, col="black")
    lines(index_prevmonth_today:12,(percentile.df[2,index_prevmonth_today:12]),lty=1,lwd=2,col="orange", type="b", pch=18, cex=2)
    lines(index_prevmonth_today:12,(percentile.df[3,index_prevmonth_today:12]),lty=1,lwd=2,col="yellow", type="b", pch=15, cex=1.5)
    lines(index_prevmonth_today:12,(percentile.df[4,index_prevmonth_today:12]),lty=1,lwd=2,col="green", type="b", pch=23, cex=1.5)
    lines(index_prevmonth_today:12,(percentile.df[1,index_prevmonth_today:12]),lty=1,lwd=2, type="b", pch=19, cex=1.2, col="red")
    legend("bottomright", inset=.04, legend=c("5th", "10th", "25th", "50th"), col=c("red", "orange", "yellow", "green"), lty=1:1, cex=1.2)
    axis(1, at=1:12, labels=month.abb[month_order])
    axis(2, labels=TRUE)
    text(2.6, 0.5, "Emergency storage")
    text(2.6, 8.9, "90% full")
    #Axis(side=1, labels=FALSE)
    
  })
}

shinyApp(ui, server)