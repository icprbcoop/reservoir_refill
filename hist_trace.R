#Develop_version, 1 file
#February 2018
#@author: aseck@icprb.org
#Patuxent Reservoir Refill tool
#inputs: Monthly inflows, Monthly withdrawals, minimum water quality release, beginning storage, dead storage, date
#outputs: Historical traces

#packages#######################################################
library(lubridate)
library(tidyverse)
library(data.table)
library(ggplot2)

#dev- remove all variables except for functions
rm(list = setdiff(ls(), lsf.str()))
dev.off()

#inputs####################################################### 
todayis <- as.Date("2016-10-5", "%Y-%m-%d")  #input variable (to be)
startstorage <- 7.11                        #input variable (to be)
deadstorage <- 0
minwqrel <- 16
capacity <- 10.12

inflow.df = read.csv("inflows.csv", header = TRUE, sep=',')
withdrawals.df = read.csv("monthly_withdrawals.csv", header = FALSE, sep=',')
month_order <- c(6,7,8,9,10,11,12,1,2,3,4,5)

#functions####################################################### 
numberOfDays <- function(date) {
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}

#Processing#######################################################
 
#daily to monthly inflows
monthinflow.df <- rename(inflow.df, Inflow=Patuxent.Reservoir.Inflow..MGD) %>%  group_by(Year, Month) %>% summarize_all(sum) 
monthinflow1.df <- select(monthinflow.df, Year, Month, Inflow)
monthinflow2.df <- monthinflow1.df %>% 
  na.omit()  %>% 
  select(Inflow, Month) %>% 
  group_by(Year) %>%
  dplyr::mutate(i1 = row_number()) %>% 
  spread(Year, Inflow) %>%  
  select(-i1)
monthinflow3.df = setDT(monthinflow2.df)[, lapply(.SD, function(x) first(na.omit(x))), by = Month]
monthinflow4.df <- monthinflow3.df %>%
  slice(match(month_order, Month))
  #group_by(Month) #%>% 
  #summarise_all(funs(na.omit))
  #slice(1:12)
  #distinct(Month, .keep_all = TRUE)

#if {month(todaysi)}
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
d.df$MonthIndex <- c(6,7,8,9,10,11,12,13,14,15,16,17)

quants <- c(0.05,0.50,0.9,0.95)
percentile.df <- apply( d.df[2:81] , 1 , quantile , probs = quants , na.rm = TRUE )
#need to use month function for this
colnames(percentile.df) <- c("June", "July", "August", "September", "October", "November", "December", "January", "February", "March", "April", "May")
#percentile5.df <- percentile.df[1, ]

#Plots###############################################  
plot(1:12,percentile.df[2,],type="l",ylim=range(percentile.df),xlab="Month",ylab="Storage (BG)")
polygon(c(1:12,12:1),c(percentile.df[1,],rev(percentile.df[4,])),col="skyblue",border=NA)
#polygon(c(1:12,12:1),c(percentile.df[1,],rev(percentile.df[3,])),col="skyblue",border=NA)
lines((percentile.df[2,]),lty=1,lwd=2)
lines((percentile.df[3,]),lty=1,lwd=2,col="red")
legend("topleft", inset=.02, legend=c("50th", "90th"), col=c("black", "red"), lty=1:1, cex=0.8)

#functions#######################################################
numberOfDays <- function(date) {
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}

