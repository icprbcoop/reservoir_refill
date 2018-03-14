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

d.df$Month <- monthinflow4.df$Month
d.df$MonthIndex <- month_index

##################put in function###############################
#remove previous months (put NA values)
month_today <- c(month(todayis))
index_month_today <- match(c(month_today),d.df$Month) 
d.df$RegIndex <- c(1:12)

wide.df <- d.df
long.df <- wide.df %>% 
  tidyr::gather(year, value, -Month, -MonthIndex, -RegIndex)


sub.df <- long.df %>% 
  dplyr::mutate(value = dplyr::if_else(RegIndex < index_month_today, as.numeric(NA), value))

wide.sub <- sub.df %>% 
  tidyr::spread(year, value)%>%
  slice(match(month_order, Month))


test.df <- wide.sub
test1.df <- dplyr::select(test.df, -c(Month, MonthIndex, RegIndex, 1929)) 

for(j in names(test1.df)){
  for(i in seq(nrow(test1.df))) {
    if (i == 1 | i== b) {
      test1.df[[j]][i] <- test1.df[[j]][i]
    } else {
      test1.df[[j]][i] <-  test1.df[[j]][i - 1] - startstorage
    }
  }
  
}

e.df <- test1.df
e.df <- replace(e.df, e.df > capacity, capacity)
e.df <- replace(e.df, e.df < deadstorage, deadstorage)


percentile.df <- apply( e.df[1:80] , 1 , quantile , probs = quants , na.rm = TRUE )
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