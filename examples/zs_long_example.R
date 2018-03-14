todayis <- as.Date("2016-3-5", "%Y-%m-%d")  #input variable (to be)
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
