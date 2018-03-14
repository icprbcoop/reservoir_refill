#test.df <- tibble::tibble(month = rep(seq(12), 1),
#                          value1 = rep(1, 12),
#                          value2 = rep(2, 12)
#)

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


