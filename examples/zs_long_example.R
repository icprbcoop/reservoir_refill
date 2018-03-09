wide.df <- d.df
long.df <- wide.df %>% 
  tidyr::gather(year, value, -Month, -MonthIndex)

sub.df <- long.df %>% 
  dplyr::mutate(value = dplyr::if_else(MonthIndex < 10, as.numeric(NA), value))

wide.sub <- sub.df %>% 
  tidyr::spread(year, value)
