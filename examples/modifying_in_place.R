# I pulled this idea from http://adv-r.had.co.nz/Functionals.html
# under the Modifying in Place heading.
# This is an example of how to update value based on output from the
# previous iteration of the a loop. Each iteration updates the values
# in the orginal data frame. Therefore, the subsequent iteration will
# use the value calulated during the previous iteration.


library(tidyverse)

test.df <- tibble::tibble(month = rep(seq(12), 2),
                          year = c(rep(1989, 12),
                                   rep(1990, 12)),
                          value = rep(1, 24))

for(i in seq(nrow(test.df))) {
  if (i == 1) {
    test.df[["value"]][i] <- test.df[["value"]][i]
  } else {
    test.df[["value"]][i] <- test.df[["value"]][i - 1] + 1
  }
}
