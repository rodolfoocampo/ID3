library(readr)
library(dplyr)

vector <- c("a","c","c","c","c")
length(unique(vector))

entropy <- function(vector){
  h <- 0
  vector <- factor(vector)
  total_obs <- length(vector)
  for(i in levels(vector)){
    # class sum is the number of occurences for class i
    class_sum <- (sum(vector == i))
    # probability of the class i 
    pi <- class_sum/total_obs
   
    # h accumulates the variable's entropy
    h <- h + (log2(1/pi))*pi
    print(h)
  }
}



