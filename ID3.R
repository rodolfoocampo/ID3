library(readr)
library(dplyr)

mtcars
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
  }
  h
}

mtcars

whole_data <- mtcars
# dummy variable for testing -> will remove
target_colname <- 'mpg'
attribute_colname <- 'cyl'


information_gain <- function(attribute_colname, target_colname, whole_data){
  # calculate entropy for target vector
  target_entropy <- entropy(whole_data[,target_colname])
  target_entropy_i <- 0
  attribute_factors <- as.factor(whole_data[,attribute_colname])
  total <- length(attribute_factors)
  entropy_i_sum <- 0
  for(i in levels(attribute_factors)){
    print(i)
    subset_i <- filter(whole_data, whole_data[,attribute_colname] == i)
    target_entropy_i <- entropy(subset_i[,target_colname])
    print(target_entropy_i)
    class_sum <- sum(attribute_factors == i)
    pi <- class_sum/total
    # Calculamos la probabilidad y la multiplicamos por la entropia del target en el subset
    entropy_i_sum <- entropy_i_sum + target_entropy_i*pi 
  }
  IG <- target_entropy - entropy_i_sum
  IG
}

ig <- information_gain('gear', 'mpg', mtcars)

