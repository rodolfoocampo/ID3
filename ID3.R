library(readr)
library(dplyr)
library(data.tree)



#### ENTROPY FUNCTION #####
entropy <- function(vector){
  h <- 0
  vector <- factor(vector)
  total_obs <- length(vector)
  for(i in levels(vector)){
    # class sum is the number of occurences for class i
    class_sum <- (sum(vector == i))
    # probability of the class i 
    pri <- class_sum/total_obs
   
    # h accumulates the variable's entropy
    h <- h + (log2(1/pri))*pri
  }
  h
}



##### INFORMATION GAIN FUNCTION


information_gain <- function(attribute_colname, target_colname, whole_data){
  # calculate entropy for target vector
  target_entropy <- entropy(whole_data[,target_colname])
  target_entropy_i <- 0
  attribute_factors <- as.factor(whole_data[,attribute_colname])
  total <- length(attribute_factors)
  entropy_i_sum <- 0
  for(i in levels(attribute_factors)){
    
    subset_i <- filter(whole_data, whole_data[,attribute_colname] == i)
    target_entropy_i <- entropy(subset_i[,target_colname])
    
    class_sum <- sum(attribute_factors == i)
    pri <- class_sum/total
    # Calculamos la probabilidad y la multiplicamos por la entropia del target en el subset
    entropy_i_sum <- entropy_i_sum + target_entropy_i*pri 
  }
  IG <- target_entropy - entropy_i_sum
  IG
}

###### ID3 ALGORITHM



node <- Node$new('mushroom')
train_id3 <- function(whole_data, target_colname, root){
  if(entropy(whole_data[,target_colname]) == 0){
    child <- root$AddChild(whole_data[1,target_colname])
    root$variable <- target_colname
  } else if (length(whole_data) == 1) {
    freqs <- table(whole_data[,target_colname]) %>% as.data.frame() 
    value <- filter(freqs, Freq == max(freqs$Freq))[1,1] 
    child <- root$AddChild(value)
    root$variable <- target_colname
  } else {
    attributes <- names(select(whole_data, -which(names(whole_data) == target_colname))) %>% as.factor()
    min_val <- 100000
    for (i in levels(attributes)){
      ig <- information_gain(i,target_colname,whole_data)
      if(ig < min_val){
        min_val <- ig
        min_var <- i
      }
    }
    root$variable <- min_var
    at_factors <- whole_data[,min_var] %>% as.factor()
    for(i in levels(at_factors)){
      subset_i <- filter(whole_data, whole_data[,min_var]==i) %>% select(-which(names(whole_data) == min_var))
      child <- root$AddChild(i)
      train_id3(subset_i, target_colname, child)      
    }
    print(root, "variable")
  }
  root
}

feat_cars <- mtcars[1,]

predict_id3 <- function(tree, observation){
  if(tree$children[[1]]$isLeaf){
    return (tree$children[[1]]$name)
  } else {
    child <- tree$children[[observation[[tree$variable]]]]
    return (predict_id3(child, observation) )
  }
  
}




predict_id3(tree, feat_cars)


tree <- train_id3(mushroom[2:5,], target_colname, root=node)
tree <- train_id3(mtcars[6:32,], 'mpg', root=node)
print(tree, 'variable')

