
library(tidyverse)



ds <- read_csv(file = "Data.csv")


# Spliting the dataset into training and testing

library(caTools)
set.seed(123)
split <- sample.split(ds$Purchased,
                      SplitRatio = .8)

training_set <- subset(ds, split == T)
test_set <- subset(ds, split == F)

# training_set <- training_set %>% 
#   mutate_if(is.double, scale)
# 
# test_set <- test_set %>% 
#   mutate_if(is.double, scale)


