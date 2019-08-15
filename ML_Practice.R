#***** Predicting heights based on gender *******
#***** Traiing on mimicing an ultimate evaluation process ******


#***** Split data into two : ******
#***** "training set" for which we knew the outcome. Used to develop Algo ******
#***** "test set" for whihc we do not know the outcome. Used only for evaluation ****** 


library(tidyverse)
library(caret)
library(dslabs)
data(heights)

# define the outcome and predictors
y <- heights$sex
x <- heights$height


# generate training and test sets using function createDataPartition 
# This function generates indexes for randomly splitting the data into 
# training and test sets
# CreateDataPartition :- Arguments definition 
# -------- times : used to define how many random samples of indexes to return
# -------- p : used to define what proportion of the data is represented by index
# -------- list : used to decide return type of indexes i.e. list or not

set.seed(2007)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]


# guess the outcome & converting the output into factors
#*** In machine learning applications, it is useful to use factors to represent 
#*** the categorical outcomes because R functions developed for machine learning, 
#*** such as those in the caret package, require or recommend that 
#*** categorical outcomes be coded as factors.


y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))


# compute accuracy

# overall accuracy is simply defined as the overall proportion that is predicted correctly
mean(y_hat == test_set$sex)

# Our guess is 50% accurate but as per data analysis, males are slightly
# taller than female

heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

# using the above insight predict Male if height is within 
# two standard deviations from the average male

y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)

# Using training set to optimize the cutoff and evaluate accuracy

# examine the accuracy of 10 cutoffs
cutoff <- seq(61, 70)

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})

max(accuracy)

# This is 83% accuracy much higher than our prediction of 50%
# cutoff used for this prediction 

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

# Using this cutoff on test set to confirm our accuracy is not overly optimistic

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% factor(levels = levels(test_set$sex))

y_hat <- factor(y_hat)

mean(y_hat == test_set$sex)

#***** This yeilds that accuracy of guessing was lower than the cutoff result too.
