# Q1 . For each of the following, indicate whether the outcome is 
#      continuous or categorical.

# Ans :
# Digit reader - Categorical
# Height	- Continuous
# Spam filter - Categorical
# Stock prices - Continuous
# Sex	- Categorical

# Q2.How many features are available to us for prediction in the mnist digits dataset?
# Ans: 784 - deduced by following code

# Download minst dataset from dslabs
mnist <- read_mnist()
ncol(mnist$train$images)


### 2.1 Assessment ###

## The code below cerate dataset for data analysis for following exercises:

library(dslabs)
library(dplyr)
library(lubridate)
library(caret)
library(dslabs)

data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type


## The type column of dat indicates whether students took classes 
## in person ("inclass") or online ("online"). What proportion of the inclass 
## group is female? What proportion of the online group is female?

dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))


## In the course videos, height cutoffs were used to predict sex. Instead of 
## using height, use the type variable. Use what you learned about Q1 to make an 
## informed guess about sex based on the most prevalent sex for each type. Report
## the accuracy of your prediction of sex based on type. You do not need to split
## the data into training and test sets.

## Enter your accuracy as a percentage or decimal (eg "50%" or "0.50") to at least
## the hundredths place.

y_hat <- ifelse(x == "online", "Male", "Female") %>% 
  factor(levels = levels(y))
mean(y_hat==y)


## Write a line of code using the table function to show the confusion matrix 
## between y_hat and y. Use the exact format function(a, b) for your answer and 
## do not name the columns and rows.

table (y_hat, y)


## What is the sensitivity of this prediction? You can use the sensitivity 
## function from the caret package. Enter your answer as a percentage or decimal 
## (eg "50%" or "0.50") to at least the hundredths place.

library(caret)
sensitivity(y_hat, y)


## What is the specificity of this prediction? You can use the specificity 
## function from the caret package. Enter your answer as a percentage or decimal 
## (eg "50%" or "0.50") to at least the hundredths place.

specificity(y_hat, y)


## What is the prevalence (% of females) in the dat dataset defined above? 
## Enter your answer as a percentage or decimal (eg "50%" or "0.50") to at least 
## the hundredths place.

mean(y == "Female") # will give the prevalence of females in the dataset.

## We could get all above information using the confusionMatrix function.



### We will practice building a machine learning algorithm using a new dataset, 
### iris, that provides multiple predictors for us to use to train. To start, 
### we will remove the setosa species and we will focus on the versicolor and 
### virginica iris species using the following code:


library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species


## First let us create an even split of the data into train and test partitions 
## using createDataPartition. The code with a missing line is given below:

## set.seed(2)    # if using R 3.6 or later, use set.seed(2, sample.kind="Rounding")
## # line of code
## test <- iris[test_index,]
## train <- iris[-test_index,]

## Ans:

set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]



## Next we will figure out the singular feature in the dataset that yields the 
## greatest overall accuracy when predicting species. You can use the code from 
## the introduction and from Q7 to start your analysis.

## Using only the train iris dataset, for each feature, perform a simple search 
## to find the cutoff that produces the highest accuracy, predicting virginica 
## if greater than the cutoff and versicolor otherwise. Use the seq function 
## over the range of each feature by intervals of 0.1 for this search.

## Which feature produces the highest accuracy?
  
# Method 1

foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)	

# Method 2

cutoff <- seq(1.0, 8.0)

# Sepal.Length

accuracy <- map_dbl(cutoff, function(x) {
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

max(accuracy)

# Petal.Length

accuracy <- map_dbl(cutoff, function(x) {
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

max(accuracy)

# Sepal.Width

accuracy <- map_dbl(cutoff, function(x) {
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

max(accuracy)

# Petal.Width

accuracy <- map_dbl(cutoff, function(x) {
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

max(accuracy)



## Using the smart cutoff value calculated on the training data from Q8, what is 
## the overall accuracy in the test data?

predictions <- foo(train[,3])
rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)


## Notice that we had an overall accuracy greater than 96% in the training data,
## but the overall accuracy was lower in the test data. This can happen often if
## we overtrain. In fact, it could be the case that a single feature is not the 
## best choice. For example, a combination of features might be optimal. Using 
## a single feature and optimizing the cutoff as we did on our training data can
## lead to overfitting.

## Given that we know the test data, we can treat it like we did our training 
## data to see if the same feature with a different cutoff will optimize our 
## predictions.

## Which feature best optimizes our overall accuracy?

foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==test$Species)
  })
}
predictions <- apply(test[,-5],2,foo)
sapply(predictions,max)



## Now we will perform some exploratory data analysis on the data.
##    plot(iris,pch=21,bg=iris$Species)
## Notice that Petal.Length and Petal.Width in combination could potentially be 
## more information than either feature alone.

## Optimize the the cutoffs for Petal.Length and Petal.Width separately in the 
## train dataset by using the seq function with increments of 0.1. Then, report 
## the overall accuracy when applied to the test dataset by creating a rule that
## predicts virginica if Petal.Length is greater than the length cutoff OR 
## Petal.Width is greater than the width cutoff, and versicolor otherwise.

## What is the overall accuracy for the test data now?

library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

plot(iris,pch=21,bg=iris$Species)

set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

petalLengthRange <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)

length_predictions <- sapply(petalLengthRange,function(i){
  y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.7

width_predictions <- sapply(petalWidthRange,function(i){
  y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5

y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
mean(y_hat==test$Species)



### Assessment practise 
## We have a hypothetical population of 1 million individuals with the following 
## conditional probabilities as described below:

## The test is positive 85% of the time when tested on a patient with the 
## disease (high sensitivity):  P(test+|disease)=0.85 
## The test is negative 90% of the time when tested on a healthy patient 
## (high specificity):  P(test−|heathy)=0.90 
## The disease is prevalent in about 2% of the community:  P(disease)=0.02


set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

## What is the probability that a test is positive?

mean(test)

## What is the probability that an individual has the disease if the test is negative?

mean(disease[test==0])


## What is the probability that you have the disease if the test is positive?
## Remember: calculate the conditional probability the disease is positive 
## assuming a positive test.

mean(disease[test==1])


## If the test is positive, what is the relative risk of having the disease?
## First calculate the probability of having the disease given a positive test, 
## then normalize it against the disease prevalence.

mean(disease[test==1]) / 0.02



## We are now going to write code to compute conditional probabilities for being
## male in the heights dataset. Round the heights to the closest inch. Plot the 
## estimated conditional probability  P(x)=Pr(Male|height=x)  for each  x .


library(dslabs)
data("heights")
# Following would complete the missing code
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
# code end here
  qplot(height, p, data =.)



## In the plot we just made in Q1 we see high variability for low values of 
## height. This is because we have few data points. This time use the quantile 
## 0.1,0.2,…,0.9  and the cut function to assure each group has the same number 
## of points. Note that for any numeric vector x, you can create groups based 
## on quantiles like this: cut(x, quantile(x, seq(0, 1, 0.1)), 
## include.lowest = TRUE).


ps <- seq(0, 1, 0.1)
heights %>%
  ## Following code would give perfect quantiles
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  ## end
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)




## You can generate data from a bivariate normal distrubution using the MASS 
## package using the following code:

    # Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
    # dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
    # data.frame() %>% setNames(c("x", "y"))
## And you can make a quick plot using plot(dat).

## Using an approach similar to that used in the previous exercise, let's 
## estimate the conditional expectations and make a plot.

ps <- seq(0, 1, 0.1)
dat %>%
  # following code will complete the missing code
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  # code ends here
  qplot(x, y, data =.)





set.seed(1) # set.seed(1, sample.kind="Rounding") if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))



## Create a data set using the following code:

    # set.seed(1) # set.seed(1, sample.kind="Rounding") if using R 3.6 or later
    # n <- 100
    # Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
    # dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
    # data.frame() %>% setNames(c("x", "y"))

## Build 100 linear models using the data above and calculate the mean 
## and standard deviation of the combined models. 
## First, set the seed to 1 again (make sure to use sample.kind="Rounding" 
## if your R is version 3.6 or later). Then, within a replicate loop, 
## (1) partition the dataset into test and training sets with p=0.5 and using 
## dat$y to generate your indices, (2) train a linear model predicting y from x,
## (3) generate predictions on the test set, and (4) calculate the RMSE of that
## model. Then, report the mean and standard deviation (SD) of the RMSEs from 
## all 100 models.



set.seed(1)

  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- fit$coef[1] + fit$coef[2]*test_set$x
    sqrt(mean((y_hat - test_set$y) ^ 2))
  })

structure(c(mean(rmse), sd(rmse)), names = c('mean', 'sd'))



## Repeat the exercise above but using larger datasets. Write a function that 
## takes a size n, then (1) builds a dataset using the code provided in question
## above but with n observations instead of 100 and without the set.seed(1), 
## (2) runs the replicate loop that you wrote to answer above question, which 
## builds 100 linear models and returns a vector of RMSEs, 
## and (3) calculates the mean and standard deviation.

set.seed(1)    # if R 3.6 or later, set.seed(1, sample.kind="Rounding")
n <- c(100, 500, 1000, 5000, 10000)
res <- sapply(n, function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat-test_set$y)^2))
  })
  c(avg = mean(rmse), sd = sd(rmse), valofrmse = rmse)
})

res


## making the correlation between x and y larger

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)
rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})

mean(rmse)
sd(rmse)


## Compare the RMSE when using just x_1, just x_2 and both x_1 and x_2. Train a 
## linear model for each.

## Which of the three models performs the best (has the lowest RMSE)?


set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))



## partition into a test and training set of equal size. Compare the RMSE when 
## using just x_1, just x_2, and both x_1 and x_2.

## Compare the results from previous question. What can you conclude?

set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

# Conclusion : Adding extra predictors can improve RMSE substantially, but 
#              not when the added predictors are highly correlated with other 
#              predictors.


