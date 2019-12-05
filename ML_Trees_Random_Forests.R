### Trees and Random Forests

## Creating a simple dataset where the outcome grows 0.75 units on average for every increase in a predictor 
## use rpart to fit a regression tree and saves the result to fit

  library(rpart)
  n <- 1000
  sigma <- 0.25
  set.seed(1) #set.seed(1, sample.kind = "Rounding") if using R 3.6 or later
  x <- rnorm(n, 0, 1)
  y <- 0.75 * x + rnorm(n, 0, sigma)
  dat <- data.frame(x = x, y = y)
  fit <- rpart(y ~ ., data = dat)


## make a scatter plot of y versus x along with the predicted values based on the fit.
  library(ggplot2)
  library(tidyverse)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)



## run Random Forests instead of a regression tree using randomForest from the __randomForest__ package, and 
## remake the scatterplot with the prediction line. 
 
  library(randomForest)
  fit <- randomForest(y ~ x, data = dat) 
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)
  

## Use the plot function to see if the Random Forest from above code has converged or if we need more trees.
  plot(fit,type = "l")
  

## It seems that the default values for the Random Forest result in an estimate that is too flexible (unsmooth). 
## Re-run the Random Forest but this time with a node size of 50 and a maximum of 25 nodes. Remake the plot.
  
  
  library(randomForest)
  fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
    dat %>% 
    mutate(y_hat = predict(fit)) %>% 
    ggplot() +
    geom_point(aes(x, y)) +
    geom_step(aes(x, y_hat), col = 2)
    
    plot(fit,type = "l")
  
  