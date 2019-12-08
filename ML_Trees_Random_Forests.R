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
  
  
    
### Caret package assessment
    
    
## Use the rpart function to fit a classification tree to the tissue_gene_expression dataset. Use the train function to estimate 
## the accuracy. 
## Plot the accuracies to report the results of the best model. Set the seed to 1991. Which value of cp gives the highest accuracy?    
    
    library(rpart)
    library(tidyverse)
    library(caret)
    library(dslabs)
    data("tissue_gene_expression")
    set.seed(1991)
    
    fit <- with(tissue_gene_expression, {
      train(x , y, 
            method = "rpart",
            tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))
    })
    
    ggplot(fit)
    

## There are only 6 placentas in the dataset. By default, rpart requires 20 observations before splitting a node. That means 
## that it is difficult to have a node in which placentas are the majority. Rerun the analysis of previous exercise
## but this time, allow rpart to split any node. Look at the confusion matrix again to determine whether the accuracy increases. 
## Again, set the seed to 1991.
    
    library(rpart)
    library(tidyverse)
    library(caret)
    library(dslabs)
    data("tissue_gene_expression")
    set.seed(1991)
    
    fit <- with(tissue_gene_expression, {
      train(x , y, 
            method = "rpart",
            control = rpart.control(minsplit = 0),
            tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))
    })
    
    ggplot(fit)
    confusionMatrix(fit)


## Plot the tree from the best fitting model of the analysis of previous question.
## Which gene is at the first split?
    
    plot(fit$finalModel)
    text(fit$finalModel)

    
## With just seven genes, we are able to predict the tissue type. Now let's see if we can predict the tissue type with even 
## fewer genes using a Random Forest. Use the train function and the rf method to train a Random Forest model and save it to 
## an object. Try out values of mtry ranging from seq(50, 200, 25) (you can also explore other values on your own). 
## What mtry value maximizes accuracy? To permit small nodesize to grow as we did with the classification trees, 
## use the following argument: nodesize = 1
    
    
    library(rpart)
    library(tidyverse)
    library(caret)
    library(dslabs)
    library(randomForest)
    data("tissue_gene_expression")
    set.seed(1991)
    
    fit <- with(tissue_gene_expression, {
      train(x , y, 
            method = "rf",
            nodesize = 1,
            tuneGrid = data.frame(mtry = seq(50, 200, 25)))
    })
    
    ggplot(fit)
    
## Use the function varImp on the output of train and save it to an object called imp.
    
    imp <- varImp(fit)
    imp


