## Comprehension check - Ensembles

library(caret)
library(tidyverse)
library(dslabs)

## Q1 Use the training set to build a model with several of the models available from the caret package. We will test
## out 10 of the most common machine learning models in this exercise.
## Apply all of these models using train with all the default parameters.

models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

set.seed(1) 
#set.seed(1, sample.kind = "Rounding")` in R 3.6 or later
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models


## Q2 Now that you have all the trained models in a list, use sapply or map to create a matrix of predictions for the 
## test set. You should end up with a matrix with length(mnist_27$test$y) rows and length(models) columns.

## What are the dimensions of the matrix of predictions?

pred <- sapply(fits, function(fit){
  predict(fit, newdata = mnist_27$test)
})

dim(pred)

# Or
nrow(pred)
ncol(pred)



## Q3 Now compute accuracy for each model on the test set.

## Report the mean accuracy across all models.

accuracy <- colMeans(pred == mnist_27$test$y)
accuracy
mean(accuracy)


## Q4 Next, build an ensemble prediction by majority vote and compute the accuracy of the ensemble.

## What is the accuracy of the ensemble?

votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)


## Q5 How many of the individual methods do better than the ensemble?

ind <- accuracy > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]


## Q6 It is tempting to remove the methods that do not perform well and re-do the ensemble. The problem with this 
## approach is that we are using the test data to make a decision. However, we could use the minimum accuracy 
## estimates obtained from cross validation with the training data for each model. Obtain these estimates and save 
## them in an object. Report the mean of these training set accuracy estimates.

## What is the mean of these training set accuracy estimates?

acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)


## Q7 Now let's only consider the methods with an estimated accuracy of greater than or equal to 0.8 when constructing
## the ensemble.

## What is the accuracy of the ensemble now?

ind <- acc_hat >= 0.8
votes <- rowMeans(pred[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)



### Dimesnion Reduction

## Q1 

data("tissue_gene_expression")
dim(tissue_gene_expression$x)


pc <- prcomp(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()


## Q2

avgs <- rowMeans(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], avg = avgs, 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(avgs, pc_1, color = tissue)) +
  geom_point()
cor(avgs, pc$x[,1])


## Q3

x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()


## Q4

for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}


## Q5

plot(summary(pc)$importance[3,])

