### Breast Cancer Project Part 1 ###


##### To Update all packages ####
# update.packages()
##### ###### ###### ###### ######

options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)

data(brca)

## Q1. Part 1
## How many samples in the dataset
nrow(brca$x)
dim(brca$x)[1]


## @1 Part 2
## How many Predictors are in the matrix

dim(brca$x)[2]


## Q1 Part 3
## What proportion of the samples are malignant?

mean(brca$y == 'M')


## Q1 Part 4
## Which column number has the highest mean?

which.max(colMeans(brca$x))


## Q1 Part 5
## Which column number has the lowest standard deviation?

which.min(colSds(brca$x))


## Q2 Scaling the Matrix
## Part 1 After scaling, what is the standard deviation of the first column?

x_subtract_val <- sweep(brca$x, 2, colMeans(brca$x))

x_scale_by_divide <- sweep(x_subtract_val, 2, colSds(brca$x), FUN = "/")

sd(x_scale_by_divide[,1])


## Part 2 After scaling, what is the median value of the first column?

median(x_scale_by_divide[,1])



## Q3 Distance
## PArt 1  What is the average distance between the first sample, which is benign, and other benign samples?

dist_samples <- dist(x_scale_by_divide)
dist_btw_benign <- as.matrix(dist_samples)[1, brca$y == "B"]
mean(dist_btw_benign)
# OR
# mean(dist_btw_benign[2:length(dist_btw_benign)])


## Part 2  What is the average distance between the first sample and malignant samples?

dist_btw_malignant_benign <- as.matrix(dist_samples)[1, brca$y == "M"]
mean(dist_btw_malignant_benign)



## Q4 Heatmap

## Part 1 Make a heatmap of the relationship between features using the scaled matrix.

relation_features <- dist(t(x_scale_by_divide))
heatmap(as.matrix(relation_features), labRow = NA, labCol = NA)



## Q5 Hierarchial Clusterring

hire_clust_features <- hclust(relation_features)
tree_groups <- cutree(hire_clust_features, k = 5)
split(names(tree_groups), tree_groups)


###############################################################

### BREAST CANCER Project Part 2 ###

## Q6. PCA Proportion of variance 

## Part 1 What proportion of variance is explained by the first principal component?
## Part 2 How many principal components are required to explain at least 90% of the variance?

prop_variance <- prcomp(x_scale_by_divide)

summary(prop_variance)

## Ans for Part 1 : First value of Promportion of variance in PC1
## Ans for Part2 : First value that is greater than 90% i.e. Cumulative Proportion PC7



## Q7 PCA : Plotting PCs
## Plot the first two principal components with color representing tumor type (benign/malignant).

data.frame(prop_variance$x[,1:2], type = brca$y) %>% 
  ggplot(aes(PC1, PC2, color = type)) +
  geom_point()



## Q8 PCA : PC Box Plot
## Make a boxplot of the first 10 PCs grouped by tumor type.

data.frame(type = brca$y, prop_variance$x[,1:10]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()



############################################################################

### BREAST CANCER Project  Part 3 ###

set.seed(1)
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scale_by_divide[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scale_by_divide[-test_index,]
train_y <- brca$y[-test_index]


## Q9 Training & Test Sets

## Part 1 What proportion of the training set is benign?

mean(train_y == "B")


## Part 2 What proportion of the test set is benign?

mean(test_y == "B")


## Q10 K-means Clustering

predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

## 10 a
set.seed(3)
k <- kmeans(train_x, centers = 2)
kmean_predict <- ifelse(predict_kmeans(test_x, k) == 1, "B", "M")
mean(kmean_predict == test_y)


## 10 b
## Part 1 What proportion of benign tumors are correctly identified?

sensitivity(factor(kmean_predict), test_y, positive = "B")


## Part 2 What proportion of malignant tumors are correctly identified?

sensitivity(factor(kmean_predict), test_y, positive = "M")


## Q11 Logistic Regression Model
## Fit a logistic regression model on the training set using all predictors. Ignore warnings about the algorithm not 
## converging. Make predictions on the test set.
##
## What is the accuracy of the logistic regression model on the test set?

train_glm <- train(train_x, train_y, method = "glm")
predict_on_glm <- predict(train_glm, test_x)
mean(predict_on_glm == test_y)


## Q12 LDA & QDA Models
## Train an LDA model and a QDA model on the training set. Make predictions on the test set using each model.

## Part 1 What is the accuracy of the LDA model on the test set?

train_lda <- train(train_x, train_y, method = "lda")
predict_on_lda <- predict(train_lda, test_x)
mean(predict_on_lda == test_y)


## Part 2 What is the accuracy of the QDA model on the test set?

train_qda <- train(train_x, train_y, method = "qda")
predict_on_qda <- predict(train_qda, test_x)
mean(predict_on_qda == test_y)


## Q 13 Loess Model

## What is the accuracy of the loess model on the test set?
  
set.seed(5)

train_loess <- train(train_x, train_y, method = "gamLoess")
predict_loess <- predict(train_loess, test_x)
mean(predict_loess == test_y)



###############################################################################################

### BREAST CANCER Project Part 4

## Q 14 k-nearets neighbours model

## Train a k-nearest neighbors model on the training set. Try odd values of  k  from 3 to 21. Use the final model 
## to generate predictions on the test set.

## Part 1 What is the final value of  k  used in the model?

set.seed(7)

tuning_seq <- data.frame(k = seq(3, 21, 2))
train_knn <- train(train_x, train_y, method = "knn", tuneGrid = tuning_seq)
train_knn$bestTune


## Part 2 What is the accuracy of the kNN model on the test set?

predict_knn <- predict(train_knn, test_x)
mean(predict_knn == test_y)



## Q15 Random Forest Model

## 15 a   Train a random forest model on the training set. Generate predictions on the test set.

## Part 1  What value of mtry gives the highest accuracy?

set.seed(9)

tuning_seq2 <- data.frame(mtry = c(3,5,7,9))
train_randForest <- train(train_x, train_y, method = "rf", tuneGrid = tuning_seq2, importance = TRUE)
train_randForest$bestTune


## Part 2  What is the accuracy of the random forest model on the test set?

predict_randforest <- predict(train_randForest, test_x)
mean(predict_randforest == test_y)


## Part 3  What is the most important variable in the random forest model?

varImp(train_randForest)



## Q 16 Creating an ensemble

## 16 a. Create an ensemble using the predictions from the 7 models created in the previous exercises: k-means, logistic 
## regression, LDA, QDA, loess, k-nearest neighbors, and random forest. Use the ensemble to generate a majority 
## prediction of the tumor type (if most models suggest the tumor is malignant, predict malignant).

## What is the accuracy of the ensemble prediction?


ensemble <- cbind(glm = predict_on_glm == "B", lda = predict_on_lda == "B", qda = predict_on_qda == "B", 
                  loess = predict_loess == "B", rf = predict_randforest == "B", knn = predict_knn == "B", 
                  kmeans = kmean_predict == "B")

ensemble_predict <- ifelse(rowMeans(ensemble) > 0.5, "B", "M")
mean(ensemble_predict == test_y)


## 16 b Make a table of the accuracies of the 7 models and the accuracy of the ensemble model.

## Which of these models has the highest accuracy?

models <- c("K-Means", "Logistic Regression", "LDA", "QDA", "Loesss", "k nearest neighbour", "Random Forest", "ensemble")

accuracy <- c(mean(kmean_predict == test_y),
              mean(predict_on_glm == test_y),
              mean(predict_on_lda == test_y),
              mean(predict_on_qda == test_y),
              mean(predict_loess == test_y),
              mean(predict_knn == test_y),
              mean(predict_randforest == test_y),
              mean(ensemble_predict == test_y))

data.frame(Model = models, Accuracy = accuracy)


