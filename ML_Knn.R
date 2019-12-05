### Distance ###



library(dslabs)
library(tidyverse)
library(caret)

data("tissue_gene_expression")

# The dataset includes
# dim(tissue_gene_expression$x) :-  gene expression levels of 500 genes from 189 
#                                   biological samples representing seven different
#                                   tissues
# table(tissue_gene_expression$y) :- tissue types

## lines of code to computes the Euclidean distance between each observation and 
## stores it in the object.

d <- dist(tissue_gene_expression$x)



## compare the distances between observations 1 and 2 (both cerebellum), 
## observations 39 and 40 (both colon), and observations 73 and 74 (both 
## endometrium).

ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]



## Make a plot of all the distances using the image function to see the pattern 

image(as.matrix(d))



### Knn ###

## predict sex based on height by using knn and calculate F_1.


datval <- heights$sex
set.seed(1)
test_index <- createDataPartition(datval, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]     

ks <- seq(1, 101, 3)

F_1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(train_set$sex))
  F_meas(data = y_hat, reference = test_set$sex)
})

max(F_1)
ks[which.max(F_1)]

# to check through plot

plot(ks, F_1)



## use the same gene expression example used in the Comprehension Check: 
## Distance exercises
## Split the data into training and test sets, and report the accuracy you obtain.
## Try it for k = 1, 3, 5, 7, 9, 11. Set the seed to 1 before splitting the data.

set.seed(1)
library(caret)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
test_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[-test_index,], y[-test_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                   type = "class")
  mean(y_hat == y[test_index])
})

##### For practise incomplete code need to be complete #####
data('tissue_gene_expression')

ks <- seq(1, 11, 2)

set.seed(1, sample.kind="Rounding")

train_index <- createDataPartition(tissue_gene_expression$y, p = 0.5, list = FALSE)

x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

# split x and y into train and test sets
train_set_x <- x[train_index,]
test_set_x <- x[-train_index,]

####### ######## ######## ######### ######### #########



## Cross Validation ##

## Because x and y are completely independent, one should not be able to predict
## y using x with accuracy greater than 0.5. Confirm this by running 
## cross-validation using logistic regression to fit the model. Because we have 
## so many predictors, we selected a random sample x_subset. 
## Use the subset when training the model.

set.seed(1996)

n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

## Which code correctly performs this cross-validation?

fit <- train(x_subset, y, method = "glm")
fit$results

## BiocManager::install("genefilter")
## library(genefilter)
## tt <- colttests(x, y) 
##
## Which of the following lines of code correctly creates a vector of the 
## p-values called pvals?

pvals <- tt$p.value



## Create an index ind with the column numbers of the predictors that were 
## "statistically significantly" associated with y. Use a p-value cutoff of 
## 0.01 to define "statistically significantly."

## How many predictors survive this cutoff?

ind <- which(pvals <= 0.01)
length(ind)

# Another correct method tried 
tt <- colttests(x, y)
pvals <- tt$p.value
sum(pvals<0.01)



## re-run the cross-validation after redefinining x_subset to be the subset of x
## defined by the columns showing "statistically significant" association with y.

## What is the accuracy now?

x_subset <- x[,ind]
fit <- train(x_subset, y, method = "glm")
fit$results



## Use the train function with kNN to select the best k for predicting tissue 
## from gene expression on the tissue_gene_expression dataset from dslabs. 
## Try k = seq(1,7,2) for tuning parameters. 
# For this question, do not split the data into test and train sets 
## (understand this can lead to overfitting, but ignore this for now).

## What value of k results in the highest accuracy?

data("tissue_gene_expression")
fit <- with(tissue_gene_expression, train(x, y, method = "knn", tuneGrid = data.frame( k = seq(1, 7, 2))))
ggplot(fit)
fit$results



### Bootstrap ###

## create 10 bootstrap samples for the mnist_27 dataset

library(caret)
library(dslabs)
set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

## How many times do 3, 4, and 7 appear in the first resampled index?

sum(indexes[[1]] == 3)
sum(indexes[[1]] == 4)
sum(indexes[[1]] == 7)

## How many times 3 appear in all the resampled indexes

x=sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)


## Generate a random dataset (y <- rnorm(100, 0, 1)) , Estimate the 75th 
## quantile, which we know is qnorm(0.75), with the sample 
## quantile: quantile(y, 0.75).

## Set the seed to 1 and perform a Monte Carlo simulation with 10,000 repetitions
## , generating the random dataset and estimating the 75th quantile each time. 
## What is the expected value and standard error of the 75th quantile?

y <- rnorm(100, 0, 1)
set.seed(1)
N <- 10000
M <- replicate(N, {
      y <- rnorm(100, 0, 1)
      quantile(y,0.75)
})

mean(M)
sd(M)



## Completed

set.seed(1)
y <- rnorm(100, 0, 1)
set.seed(1)
indexes <- createResample(y, 10)

M <- sapply(indexes,function(ival) {
            B <- y[ival]
            quantile(B, 0.75)
})
mean(M)
sd(M)


##

set.seed(1)
y <- rnorm(100, 0, 1)
set.seed(1)
indexes <- createResample(y, 10000)

M <- sapply(indexes,function(ival) {
  B <- y[ival]
  quantile(B, 0.75)
})
mean(M)
sd(M)



### Generative Models

## FOR LDA :  Create a dataset of samples from just cerebellum and hippocampus, two parts of the brain,
## and a predictor matrix with 10 randomly selected columns

library(dslabs)
library(caret)
data("tissue_gene_expression")

set.seed(1993) #set.seed(1993, sample.kind="Rounding") if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

# estimate the accuracy of LDA.

fit_LDA <- train(x, y, method = "lda")


## Which TWO genes appear to be driving the algorithm?

library(tidyverse)

t(fit_LDA$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()



## FOR QDA :  Create a dataset of samples from just cerebellum and hippocampus, two parts of the brain,
## and a predictor matrix with 10 randomly selected columns

library(dslabs)      
library(caret)
data("tissue_gene_expression")

set.seed(1993) #set.seed(1993, sample.kind="Rounding") if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

# estimate the accuracy of QDA.

fit_QDA <- train(x, y, method = "qda")


## Which TWO genes drive the algorithm when using QDA instead of LDA?

t(fit_QDA$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()


## Which TWO genes drive the algorithm after performing the scaling?

fit_LDA <- train(x, y, method = "lda", preProcess="center")



## Repeat the LDA analysis from above but using all tissue types.

library(dslabs)      
library(caret)
data("tissue_gene_expression")

set.seed(1993) #set.seed(1993, sample.kind="Rounding") if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

fit_LDA <- train(x, y, method = "lda", preProcess="center")

fit_LDA$results["Accuracy"]
