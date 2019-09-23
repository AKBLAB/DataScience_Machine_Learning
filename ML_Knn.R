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



##### Not finished 23-Sep-2019

library(dslabs)
data("tissue_gene_expression")

#create training index (rather than test index) as suggested by comments
set.seed(1)

train_index <- createDataPartition(tissue_gene_expression$y, p = 0.5, list = FALSE)

# split original data set into x and y

# x <- tissue_gene_expression$x
# y <- tissue_gene_expression$y

# split x into train and test sets

test_set_x = as.data.frame(tissue_gene_expression$x)[train_index, ]
train_set_x = as.data.frame(tissue_gene_expression$x)[-train_index, ]


ks <- seq(1, 11, 2)

acc <- sapply(ks, function(k){
        fit <- knn3(y ~ x, data = train_set_x, k = ks)
        y_hat <- predict(fit, test_set_x, type = "class") %>% 
          factor(levels = levels(train_set_x$y))
        F_meas(data = y_hat, reference = test_set_x$y)
})

ks
acc


  
