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