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


