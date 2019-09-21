## Matrices ##


# For each observation in the mnist training data, compute the proportion of pixels
# that are in the grey area, defined as values between 50 and 205. (To visualize 
# this, you can make a boxplot by digit class.)

# What proportion of the 60000*784 pixels in the mnist training data are in the 
# grey area overall, defined as values between 50 and 205?
  

library(matrixStats)
library(ggplot2)

# by using box plot 

mnist <- read_mnist()
y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")

# by using rowMeans function

respropotion <- mean(rowMeans(mnist$train$images>50 & mnist$train$images<205))
respropotion
