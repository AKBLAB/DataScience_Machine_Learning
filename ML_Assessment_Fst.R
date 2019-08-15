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


