### Matrix Factorization

## we will construct a dataset that represents grade scores for 100 students in 24 different subjects. The overall 
## average has been removed so this data represents the percentage point each student received above or below the 
## average test score. So a 0 represents an average grade (C), a 25 is a high grade (A+), and a -25 represents a low 
## grade (F).

set.seed(1987)

n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))


## Our goal is to describe the student performances as succinctly as possible. For example, we want to know if these 
## test results are all just a random independent numbers. Are all students just about as good? Does being good in one 
## subject  imply you will be good in another? How does the SVD help with all this? We will go step by step to show that
## with just three relatively small pairs of vectors we can explain much of the variability in this  100×24  dataset. 


## Q1

my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)


## Q2 Examine corelation

my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

## Q3

s <- svd(y)
names(s)

y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

ss_y <- apply(y^2, 2, sum)
ss_yv <- apply((y%*%s$v)^2, 2, sum)
sum(ss_y)
sum(ss_yv)

## Q4.

plot(seq(1,ncol(y)), ss_y)

plot(seq(1,ncol(y)), ss_yv)


## Q5

plot(s$d, sqrt(ss_yv))


## Q6

sum(s$d[1:3]^2) / sum(s$d^2)


## Q7

identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))


## Q8

rowMeans(y)
UD <- sweep(s$u, 2, s$d, FUN = "*")
plot(UD[,1], rowMeans(y)) 


## Q9

my_image(s$v)


## Q10

plot(s$u[,1], ylim = c(-0.25, 0.25))
plot(s$v[,1], ylim = c(-0.25, 0.25))
with(s, my_image((u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE])))
my_image(y)


## Q11

plot(s$u[,2], ylim = c(-0.5, 0.5))
plot(s$v[,2], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 2, drop=FALSE]*d[2]) %*% t(v[, 2, drop=FALSE])))
my_image(resid)


## Q12

plot(s$u[,3], ylim = c(-0.5, 0.5))
plot(s$v[,3], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 3, drop=FALSE]*d[3]) %*% t(v[, 3, drop=FALSE])))
my_image(resid)


## Q13

y_hat <- with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(y, zlim = range(y))
my_image(y_hat, zlim = range(y))
my_image(y - y_hat, zlim = range(y))


### Clustering

## Q1

library(dslabs)
library(tidyverse)
data("tissue_gene_expression")
#x <- tissue_gene_expression$x
#y <- tissue_gene_expression$y
d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))


## Q2

h <- hclust(d)
plot(h)

# Liver


## Q3

cl <- kmeans(tissue_gene_expression$x, centers = 7)
table(cl$cluster, tissue_gene_expression$y)


## Q4

library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]

heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)

