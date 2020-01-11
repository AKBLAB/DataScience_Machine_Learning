### Recommendation Systems

library(dslabs)
library(caret)
library(tidyverse)
data("movielens")

## Q1. 

movielens %>% 
  group_by(movieId, year) %>%
  mutate(n = sqrt(n())) %>%
  summarize(n = n()) %>%
  ggplot(aes(year, n, group=year)) +
  geom_boxplot() + xlim(1980,2000)

## OR

movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Q2

res2 <- movielens %>%
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(avg_rating = mean(rating), n = n(), title=title[1], years=2018 - first(year)) %>%
  mutate(n_year = n / years) %>%
  top_n(25, n_year) %>%
  arrange(desc(n_year))

## OR

movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))

## Q3

movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

## Q6.

library(lubridate)
movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

## Q8

movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



### Regularization

## An education expert is advocating for smaller schools. The expert bases this recommendation on the fact that among 
## the best performing schools, many are small schools. Let's simulate a dataset for 1000 schools. First simulate the
## number of students in each school.


set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))

## Assign a true quality for each school that is completely independent from size. 

set.seed(1)

mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

## Check the top 10 schools

schools %>% top_n(10, quality) %>% arrange(desc(quality))

## Now let's have the students in the school take a test. There is random variability in test taking, so we will 
## simulate the test scores as normally distributed with the average determined by the school quality with a 
## standard deviation of 30 percentage points.

set.seed(1)

mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))



## Q1

schools_top10 <- schools %>%
  top_n(10, score) %>% 
  arrange(desc(score))

schools_top10

## Q2

schools %>%
  summarize(median(size))
## OR
median(schools$size)


schools_top10 %>%
  summarize(median(size))
## OR
schools %>% top_n(10, score) %>% .$size %>% median()


## Q3

schools_bottom10 <- schools %>%
  top_n(10, -score) %>% 
  arrange(score)
schools_bottom10

schools_bottom10 %>%
  summarize(median(size))

## OR

median(schools$size)
schools %>% top_n(-10, score) %>% .$size %>% median()


## Q4

schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2) 

# The standard error of the score has larger variability when the school is smaller, which is why both the best and 
# the worst schools are more likely to be small.


## Q5

overall <- mean(sapply(scores, mean))

alpha <- 25
schools5 <- schools %>%
  mutate(score_dev = overall + (score - overall) * size / (size + alpha)) %>%
  arrange(desc(score_dev))
#    mutate(quality_new = score_dev-80)
schools5 %>%
  top_n(10, score_dev)

## OR

alpha <- 25
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))


## Q6

alphas <- seq(10,250)
rmses <- sapply(alphas, function(alpha){
  schools %>%
    mutate(score_dev = overall + (score - overall) * size / (size + alpha)) %>%
    summarize(rmse = sqrt(1/1000 * sum((score_dev-quality)^2))) %>%
    pull(rmse)
})

alphas[which.min(rmses)]

plot(alphas, rmses)

## OR

alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]


## Q7

alpha_best <- 135
schools7 <- schools %>%
  mutate(score_dev = overall + (score - overall) * size / (size + alpha_best)) %>%
  arrange(desc(score_dev)) %>%
  top_n(10, score_dev)
schools7

## OR

alpha <- alphas[which.min(rmse)]  
score_reg <- sapply(scores, function(x)
  overall+sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))


## Q8

alphas <- seq(10,250)
rmses <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})

alphas[which.min(rmses)]

plot(alphas, rmses)

## OR

alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]  

