### Trees and Random Forests

## Creating a simple dataset where the outcome grows 0.75 units on average for every increase in a predictor 
## use rpart to fit a regression tree and saves the result to fit

  library(rpart)
  n <- 1000
  sigma <- 0.25
  set.seed(1) #set.seed(1, sample.kind = "Rounding") if using R 3.6 or later
  x <- rnorm(n, 0, 1)
  y <- 0.75 * x + rnorm(n, 0, sigma)
  dat <- data.frame(x = x, y = y)
  fit <- rpart(y ~ ., data = dat)


## make a scatter plot of y versus x along with the predicted values based on the fit.
  library(ggplot2)
  library(tidyverse)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)



## run Random Forests instead of a regression tree using randomForest from the __randomForest__ package, and 
## remake the scatterplot with the prediction line. 
 
  library(randomForest)
  fit <- randomForest(y ~ x, data = dat) 
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)
  

## Use the plot function to see if the Random Forest from above code has converged or if we need more trees.
  plot(fit,type = "l")
  

## It seems that the default values for the Random Forest result in an estimate that is too flexible (unsmooth). 
## Re-run the Random Forest but this time with a node size of 50 and a maximum of 25 nodes. Remake the plot.
  
  
  library(randomForest)
  fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
    dat %>% 
    mutate(y_hat = predict(fit)) %>% 
    ggplot() +
    geom_point(aes(x, y)) +
    geom_step(aes(x, y_hat), col = 2)
    
    plot(fit,type = "l")
  
  
    
### Caret package assessment
    
    
## Use the rpart function to fit a classification tree to the tissue_gene_expression dataset. Use the train function to estimate 
## the accuracy. 
## Plot the accuracies to report the results of the best model. Set the seed to 1991. Which value of cp gives the highest accuracy?    
    
    library(rpart)
    library(tidyverse)
    library(caret)
    library(dslabs)
    data("tissue_gene_expression")
    set.seed(1991)
    
    fit <- with(tissue_gene_expression, {
      train(x , y, 
            method = "rpart",
            tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))
    })
    
    ggplot(fit)
    

## There are only 6 placentas in the dataset. By default, rpart requires 20 observations before splitting a node. That means 
## that it is difficult to have a node in which placentas are the majority. Rerun the analysis of previous exercise
## but this time, allow rpart to split any node. Look at the confusion matrix again to determine whether the accuracy increases. 
## Again, set the seed to 1991.
    
    library(rpart)
    library(tidyverse)
    library(caret)
    library(dslabs)
    data("tissue_gene_expression")
    set.seed(1991)
    
    fit <- with(tissue_gene_expression, {
      train(x , y, 
            method = "rpart",
            control = rpart.control(minsplit = 0),
            tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))
    })
    
    ggplot(fit)
    confusionMatrix(fit)


## Plot the tree from the best fitting model of the analysis of previous question.
## Which gene is at the first split?
    
    plot(fit$finalModel)
    text(fit$finalModel)

    
## With just seven genes, we are able to predict the tissue type. Now let's see if we can predict the tissue type with even 
## fewer genes using a Random Forest. Use the train function and the rf method to train a Random Forest model and save it to 
## an object. Try out values of mtry ranging from seq(50, 200, 25) (you can also explore other values on your own). 
## What mtry value maximizes accuracy? To permit small nodesize to grow as we did with the classification trees, 
## use the following argument: nodesize = 1
    
    
    library(rpart)
    library(tidyverse)
    library(caret)
    library(dslabs)
    library(randomForest)
    data("tissue_gene_expression")
    set.seed(1991)
    
    fit <- with(tissue_gene_expression, {
      train(x , y, 
            method = "rf",
            nodesize = 1,
            tuneGrid = data.frame(mtry = seq(50, 200, 25)))
    })
    
    ggplot(fit)
    
## Use the function varImp on the output of train and save it to an object called imp.
    
    imp <- varImp(fit)
    imp


    
### Titanic Exercise Part 1
    
## Background

## The Titanic was a British ocean liner that struck an iceberg and sunk on its maiden voyage in 1912 from the United Kingdom 
## to New York. More than 1,500 of the estimated 2,224 passengers and crew died in the accident, making this one of the largest 
## maritime disasters ever outside of war. The ship carried a wide range of passengers of all ages and both genders, 
## from luxury travelers in first-class to immigrants in the lower classes. However, not all passengers were equally likely to 
## survive the accident. You will use real data about a selection of 891 passengers to predict which passengers survived.

    library(titanic)    # loads titanic_train data frame
    library(caret)
    library(tidyverse)
    library(rpart)
    
    # 3 significant digits
    options(digits = 3)
    
    # clean the data - `titanic_train` is loaded with the titanic package
    titanic_clean <- titanic_train %>%
      mutate(Survived = factor(Survived),
             Embarked = factor(Embarked),
             Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
             FamilySize = SibSp + Parch + 1) %>%    # count family members
      select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)
    
## Q1 
    
    set.seed(42)
    
    y <- titanic_clean$Survived
    
    testIndex <- createDataPartition(y, times = 1,p = 0.2, list = FALSE)
    trainset <- titanic_clean %>% slice(-testIndex)
    testset <- titanic_clean %>% slice(testIndex)
    nrow(trainset)
    nrow(testset)
    
    mean(trainset$Survived == 1)
    

## Q2 The simplest prediction method is randomly guessing the outcome without using additional predictors. These methods will help 
##  us determine whether our machine learning algorithm performs better than chance. How accurate are two methods of guessing 
## Titanic passenger survival?
    
## Set the seed to 3. For each individual in the test set, randomly guess whether that person survived or not by sampling from 
## the vector c(0,1). Assume that each person has an equal chance of surviving or not surviving.
    
## What is the accuracy of this guessing method?
    
    set.seed(3)
    samplvalue <- sample(c(0,1),nrow(testset), replace = TRUE)
    mean(samplvalue == testset$Survived)


## Q3
    
    trainset %>%
      group_by(Sex) %>%
      summarize(Survived = mean(Survived == 1)) %>%
      filter(Sex == "female") %>%
      pull(Survived)
    
    trainset %>%
      group_by(Sex) %>%
      summarize(Survived = mean(Survived == 1)) %>%
      filter(Sex == "male") %>%
      pull(Survived)
    
    
    
    
    pred = list()
    for (i in 1:length(testset$Survived)) {
      if (testset[i,]$Sex == 'male') {
        pred[i] = 0
      }
      else {
        pred[i] = 1 
      }
    }
    mean(as.numeric(as.character( testset$Survived)) == pred)
    
    # OR
    
    sex_model <- ifelse(testset$Sex == "female", 1, 0)    # predict Survived=1 if female, 0 if male
    mean(sex_model == testset$Survived)    # calculate accuracy
    

## Q4a
    trainset %>%
      group_by(Pclass) %>%
      summarize(Survived = mean(Survived == 1))
    
## Q4b
    simple_class_model <- ifelse(testset$Pclass == 1, 1, 0)
    mean(simple_class_model == testset$Survived)
    confusionMatrix(data= factor(simple_class_model), reference = factor(testset$Survived))
    
## Q4c
    
    trainset %>%
      group_by(Sex, Pclass) %>%
      summarize(Survived = mean(Survived == 1)) %>%
      filter(Survived > 0.5)
    
## Q4d
    simple_gender_model <- ifelse(testset$Pclass %in% 1:2 & testset$Sex == "female", 1, 0)
    mean(simple_gender_model == testset$Survived)
    confusionMatrix(data= factor(simple_gender_model), reference = factor(testset$Survived))
    
    ## OR
    
    sex_class_model <- ifelse(testset$Sex == "female" & testset$Pclass != 3, 1, 0)
    mean(sex_class_model == testset$Survived)
    
    
## Q5 a
    
    survival_pred_sex <-as.factor(sex_model)
    confusionMatrix(survival_pred_sex, testset$Survived)
    
    survival_pred_Sex_class <- as.factor(simple_gender_model)
    confusionMatrix(survival_pred_Sex_class, testset$Survived)
    
    survival_pred_class <- as.factor(simple_class_model)
    confusionMatrix(survival_pred_class, testset$Survived)
    
## Q6
    
    library(yardstick)
    F_meas(as.factor(sex_model), as.factor(testset$Survived))
    F_meas(as.factor(simple_gender_model), as.factor(testset$Survived))
    F_meas(as.factor(simple_class_model), as.factor(testset$Survived))


## Q7
    
    set.seed(1)
    fit_lda <- train(Survived ~ Fare, data = trainset, method = 'lda')
    Survived_hat <- predict(fit_lda, testset)
    mean(testset$Survived == Survived_hat)
    
    set.seed(1)
    fit_qda <- train(Survived ~ Fare, data = trainset, method = 'qda')
    Survived_hat <- predict(fit_qda, testset)
    mean(testset$Survived == Survived_hat)

## Q8
    set.seed(1)
    fit_logreg_a <- glm(Survived ~ Age, data = trainset, family = 'binomial')
    survived_hat_a <- ifelse(predict(fit_logreg_a, testset) >= 0, 1, 0)
    mean(survived_hat_a == testset$Survived)
    
    set.seed(1)
    fit_logreg_b <- glm(Survived ~ Sex + Pclass + Fare + Age, data = trainset, family = 'binomial')
    survived_hat_b <- ifelse(predict(fit_logreg_b, testset) >= 0, 1, 0)
    mean(survived_hat_b == testset$Survived)
    
    set.seed(1)
    str(trainset)
    fit_logreg_c <- glm(Survived ~ ., data = trainset, family = 'binomial')
    survived_hat_c <- ifelse(predict(fit_logreg_c, testset) >= 0, 1, 0)
    mean(survived_hat_c == testset$Survived)
    
## Q9 a
    set.seed(6)
    k <- seq(3,51,2)
    fit_knn9a <- train(Survived ~ ., data = trainset, method = "knn", tuneGrid = data.frame(k))
    fit_knn9a$bestTune
    
## Q9 b
    
    ggplot(fit_knn9a)

    
## Q9 C
    survived_hat <- predict(fit_knn9a, testset) %>% factor(levels = levels(testset$Survived))
    cm_test <- confusionMatrix(data = survived_hat, reference = testset$Survived)
    cm_test$overall["Accuracy"]

## Q10
    set.seed(8)
    fit_knn10 <- train(Survived ~ ., 
                       data=trainset, 
                       method = "knn",
                       tuneGrid = data.frame(k = seq(3, 51, 2)),
                       trControl = trainControl(method = "cv", number=10, p=0.9))
    fit_knn10
    survived_hat <- predict(fit_knn10, testset)
    cm_test <- confusionMatrix(data = survived_hat, reference = testset$Survived)
    cm_test$overall["Accuracy"]
    
## Q11 a
    set.seed(10, sample.kind = 'Rounding')
    fit_rpart11 <- train(Survived ~ ., 
                         data=trainset, 
                         method = "rpart",
                         tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
    plot(fit_rpart11)
    survived_hat <- predict(fit_rpart11, testset)
    cm_test <- confusionMatrix(data = survived_hat, reference = testset$Survived)
    cm_test$overall["Accuracy"]
    
    
# Q11 b
    fit_rpart11$finalModel
    plot(fit_rpart11$finalModel, margin=0.1)
    text(fit_rpart11$finalModel, cex = 0.75)

  
# Q12
    set.seed(14, sample.kind = 'Rounding')
    fit12_rf <- train(Survived ~., 
                      data = trainset,
                      method = "rf", 
                      tuneGrid = data.frame(mtry = seq(1, 7)), 
                      ntree = 100)
    fit12_rf$bestTune
    
    survived_hat <- predict(fit12_rf, testset)
    mean(survived_hat == testset$Survived)
    
    varImp(fit12_rf)
    
    
    
    