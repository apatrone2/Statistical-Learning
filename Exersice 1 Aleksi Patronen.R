## Exercise 1 for Statistical Learning

#Task 1
#(a)

n <- 500 #how many observations
p <- 20  #how many parameters
ntr <- 5 # vectors of sampling space
sig <- 2.0 # standard deviation
X <- matrix(rnorm(n*p),ncol=p,nrow=n) # creates observation matrix with n obs. and p parameters
set.seed(11) # set random seed to 11
b_imp <- rnorm(ntr) # generates multivar. normal distribution in Space 5
b_zero <- rep(0,(p-ntr)) # vector that is repeat of 0 of p - ntr length 
b_true <- c(b_imp,b_zero) # appends b_imp and b_zero vectors 
y <- X%*%b_true + sig*rnorm(n) # creates response variate y, by taking 
#values of X *  b*true and adding random noise


# (b)
mid <- 500*0.7

training_X <- X[1:mid,]
test_X <- X[(mid+1): 500, ]

training_y <- y[1:mid, ]
test_y <- y[(mid+1):500, ]

training_data <- data.frame(training_X, training_y)
test_data <- data.frame(test_X, test_y)



#(c)
model <- lm(training_y ~ X1+ X2+ X3+ X4+ X5+ X6+ X7+ X8+ X9+ X10+ X11+ X12+ X13+ X14+ X15+ X16+ X17+ X18+ X19+ X20, data = training_data)
#long linear fit with lm functions
summary(model)
anova(model)
# Models show very high F values for variables X1, X3,X4 and X5. as high as 191.9854
# These variables can be consider significant
#On second run of the code the program gives slightly different
#results. Now highest F value is 163.0345
# Problems with seed.

#(d)

#sort parameters by lowest p-value

sorted_p_values <- sort(summary(model)$coefficients[,4]) # sort by p value
length(sorted_p_values)
Xs <- names(sorted_p_values) #take just variable names
Xs <- Xs[!Xs == "(Intercept)"]
Y <- "training_y"


fits <- lapply(seq_along(Xs), function(x){lm(paste(Y, '~ .'), data = training_data[, c(Y, Xs[1:x])]) })
#code found and copy pasted: https://stackoverflow.com/questions/66004870/iteratively-adding-variables-to-an-lm-function-in-r

training_MSEs <- c()
testing_MSEs <- c()

for (i in 1:20){
  training_hat <- predict.lm(fits[[i]])
  train_MSE <- mean((training_y - training_hat)^2)
  
  y_hat_test <- predict(fits[[i]], newdata = test_data)
  test_MSE <- mean((test_data$test_y - y_hat_test)^2)
  
  training_MSEs <- c(training_MSEs, train_MSE)
  testing_MSEs <- c(testing_MSEs, test_MSE)
  }

length(training_MSEs)
length(testing_MSEs)

#Create the required plots
variables <- seq(1:20)

plot(y = training_MSEs, x= variables, type = "l", col = "grey")
points(x = variables, y = testing_MSEs, type = "l", col = "red")

min(testing_MSEs)


#Task 2
#setwd("C:/Users/Aleksi/Desktop/KOulu/Statistical Learning/Exercises/Exercise 1/Data")
#View(seeds_dataset.txt)

#Kama, Rosa and Canadian
install.packages("ggplot2")

library(nnet)
library(dplyr)

set.seed(3)

seeds_dataset.f <- factor(seeds_dataset$V8, labels = c("Kama", "Rosa", "Canadian"))
seeds <- data.frame(seeds_dataset, seeds_dataset.f)

# ind <- sample(2, nrow(seeds),
#               replace = TRUE,
#               prob = c(0.6, 0.4))
# Different type sampling function.

training_data_seeds <- sample_n(seeds, size = (0.7*nrow(seeds)))
test_data_seeds <- sample_n(seeds, size = (0.3*nrow(seeds)))

#(a)    Perform multinomial logistic regression of all variables against the variety response 
#       using the multinom() function in library(nnet). Perform predictions based on the test 
#       data and calculate the accuracy.

multinomal_model <- multinom(data = training_data_seeds, 
                             formula = training_data_seeds$seeds_dataset.f ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V7,)


test_results <- predict(multinomal_model, newdata = test_data_seeds )
table(Actual_Values = test_data_seeds$seeds_dataset.f, Predicted_Values = test_results)
#accuracy was calculated by hand

acc <- (21 + 21 + 19)/(21+21+19+1+1); acc
# 0.968254 is quite good


#b.	Do K-nearest neighbors (KNN) using the knn() function in library(class).
#   Make a loop over K-values from 1 to 25, and calculate the test accuracy for the best model
library(class)

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}# accuracy function

#used raw  numbers instead of factor, didint get working with factors.
for (i in 1:25){
pr <- knn(train = training_data_seeds[-9], test = test_data_seeds[-9], cl = training_data_seeds[,8], k = i)
tab <- table(pr,test_data_seeds$seeds_dataset.f)
a <- accuracy(tab)
print(a)
}

#Best results yield when K = 1,2,3,5. Then test accuracy is 100%.
# I cant tell whether model is very accurate with relatively low numbers or
# there's something wrong with the model.

# c.	Finally, make an linear discriminant analysis (LDA) using the lda() function
# library(MASS). Perform predictions based on the test data and calculate the accuracy.
# Present a plot of the classification result that clearly shows how the test predictions
# relate to the training predictions. Which model performs best? Present your code
# with clear explanations as well as a discussion of the result and your main conclusion.


library(MASS)

#Creating the model
linear <- lda(training_data_seeds$seeds_dataset.f ~ V1+V2+V3+V4+V5+V6+V7 
                , data = training_data_seeds)
# predicting with the test data
p <- predict(linear, test_data_seeds)$class
#creating a table 
tab <- table(Predicted = p, Actual = test_data_seeds$seeds_dataset.f)
accuracy(tab) # 93.65079
#model predicted with 93.6% accuracy









