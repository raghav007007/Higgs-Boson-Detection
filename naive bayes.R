#import training data
train <- read.csv(file.choose(), header = T)

#Sampling
ind <- sample(2, nrow(train), replace = T, prob = c(0.80, 0.2))
train_sample <- train[ind==1,-c(1,32)]
test_sample <- train[ind==2,-c(1,32)]

#Packages
install.packages("naivebayes")
library(naivebayes)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("psych")
library(psych)

#naive Bayes: based on bayes theorum
model_knn <- naive_bayes(train$Label~., data = train)
model_knn
#for categorical variable, we get probabiity and for numeric, we have mean and standard deviation
# when we have mean and standard deviation, we can calc. any probablity
plot(model)
#density plot for each variable

#prediction
p <- predict(model, train_sample)

#Confusion matrix  
table(actual = train_sample$Label, predicted = p)  
#97038+38141 = 135179
#Accuracy : 135179/199846= 0.6764158 = 0.67.64158%

#For test data
p <- predict(model, test_sample)
table(actual = test_sample$Label, predicted = p)    
#24492+9465 = 33957
#Accuracy : 33957/50124= 0.6774599 = 0.67.74599





