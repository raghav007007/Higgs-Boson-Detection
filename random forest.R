install.packages("randomForest")
library(randomForest)

train  <- read.csv(file.choose(),header = T)

#Random forest does not accept missing values
train[train==-999.000] <- NA
train_missing <- subset(train, select=colMeans(is.na(train)) == 0) 
train_missing <- train_missing[,-1] #EVENT ID

#Binary dependent variable
train_missing$Label=as.numeric(train_missing$Label)-1

#Sampling
set.seed(100)
ind <- sample(2, nrow(train_missing), replace = T, prob = c(0.80, 0.2))
train_sample <- train_missing[ind==1,]
test_sample <- train_missing[ind==2,]

#Random Forest:
#developed by aggregating trees (D.T's)
#Can be used for classification and regression
#Avoids overfitting
#can deal with large feature variables
# FEature selection based on importance of variables

set.seed(100)
train_sample$Label <- as.factor(train_sample$Label)
test_sample$Label <- as.factor(test_sample$Label)
str(train_missing$Label)
rf <- randomForest(Label~., data = train_sample, ntree= 15)
rf
#20 trees
rf1 <- randomForest(Label~., data = train_sample, ntree= 20)  
rf1
#After this model might overfit
#mtry 5: variables tried at each split
rf2 <- randomForest(Label~., data = train_sample, ntree= 10 , mtry= 5) 
rf2

#Predictions with all above
installed.packages("caret")
library(caret)

test_sample_Label <- test_sample$Label
test_sample <- test_sample[,-33]
pred <- predict(rf, test_sample)
pred1 <- predict(rf1, test_sample)
pred2 <- predict(rf2, test_sample)

#Confusion matrix 1:
table(actual= test_sample_Label, predictions = pred)

#Confusion matrix 2:
table(actual= test_sample_Label, predictions = pred1)

#Confusion matrix 3:
table(actual= test_sample_Label, predictions = pred2)

data <- train
data_Label <- data[,33]
data <- data[,-33]
rf3 <- rfcv(test_sample, test_sample$Label, cv.folds= 5)

#Confusion matrix
table(actual = test_sample_Label, predicted= rf3)

length(rf3)

# Not run:
result <- replicate(5, rfcv(test_sample, test_sample_Label), simplify=FALSE)
error.cv <- sapply(result, "[[", "error.cv")
matplot(result[[1]]$n.var, cbind(rowMeans(error.cv), error.cv), type="l",
        lwd=c(2, rep(1, ncol(error.cv))), col=1, lty=1, log="x",
        xlab="Number of variables", ylab="CV Error")
## End(Not run)





