train <- read.csv(file.choose(), header = T)
table(train$Label)
set.seed(100)

#Sampling
ind <- sample(2, nrow(train), replace = T, prob = c(0.80, 0.2))
train_sample <- train[ind==1,]
test_sample <- train[ind==2,]


train_sample_ext <- train_sample[,-33]
label <- train_sample$Label
train_matrix <- as.matrix(train_sample_ext)
dtrain <- xgb.DMatrix(train_matrix, label = label, missing = -999.000)

#https://www.slideshare.net/ShangxuanZhang/kaggle-winning-solution-xgboost-algorithm-let-us-learn-from-its-author
param <- list()






