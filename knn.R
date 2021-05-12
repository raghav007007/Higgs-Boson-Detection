train  <- read.csv(file.choose(),header = T)
str(train)
train[train==-999.000] <- NA
train$Label=as.numeric(train$Label)-1
train$Label <- as.factor(train$Label)
summary(train)

#Scaling: Normalization
normalizer <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

#Converting to dataframe
train_missing <- subset(train, select=colMeans(is.na(train)) == 0) 
data <- train_missing[,-22]
train_norm<- normalizer(data)
train_norm <- as.data.frame(train_norm)

#Sampling
train_sam <- data[1:200000,]
test_sam <- data[200001:250000,]
train_Lab <- train[1:200000,33]
test_Lab <- train[200001:250000,33]

#Model: knn
install.packages("class")
library(class)

#k : how many nearest neighbours you want
modelknn <- knn(train = train_sam, test = test_sam, cl= train_Lab, k= 20)

#Confusion matrix
table(actual= test_Lab, pred= modelknn)
# correct classification : 32819 + 23 = 32842
#Missclassification : 17140 + 18= 17158
#Accuracy: 32842/50000 = 65.68%

table(train$Label)
164333/250000

#KNN is performing below worst case

#k = 10
modelknn <- knn(train = train_sam, test = test_sam, cl= train_Lab, k= 10)
#Confusion matrix
table(actual= test_Lab, pred= modelknn)
#accuracy decreases

#k = 15
modelknn <- knn(train = train_sam, test = test_sam, cl= train_Lab, k= 15)
#Confusion matrix
table(actual= test_Lab, pred= modelknn)

#k = 30
modelknn <- knn(train = train_sam, test = test_sam, cl= train_Lab, k= 30)
#Confusion matrix
table(actual= test_Lab, pred= modelknn)
