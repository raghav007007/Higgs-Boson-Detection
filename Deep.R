install.packages("keras")
library(keras)
install_keras()
# in terminal: sudo apt-get install python-pip python-virtualenv
# Keras is a high level NN API for deep leaarning
#we will use tensorflow backgound

train <- read.csv(file.choose(), header = T)
#replacing all missing values ie. -999.000 by NA in both test and train data.
train[train==-999.000] <- NA
train$Label=as.numeric(train$Label) -1

#change data to matrix
data <- as.matrix(train)

#removing dimesion names
dimnames(data) <- NULL

#Normalization
data[,1:32] <- normalize(data[,1:32])
summary(data)

#data partition
#Let's fix the seeds at 100
set.seed(100)
ind <- sample(2, nrow(data), replace = T, prob = c(0.80, 0.2))
train_sample <- data[ind==1, 1:31]
test_sample <- data[ind==2, 1:31]
train_target <- data[ind==1, 32]
test_target <- data[ind==2, 32]

#One Hot Encoding
trainingLabels <- to_categorical(train_target)
testLabels <- to_categorical(test_target)
head(testLabels)


#Model
# relu: rectified linear units
# softmax activation function in o/p layer helps to keep range b/w o&1 which can be used as probabilities
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 150, activation = 'relu', input_shape = c(32)) %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dense(units = 2, activation = 'softmax')
summary(model)

#Compile and configure
model %>% 
  compile(loss = 'binary_crossentropy',
          optimizer = 'adam',
          metrics = 'accuracy')
modelDNN <- model %>%
              fit(train_sample, trainingLabels,
                  epochs = 100, batch_size = 31,
                  validation_split = 0.2)
