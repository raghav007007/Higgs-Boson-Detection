train <- read.csv(file.choose(), header = T)
train[train==-999.000] <- NA
train$Label=as.numeric(train$Label)-1

#Sampling
set.seed(100)
ind <- sample(2, nrow(train), replace = T, prob = c(0.80, 0.2))
train_sample <- train[ind==1,]
test_sample <- train[ind==2,]
attach(train_sample)

#Decision trees with rpart
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
tree <- rpart(Label~DER_deltaeta_jet_jet+DER_deltar_tau_lep+DER_lep_eta_centrality+DER_mass_jet_jet
              +DER_mass_MMC, data = train_sample)
rpart.plot(tree, type = 4)
#0.34 probability of a signal
#35% of data has DER_mass_MMC < 101
#0.16 is the probability to have signal.

#Predict
test_sample2 <- test_sample[,-33]
pred <- predict(tree, test_sample2)

#missclassification error for test
testPred <- predict(tree, newdata = test_sample2)
tab <- table(testPred, test_sample$Label)
print(tab)
1-sum(diag(tab))/sum(tab)

# Confusion matrix
table(actual= test_sample$Label, prediction= pred)

  
  
  
  
  
  
  
  