#import training data
train <- read.csv(file.choose(), header = T)

#import testing data
test <- read.csv(file.choose(), header = T)

#viewing train data
View(train)

#data summary
summary(train)

## Data Cleaning 
#we are given that -999.000 is the weight asigned where values are absent.
#lets check how many missing values do we have.
#First we have to install dplyr pacakge and its library.
install.packages("dplyr")
library(dplyr)

#True are total missing values.
train %>% count(DER_mass_MMC == -999.000)#1

#similarly with other variables
train %>% count(DER_mass_transverse_met_lep == -999.000)#2
train %>% count(DER_mass_vis == -999.000)#3
train %>% count(DER_pt_h == -999.000)#4
train %>% count(DER_deltaeta_jet_jet == -999.000)#5
train %>% count(DER_mass_jet_jet == -999.000)#6
train %>% count(DER_prodeta_jet_jet == -999.000)#7
train %>% count(DER_deltar_tau_lep == -999.000)#8
train %>% count(DER_pt_tot == -999.000)#9
train %>% count(DER_sum_pt == -999.000)#10
train %>% count(DER_pt_ratio_lep_tau == -999.000)#11
train %>% count(DER_met_phi_centrality == -999.000)#12
train %>% count(PRI_tau_eta == -999.000)#13
train %>% count(PRI_tau_phi == -999.000)#14
train %>% count(PRI_lep_pt == -999.000)#15
train %>% count(PRI_lep_eta == -999.000)#16
train %>% count(PRI_lep_phi == -999.000)#17
train %>% count(PRI_met == -999.000)#18
train %>% count(PRI_met_phi == -999.000)#19
train %>% count(PRI_met_sumet == -999.000)#20
train %>% count(PRI_jet_num == -999.000)#21
train %>% count(PRI_jet_leading_pt == -999.000)#22
train %>% count(PRI_jet_leading_eta == -999.000)#23
train %>% count(PRI_jet_leading_phi == -999.000)#24
train %>% count(PRI_jet_subleading_eta == -999.000)#25
train %>% count(PRI_jet_subleading_phi == -999.000)#26
train %>% count(PRI_jet_all_pt == -999.000)#27
train %>% count(Weight == -999.000)#28
#there are total 9 variables with missing values.

#replacing all missing values ie. -999.000 by NA in both test and train data.
train[train==-999.000] <- NA
test[test==-999.000] <- NA

#Total signals and background noises
table(train$Label)
#85667 signals ie. Tau-Tau decay.

#converting above data to binary form.
#0: backgound noise
#1: signal
train$Label=as.numeric(train$Label)-1

#Worst case: If all the values are predicted as background noise
table(train$Label)
#We will have 65.73% accuracy.Thus model should have accuracy better than 65.73%

#Let's fix the seeds at 100
set.seed(100)

####### Algorithm 1: Generalized Boosted Regression Modelling(GBM)
#installing package
install.packages("gbm")
#installing library
library(gbm)

#Model 1: Cross Folds= 3, Depth= 2
gbmModel1 = gbm(Label~.-Weight, data=train, 
                cv.folds = 3,                 
                weights = weight,          
                interaction.depth=2,            
                verbose=TRUE) 

#Prediction for Model 1 and type as "response"
gbmPredictionRes1 = predict(gbmModel1, newdata=train, n.trees=gbmModel1$n.trees, type="response")

#Receiver Operating Characteristic Curve(ROC)
aucRes = roc(train$Label, gbmPredictionRes1)
plot(aucRes, print.thres=TRUE)

#Minimum threshold value
threshold=0.002
#Confusion Matrix
table(train$Label,gbmPredictionRes1>=threshold)


#Prediction for Model 1 and type as "Link"
gbmPredictionLin1 = predict(gbmModel1, newdata=train, n.trees=gbmModel1$n.trees, type="link")
aucLin = roc(train$Label, gbmPredictionLin1)
plot(aucLin, print.thres=TRUE)
threshold = -6.085
table(train$Label,gbmPredictionLin1>=threshold)
#We have Response type accuracy of 74.68% and for Link we have 74.96%


#Model 2: Cross Folds= 5, Depth= 3
gbmModel2 = gbm(Label~.-Weight, data=train, 
                cv.folds = 5,                 
                weights=train$Weight,           
                interaction.depth=2,            
                verbose=TRUE) 

#Prediction for Model 2 and type as "Response"
gbmPredictionLin2 = predict(gbmModel2, newdata=train, n.trees=gbmModel2$n.trees, type="link")
aucRes = roc(train$Label, gbmPredictionRes2)
plot(aucRes, print.thres=TRUE)
threshold = 0.002
table(train$Label,gbmPredictionRes2>=threshold)


#Prediction for Model 3 and type as "Link"
gbmPredictionRes2 = predict(gbmModel2, newdata=train, n.trees=gbmModel2$n.trees, type="response")
aucLin = roc(train$Label, gbmPredictionLin2)
plot(aucLin, print.thres=TRUE)
threshold = -6.056
table(train$Label,gbmPredictionLin2>=threshold)


#Model 3: Cross Folds= 6, Depth= 3
gbmModel3 = gbm(Label~.-Weight, data=train, 
                cv.folds = 6,                 
                weights=train$Weight,           
                interaction.depth=3,            
                verbose=TRUE) 


#Prediction for Model 3 and type as "Response"
gbmPredictionRes3 = predict(gbmModel3, newdata=train, n.trees=gbmModel3$n.trees, type="response")
aucRes = roc(train$Label, gbmPredictionRes3)
plot(aucRes, print.thres=TRUE)
threshold = 0.002
table(train$Label,gbmPredictionRes3>=threshold)


#Prediction for Model 4 and type as "Link
gbmPredictionLin3 = predict(gbmModel3, newdata=train, n.trees=gbmModel3$n.trees, type="link")
aucLin = roc(train$Label, gbmPredictionLin3)
plot(aucLin, print.thres=TRUE)
threshold = -6.039
table(train$Label,gbmPredictionLin3>=threshold)


#Model 4 Cross Folds= 10, Depth=3
gbmModel4 = gbm(Label~.-Weight, data=train, 
                cv.folds = 10,                 
                weights=train$Weight,           
                interaction.depth=3,            
                verbose=TRUE)

#Prediction for Model 2 and type as "Response"
gbmPredictionRes4 = predict(gbmModel4, newdata=train, n.trees=gbmModel4$n.trees, type="response")
aucRes = roc(train$Label, gbmPredictionRes4)
aucRes = roc(train$Label, gbmPredictionRes4)
plot(aucRes, print.thres=TRUE)
threshold = 0.002
table(train$Label,gbmPredictionRes4>=threshold)
#True Positive Rate for Response type.(should be as high as possible)
22349+45890
45890/68239

#False Positive Rate for Response type.(should be as low as possible)
141984+39777
39777/181761

#Prediction for Model 4 and type as "Link
gbmPredictionLin4 = predict(gbmModel4, newdata=train, n.trees=gbmModel4$n.trees, type="link")

#Histogram of predicted values for Model 4
qplot(gbmPredictionLin4,
      geom="histogram",
      main = "Predicted values Link", 
      xlab = "Predicted values",  
      xlim=c(-5.75,-6.6), fill = "Red" )
#We see that values are at peak before 0.002

aucLin = roc(train$Label, gbmPredictionLin4)
plot(aucLin, print.thres=TRUE)
threshold = -6.052
table(train$Label,gbmPredictionLin4>=threshold)
#True Positive Rate for Link type.(should be as high as possible)
41561+11091
41561/52652

#False Positive rate for Link type(should be as low as possible)
44106+153242
44106/197348
#For predicting a " Tau- Tau" decay, Link type of Model 4 is better but for predicting a background noise, Response type of model 4 is better

