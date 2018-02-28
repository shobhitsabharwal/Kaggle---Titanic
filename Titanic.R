#Kaggle
#Titanic 
rm(list=ls())

#Read the data
dataset = read.csv("train.csv")

summary(dataset)


#filling empty value of Embarked with most occurring value ie S
dataset$Embarked[dataset$Embarked==""] = "S"

#Preprocessing the data
dataset$Pclass = as.factor(dataset$Pclass)
dataset$Sex = as.factor(dataset$Sex)
dataset$Embarked = as.factor(dataset$Embarked)
dataset$Age[is.na(dataset$Age)] = -1
dataset$Fare[is.na(dataset$Fare)] = median(dataset$Fare)

summary(dataset)

newDataset = dataset[,c(2,3,5,6,7,8,10,12)]

#splitting the newDataset into 2 part (81:19)
library(caTools)
set.seed(123)
split = sample.split(newDataset$Survived, SplitRatio = 0.81)
training_set = subset(newDataset, split == TRUE)
test_set = subset(newDataset, split == FALSE)

#Fiting Random forest model on test_set
rf = randomForest(test_set[,-1], as.factor(test_set[,1]), ntree =1000, importance = TRUE)

#predicting the survival value of test_set
y_pred = predict(rf, newdata = test_set[,-1])

#confusion matrix
table(test_set[,1], y_pred)

#Prediction on test data 

#Preprocessing the test Data
test = read.csv("test.csv")
test$Pclass = as.factor(test$Pclass)
test$Sex = as.factor(test$Sex)
test$Embarked = as.factor(test$Embarked)
test$Age[is.na(test$Age)] = -1
test$Fare[is.na(test$Fare)] = 14.454

newtest = test[,c(2,4,5,6,7,9,11)]
newtest$Survived = as.integer(0)
newtest = newtest[,c('Survived','Pclass','Sex','Age','SibSp', 'Parch', 'Fare', 'Embarked')]

#Workround for error : Type of predictors in new data do not match that of the training data.
newtest <- rbind(training_set[1, ] , newtest)
newtest <- newtest[-1,]

#Prediction on test data
submission = data.frame(PassengerId = test$PassengerId)
submission$Survived= predict(rf, newdata = newtest[,-1])

#saving file on disired format
write.csv(submission,"finalOutput.csv", row.names = FALSE)

