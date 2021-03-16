#Maya Caicedo
#MSc Data Science
#Applied Statistics and Data Mining Coursework
#Task 1: Decision Trees in R

#Installing and running required packages
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("party")
install.packages("rpart.plot")
library(ggplot2)
library(tidyverse)
library(party)
library(rpart.plot)

#Importing data file as a data frame
contraceptionData <- read.table("contraceptive.txt", sep = ",")

#Renaming the columns in the data frame to their variable names
colnames(contraceptionData) <- c("wife_age","wife_education","husband_education",
                                 "number_children","wife_religion","wife_working",
                                 "husband_occupation","standard_living",
                                 "media_exposure","contraceptive")
#Data visualization
#Plotting first plot of wife's age against the number of children she has
ggplot(data = contraceptionData) + 
  geom_point(mapping = aes(x = wife_age, y = number_children, 
                           color = wife_education)) +
  ggtitle("Wife's Age Against Number of Children Had Including Education") +
  xlab("Wife's Age") + ylab("Number of Children") +
  labs(color = "Wife's Education\n         Level")

#Plotting second plot of wife's education level against number of children she has 
ggplot(data = contraceptionData) + 
  geom_point(mapping = aes(x = wife_education, y = number_children)) +
  ggtitle("                Education Level Against Number of Children Had") +
  xlab("Wife's Education Level (1 = low, 4 = high)") + ylab("Number of Children")

#Creating decision trees
#Making a separate column for the converted "contraceptive" data for prediction 
contraceptionData$contraceptiveF <- as.factor(contraceptionData$contraceptive)

#Setting the seed to reproduce the same sample over and over
set.seed(1234)

#Creating a data frame to store a random permutation of a vector
#80% of data used for training, 20% used for validation 
pd <- sample(2, nrow(contraceptionData), replace = T, prob = c(0.8,0.2))

train <- contraceptionData[pd == 1,]
validate <- contraceptionData[pd == 2,]

#Assembling the tree
#Prediction made based on the wife's education level, her religion, the standard
#of living index, the wife's age, and her work status
contraceptionTree <- ctree(contraceptiveF ~ wife_education + wife_religion + 
                           standard_living + wife_age + wife_working, data = train)
print(contraceptionTree)
plot(contraceptionTree, type = "simple")

#Print prediction results 
predict (contraceptionTree)

#Creating frequency table for results of prediction on the train data 
tab <- table(predict(contraceptionTree), train$contraceptiveF)
print(tab)

#Calculating the classification accuracy on the train data
accuracy <- sum(diag(tab))/sum(tab)
accuracy
#Calculating the classification error on the train data
1 - accuracy

#Creating confusion matrix for results of prediction on the validate data
predictTab <- table(predict(contraceptionTree, newdata = validate), 
                    validate$contraceptiveF)
predictTab

#Calculating classification accuracy on the validate data 
predictAccuracy <- sum(diag(predictTab))/sum(predictTab)
predictAccuracy
#Calculating the classification error on the validate data 
1 - predictAccuracy

#Making a more aesthetically satisfying tree using rpart.plot package
niceTree <- rpart(contraceptiveF ~ wife_education + wife_religion + 
                  standard_living + wife_age + wife_working, data = train, 
                  method = 'class' )
rpart.plot(niceTree, extra = 104)

#Print prediction results
niceTreePredict <- predict(niceTree, train, type= "class")
niceTreePredict2 <- predict(niceTree, validate, type = "class")

#Creating confusion matrix for results of prediction on train data 
niceTreeTab <- table(train$contraceptiveF, niceTreePredict)
niceTreeTab

#Calculating classification accuracy on the train data
niceTreeAccuracy <- sum(diag(niceTreeTab))/sum(niceTreeTab)
niceTreeAccuracy

#Calculating classification error on the train data 
1 - niceTreeAccuracy

#Creating confusion matrix for results of prediction on validate data
niceTreeValidate <- table(validate$contraceptiveF, niceTreePredict2)
niceTreeValidate

#Calculating classification accuracy on the validate data
niceTreeValidateAccuracy <- sum(diag(niceTreeValidate))/sum(niceTreeValidate)
niceTreeValidateAccuracy

#Calculating classification error on the validate data
1 - niceTreeValidateAccuracy

#Creating a model for a third tree to try and improve accuracy
contraceptionTree2 <- ctree(contraceptiveF ~ wife_education + husband_education +
                              number_children + wife_religion + wife_working +
                              husband_occupation + media_exposure +
                             standard_living + wife_age, data = train)
print(contraceptionTree2)
plot(contraceptionTree2, type = "simple")

#Predicting results for the third tree
contraceptionTree2Predict <- predict(contraceptionTree2)

#Creating confusion matrix for results of prediction on training data
contraceptionTree2Table <- table(train$contraceptiveF, contraceptionTree2Predict)
contraceptionTree2Table

#Calculating classification accuracy on the training data
contraceptionTree2Accuracy <- 
  sum(diag(contraceptionTree2Table))/sum(contraceptionTree2Table)
contraceptionTree2Accuracy

#Calculating classification error on the training data
1 - contraceptionTree2Accuracy

#Creating confusion matrix for results of prediction on validation data
contraceptionTree2Validate <- table(predict(contraceptionTree2, newdata = validate), 
                    validate$contraceptiveF)
contraceptionTree2Validate

#Calculating classification accuracy on the validation data
contraceptionTree2ValidateAccuracy <- 
  sum(diag(contraceptionTree2Validate))/sum(contraceptionTree2Validate)
contraceptionTree2ValidateAccuracy

#Calculating classification error on the validation data
1 - contraceptionTree2ValidateAccuracy
