# Codename    : Housing Integrated
# Code Purpose: Measurement of Accuracy level of classification of any Housing
# Input data  : Housing Data
# Output File : Accuracy in percentage and Confusion matrix
# Use of Code : Housing data should be following variable: 
#
#       Vac_level (There are only 2 levels = 10 & 12)
#       R_95P,
#       R_Crest_Factor,
#       R_RMS,
#       Theta_Var,
#       Theta_Skew,
#       Theta_Kurt
#
# Code Created: 08-Jan-2015
# Author      : Pratyay Karar, Sneha Dutta

#rm(list=ls())

#setwd('C:\\Users\\Pratyay Karar\\Desktop\\Z\\Z_Al\\AL_Housing')
args <- commandArgs(TRUE)
train <- read.csv(args[1])

#train = read.csv('Housing.csv', header=TRUE, sep=",")

train$Vac_level_1 <- ifelse(train$Vac_level == '10',0,1)
train$Vac_level_1 <- factor(train$Vac_level_1)

n = dim(train)[1]
n1 = ifelse(n<=49,5,n/10)
n2 = n1/2.5

library('rpart')
fit<- rpart(Vac_level_1 ~ ., data=train[,c("R_95P","R_Crest_Factor","R_RMS","Theta_Var","Theta_Skew","Theta_Kurt","Vac_level_1")], 
                method = "class", control= rpart.control(minsplit = n1, minbucket = n2, cp = 0.00,maxdepth = 10))



train$Pred <- predict(fit, data = train, type = "class")

class.pred.1=table(train$Vac_level_1, train$Pred)
print("class.pred :")
print(class.pred.1)
Accuracy.1 = 100*sum(diag(class.pred.1))/sum(class.pred.1)
print(Accuracy.1)
print("Accuracy :")
print(Accuracy.1)
