#setwd("E:/Zreyas/Oil_state/V16Nov/Oil_state/")
args <- commandArgs(TRUE)
library('rpart')
data <- read.csv(args[1])
test <- read.csv(args[2])

features_train <- c("Crest_Factor_R","Variance_Theta","oil_state")
features_test <- c("Crest_Factor_R","Variance_Theta")


train <- data[,features_train]
test <- test[,features_test]

fit<- rpart(oil_state ~ ., data=train, method = "class", control= rpart.control(minsplit = 2, minbucket = 2, cp = 0.001,maxdepth = 7))


result <- predict(fit, newdata = test, type = "class")

pred <- as.character(result)
pred <- as.numeric(pred)
print("predicted Output :")
print(pred)
  





