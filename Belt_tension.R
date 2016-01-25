# stating the working directory or state the path of the train and test file
#setwd("E:/Zreyas/Integration_code/analytics/Integration_code/Belt_tension/")

args <- commandArgs(TRUE)
library('rpart')
data <- read.csv(args[1])
test <- read.csv(args[2])


features_train <- c("VarThetaNonZeroMean",  "KurtRWhole","KurtThetaNonZeroMean" , "CrestRWhole" , "RMSRWhole","belt")
features_test <- c("VarThetaNonZeroMean",  "KurtRWhole","KurtThetaNonZeroMean" , "CrestRWhole" , "RMSRWhole")

train <- data[,features_train]
test <- test[,features_test]

fit<- rpart(belt ~ ., data=train, method = "class", control= rpart.control(minsplit = 5, minbucket = 2, cp = 0.001,maxdepth = 7))


result <- predict(fit, newdata = test, type = "class")

pred <- as.character(result)
pred <- as.numeric(pred)

print(pred)
