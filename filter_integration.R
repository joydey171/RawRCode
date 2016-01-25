#setwd("E:/Zreyas/Filter_state/V_Nov22/")
args <- commandArgs(TRUE)

library('e1071')
library("flexmix")
library("rpart")

source(args[1])
r_test <- read.csv(args[3])
train <- read.csv(args[2])
testinHG <- pre_processing(r_test)

test_features <- feature_design(testinHG)

#feat_train <- c("sigma2","Variance","Skewness","Kurtosis", "Filter_Stage")
#feat_test <- c("sigma2","Variance","Skewness","Kurtosis")

fit<- rpart(Filter_Stage ~ Range, data= train, method = "class", control= rpart.control(minsplit = 5, minbucket = 2, cp = 0,maxdepth = 10))

result <- predict(fit, newdata = test_features, type = "class")

pred <- as.character(result)
pred <- as.numeric(pred)
print("predicted Output :")
print(pred)


