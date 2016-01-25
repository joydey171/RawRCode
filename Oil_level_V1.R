# stating the working directory or state the path of the train and test file
#setwd("E:/Zreyas/Oil_state/V_1.2/")
args <- commandArgs(TRUE)
train <- read.csv(args[1])
test <- read.csv(args[2])
print(train)
print(test)
train$v <- round(train$v)
train$b <- round(train$b)
train$p_stat <- as.factor(ifelse(train$v/train$b > 5.5,"H","L"))

train$label <- as.factor(train$label)
n_train <- nrow(train)
rel_col_trn <- c("p_stat","kurtosis","sd","label")
train <- train[,rel_col_trn]

test$p_stat <- as.factor(ifelse(test$v/test$b > 5,"H","L"))
rel_col_tst <- c("p_stat","kurtosis","sd")
test <- test[,rel_col_tst]



test_dum <- rbind(train[,-which(names(train)=="label")],test)
n_dum <- nrow(test_dum)

set.seed(330)
library(kknn)
mod <- kknn(label~.,train,test_dum,kernel = "optimal", k =2)
pred_dum <- predict(mod,test_dum)
pred <- pred_dum[c((n_train+1):n_dum)]
pred <- as.character(pred)
pred <- as.numeric(pred)
print("predicted Output :")
print(pred)

