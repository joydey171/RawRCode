rm(list=ls())

set.seed(123)

train = read.csv('G:/Zreyas Technology Project/Filter/10 hp/Data/combined_train_10hp.csv',header=TRUE, sep=",")
test = read.csv('G:/Zreyas Technology Project/Filter/10 hp/Data/30st_Dec_trial_3_10hp.csv', header=TRUE, sep=",")

train$Filter_state <- factor(train$Filter_state)  ;  test$Filter_state = factor( test$Filter_state )

library('rpart')
fit = rpart(Filter_state ~ ., data=train[,c(1,17)], method = "class", control= rpart.control(minsplit = 10, minbucket = 4, cp = 0.001,maxdepth = 10))

library('rattle')

fancyRpartPlot(fit)

test$Pred <- predict(fit, newdata = test, type = "class")

( class.pred.test = table( test$Filter_state, test$Pred ) )

( Accuracy.test = sum(diag(class.pred.test))/sum(class.pred.test) )

train$Pred <- predict(fit, newdata = train, type = "class")

( class.pred.train = table( train$Filter_state, train$Pred ) )

( Accuracy.train = sum(diag(class.pred.train))/sum(class.pred.train) )


