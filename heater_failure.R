#setwd("E:/Zreyas/Integration_code/analytics/Integration_code/P_classification/")
#trn <- read.csv("train.csv")
#test <- read.csv("test.csv")

args <- commandArgs(TRUE)
trn <- read.csv(args[1])
test <- read.csv(args[2])

feat <- c("AV1_1","AV2_1","AV3_1")

set.seed(330)
library(kknn)


tst <- test[,feat]

trnp1 <- trn[,c(feat,"P1.classification")]
trnp2 <- trn[,c(feat,"P2.classification")]
trnp3 <- trn[,c(feat,"P3.classification")]
modp1 <- kknn(P1.classification~.,trnp1,tst,kernel = "optimal", k =2)
predp1 <- predict(modp1,trnp1)


modp2 <- kknn(P2.classification~.,trnp2,tst,kernel = "optimal", k =2)
predp2 <- predict(modp2,trnp2)


modp3 <- kknn(P3.classification~.,trnp3,tst,kernel = "optimal", k =2)
predp3 <- predict(modp3,trnp3)

data.frame(PredP1 = predp1, PredP2 = predp2, PredP3 = predp3)
print(predp1 : predp2: predp3)
