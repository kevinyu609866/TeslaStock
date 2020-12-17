library(dplyr)
library(dslabs)
library(ISLR)
names <- c("Open","High", "Low", "Close", "AdjustedClose","Volume", "DailyReturn","ELGT","Classifier")

TSLA <- read.csv("C:/Users/Kevin/Desktop/fall 2020/6350/midterm 1/TSLA.csv")
for (i in 1:nrow(TSLA)) {
  if(i > 1) {
    TSLA$DailyReturn[i] <- TSLA$Open[i] - TSLA$Open[i - 1]
    TSLA$EarnLossGapTrade[i] <- TSLA$Open[i] - TSLA$Close[i-1]
    TSLA$Classifier[i] <- TSLA$Open[i - 1] * 0.006 
    
    if (TSLA$DailyReturn[i] > TSLA$Classifier[i]) {
      TSLA$Class[i] <- 1
    }
    else if (TSLA$DailyReturn[i] < (TSLA$Classifier[i] * -1)) {
      TSLA$Class[i] <- -1
    }
    else {
      TSLA$Class[i] <- 0
    }
  }
}

TSLA$DailyReturn[which(is.na(TSLA$DailyReturn))] <- mean(TSLA$DailyReturn[2:nrow(TSLA)])
TSLA$EarnLossGapTrade[which(is.na(TSLA$EarnLossGapTrade))] <- mean(TSLA$EarnLossGapTrade[2:nrow(TSLA)])
TSLA$Classifier[which(is.na(TSLA$Classifier))] <- mean(TSLA$Classifier[2:nrow(TSLA)])
TSLA$Class[which(is.na(TSLA$Class))] <- 0

TSLA$IMOVEMENT <- NA
TSLA$IClass <- NA
TSLA$IClassifier <- NA

for (i in 1:nrow(TSLA)) {
  if(i > 2) {
    TSLA$IMOVEMENT[i] <- TSLA$Open[i - 1] - TSLA$Open[i - 2]
    #TSLA$IEarnLossGapTrade[i] <- TSLA$Open[i] - TSLA$Close[i-1]
    TSLA$IClassifier[i] <- TSLA$Open[i - 2] * 0.006
    
    if (TSLA$IMOVEMENT[i] > TSLA$IClassifier[i]) {
      TSLA$IClass[i] <- 1
    }
    else if (TSLA$IMOVEMENT[i] < (TSLA$IClassifier[i] * -1)) {
      TSLA$IClass[i] <- -1
    }
    else {
      TSLA$IClass[i] <- 0
    }
  }
}

table(TSLA$Class)

orgtablelength = nrow(TSLA)
count = 0
for (i in 1:orgtablelength) {
  if (i > 1) {
    if (TSLA$Class[i] == 0) {
      count = count + 1
      TSLA[(orgtablelength + count),] = TSLA[i,]
    }
  }
 
}

TSLA$IMOVEMENT[which(is.na(TSLA$IMOVEMENT))] <- mean(TSLA$IMOVEMENT[3:nrow(TSLA)])
TSLA$IClassifier[which(is.na(TSLA$IClassifier))] <- mean(TSLA$IClassifier[3:nrow(TSLA)])
TSLA$IClass[which(is.na(TSLA$IClass))] <- 0

MeanTable <- data.frame(matrix(ncol = 9, nrow = 3))
colnames(MeanTable) <- c("Open","High", "Low", "Close", "AdjustedClose","Volume", "DailyReturn","ELGT","Classifier")
StdDevTable <- data.frame(matrix(ncol = 9, nrow = 3))
colnames(StdDevTable) <- c("Open","High", "Low", "Close", "AdjustedClose","Volume", "DailyReturn","ELGT","Classifier")
SofFTable <- data.frame(matrix(ncol = 9, nrow = 3))
colnames(SofFTable) <- c("Open","High", "Low", "Close", "AdjustedClose","Volume", "DailyReturn","ELGT","Classifier")
DiscTable <- data.frame(matrix(ncol = 9, nrow = 3))
colnames(DiscTable) <- c("Open","High", "Low", "Close", "AdjustedClose","Volume", "DailyReturn","ELGT","Classifier")

Up <- data.frame(TSLA %>% filter(TSLA$Class == 1))
Down <- data.frame(TSLA %>% filter(TSLA$Class == -1 ))
Stable <- data.frame(TSLA %>% filter(TSLA$Class == 0))
# 
# par(mfrow = c(1,3))
# hist(Up$Open,xlab = "Opening Price", main = "Histogram for Up Class Price")
# hist(Down$Open,xlab = "Opening Price", main = "Histogram for Down Class Price")
# hist(Stable$Open,xlab = "Opening Price", main = "Histogram for Stable Class Price")
# 
# hist(Up$High,xlab = "Day's Highest Price", main = "Histogram of Up Class Highest Price")
# hist(Down$High,xlab = "Day's Highest Price", main = "Histogram of Down Class Highest Price")
# hist(Stable$High,xlab = "Day's Highest Price", main = "Histogram of Stable Class Highest Price")
# 
# hist(Up$Low,xlab = "Day's Lowest Price", main = "Histogram for Up Class Lowest Price")
# hist(Down$Low,xlab = "Day's Lowest Price", main = "Histogram for Down Class Lowest Price")
# hist(Stable$Low,xlab = "Day's Lowest Price", main = "Histogram for Stable Class Lowest Price")
# 
# hist(Up$Close,xlab = "Day's Closing Price", main = "Histogram for Up Class Closing Price")
# hist(Down$Close,xlab = "Day's Closing Price", main = "Histogram for Down Class Closing Price")
# hist(Stable$Close,xlab = "Day's Closing Price", main = "Histogram for Stable Class Closing Price")
# 
# hist(Up$Adj.Close,xlab = "Day's Adjusted Closing Price", main = "Histogram for Up Class Adjusted Closing Price")
# hist(Down$Adj.Close,xlab = "Day's Adjusted Closing Price", main = "Histogram for Down Class Adjusted Closing Price")
# hist(Stable$Adj.Close,xlab = "Day's Adjusted Closing Price", main = "Histogram for Stable Class Adjusted Closing Price")
# 
# hist(Up$Volume * .001,xlab = "Day's Total Traded Volume(Scaled down by 1/1000)", main = "Histogram for Up Class Daily Volume")
# hist(Down$Volume * .001,xlab = "Day's Total Traded Volume(Scaled down by 1/1000)", main = "Histogram for Down Class Daily Volume")
# hist(Stable$Volume * .001,xlab = "Day's Total Traded Volume(Scaled down by 1/1000)", main = "Histogram for Stable Class Daily Volume")
# 
# hist(Up$DailyReturn ,xlab = "Daily Return", main = "Histogram for Up Class Gain or Loss")
# hist(Down$DailyReturn ,xlab = "Daily Return", main = "Histogram for Down Class Gain or Loss")
# hist(Stable$DailyReturn ,xlab = "Daily Return", main = "Histogram for Stable Class Gain or Loss")
# 
# hist(Up$EarnLossGapTrade, xlab = "Daily Difference in Gap Trading", main = "Histogram for Up Class Gap Trade Difference")
# hist(Down$EarnLossGapTrade, xlab = "Daily Difference in Gap Trading", main = "Histogram for Down Class Gap Trade Difference")
# hist(Stable$EarnLossGapTrade, xlab = "Daily Difference in Gap Trading", main = "Histogram for Stable Class Gap Trade Difference")
# 
# hist(Up$Classifier, xlab = "Daily Threshold(Opening Price * .6%)", main = "Histogram for Up Class Value used to classify")
# hist(Down$Classifier, xlab = "Daily Threshold(Opening Price * .6%)", main = "Histogram for Down Class Value used to classify")
# hist(Stable$Classifier, xlab = "Daily Threshold(Opening Price * .6%)", main = "Histogram for Stable Class Value used to classify")
# 
# prop.table(table(Up$Volume * .001< 50000))
# prop.table(table(Down$Volume * .001< 50000))
# prop.table(table(Stable$Volume * .001< 50000))
# 
UpDown <- rbind(Up,Down)
UpStable <- rbind(Up,Stable)
DownStable <- rbind(Down,Stable)
# 
# t.test(Up$Open,Down$Open)
# t.test(Up$Open,Stable$Open)
# t.test(Down$Open,Stable$Open)
# 
# t.test(Up$High, Down$High)
# t.test(Up$High, Stable$High)
# t.test(Down$High,Stable$High)
# 
# t.test(Up$Low, Down$Low)
# t.test(Up$Low, Stable$Low)
# t.test(Down$Low,Stable$Low)
# 
# t.test(Up$Close, Down$Close)
# t.test(Up$Close, Stable$Close)
# t.test(Down$Close, Stable$Close)
# 
# t.test(Up$Adj.Close, Down$Adj.Close)
# t.test(Up$Adj.Close, Stable$Adj.Close)
# t.test(Down$Adj.Close, Stable$Adj.Close)
# 
# t.test(Up$Volume, Down$Volume)
# t.test(Up$Volume, Stable$Volume)
# t.test(Down$Volume, Stable$Volume)
# 
# t.test(Up$DailyReturn, Down$DailyReturn)
# t.test(Up$DailyReturn, Stable$DailyReturn)
# t.test(Down$DailyReturn, Stable$DailyReturn)
# 
# t.test(Up$EarnLossGapTrade, Down$EarnLossGapTrade)
# t.test(Up$EarnLossGapTrade, Stable$EarnLossGapTrade)
# t.test(Down$EarnLossGapTrade, Stable$EarnLossGapTrade)
# 
# t.test(Up$Classifier, Down$Classifier)
# t.test(Up$Classifier, Stable$Classifier)
# t.test(Down$Classifier, Stable$Classifier)

StTSLA <- data.frame(scale(TSLA[2:10]))
#StTSLA['Class'] <- TSLA['Class']
StTSLA['Class'] <- TSLA['IClass']
Up <- data.frame(StTSLA %>% filter(StTSLA$Class == 1))
Down <- data.frame(StTSLA %>% filter(StTSLA$Class == -1 ))
Stable <- data.frame(StTSLA %>% filter(StTSLA$Class == 0))
#  
# set.seed(1)
# Upindexes = sample(1:nrow(Up), size = 0.2*nrow(Up))
# Uptest <- Up[Upindexes,]
# Uptrain <- Up[-Upindexes,]
# 
# Downindexes = sample(1:nrow(Down), size = 0.2*nrow(Down))
# Downtest <- Down[Downindexes,]
# Downtrain <- Down[-Downindexes,]
# 
# Stableindexes = sample(1:nrow(Stable), size = 0.2*nrow(Stable))
# Stabletest <- Stable[Stableindexes,]
# Stabletrain <- Stable[-Stableindexes,]
# 
# TRAINSET <- bind_rows(Uptrain, Downtrain, Stabletrain)
# TESTSET <- bind_rows(Uptest,Downtest,Stabletest)

TRAINSET <- bind_rows(Up,Down,Stable)
TSLA <- TSLA[-c(1,10,13:14)]

table(TESTSET$Class)
table(TRAINSET$Class)

TESTSET <- na.omit(TESTSET)

library(caret)
library(class)
accuracy <- function(x) {sum(diag(x))/(sum(rowSums(x))) * 100}

dim(TRAINSET)
dim(TSLA)
KNN1train <-knn(train = TRAINSET, test = TSLA, cl = TRAINSET$Class, k = 1)
conf1train <- table(KNN1train, TSLA$Class)
# KNN1test <- knn(train = TESTSET, test= TRAINSET, cl = TESTSET$Class, k = 1)
# conf1test <- table(KNN1test, TRAINSET$Class)
train1perf <- accuracy(conf1train)
#test1perf <- accuracy(conf1test)

KNN3train <-knn(train = TRAINSET, test = TESTSET, cl = TRAINSET$IClass, k = 3)
conf3train <- table(KNN3train, TESTSET$Class)
KNN3test <- knn(train = TESTSET, test= TRAINSET, cl = TESTSET$IClass, k = 3)
conf3test <- table(KNN3test, TRAINSET$Class)
train3perf <- accuracy(conf3train)
test3perf <- accuracy(conf3test)

KNN5train <-knn(train = TRAINSET, test = TESTSET, cl = TRAINSET$Class, k = 5)
conf5train <- table(KNN5train, TESTSET$Class)
KNN5test <- knn(train = TESTSET, test= TRAINSET, cl = TESTSET$Class, k = 5)
conf5test <- table(KNN5test, TRAINSET$Class)
train5perf <- accuracy(conf5train)
test5perf <- accuracy(conf5test)

KNN10train <-knn(train = TRAINSET, test = TESTSET, cl = TRAINSET$Class, k = 10)
conf10train <- table(KNN10train, TESTSET$Class)
KNN10test <- knn(train = TESTSET, test= TRAINSET, cl = TESTSET$Class, k = 10)
conf10test <- table(KNN10test, TRAINSET$Class)
train10perf <- accuracy(conf10train)
test10perf <- accuracy(conf10test)

KNN15train <-knn(train = TRAINSET, test = TESTSET, cl = TRAINSET$Class, k = 15)
conf15train <- table(KNN15train, TESTSET$Class)
KNN15test <- knn(train = TESTSET, test= TRAINSET, cl = TESTSET$Class, k = 15)
conf15test <- table(KNN15test, TRAINSET$Class)
train15perf <- accuracy(conf15train)
test15perf <- accuracy(conf15test)

KNN20train <-knn(train = TRAINSET, test = TESTSET, cl = TRAINSET$Class, k = 20)
conf20train <- table(KNN20train, TESTSET$Class)
KNN20test <- knn(train = TESTSET, test= TRAINSET, cl = TESTSET$Class, k = 20)
conf20test <- table(KNN20test, TRAINSET$Class)
train20perf <- accuracy(conf20train)
test20perf <- accuracy(conf20test)

KNN35train <-knn(train = TRAINSET, test = TESTSET, cl = TRAINSET$Class, k = 35)
conf35train <- table(KNN35train, TESTSET$Class)
KNN35test <- knn(train = TESTSET, test= TRAINSET, cl = TESTSET$Class, k = 35)
conf35test <- table(KNN35test, TRAINSET$Class)
train35perf <- accuracy(conf35train)
test35perf <- accuracy(conf35test)

KNN50train <-knn(train = TRAINSET, test = TESTSET, cl = TRAINSET$Class, k = 50)
conf50train <- table(KNN50train, TESTSET$Class)
KNN50test <- knn(train = TESTSET, test= TRAINSET, cl = TESTSET$Class, k = 50)
conf50test <- table(KNN50test, TRAINSET$Class)
train50perf <- accuracy(conf50train)
test50perf <- accuracy(conf50test)

par(mfrow=c(1,1))
TrainPlotPerf <- rbind(data.frame(c(1,3,5,10,15,20,35,50),(c(train1perf,train3perf,train5perf,train10perf,train15perf,train20perf,train35perf, train50perf))))
PlotPerf <- rbind(data.frame(c(1,3,5,10,15,20,35,50),(c(test1perf,test3perf,test5perf,test10perf,test15perf,test20perf, test35perf,test50perf))))

plot(TrainPlotPerf, type = "o", xlab = "Amount of K nearest neighbors", ylab = "Correctly Predicted Accuracy", col = "red", main = "Visualization of K neighors and Corresponding Accuracy")
lines(PlotPerf, type = "o", col= "blue")
legend("topright", legend = c("Training Performance","Testing Performance"), col = c("red","blue"), pch = 1)

set.seed(132)
highesttestpref <- 0
testperf <- 0 
bestK <- 0
for (i in 1:5) {
  KNN <- knn(train = TRAINSET, test = TESTSET, cl = TRAINSET$Class , k =i)
  conftrain <- prop.table(table(KNN, TESTSET$Class))
  testperf <- accuracy(conftrain)
  if (testperf > highesttestpref ) {
    highesttestpref <- testperf
    bestK <- i
  }
}
bestK

highesttestpref <- 0
testperf <- 0 
bestK <- 0
for (i in 1:5) {
  KNN <- knn(train = TESTSET, test = TRAINSET, cl = TESTSET$Class , k =i)
  conftrain <- prop.table(table(KNN, TRAINSET$Class))
  testperf <- accuracy(conftrain)
  if (testperf > highesttestpref ) {
    highesttestpref <- testperf
    bestK <- i
  }
}
bestK

prop.table(conf1train)
accuracy(conf1train)
accuracy(conf1test)

prop.table(conf5train)
prop.table(conf5test)
accuracy(conf5train)
accuracy(conf5test)

TSLA$Open <- TSLA$Open * .716
TSLA$High <- TSLA$High * .710
TSLA$Low <- TSLA$Low * .687
TSLA$Close <- TSLA$Close * .677
TSLA$Adj.Close <- TSLA$Adj.Close * .677
TSLA$Volume <- TSLA$Volume * .742
TSLA$Classifier <- TSLA$Classifier * .618

StTSLA <- data.frame(scale(TSLA[2:9]))
StTSLA['Class'] <- TSLA['Class']
Up <- data.frame(StTSLA %>% filter(StTSLA$Class == 1))
Down <- data.frame(StTSLA %>% filter(StTSLA$Class == -1 ))
Stable <- data.frame(StTSLA %>% filter(StTSLA$Class == 0))

set.seed(1)
Upindexes = sample(1:nrow(Up), size = 0.2*nrow(Up))
Uptest <- Up[indexes,]
Uptrain <- Up[-indexes,]

Downindexes = sample(1:nrow(Down), size = 0.2*nrow(Down))
Downtest <- Down[indexes,]
Downtrain <- Down[-indexes,]

Stableindexes = sample(1:nrow(Stable), size = 0.2*nrow(Stable))
Stabletest <- Stable[indexes,]
Stabletrain <- Stable[-indexes,]

TRAINSET <- bind_rows(Uptrain, Downtrain, Stabletrain)
TESTSET <- bind_rows(Uptest,Downtest,Stabletest)
TESTSET <- na.omit(TESTSET)

KNN1train <-knn(train = TRAINSET, test = TESTSET, cl = TRAINSET$IClass, k = 1)
conf1train <- table(KNN1train, TESTSET$Class)
KNN1test <- knn(train = TESTSET, test= TRAINSET, cl = TESTSET$IClass, k = 1)
conf1test <- table(KNN1test, TRAINSET$Class)
train1perf <- accuracy(conf1train)
test1perf <- accuracy(conf1test)

KNN3train <-knn(train = TRAINSET, test = TESTSET, cl = TRAINSET$Class, k = 3)
conf3train <- table(KNN3train, TESTSET$Class)
KNN3test <- knn(train = TESTSET, test= TRAINSET, cl = TESTSET$Class, k = 3)
conf3test <- table(KNN3test, TRAINSET$Class)
train3perf <- accuracy(conf3train)
test3perf <- accuracy(conf3test)

KNN5train <-knn(train = TRAINSET, test = TESTSET, cl = TRAINSET$Class, k = 5)
conf5train <- table(KNN5train, TESTSET$Class)
KNN5test <- knn(train = TESTSET, test= TRAINSET, cl = TESTSET$Class, k = 5)
conf5test <- table(KNN5test, TRAINSET$Class)
train5perf <- accuracy(conf5train)
test5perf <- accuracy(conf5test)

KNN10train <-knn(train = TRAINSET, test = TESTSET, cl = TRAINSET$Class, k = 10)
conf10train <- table(KNN10train, TESTSET$Class)
KNN10test <- knn(train = TESTSET, test= TRAINSET, cl = TESTSET$Class, k = 10)
conf10test <- table(KNN10test, TRAINSET$Class)
train10perf <- accuracy(conf10train)
test10perf <- accuracy(conf10test)

KNN15train <-knn(train = TRAINSET, test = TESTSET, cl = TRAINSET$Class, k = 15)
conf15train <- table(KNN15train, TESTSET$Class)
KNN15test <- knn(train = TESTSET, test= TRAINSET, cl = TESTSET$Class, k = 15)
conf15test <- table(KNN15test, TRAINSET$Class)
train15perf <- accuracy(conf15train)
test15perf <- accuracy(conf15test)

KNN20train <-knn(train = TRAINSET, test = TESTSET, cl = TRAINSET$Class, k = 20)
conf20train <- table(KNN20train, TESTSET$Class)
KNN20test <- knn(train = TESTSET, test= TRAINSET, cl = TESTSET$Class, k = 20)
conf20test <- table(KNN20test, TRAINSET$Class)
train20perf <- accuracy(conf20train)
test20perf <- accuracy(conf20test)

KNN35train <-knn(train = TRAINSET, test = TESTSET, cl = TRAINSET$Class, k = 35)
conf35train <- table(KNN35train, TESTSET$Class)
KNN35test <- knn(train = TESTSET, test= TRAINSET, cl = TESTSET$Class, k = 35)
conf35test <- table(KNN35test, TRAINSET$Class)
train35perf <- accuracy(conf35train)
test35perf <- accuracy(conf35test)

KNN50train <-knn(train = TRAINSET, test = TESTSET, cl = TRAINSET$Class, k = 50)
conf50train <- table(KNN50train, TESTSET$Class)
KNN50test <- knn(train = TESTSET, test= TRAINSET, cl = TESTSET$Class, k = 50)
conf50test <- table(KNN50test, TRAINSET$Class)
train50perf <- accuracy(conf50train)
test50perf <- accuracy(conf50test)

par(mfrow=c(1,1))
TrainPlotPerf <- rbind(data.frame(c(1,3,5,10,15,20,35,50),(c(train1perf,train3perf,train5perf,train10perf,train15perf,train20perf,train35perf, train50perf))))
PlotPerf <- rbind(data.frame(c(1,3,5,10,15,20,35,50),(c(test1perf,test3perf,test5perf,test10perf,test15perf,test20perf, test35perf,test50perf))))

plot(TrainPlotPerf, type = "o", xlab = "Amount of K nearest neighbors", ylab = "Correctly Predicted Accuracy", col = "red", main = "Visualization of K neighors and Corresponding Accuracy")
lines(PlotPerf, type = "o", col= "blue")
legend("topright", legend = c("Training Performance","Testing Performance"), col = c("red","blue"), pch = 1)


# MeanTable$Open[1] <- mean(Up$Open)
# MeanTable$High[1] <- mean(Up$High)
# MeanTable$Low[1] <- mean(Up$Low)
# MeanTable$Close[1] <- mean(Up$Close)
# MeanTable$AdjustedClose[1] <- mean(Up$Adj.Close)
# MeanTable$Volume[1]<- mean(Up$Volume)
# MeanTable$DailyReturn[1] <- mean(Up$DailyReturn)
# MeanTable$ELGT[1] <- mean(Up$EarnLossGapTrade)
# MeanTable$Classifier[1] <- mean(Up$Classifier)
# 
# MeanTable$Open[2] <- mean(Down$Open)
# MeanTable$High[2] <- mean(Down$High)
# MeanTable$Low[2] <- mean(Down$Low)
# MeanTable$Close[2] <- mean(Down$Close)
# MeanTable$AdjustedClose[2] <- mean(Down$Adj.Close)
# MeanTable$Volume[2]<- mean(Down$Volume)
# MeanTable$DailyReturn[2] <- mean(Down$DailyReturn)
# MeanTable$ELGT[2] <- mean(Down$EarnLossGapTrade)
# MeanTable$Classifier[2] <- mean(Down$Classifier)
# 
# MeanTable$Open[3] <- mean(Stable$Open)
# MeanTable$High[3] <- mean(Stable$High)
# MeanTable$Low[3] <- mean(Stable$Low)
# MeanTable$Close[3] <- mean(Stable$Close)
# MeanTable$AdjustedClose[3] <- mean(Stable$Adj.Close)
# MeanTable$Volume[3]<- mean(Stable$Volume)
# MeanTable$DailyReturn[3] <- mean(Stable$DailyReturn)
# MeanTable$ELGT[3] <- mean(Stable$EarnLossGapTrade)
# MeanTable$Classifier[3] <- mean(Stable$Classifier)
# 
# StdDevTable$Open[1] <- sd(Up$Open)
# StdDevTable$High[1] <- sd(Up$High)
# StdDevTable$Low[1] <- sd(Up$Low)
# StdDevTable$Close[1] <- sd(Up$Close)
# StdDevTable$AdjustedClose[1] <- sd(Up$Adj.Close)
# StdDevTable$Volume[1] <- sd(Up$Volume)
# StdDevTable$DailyReturn[1] <- sd(Up$DailyReturn)
# StdDevTable$ELGT[1] <- sd(Up$EarnLossGapTrade)
# StdDevTable$Classifier[1] <- sd(Up$Classifier)
# 
# StdDevTable$Open[2] <- sd(Down$Open)
# StdDevTable$High[2] <- sd(Down$High)
# StdDevTable$Low[2] <- sd(Down$Low)
# StdDevTable$Close[2] <- sd(Down$Close)
# StdDevTable$AdjustedClose[2] <- sd(Down$Adj.Close)
# StdDevTable$Volume[2] <- sd(Down$Volume)
# StdDevTable$DailyReturn[2] <- sd(Down$DailyReturn)
# StdDevTable$ELGT[2] <- sd(Down$EarnLossGapTrade)
# StdDevTable$Classifier[2] <- sd(Down$Classifier)
# 
# StdDevTable$Open[3] <- sd(Stable$Open)
# StdDevTable$High[3] <- sd(Stable$High)
# StdDevTable$Low[3] <- sd(Stable$Low)
# StdDevTable$Close[3] <- sd(Stable$Close)
# StdDevTable$AdjustedClose[3] <- sd(Stable$Adj.Close)
# StdDevTable$Volume[3] <- sd(Stable$Volume)
# StdDevTable$DailyReturn[3] <- sd(Stable$DailyReturn)
# StdDevTable$ELGT[3] <- sd(Stable$EarnLossGapTrade)
# StdDevTable$Classifier[3] <- sd(Stable$Classifier)
# 
# SofFTable$Open[1] <- sqrt((StdDevTable$Open[1]^2)/ nrow(Up))
# SofFTable$High[1] <- sqrt((StdDevTable$High[1]^2)/ nrow(Up))
# SofFTable$Low[1] <- sqrt((StdDevTable$Low[1]^2)/ nrow(Up))
# SofFTable$Close[1] <- sqrt((StdDevTable$Close[1]^2)/ nrow(Up))
# SofFTable$AdjustedClose[1] <- sqrt((StdDevTable$AdjustedClose[1]^2)/ nrow(Up))
# SofFTable$Volume[1] <- sqrt((StdDevTable$Volume[1]^2)/ nrow(Up))
# SofFTable$DailyReturn[1] <- sqrt((StdDevTable$DailyReturn[1]^2)/ nrow(Up))
# SofFTable$ELGT[1] <- sqrt((StdDevTable$ELGT[1]^2)/ nrow(Up))
# SofFTable$Classifier[1] <- sqrt((StdDevTable$Classifier[1]^2)/ nrow(Up))
# 
# SofFTable$Open[2] <- sqrt((StdDevTable$Open[2]^2)/ nrow(Down))
# SofFTable$High[2] <- sqrt((StdDevTable$High[2]^2)/ nrow(Down))
# SofFTable$Low[2] <- sqrt((StdDevTable$Low[2]^2)/ nrow(Down))
# SofFTable$Close[2] <- sqrt((StdDevTable$Close[2]^2)/ nrow(Down))
# SofFTable$AdjustedClose[2] <- sqrt((StdDevTable$AdjustedClose[2]^2)/ nrow(Down))
# SofFTable$Volume[2] <- sqrt((StdDevTable$Volume[2]^2)/ nrow(Down))
# SofFTable$DailyReturn[2] <- sqrt((StdDevTable$DailyReturn[2]^2)/ nrow(Down))
# SofFTable$ELGT[2] <- sqrt((StdDevTable$ELGT[2]^2)/ nrow(Down))
# SofFTable$Classifier[2] <- sqrt((StdDevTable$Classifier[2]^2)/ nrow(Down))
# 
# SofFTable$Open[3] <- sqrt((StdDevTable$Open[3]^2)/ nrow(Stable))
# SofFTable$High[3] <- sqrt((StdDevTable$High[3]^2)/ nrow(Stable))
# SofFTable$Low[3] <- sqrt((StdDevTable$Low[3]^2)/ nrow(Stable))
# SofFTable$Close[3] <- sqrt((StdDevTable$Close[3]^2)/ nrow(Stable))
# SofFTable$AdjustedClose[3] <- sqrt((StdDevTable$AdjustedClose[3]^2)/ nrow(Stable))
# SofFTable$Volume[3] <- sqrt((StdDevTable$Volume[3]^2)/ nrow(Stable))
# SofFTable$DailyReturn[3] <- sqrt((StdDevTable$DailyReturn[3]^2)/ nrow(Stable))
# SofFTable$ELGT[3] <- sqrt((StdDevTable$ELGT[3]^2)/ nrow(Stable))
# SofFTable$Classifier[3] <- sqrt((StdDevTable$Classifier[3]^2)/ nrow(Stable))
# 
# DiscTable$Open[1] <- MeanTable$Open[1]/SofFTable$Open[1]
# DiscTable$High[1] <- MeanTable$High[1]/SofFTable$High[1]
# DiscTable$Low[1] <- MeanTable$Low[1]/SofFTable$Low[1]
# DiscTable$Close[1] <- MeanTable$Close[1]/SofFTable$Close[1]
# DiscTable$AdjustedClose[1] <- MeanTable$AdjustedClose[1]/SofFTable$AdjustedClose[1]
# DiscTable$Volume[1] <- MeanTable$Volume[1]/SofFTable$Volume[1]
# DiscTable$DailyReturn[1] <- MeanTable$DailyReturn[1]/SofFTable$DailyReturn[1]
# DiscTable$ELGT[1] <- MeanTable$ELGT[1]/SofFTable$ELGT[1]
# DiscTable$Classifier[1] <- MeanTable$Classifier[1]/SofFTable$Classifier[1]
# 
# DiscTable$Open[2] <- MeanTable$Open[2]/SofFTable$Open[2]
# DiscTable$High[2] <- MeanTable$High[2]/SofFTable$High[2]
# DiscTable$Low[2] <- MeanTable$Low[2]/SofFTable$Low[2]
# DiscTable$Close[2] <- MeanTable$Close[2]/SofFTable$Close[2]
# DiscTable$AdjustedClose[2] <- MeanTable$AdjustedClose[2]/SofFTable$AdjustedClose[2]
# DiscTable$Volume[2] <- MeanTable$Volume[2]/SofFTable$Volume[2]
# DiscTable$DailyReturn[2] <- MeanTable$DailyReturn[2]/SofFTable$DailyReturn[2]
# DiscTable$ELGT[2] <- MeanTable$ELGT[2]/SofFTable$ELGT[2]
# DiscTable$Classifier[2] <- MeanTable$Classifier[2]/SofFTable$Classifier[2]
# 
# DiscTable$Open[3] <- MeanTable$Open[3]/SofFTable$Open[3]
# DiscTable$High[3] <- MeanTable$High[3]/SofFTable$High[3]
# DiscTable$Low[3] <- MeanTable$Low[3]/SofFTable$Low[3]
# DiscTable$Close[3] <- MeanTable$Close[3]/SofFTable$Close[3]
# DiscTable$AdjustedClose[3] <- MeanTable$AdjustedClose[3]/SofFTable$AdjustedClose[3]
# DiscTable$Volume[3] <- MeanTable$Volume[3]/SofFTable$Volume[3]
# DiscTable$DailyReturn[3] <- MeanTable$DailyReturn[3]/SofFTable$DailyReturn[3]
# DiscTable$ELGT[3] <- MeanTable$ELGT[3]/SofFTable$ELGT[3]
# DiscTable$Classifier[3] <- MeanTable$Classifier[3]/SofFTable$Classifier[3]
# 
