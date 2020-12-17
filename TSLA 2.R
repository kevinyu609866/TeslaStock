library(dplyr)
library(dslabs)
library(ISLR)
names <- c("Open","High", "Low", "Close", "AdjustedClose","Volume", "DailyReturn","ELGT","Classifier")
TSLA <- read.csv("C:/Users/Kevin/Desktop/fall 2020/6350/midterm 1/TSLA.csv")

ncol(TSLA)
#Data Setup
for (i in 1:nrow(TSLA)) {
  if(i > 1) {
    TSLA$DailyReturn[i] <- TSLA$Open[i] - TSLA$Open[i - 1]
    TSLA$EarnLossGapTrade[i] <- TSLA$Open[i] - TSLA$Close[i-1]
    TSLA$Classifier[i] <- TSLA$Open[i - 1] * 0.006 
    
    if (TSLA$DailyReturn[i] > TSLA$Classifier[i]) {
      TSLA$Class[i] <- 1
    }
    else if (TSLA$DailyReturn[i] < (TSLA$Classifier[i] * -1)) {
      TSLA$Class[i] <- 2
    }
    else {
      TSLA$Class[i] <- 3
    }
  }
}

TSLA$DailyReturn[which(is.na(TSLA$DailyReturn))] <- mean(TSLA$DailyReturn[2:nrow(TSLA)])
TSLA$EarnLossGapTrade[which(is.na(TSLA$EarnLossGapTrade))] <- mean(TSLA$EarnLossGapTrade[2:nrow(TSLA)])
TSLA$Classifier[which(is.na(TSLA$Classifier))] <- mean(TSLA$Classifier[2:nrow(TSLA)])
TSLA$Class[which(is.na(TSLA$Class))] <- 3

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
      TSLA$IClass[i] <- 2
    }
    else {
      TSLA$IClass[i] <- 3
    }
  }
}

table(TSLA$Class)

orgtablelength = nrow(TSLA)
count = 0
for (i in 1:orgtablelength) {
  if (i > 1) {
    if (TSLA$Class[i] == 3) {
      count = count + 1
      TSLA[(orgtablelength + count),] = TSLA[i,]
    }
  }
  
}

TSLA$IMOVEMENT[which(is.na(TSLA$IMOVEMENT))] <- mean(TSLA$IMOVEMENT[3:nrow(TSLA)])
TSLA$IClassifier[which(is.na(TSLA$IClassifier))] <- mean(TSLA$IClassifier[3:nrow(TSLA)])
TSLA$IClass[which(is.na(TSLA$IClass))] <- 3

#Q1 standardization
SDATA <- scale(TSLA[,c(2:10,12,14)])
SDATA <- cbind(TSLA[,c(11,13)],SDATA)

#Q1 PCA
library(factoextra)
pr.out <- prcomp(SDATA[3:13], scale = FALSE, center = TRUE)
variance <- data.frame(summary(pr.out)$importance[2,])

pr.var=pr.out$sdev^2
pve=pr.var/sum(pr.var)
pve
#plot curves
plot(cumsum(pve),xlab="PC",ylab="Cumulative Propotion of Var Explained",ylim=c(0,1),type='b')
plot((1:11), variance$summary.pr.out..importance.2..., type = 'o', xlab = 'R', ylab = 'PVE of R', main = "PVE Plot")

#First 3 eigenvalues
corr = cor(SDATA[3:ncol(SDATA)])
corr = round(corr,2)
EV = eigen(corr)
eigenvalues = round(EV$values,2)
eigenvectors = EV$vectors
round((eigenvalues/sum(eigenvalues))  * 100,2)

library(rgl)
library(ggplot2)
library(plotly)
centpca <- pr.out$x[,1:3]

variance <- data.frame(summary(pr.out)$importance[2,])
plot((1:11), variance$summary.pr.out..importance.2..., type = 'o', xlab = 'R', ylab = 'PVE of R', main = "PVE Plot")
plot((1:11), cumsum(variance$summary.pr.out..importance.2...), type = 'o', xlab = 'R', ylab = 'PVE of R', main = "PVE Plot")

index = 0
sum = 0
for (i in 1:nrow(variance)) {
  if (sum < .95){
    sum = sum + variance$summary.pr.out..importance.2...[i]    
  }
  else {
    index = i - 1
    break
  }
  
}

sum
index
newdata <- pr.out$x[,1:5]
centpca <- pr.out$x[,1:3]
NDATA <- cbind(SDATA[,c('Class','IClass')],newdata)
  
Up <- data.frame(NDATA %>% filter(NDATA$Class == 1))
Down <- data.frame(NDATA %>% filter(NDATA$Class == 2 ))
Stable <- data.frame(NDATA %>% filter(NDATA$Class == 3))

UpDown <- bind_rows(Up,Down)
plot3d(centpca, col = UpDown$Class)
DownStable <- bind_rows(Down,Stable)
plot3d(centpca, col = DownStable$Class)
UpStable <- bind_rows(Up,Stable)
plot3d(centpca, col = UpStable$Class)

#Question 2
library(factoextra)
library(gridExtra)
set.seed(1)
km <- kmeans(SDATA, centers = 5, nstart = 50)
km$size

km1 <- kmeans(SDATA, centers = 1, nstart = 50)
kmeans1 <- fviz_cluster(km1, data= SDATA, geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
km2 <- kmeans(SDATA, centers = 2, nstart = 50)
kmeans2 <- fviz_cluster(km2, data= SDATA, geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
km3 <- kmeans(SDATA, centers = 3, nstart = 50)
kmeans3 <- fviz_cluster(km3, data= SDATA, geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
km4 <- kmeans(SDATA, centers = 4, nstart = 50)
kmeans4 <- fviz_cluster(km4, data= SDATA, geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
km5 <- kmeans(SDATA, centers = 5, nstart = 50)
kmeans5 <- fviz_cluster(km5, data= SDATA, geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
grid.arrange(kmeans1,kmeans2,kmeans3,kmeans4,kmeans5)

km6 <- kmeans(SDATA, centers = 6, nstart = 50)
kmeans6 <- fviz_cluster(km6, data= SDATA, geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
km7 <- kmeans(SDATA, centers = 7, nstart = 50)
kmeans7 <- fviz_cluster(km7, data= SDATA, geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
km8 <- kmeans(SDATA, centers = 8, nstart = 50)
kmeans8 <- fviz_cluster(km8, data= SDATA, geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
km9 <- kmeans(SDATA, centers = 9, nstart = 50)
kmeans9 <- fviz_cluster(km9, data= SDATA, geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
kmx <- kmeans(SDATA, centers = 10, nstart = 50)
kmeansx <- fviz_cluster(kmx, data= SDATA, geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
grid.arrange(kmeans6,kmeans7,kmeans8,kmeans9,kmeansx)
RV = data.frame(matrix(ncol = 1, nrow = 10))
colnames(RV) = c("Perf")
RV$Perf = c(1 - km1$tot.withinss/km1$totss, 1 - km2$tot.withinss/km2$totss,1 - km3$tot.withinss/km3$totss,1 - km4$tot.withinss/km4$totss,1 - km5$tot.withinss/km5$totss,
            1 - km6$tot.withinss/km6$totss,1 - km7$tot.withinss/km7$totss,1 - km8$tot.withinss/km8$totss,1 - km9$tot.withinss/km9$totss,1 - kmx$tot.withinss/kmx$totss
)
plot(RV$Perf, xlab = "k", ylab = "Reduction of Variance (Perf(k))", main = "K-means perfomance per K")
lines(RV$Perf)

SDATA$cluster <- as.factor(km6$cluster)
ClusterTable = with(SDATA, table(cluster,Class))

ginie_index = c()
for(a in 1:10){
  CLUSTERS = kmeans(SDATA,iter.max = 10, a)
  CLUSTERS$cluster
  SDATAG <- cbind(SDATA,CLUSTERS$cluster)
  gini = c()
  k = seq(1,a)
  for(i in k){
    cluster_i = SDATAG[SDATAG$`CLUSTERS$cluster` == 1,]
    Fs_1 = dim(cluster_i[cluster_i$Class == 1,])[1]/dim(cluster_i)[1]
    Fs_2 = dim(cluster_i[cluster_i$Class == 2,])[1]/dim(cluster_i)[1]
    Fs_3 = dim(cluster_i[cluster_i$Class == 3,])[1]/dim(cluster_i)[1]
    gini[i] = Fs_1*(1-Fs_1) + Fs_2*(1-Fs_2) + Fs_3*(1-Fs_3) 
  }
  ginie_index[a] = sum(gini)/a
}
par(mfrow = c(1,1))
plot(1:10, ginie_index, type = "o", main = "Gini(K) vs K", xlab = "number of clusters", ylab = "Gini Index")
library(reldist)
gini(ClusterTable)

F <- function(cluster, i) {
  f <- 0
  
  f <- table(cluster$Class)[i]/sum(table(cluster$Class))
  return(f)
}

gini <- function(F1,F2,F3) {
  gini = F1 * (1 - F1) + F2 * (1 - F2) + F3 * (1 - F3)
  
  return(gini)
}

cluster1 = SDATA$cluster == 1
cluster2 = SDATA$cluster == 2
cluster3 = SDATA$cluster == 3
cluster4 = SDATA$cluster == 4
cluster5 = SDATA$cluster == 5
cluster6 = SDATA$cluster == 6
Clu1 = SDATA[cluster1,]
Clu2 = SDATA[cluster2,]
Clu3 = SDATA[cluster3,]
Clu4 = SDATA[cluster4,]
Clu5 = SDATA[cluster5,]
Clu6 = SDATA[cluster6,]

ClusterTable
CL1C1 <- F(Clu1,1)
CL1C2 <- F(Clu1,2)
CL1C3 <- F(Clu1,3)
giniC1 <- gini(CL1C1, CL1C2, CL1C3)

CL2C1 <- F(Clu2,1)
CL2C2 <- F(Clu2,2)
CL2C3 <- 0
giniC2 <- gini(CL2C1, CL2C2, CL2C3)

CL3C1 <- F(Clu3,1)
CL3C2 <- F(Clu3,2)
CL3C3 <- F(Clu3,3)
giniC3 <- gini(CL3C1, CL3C2, CL3C3)

CL4C1 <- F(Clu4,1)
CL4C2 <- F(Clu4,2)
CL4C3 <- F(Clu4,3)
giniC4 <- gini(CL4C1, CL4C2, CL4C3)

CL5C1 <- F(Clu5,1)
CL5C2 <- F(Clu5,2)
CL5C3 <- F(Clu5,3)
giniC5 <- gini(CL5C1, CL5C2, CL5C3)

CL6C1 <- F(Clu6,1)
CL6C2 <- F(Clu6,2)
CL6C3 <- F(Clu6,3)
giniC6 <- 1 * (1 - 1) + 0 * (1 - 0) + 0 * (1 - 0)


giniarr <- c(giniC1,giniC2,giniC3,giniC4,giniC5,giniC6)
sum(giniarr)

#Q3 K* = 6
km6$centers
km6$size
giniarr <- c(giniC1,giniC2,giniC3,giniC4,giniC5,giniC6)
km6$withinss
ClusterTable #frequencies of each class
plot3d(centpca, col = SDATA$cluster)

#Q4 
#Training/Testsets for each class cj
set.seed(1)
Up <- data.frame(SDATA %>% filter(SDATA$Class == 1))
Down <- data.frame(SDATA %>% filter(SDATA$Class == 2 ))
Stable <- data.frame(SDATA %>% filter(SDATA$Class == 3))

UpIndexes <- sample(1:nrow(Up), size = 0.2 * nrow(Up))
DownIndexes <- sample(1:nrow(Down), size = 0.2 * nrow(Down))
StableIndexes <- sample(1:nrow(Stable), size = 0.2 * nrow(Stable))

UpTest <- Up[UpIndexes,]
DownTest <- Down[DownIndexes,]
StableTest <- Stable[StableIndexes,]

UpTrain <- Up[-UpIndexes,]
DownTrain <- Down[-DownIndexes,]
StableTrain <- Stable[-StableIndexes,]

nrow(UpTest)
nrow(DownTest)
nrow(StableTest)
nrow(UpTrain)
nrow(DownTrain)
nrow(StableTrain)


TRAINSET <- bind_rows(UpTrain,DownTrain,StableTrain)
TESTSET<- bind_rows(UpTest, DownTest, StableTest)

TRAINSET <- transform(TRAINSET, Class = as.factor(Class))
TESTSET <- transform(TESTSET, Class = as.factor(Class))

table(TRAINSET$Class) #does not need cloning
table(TESTSET$Class) #does not need cloning

#Q5 random forest 
set.seed(1)
library(randomForest)
library(party)
library(caret)

model1 <- randomForest(Class~ IMOVEMENT + DailyReturn, data = TRAINSET, na.action = na.omit)
plot(model1)

model100<- randomForest(Class ~ . - Class- DailyReturn - Classifier - cluster, data= TRAINSET, ntrees = 100, ntry = (ncol(TRAINSET)))
modeltest100<- randomForest(Class ~. - Class- DailyReturn - Classifier- cluster, data= TESTSET, ntrees = 100, ntry = (ncol(TESTSET)))

trainpred100 <- predict(model100, newdata= TESTSET)
CONFTRAIN100 <- confusionMatrix(as.factor(TESTSET$Class),as.factor(trainpred100))

testpred100 <- predict(modeltest100, newdata= TRAINSET)
CONFTEST100 <- confusionMatrix(as.factor(TRAINSET$Class),as.factor(testpred100))

model200<- randomForest(Class ~ . - Class- DailyReturn - Classifier- cluster, data= TRAINSET, ntrees = 200, ntry = (ncol(TRAINSET)))
modeltest200<- randomForest(Class ~. - Class- DailyReturn - Classifier- cluster, data= TESTSET, ntrees = 200, ntry = (ncol(TESTSET)))

trainpred200 <-predict(model200, newdata= TESTSET)
CONFTRAIN200 <- confusionMatrix(as.factor(TESTSET$Class),as.factor(trainpred200))

testpred200 <- predict(modeltest200, newdata= TRAINSET)
CONFTEST200 <- confusionMatrix(as.factor(TRAINSET$Class),as.factor(testpred200))


model300<- randomForest(Class ~. - Class- DailyReturn - Classifier- cluster, data= TRAINSET, ntrees = 300, ntry = (ncol(TRAINSET)))
modeltest300<- randomForest(Class ~.- Class- DailyReturn - Classifier- cluster, data= TESTSET, ntrees = 300, ntry = (ncol(TESTSET)))

trainpred300 <- predict(model300, newdata= TESTSET)
CONFTRAIN300 <- confusionMatrix(as.factor(TESTSET$Class),as.factor(trainpred300))

testpred300 <- predict(modeltest300, newdata= TRAINSET)
CONFTEST300 <- confusionMatrix(as.factor(TRAINSET$Class),as.factor(testpred300))

model400<- randomForest(Class ~.- Class- DailyReturn - Classifier- cluster, data= TRAINSET, ntrees = 400, ntry = (ncol(TRAINSET)))
modeltest400<- randomForest(Class ~.- Class- DailyReturn - Classifier- cluster, data= TESTSET, ntrees = 400, ntry = (ncol(TESTSET)))

trainpred400 <- predict(model400, newdata= TESTSET)
CONFTRAIN400 <- confusionMatrix(as.factor(TESTSET$Class),as.factor(trainpred400))

testpred400 <- predict(modeltest400, newdata= TRAINSET)
CONFTEST400 <- confusionMatrix(as.factor(TRAINSET$Class),as.factor(testpred400))

accuracy <- function(x) {sum(diag(x))/(sum(rowSums(x))) * 100}

CONFTRAIN100$overall['Accuracy']
ntrees <- c(100,200,300,400)
library(ggplot2)
b = bind_cols(ntrees, c(CONFTEST100$overall['Accuracy'],CONFTEST200$overall['Accuracy'],CONFTEST300$overall['Accuracy'],CONFTEST400$overall['Accuracy']),c(CONFTRAIN100$overall['Accuracy'],CONFTRAIN200$overall['Accuracy'],CONFTRAIN300$overall['Accuracy'],CONFTRAIN400$overall['Accuracy']))
ggplot() + 
  geom_line(data = b, aes(x = ...1, y = ...2, color = "Test")) +
  geom_line(data = b, aes(x = ...1, y = ...3, color = "Train")) + 
  xlab("Number of Trees") + ylab("Accuracy Percentage") +
  scale_colour_manual("", breaks = c("Test","Train"), values = c("blue","red"))

#Display 4 test set confusion matrcies
CONFTEST100
CONFTEST200
CONFTEST300
CONFTEST400

#Question 6
CONF100 <- prop.table(CONFTEST100$table)/rowSums(prop.table(CONFTEST100$table))
CONF200 <- prop.table(CONFTEST200$table)/rowSums(prop.table(CONFTEST200$table))
CONF300 <- prop.table(CONFTEST300$table)/rowSums(prop.table(CONFTEST300$table))
CONF400 <- prop.table(CONFTEST400$table)/rowSums(prop.table(CONFTEST400$table))
UpPerf <- c(CONF100[1],CONF200[1],CONF300[1],CONF400[1])
DownPerf <- c(CONF100[5],CONF200[5],CONF300[5],CONF400[5])
StablePerf <- c(CONF100[9],CONF200[9],CONF300[9],CONF400[9])

a <- bind_cols(ntrees, UpPerf,DownPerf, StablePerf)
ggplot() +
  geom_line(data = a, aes(x = ntrees, y = UpPerf, color = "Up")) +
  geom_line(data = a, aes(x = ntrees, y = DownPerf, color = "Down")) +
  geom_line(data = a, aes(x = ntrees, y = StablePerf, color = "Stable")) +
  xlab("Number of Trees") + ylab("Accuracy Percentage") +
  scale_colour_manual("", breaks = c("Up","Down","Stable"), values = c("red","blue","green"))

#question 7
BestRF <- model400
model400$importance
sort(model400$importance, decreasing = TRUE)

#question 8
par(mfrow=c(2,2))
hist(Up$EarnLossGapTrade)
hist(Down$EarnLossGapTrade)
hist(Stable$EarnLossGapTrade)

ks.test(Up$EarnLossGapTrade, Down$EarnLossGapTrade)
ks.test(Up$EarnLossGapTrade, Stable$EarnLossGapTrade)
ks.test(Down$EarnLossGapTrade, Stable$EarnLossGapTrade)

hist(Up$IClass)
hist(Down$IClass)
hist(Stable$IClass)

ks.test(Up$IClass,Down$IClass)
ks.test(Up$IClass,Stable$IClass)
ks.test(Down$IClass, Stable$IClass)

#Question 9
#cluster 6 -> cluster1
km6$size
Clu1

library(DMwR)
table(Clu1$Class)
Clu1$Class = as.factor(Clu1$Class)
set.seed(1)
SMOTECLU1 <- SMOTE(Class ~ ., Clu1, perc.over = 2000, perc.under = 100)
na.omit(SMOTECLU1)
table(SMOTECLU1$Class)

SMOTECLU1 <- SMOTE(Class ~ ., SMOTECLU1, perc.over = 3000, perc.under = 200)
table(SMOTECLU1$Class)

Clu1Indexes <- sample(1:nrow(SMOTECLU1), size = 0.2 * nrow(SMOTECLU1))
Clu1Test <- SMOTECLU1[Clu1Indexes,]
Clu1Train <- SMOTECLU1[-Clu1Indexes,]

table(Clu1Train$Class)
table(Clu1Test$Class)

newmodel<- randomForest(Class ~ . - Class- DailyReturn - Classifier- cluster, data= Clu1Train, ntrees = 300, ntry = (ncol(Clu1Train)), na.action = na.exclude)

#Question 10

newmodelpred <- predict(newmodel, newdata= Clu1Test)
NEWCONFTEST300 <- confusionMatrix(as.factor(Clu1Test$Class),as.factor(newmodelpred))
NEWCONF300 <- prop.table(NEWCONFTEST300$table)/rowSums(prop.table(NEWCONFTEST300$table))
#Question 11
library(e1071)
NEWTRA <- bind_rows(UpTrain,DownTrain)
NEWTST <- bind_rows(UpTest, DownTest)

NEWTRA <- transform(NEWTRA, Class = as.factor(Class))
NEWTST <- transform(NEWTST, Class = as.factor(Class))

svmmodel<- svm(Class~. - Class- DailyReturn - Classifier- cluster, data= NEWTRA , kernel = "linear")
svmpred <- predict(svmmodel, newdata = NEWTST)
SVMTRAIN <- confusionMatrix(as.factor(NEWTST$Class),as.factor(svmpred))
SVMTRAINCONF <- prop.table(SVMTRAIN$table)/rowSums(prop.table(SVMTRAIN$table))

svmmodel<- svm(Class~. - Class- DailyReturn - Classifier- cluster, data= NEWTST , kernel = "linear")
svmpred <- predict(svmmodel, newdata = NEWTRA)
SVMTEST <- confusionMatrix(as.factor(NEWTRA$Class),as.factor(svmpred))
SVMTESTCONF <- prop.table(SVMTEST$table)/rowSums(prop.table(SVMTEST$table))

write.table(corr, "C:/Users/Kevin/Desktop/fall 2020/6350/CORRMATRIX.csv",quote=FALSE, sep = ",",
            row.names=FALSE, col.names=FALSE )
write.table(Values, "C:/Users/Kevin/Desktop/fall 2020/6350/VALUES.CSV",quote=FALSE, sep = ",",
            row.names=FALSE, col.names=FALSE )
write.table(eigenvectors, "C:/Users/Kevin/Desktop/fall 2020/6350/VECTORS.CSV",quote=FALSE, sep = ",",
            row.names=FALSE, col.names=FALSE )


install.packages("GuessCompx")
library(GuessCompx)

CompEst(SDATA,km6)
CompEst(SDATA,model400)
CompEst(SDATA, svmmodel)
