library(gdata)
#reading the best filled dataset for classification
BostonConsolidate <- read.csv("/Users/hpanjwani/R Workspace/Assignment 2/Output/Hourly_filled_data.csv", header=T,na.strings=c(""))
#View(BostonConsolidate)

#A. Logistic Regression steps
#1. we need to check for missing values and look how many unique values there are for each variable using the sapply() function which applies the function passed as argument to each column of the dataframe.
sapply(BostonConsolidate,function(x) sum(is.na(x)))
sapply(BostonConsolidate, function(x) length(unique(x)))

#Amelia package has a special plotting function missmap() that will plot your dataset and highlight missing values
#install.packages("Amelia")
library(Amelia)
missmap(BostonConsolidate, main = "Missing values vs observed")

#use the following the data for regression
drops <- c("X", "Account", "Date", "year")
dataReg <- BostonConsolidate[ , !(names(BostonConsolidate) %in% drops)]
#View(dataReg)
#dataReg <- subset(BostonConsolidate, select=c(3,4,5,7,8,9,10,11,12))

#as.factor(dataReg$Peakhour)
#contrasts(dataReg$Peakhour)

#logit
dataReg$KWH_Class <- NULL

classList <- sapply(dataReg$kWh, function(x) {
  if(x > mean(dataReg$kWh)) {dataReg$KWH_Class <- 1} 
  else { dataReg$KWH_Class <- 0} })

dataReg$KWH_Class <- classList;

#sample model fitting
#75% of the sample size
class_smp_size <- floor(0.75 * nrow(BostonConsolidate))

#Set the seed to make your partition reproductible
set.seed(34)
train_logistic <- sample(seq_len(nrow(BostonConsolidate)), size = class_smp_size)

#Split the data into training and testing
train <- dataReg[train_logistic, ]
test <- dataReg[-train_logistic, ]
#View(train)

#logit code
modelLogit <- glm(KWH_Class ~ month + day + Weekday + hour + Peakhour + Temp,family=binomial(link='logit'),data=train)

#summary
summary(modelLogit)

#analyzing the table of deviance
anova(modelLogit, test="Chisq")

#r2 in logit
#install.packages("pscl")
library(pscl)
pR2(modelLogit)
nrow(test)
#View(test)

#a model with good predictive ability should have an AUC closer to 1 (1 is ideal) than to 0.5.
#install.packages("ROCR")
library(ROCR)
#is.atomic(test$KWH_Class)
p <- predict(modelLogit, newdata=test)
pr <- prediction(p, test["KWH_Class"])
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

plot(modelLogit, uniform=TRUE, main="Logistic Regression for Boston 1")

#B. classification - Decision Tree
# KWH > average KWH, KWH_Class = "Above_Normal" otherwise, KWH_Class = "Optimal".
library(ISLR)
BostonConsolidate$KWH_Class <- NULL

classList <- sapply(BostonConsolidate$kWh, function(x) {
  if(x > mean(BostonConsolidate$kWh)) {BostonConsolidate$KWH_Class <- "Above_Normal"} 
  else { BostonConsolidate$KWH_Class <- "Optimal"} })

BostonConsolidate$KWH_Class <- classList;
#View(BostonConsolidate)

#write the classification into the hourly file
write.csv(BostonConsolidate, file="/Users/hpanjwani/R Workspace/Assignment 2/Output/Hourly_filled_data_classification.csv")

#Decision Tree model
#install.packages("rpart")
library(rpart)

classFit1 <- rpart(KWH_Class ~ month + day + Weekday + hour + Peakhour + Temp, data=train,method="class")
summary(classFit1)
plot(classFit1)
text(classFit1, pretty=0)

#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')
#library(rattle)
#library(rpart.plot)
#library(RColorBrewer)
#fancyRpartPlot(classFit1)

#predicting for test data
DecisionPrediction1 <- predict(classFit1, test, type = "class")
#View(as.data.frame(DecisionPrediction1))

#View(test)
#saving my prediction to my dataframe and new file
submitPredictClass1 <- data.frame(X = test$KWH_Class, KWH_Class = DecisionPrediction1)
write.csv(submitPredictClass, file = "/Users/hpanjwani/R Workspace/Assignment 2/Output/myDecisionClasstreefile.csv", row.names = FALSE)


#optimal values
#table(DecisionPrediction, testBosClass)
printcp(classFit1) # display the results 
plotcp(classFit1) 
summary(classFit1)

# plot tree 
plot(classFit1, uniform=TRUE, 
     main="Classification Tree for Boston")
text(classFit1, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(classFit1, file = "/Users/hpanjwani/R Workspace/Assignment 2/Output/decisiontree1.ps", 
     title = "Classification Tree for Boston")

# prune the tree 
pfit1<- prune(classFit1, cp= classFit1$cptable[which.min(classFit1$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit1, uniform=TRUE, 
     main="Pruned Classification Tree for Boston 1")
text(pfit1, use.n=TRUE, all=TRUE, cex=.8)
post(pfit1, file = "/Users/hpanjwani/R Workspace/Assignment 2/Output/ptree1.ps", 
     title = "Pruned Classification Tree for Boston 2")
# display the pruned results 
printcp(pfit1) 
plotcp(pfit1) 
summary(pfit1)

#C. Neural Network for classification
#neural network
file4 <- read.csv("/Users/hpanjwani/R Workspace/Assignment 2/Output/Hourly_filled_data.csv", header=T,na.strings=c(""))
bostonnnet <- file4

#1. we need to check for missing values and look how many unique values there are for each variable using the sapply() function which applies the function passed as argument to each column of the dataframe.
sapply(trainBosnnet,function(x) sum(is.na(x)))
sapply(trainBosnnet, function(x) length(unique(x)))

#Amelia package has a special plotting function missmap() that will plot your dataset and highlight missing values
#install.packages("Amelia")
library(Amelia)
missmap(trainBosnnet, main = "Missing values vs observed")

datannet <- bostonnnet
datannet$KWH_Class <- NULL

classList2 <- sapply(datannet$kWh, function(x) {
  if(x > mean(datannet$kWh)) {datannet$KWH_Class <- "Above_Normal"} 
  else { datannet$KWH_Class <- "Optimal"} })

datannet$KWH_Class <- classList2;
#table(datannet$KWH_Class)
nrow(datannet)

datannet$KWH_factor <- factor(datannet$KWH_Class, levels = c("Above_Normal", "Optimal"), labels = c("1", "0"))
#View(datannet)

#normalizing the data
normalize2 <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

#selecting my new subset
drops <- c("X", "Account", "Date", "year", "KWH_Class")
dfnnet <- datannet[ , !(names(datannet) %in% drops)]
dfnnet$KWH_factor<- as.numeric(dfnnet$KWH_factor)
#View(dfnnet)

#normalizing the datannet
datannet_n <- as.data.frame(lapply(dfnnet, normalize2))
#View(datannet_n)

#import the function from Github
#install.packages("clusterGeneration")
#install.packages("neuralnet")
library(devtools)
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
library(clusterGeneration)
library(nnet)
require(nnet)
library(neuralnet)

#Sampling
#75% of the sample size
class_new <- floor(0.75 * nrow(datannet_n))

#Set the seed to make your partition reproductible
set.seed(13)
train_bostonnnet <- sample(seq_len(nrow(datannet_n)), size = class_new)
#View(train_bostonnnet)

#Split the data into training and testing
trainBosnnet <- datannet_n[train_bostonnnet, ]
testBosnnet<- datannet_n[-train_bostonnnet, ]
#View(trainBosnnet)

#applying the neural net algorithm
#fitnn <- neuralnet(KWH_factor ~ month + day + Weekday + hour + Peakhour + Temp, data = trainBosnnet, hidden=c(6), threshold=0.5, linear.output = F)
fitnn <- nnet(KWH_factor ~ month + day + Weekday + hour + Peakhour + Temp, trainBosnnet, size=1, rang=0.09, hess = F, dk=15e-4, maxit = 250)
fitnn
summary(fitnn)
plot.nnet(fitnn)
