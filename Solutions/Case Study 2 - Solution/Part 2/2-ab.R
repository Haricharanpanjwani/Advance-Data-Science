setwd("/Users/Prateek/Documents/Summer 16/TA/Summer 16/Assignment/Assignment 2/Assignment2/Part 1/Data")


Hourly_filled_DS <- read.csv("Hourly_filled_data.csv",  stringsAsFactors = FALSE)
Hourly_filled_DS$KWh_Class[Hourly_filled_DS$kWh > mean(Hourly_filled_DS$kWh)] <- 1
Hourly_filled_DS$KWh_Class[Hourly_filled_DS$kWh <= mean(Hourly_filled_DS$kWh)] <- 0
Hourly_filled_DS$KWh_Class <- factor( Hourly_filled_DS$KWh_Class, levels = c(0, 1), labels = c("Optimal", "Above_Normal"))

head(Hourly_filled_DS)

###################################################################################################

#Logistic Regression


#Data Partitioning
set.seed(200)
index <- sample(seq_len(nrow(Hourly_filled_DS)), size = floor(0.75 * nrow(Hourly_filled_DS)))
train <- Hourly_filled_DS[index, ]
test  <- Hourly_filled_DS[-index, ]


#Build Model
LogRegression <- glm(KWh_Class ~. -Account -Date -kWh -day -year, data=train, family=binomial(link="logit"))
summary(LogRegression)


#Run the model on the test set
test.pred <- predict(LogRegression, test, type='response')
logpred <- rep("Optimal",length(test.pred ))


#Cutoff = 0.5
logpred[test.pred >= 0.5] <- "Above_Normal"


#Model Evaluation
library(caret)
#confusionMatrix <- confusionMatrix(test$KWh_Class, logpred)
confusionMatrix <- table(logpred,test$KWh_Class)
confusionMatrix
Error <- (((confusionMatrix[1,2]) + (confusionMatrix[2,1])) /((confusionMatrix[2,1]) + (confusionMatrix[1,2]) + (confusionMatrix[1,1])+(confusionMatrix[2,2])))
Error


#ROC AUC CUrve
library(ROCR)
pred <- prediction(test.pred , test$KWh_Class)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main="ROC CURVE", xlab="1-Specificity", ylab="Sensitivity")
library(AUC)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc


#Lift CHART
test$probs = test.pred
test$prob = sort(test$probs,decreasing = T)
lift <- lift(KWh_Class ~ prob, data = test)
xyplot(lift,plot = "gain")

write.csv(cbind(confusionMatrix,Error), "LogisticRegressionClassificationPerformancemetrics.csv")

###################################################################################################

#Classification Tree


#Data Partitioning
set.seed(123)
index <- sample(seq_len(nrow(Hourly_filled_DS)), size = floor(0.75 * nrow(Hourly_filled_DS)))
train <- Hourly_filled_DS[index, ]
test <- Hourly_filled_DS[-index, ]


#Build Model
library(tree)
tree_Model = tree(KWh_Class ~ PeakHour + hour + month + DayOfWeek + Weekday + Temperature, data=train)
summary(tree_Model)


#Display Tree with labels
plot(tree_Model)
text(tree_Model, pretty = 0) 


#Pruning Decision Graph
cv.Hourly_filled_DS = cv.tree(tree_Model, FUN = prune.misclass)
names(cv.Hourly_filled_DS)
plot(cv.Hourly_filled_DS$size, cv.Hourly_filled_DS$dev, type = 'b')
plot(cv.Hourly_filled_DS)


#Pruning tree to remove unwanted conditions and to check for accuracy improvization
prune.Hourly_filled_DS = prune.misclass(tree_Model, best = 8)
plot(prune.Hourly_filled_DS)
text(prune.Hourly_filled_DS, pretty = 0)


#Pruned Tree Performance
prune.pred = predict(prune.Hourly_filled_DS, test, type = "class")


#Model Evaluation
confusionMatrix <- table(prune.pred, test$KWh_Class)
confusionMatrix
ErrorRate <- sum(confusionMatrix[2], confusionMatrix[3]) / sum(confusionMatrix[1:4])
ErrorRate

write.csv(cbind(confusionMatrix,ErrorRate), "ClassificationTreeClassificationPerformancemetrics.csv")

###################################################################################################

#Neural Network

library(nnet)

Hourly_filled_DS$KWh_Class <- NULL
KWh_Class = ifelse (Hourly_filled_DS$kWh > mean(Hourly_filled_DS$kWh), "Above_Normal", "Optimal")
#KWH_Class <- ifelse(Hourly_filled_DS$kWh > mean(Hourly_filled_DS$kWh) ,1, 0)
Hourly_filled_DS = data.frame(Hourly_filled_DS , KWh_Class)
dim(Hourly_filled_DS)
head(Hourly_filled_DS)
Hourly_filled_DS$kWh <- NULL
head(Hourly_filled_DS)

#Data Partitioning
set.seed(113)
index <- sample(seq_len(nrow(Hourly_filled_DS)), size = floor(0.75 * nrow(Hourly_filled_DS)))
train <- Hourly_filled_DS[index, ]
testt <- Hourly_filled_DS[-index, ]


#Build Model
nett <- nnet((KWh_Class) ~ Temperature + PeakHour+ Weekday+ month + hour, train, hidden = 3, size = 6)
test.predd <- predict(nett, testt, type=("class"))


#Model Evaluation
confusionMatrix <- table(testt$KWh_Class, test.predd)
confusionMatrix

ErrorRate <- sum(confusionMatrix[2], confusionMatrix[3])/ sum(confusionMatrix[1:4])
ErrorRate

write.csv(cbind(confusionMatrix,ErrorRate), "NeuralNetworkClassificationPerformancemetrics.csv")
