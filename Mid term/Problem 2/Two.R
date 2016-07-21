#reading the dataset from the ad.data file
dataset <- read.csv("/Users/hpanjwani/R Workspace/Mid Term/Problem 2/ad-dataset/ad.data", 
                    strip.white = TRUE, na.strings = "?", header = FALSE)

#reading the column names from the ad.names file
column.names <-  read.table("/Users/hpanjwani/R Workspace/Mid Term/Problem 2/ad-dataset/ad.names", sep=":", skip = 3, blank.lines.skip= TRUE, comment.char = "|", fill=FALSE, header=FALSE, strip.white=TRUE)

#converting column.names$V1 to vector so that assign it to dataset
name <- as.vector(column.names$V1)
#appending the 'classes' column name to vector to complete the dataset
name <- c(name, "classes")
#checking the length of the vector, to make sure vector is complete
length(name)

#setting the column names to the dataset
colnames(dataset) <- name

#changing column height and width to numeric
dataset[,1] <- as.numeric(dataset[,1])
dataset[,2] <- as.numeric(dataset[,2])
dataset[,4] <- as.numeric(dataset[,4])

#changing the value from ad/non-ad to 0/1
dataset$classes <- ifelse(dataset$classes=="ad.", 1L, 0L) 

#write to csv file
write.csv(dataset, file="/Users/hpanjwani/R Workspace/Mid Term/Problem 2/ad-dataset/InternetADs.csv", row.names = F)

#Writing Box-Plot functions to detect outliers
boxplot(dataset$height, horizontal = TRUE)
boxplot(height ~ classes, data = dataset, col = "lightgray")

boxplot(dataset$width, horizontal = TRUE)
boxplot(width ~ classes, data = dataset, col = "lightgray")

boxplot(dataset$aratio, horizontal = TRUE)
boxplot(aratio ~ classes, data = dataset, col = "lightgray")

#Storing NA in a dataset and applying the oulier functions to the dataset which doesnt contain NA
NA_dataset <- c()
NA_dataset <- c(NA_dataset, which(is.na(dataset$height)))
NA_dataset <- c(NA_dataset, which(is.na(dataset$width)))
NA_dataset <- c(NA_dataset, which(is.na(dataset$aratio)))
NA_dataset <- c(NA_dataset, which(is.na(dataset$local)))
null_dataset <- unique(NA_dataset)

#dataset without NA's
cleansed_dataset <- dataset[-null_dataset,]
null_dataset <- (dataset[null_dataset,])

#Function for Outlier Detection, where we are checking for odd height and width,
#storing the row index in the outlier.dataset
count = 0
outlier.dataset <- c();
for(i in 1:nrow(cleansed_dataset)) {
  if(((cleansed_dataset$width[i]) < (mean(cleansed_dataset$width) - (1.5)*sd(cleansed_dataset$width)) ||
     (cleansed_dataset$width[i]) > (mean(cleansed_dataset$width) + (1.5)*sd(cleansed_dataset$width))) &&
     ((cleansed_dataset$height[i]) < (mean(cleansed_dataset$height) - (1.5)*sd(cleansed_dataset$height)) ||
      (cleansed_dataset$height[i]) > (mean(cleansed_dataset$height) + (1.5)*sd(cleansed_dataset$height)))) {
    count = count + 1;
    outlier.dataset <- c(outlier.dataset, i)
  }
}
#outliers detcted - 11
#removing the outlier from cleaned dataset
cleansed_dataset <- cleansed_dataset[-outlier.dataset,]
#nrow(cleansed_dataset)...............2348

###############################################################################

#row number for where aspect ratio is < 2, because such small ads are not possible
index <- which(cleansed_dataset$aratio < 0.2, arr.ind=TRUE)
cleansed_dataset <- cleansed_dataset[-c(index),]
#nrow(cleansed_dataset)................1 outlier detceted(2347)

#If aspect ratio is > 10 and height is <= 20, there is highly unlikely such type of ads on the
#webpage. Maximum aratio for internet ads is 8.09
#36 outliers detected
index <- which((cleansed_dataset$aratio >10) & (cleansed_dataset$height <=20), arr.ind=TRUE)
cleansed_dataset <- cleansed_dataset[-c(index),]
#nrow(cleansed_dataset)...................#2311

#width > 600 and aspect ratio > 2, such type of format is also not seen in the internet ads
# 1 outlier detected
index <- which((cleansed_dataset$width > 600 & cleansed_dataset$aratio > 2) , arr.ind=TRUE)
cleansed_dataset <- cleansed_dataset[-c(index),]
#nrow(cleansed_dataset)..............2310

#width < 20 and height < 20, is such a small portion, it want even visible properly,
#therefore, we can remove those rows
#98 outliers detected
index <- which((cleansed_dataset$width <= 20 & cleansed_dataset$height <= 20)  , arr.ind=TRUE)
cleansed_dataset <- cleansed_dataset[-c(index),]
#nrow(cleansed_dataset)................2212

#after observing this data
#Height = 640, widht = 1, classes = Non ad
#Height = 1, widht = 1, classes = Non ad

# Boxplots after Outlier Reduction
boxplot(cleansed_dataset$width, horizontal = TRUE)
boxplot(height ~ classes, data = cleansed_dataset, col = "lightgray")

boxplot(cleansed_dataset$width, horizontal = TRUE)
boxplot(width ~ classes, data = cleansed_dataset, col = "lightgray")

boxplot(cleansed_dataset$aratio, horizontal = TRUE)
boxplot(aratio ~ classes, data = cleansed_dataset, col = "lightgray")

#Binding the cleansed dataset with the null_dataset
dataset <- NULL
dataset <- rbind(dataset,null_dataset)
dataset <- rbind(dataset,cleansed_dataset)

# Impute function to handle NA's in dataset
impute <- function(x) {
  if (class(x) == "numeric") { 
    ifelse(is.na(x), mean(x, na.rm = TRUE), x) 
  }
  else {
    ifelse(is.na(x), as.factor(median(x, na.rm = T)), x)
  }
}
dataset<- data.frame(lapply(dataset,impute))

write.csv(dataset, file="/Users/hpanjwani/R Workspace/Mid Term/Problem 2/ad-dataset/InternetADs_revised.csv", row.names = F)

#install.packages("caret")
library(caret)
# plotting data pairwise to check for co-relation
pairs(dataset)
ncol(dataset)

#Performing PCA to detect variable importance
require(caret)
#holds the first 4 columns
trans = preProcess(dataset[,1:4], 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
PC1 = predict(trans, dataset[,1:4])
head(PC1,4)

#457 features from url terms, each of the form "url*term1+term2...";
column.variance <- names(dataset[,5:461][, sapply(dataset[,5:461], function(v) var(v, na.rm=TRUE)==0)])
modified_dataset <- dataset
ncol(modified_dataset)
modified_dataset <- modified_dataset[, (!names(modified_dataset) %in% column.variance)]
trans2 = preProcess(modified_dataset[,5:452], 
                    method=c("BoxCox", "center", 
                             "scale", "pca"))
PC2 = predict(trans2, modified_dataset[,5:452])
head(PC2,4)

#495 features from origurl terms, in same form; for example:
column.variance_PC3 <- names(modified_dataset[,453:947][, sapply(modified_dataset[,453:947], function(v) var(v, na.rm=TRUE)==0)])
ncol(modified_dataset)
modified_dataset <- modified_dataset[, (!names(modified_dataset) %in% column.variance_PC3)]
trans3 <-  preProcess(modified_dataset[,453:937], 
                      method=c("BoxCox", "center", 
                               "scale", "pca"))
PC3 = predict(trans3, modified_dataset[,453:937])
head(PC3,3)

#472 features from ancurl terms, in same form; for example:
column.variance_PC4 <- names(modified_dataset[,938:1409][, sapply(modified_dataset[,938:1409], function(v) var(v, na.rm=TRUE)==0)])
modified_dataset <- modified_dataset[, (!names(modified_dataset) %in% column.variance_PC4)]
trans4 <-  preProcess(modified_dataset[,938:1403], 
                      method=c("BoxCox", "center", 
                               "scale", "pca"))
PC4 = predict(trans4, modified_dataset[,938:1403])
head(PC4,3)

#111 features from alt terms, in same form;
column.variance_PC5 <- names(modified_dataset[,1404:1514][, sapply(modified_dataset[,1404:1514], function(v) var(v, na.rm=TRUE)==0)])
ncol(modified_dataset)
modified_dataset <- modified_dataset[, (!names(modified_dataset) %in% column.variance_PC5)]
trans5 <-  preProcess(modified_dataset[,1404:1514], 
                      method=c("BoxCox", "center", 
                               "scale", "pca"))
PC5 = predict(trans5, modified_dataset[,1404:1514])
head(PC5,3)

#19 features from caption terms
column.variance_PC6 <- names(modified_dataset[,1515:1533][, sapply(modified_dataset[,1515:1533], function(v) var(v, na.rm=TRUE)==0)])
ncol(modified_dataset)
modified_dataset <- modified_dataset[, (!names(modified_dataset) %in% column.variance_PC6)]
trans6 <-  preProcess(modified_dataset[,1515:1533], 
                      method=c("BoxCox", "center", 
                               "scale", "pca"))
PC6 = predict(trans6, modified_dataset[,1515:1533])
head(PC6,3)

#consolidating all the important variables
consolidate.pc <- NULL
PC1 <- cbind(PC1, PC2)
PC1 <- cbind(PC1, PC3)
PC1 <- cbind(PC1, PC4)
PC1 <- cbind(PC1, PC5)
PC1 <- cbind(PC1, PC6)

consolidate.pc <- cbind(PC1, classes = dataset$classes)
View(consolidate.pc)
ncol(consolidate.pc)


##############Classification#############
#setting the seed
set.seed(1)

#80% of the sample size
training <- createDataPartition(consolidate.pc$classes, p=0.8)
sample(seq_len(nrow(consolidate.pc)), size = floor(0.8 * nrow(consolidate.pc)))

#Split the data into training and testing
train <- consolidate.pc[training[[1]], ]
test <- consolidate.pc[-training[[1]], ]

#applying impute function to remove the NA's from train dataset
train <- data.frame(lapply(train, impute))
train$classes <- as.factor(train$classes)

#applying impute function to remove the NA's from test dataset
test <- data.frame(lapply(test, impute))
test$classes <- as.factor(test$classes)

#### 1. Logistic Regression##################
######## START OF LOGISTIC REGRESSION #######
#A. Logistic Regression steps
#fitting the logistic regression model, based on the important parameters determined by PCA
#There are total of 498 important parameters determined out of 1558
library(ggplot2)
lm.fit <- glm(classes ~ ., train, family = binomial())

#summary of the logistic regression
summary(lm.fit)

#r2 in logit, we see McFadden which is similar to R2. Closer to zero implies model is good
#install.packages("pscl")
library(pscl)
pR2(lm.fit)

#predicting the value of test data to determine the accuracy
lm.probs <- predict(lm.fit, test, type = "response")
#based on the threshold 0.4(making a model biased towards ADs), we are classifying the values
lm.preds <- ifelse(lm.probs > 0.4, 1, 0)

# Plot the performance of the model applied to the evaluation set as an ROC curve.
#install.packages("ROCR")
detach("package:neuralnet", unload=TRUE)
library(ROCR)
prediction_linear <- prediction(lm.probs, test$classes)
performance_linear <- performance(prediction_linear, measure = "tpr", x.measure = "fpr")
#plotting ROC Curve
plot(performance_linear, main = "ROC Curve for Logistic Regression", xlab = "1-Specificity", ylab = "Sensitivity")

#plot the lift curve
library(caret)
test$probs <- lm.probs
test$probs <- sort(test$probs, decreasing = T)
lift <- lift(classes ~ probs, data = test)
lift
xyplot(lift, plot = "gain")

#Validation of Predicted Values, Confusion Matrix
#install.packages("e1071")
library(e1071)
library(caret)
#confusion matrix for logistic regression
#Accuracy : 0.9473
#Sesitivity: 0.8854
#specificity : 0.9585
confusionMatrix(data=factor(lm.preds),reference=factor(test$classes), positive='1')

#computing the overall error - 0.06
ads.logist.error <- 1- sum(lm.preds==test$classes)/length(test$classes)
ads.logist.error

#plotting the regression
plot(lm.fit, uniform=TRUE, main="Logistic Regression for Internet ADs")

######## END OF LOGISTIC REGRESSION #######
###########################################

#### 2. Classification - Decision Trees ##########
######## START OF DECISION TREES #######
#Decision Tree model
library(tree)
library(caret)
library (ISLR)
library(mcclust)
library(readxl)
library (grid)

#making the formula to be pasted in the decision tree
n <- names(train)
f <- as.formula(paste("classes ~", paste(n[!n %in% "classes"], collapse = " + ")))
#f

#training the decision tree based on train dataset
ads.tree = tree(f, data = train)

#summary of the decision tree
summary (ads.tree)

#display the tree structure and node levels of the tree
plot (ads.tree)
text (ads.tree, pretty = 0)

#calling cross-validation to choose tree complexity
cv.ads = cv.tree (ads.tree, FUN = prune.tree)
plot (cv.ads$size, cv.ads$dev, type='b')

#predict the classes and then based on threshold classify the values as Ads and Non-ADs
dec.prob = predict(ads.tree, test)
dec.pred = ifelse(dec.prob[,1] > 0.5, 0, 1)

#plot(dec.pred, as.numeric(test$classes))
#abline(0,1)

#pruning the tree based on the cross-validation curve
prune.ads.dec =prune.tree(ads.tree, best = 10)

#plotting the tree and filling the text
plot(prune.ads.dec)
text(prune.ads.dec, pretty = 0)

#calling cross-validation to choose tree complexity
cv.prune.ads = cv.tree (prune.ads.dec)
plot (cv.prune.ads$size, cv.prune.ads$dev, type='b')

#predict the classes and then based on threshold classify the values as Ads and Non-ADs
prune.dec.probs = predict (prune.ads.dec, newdata =test)
prune.dec.pred = ifelse(prune.dec.probs[,1] > 0.5, 0, 1)

#Validation of Predicted Values, Confusion Matrix
#Create predicted target value as 1 or 0
library(e1071)
library(caret)
#confusion Matrix for decision tree
#sensitivity = 0.86
#specificity = 0.99
#accuracy = 0.9728
confusionMatrix(data=factor(prune.dec.pred),reference=factor(test$classes), positive='1')

#computing the overall error - 0.03
ads.dec.error <- 1- sum(prune.dec.pred==test$classes)/length(test$classes)
ads.dec.error

#performance evaluation of the model
#ploting the rocr curve
library(ROCR)
roc_pred <- prediction(prune.dec.pred, as.numeric(test$classes))
ads.dec.model <- performance(roc_pred, measure="tpr", x.measure="fpr")
plot(ads.dec.model, colorize=TRUE)

#plot the lift curve
library(caret)
test$pred <- prune.dec.pred
test$pred <- sort(test$pred, decreasing = T)
lift.dec.model <- lift(classes ~ pred, data = test)
lift.dec.model
xyplot(lift.dec.model, plot = "gain")

######## END OF DECISION TREES #######
#####################################

#### 3. Neural Network  ##########
######## START OF NEURAL NETWORK #######
#C. Neural Network for classification
#import the function from Github
#install.packages("clusterGeneration")
#install.packages("neuralnet")
library(devtools)
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
library(clusterGeneration)
library(nnet)
require(nnet)
#assigning the card dataset to datannet, since we need to normalize in
#neural network we are assiging to different dataset
datannet <- consolidate.pc

#normalizing the data in the datannet, so that we can implement nnet
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

#normalizing the datannet
datannet_n <- as.data.frame(lapply(datannet, normalize))
#View(datannet_n)

#Sampling of the dataset
#75% of the sample size
class_new1 <- floor(0.75 * nrow(datannet_n))

#set the seed to make your partition reproductible
set.seed(13)
train_datannet <- sample(seq_len(nrow(datannet_n)), size = class_new1)

#split the data into training and testing
traindatannet <- datannet_n[train_datannet, ]
traindatannet$classes <- as.factor(traindatannet$classes)

testdatannet  <- datannet_n[-train_datannet,]
testdatannet$classes <- as.factor(testdatannet$classes)
#applying the neural net algorithm
#install.packages("NeuralNetTools")
library(NeuralNetTools)

#making the formula to be pasted in the neural net
n <- names(traindatannet)
f <- as.formula(paste("classes ~", paste(n[!n %in% "classes"], collapse = " + ")))
f
#training the neural network with size = 10 and 1 hidden layers
fitnn <- nnet(f, size=10, data = traindatannet,
              maxit = 200, 
              rang = 0.7, Hess = FALSE, trace = TRUE, MaxNWts = 10000, abstol = 1.0e-4, 
              decay = 15e-4, reltol = 1.0e-8, hidden = 7, threshold = 0.01,act.fct="tanh")
fitnn
#summary of the neural net
summary(fitnn)

#plotting the neural net, to visualize the nodes
library(scales)
library(reshape)
plot.nnet(fitnn, pos.col='darkgreen',neg.col='darkblue', alpha.val=0.7, rel.rsc=10, circle.cex=5, cex=1.4,circle.col='brown')

#predicting the classes using nnet model
predict.neural.ads <- predict(fitnn, newdata=testdatannet)

#converting the prediction to vector
predict.neural.ads <- as.vector(predict.neural.ads)

#changing matrix to numeric
#Create predicted target value as 1 or 0 based on threshold of 0.2
pred.ads.level <- ifelse(predict.neural.ads > 0.2, 1, 0)
#View(predict.neural.Credit)

#Validation of Predicted Values, Confusion Matrix
#Create predicted target value as 1 or 0
library(e1071)
library(caret)
#confusion Matrix for neural network
#sensitivity = 0.84
#specificity = 0.96
#accuracy = 0.95
pred.ads.level <- factor(pred.ads.level)
confusionMatrix(data=pred.ads.level,reference=factor(testdatannet$classes), positive='1')

#computing the overall error - 0.049
ads.neural.error <- 1- sum(pred.ads.level==testdatannet$classes)/length(testdatannet$classes)
ads.neural.error

#plot the ROC curve
#install.packages("ROCR")
library(ROCR)
pred.resp.nnet.num <- as.numeric(pred.ads.level)
roc_pred.nnnet <- prediction(pred.resp.nnet.num, testdatannet["classes"])
perf.nnet.model <- performance(roc_pred.nnnet, measure="tpr", x.measure="fpr")
plot(perf.nnet.model, colorize=TRUE)

#plot the lift curve
testdatannet$pred <- predict.neural.ads
testdatannet$pred <- sort(testdatannet$pred, decreasing = T)
lift <- lift( classes ~ pred, data = testdatannet)
lift
xyplot(lift, plot = "gain")

#Sensitivity/specificity curve and precision/recall curve:
plot(performance(roc_pred.nnnet, measure="sens", x.measure="spec"), colorize=TRUE)
plot(performance(roc_pred.nnnet, measure="prec", x.measure="rec"), colorize=TRUE)

##################### END OF NEURAL NETWORK ####################
################################################################

#writing prediction performance metrics to csv files
filename = "/Users/hpanjwani/R Workspace/Mid Term/Problem 2/PredictionPerformanceMetrics_Problem2.csv"
account <- c("Internet Ads eater")
write.table(account, file=filename, row.names = F, col.names = F, qmethod = "double")

ads.logist <- confusionMatrix(data=factor(lm.preds),reference=factor(test$classes), positive='1')

#writing the confusion matrix of logistic regression to the file
write.table("\n", file=filename, append = T, row.names = F, col.names = F)
write.table("Confusion Matrix for Logistic Regression", file=filename, append = T, row.names = F, col.names = F)
write.table(ads.logist$table, file=filename, append = T, col.names = F, qmethod = "double")
write.table("Confusion Matrix Accuracy", file=filename, append = T, row.names = F, col.names = F)
write.table(ads.logist$overall, file=filename, append = T, col.names = F, qmethod = "double")

#writing the overall error of logistic error to the file
write.table("\n", file=filename, append = T, row.names = F, col.names = F)
write.table("Overall Error of Logistic Regression", file=filename, append = T, row.names = F, col.names = F)
write.table(ads.logist.error, file=filename, append = T, col.names = F, qmethod = "double")

ads.dec <- confusionMatrix(data=factor(prune.dec.pred),reference=factor(test$classes), positive='1')

#writing the confusion matrix of decision tree to the file
write.table("\n", file=filename, append = T, row.names = F, col.names = F)
write.table("Confusion Matrix for Decision Tree", file=filename, append = T, row.names = F, col.names = F)
write.table(ads.dec$table, file=filename, append = T, row.names = F, col.names = F, qmethod = "double")
write.table("Confusion Matrix Accuracy", file=filename, append = T, row.names = F, col.names = F)
write.table(ads.dec$overall, file=filename, append = T, row.names = F, col.names = F, qmethod = "double")

#writing the overall error of decision tree to the file
write.table("\n", file=filename, append = T, row.names = F, col.names = F)
write.table("Overall Error of Decision Tree", file=filename, append = T, row.names = F, col.names = F)
write.table(ads.dec.error, file=filename, append = T, row.names = F, col.names = F, qmethod = "double")

ads.neural <- confusionMatrix(data=pred.ads.level,reference=factor(testdatannet$classes), positive='1')

#writing the confusion matrix of neural net to the file
write.table("\n", file=filename, append = T, row.names = F, col.names = F)
write.table("Confusion Matrix for Neural Network", file=filename, append = T, row.names = F, col.names = F)
write.table(ads.neural$table, file=filename, append = T, row.names = F, col.names = F, qmethod = "double")
write.table("Confusion Matrix Accuracy", file=filename, append = T, row.names = F, col.names = F)
write.table(ads.neural$overall, file=filename, append = T, row.names = F, col.names = F, qmethod = "double")

#writing the overall error of neural net to the file
write.table("\n", file=filename, append = T, row.names = F, col.names = F)
write.table("Overall Error of Neural Net", file=filename, append = T, row.names = F, col.names = F)
write.table(ads.neural.error, file=filename, append = T, row.names = F, col.names = F, qmethod = "double")