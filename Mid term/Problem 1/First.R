#reading the credit card dataset
library(gdata)
credit_card <- read.xls("/Users/hpanjwani/R Workspace/Mid Term/Problem 1/credit card.xls")

#changing the column names to the more readable and verbose form
colnames(credit_card) <- c("ID", "credit_limit", "sex", "education", "marriage", "age", "repayment_sept", "repayment_august", 
                           "repayment_july", "repayment_june", "repayment_may", "repayment_april", "bill_amt_sept", "bill_amt_august", 
                           "bill_amt_july", "bill_amt_june", "bill_amt_may", "bill_amt_april", "pay_amt_sept", "pay_amt_august", 
                           "pay_amt_july", "pay_amt_june", "pay_amt_may", "pay_amt_april", "default_payment")

#computing rows and structure of the dataset
nrow(credit_card)
str(credit_card)

#removing the redundant column
credit_card <- credit_card[-1,]

#changing column from factor to numeric
indx <- sapply(credit_card, is.factor)
credit_card[indx] <- lapply(credit_card[indx], function(x) as.numeric(as.character(x)))

#collecting the index of the rows, with all zero's, that means the person has not done transaction over the 6 months period
empty.transactions <- apply(credit_card[c("bill_amt_sept", "bill_amt_august", "bill_amt_july", "bill_amt_june", "bill_amt_may", "bill_amt_april", 
                        "pay_amt_sept", "pay_amt_august", "pay_amt_july", "pay_amt_june", "pay_amt_may", "pay_amt_april")] == 0,1,sum) == 12

#removing the 'empty.transactions' rows from the dataset
credit_card <- credit_card[!empty.transactions,]

#assigning credit_card to card dataset, before doing that assign NULL to it, to check card is empty
card <- NULL

#appending columns in the required format to make month wise distribution
mkRow <- function(nCol, i) {
  card <- as.list(rnorm(nCol))
  # make row mixed types by changing first column to string
  card$id <- credit_card$ID
  card$credit_limit <- credit_card$credit_limit
  card$sex <- credit_card$sex
  card$education <- credit_card$education
  card$marriage <- credit_card$marriage
  card$age <- credit_card$age
  
  #assign month as sept
  if(i == 1) {
    card$month <- as.Date("09/06/2005", format = "%m/%d/%Y")
    card$repayment_status <- credit_card$repayment_sept
    card$bill_amt <- credit_card$bill_amt_sept
    card$amout_paid <- 0
  }
  #assign month as august
  else if(i == 2) {
    card$month <- as.Date("08/06/05", format = "%m/%d/%y")
    card$repayment_status <- credit_card$repayment_august
    card$bill_amt <- credit_card$bill_amt_august
    card$amout_paid <- credit_card$pay_amt_sept
  }
  #assign month as july
  else if(i == 3) {
    card$month <- as.Date("07/06/05", format = "%m/%d/%y")
    card$repayment_status <- credit_card$repayment_july
    card$bill_amt <- credit_card$bill_amt_july
    card$amout_paid <- credit_card$pay_amt_august
  } 
  #assign month as june
  else if(i == 4) {
    card$month <- as.Date("06/06/05", format = "%m/%d/%y")
    card$repayment_status <- credit_card$repayment_june
    card$bill_amt <- credit_card$bill_amt_june
    card$amout_paid <- credit_card$pay_amt_july
  } 
  #assign month as may
  else if(i == 5) {
    card$month <- as.Date("05/06/05", format = "%m/%d/%y")
    card$repayment_status <- credit_card$repayment_may
    card$bill_amt <- credit_card$bill_amt_may
    card$amout_paid <- credit_card$pay_amt_june
  }
  #assign month as june
  else if(i == 6) {
    card$month <- as.Date("04/06/05", format = "%m/%d/%y")
    card$repayment_status <- credit_card$repayment_april
    card$bill_amt <- credit_card$bill_amt_april
    card$amout_paid <- credit_card$pay_amt_may
  }
  
  card$default_payment <- credit_card$default_payment
  do.call(cbind.data.frame, card)
}

#calling the function 6 times so that each row can be repeated as 6 times
mkFrameList <- function(nRow,nCol) {
  d <- lapply(seq_len(nRow),function(i) {
    aug <- mkRow(nCol, i)
    data.frame(aug, stringsAsFactors=FALSE)
  })
  do.call(rbind,d)
}

#assigning the credit card dataset to card in the appropriate format by calling above two functions
card <- mkFrameList(6, 0)

#removing customers who has negative bill amount less than 10% of credit limit
#we are doing this to make our dataset consistent
count <- 0
#for storing outlier ids
outlier.ids <- c()

#running through the loop, for finding the customer with expectional bill amount in negative
#and later storing it in outlier.ids
for(i in 1:nrow(card)) {
  card$credit.limit.cuttoff <- 0.10 * card$credit_limit[i]
  if(card$bill_amt[i] < 0 && (abs(card$bill_amt[i]) > card$credit.limit.cuttoff)) {
    count = count + 1;
    outlier.ids <- c(outlier.ids,card$id[i])
  }
}

#View(as.data.frame(outlier.ids))

#removing those outlier.ids from the card dataset
card <- card[!card$id %in% (outlier.ids),]

#there are rows with education =0, 5 and 6 but their description is not given
#we are replacing it with 4 (which represent others), given in the education description
card[which(card$education == 0),"education"] <- 4
card[which(card$education == 5),"education"] <- 4
card[which(card$education == 6),"education"] <- 4

View(summary(card))

#replace repayment_status "-2" with "-1", since both represent that person is paying before or on time
#replace repayment_status "> 2" with "3", since both represent that person is paying late
card[card$repayment_status == -2,]$repayment_status <- -1
card[card$repayment_status > 2,]$repayment_status <- 3

#traversal of row group by id to compute the column minimum due and delay in payment
library(dplyr)
x <- card %>% group_by(id) %>% count(id, repayment_status)
#View(x)

#calculating minimum due, one month delay, second month delay and more than second month delay column
min_due <- x[x$repayment_status == 0,]
one.month.due <- x[x$repayment_status == 1,]
two.month.due <- x[x$repayment_status == 2,]
three.month.due <- x[x$repayment_status == 3,]

#splitting card dataset into dataset which don't have sept
card.not.sept <- card[card$month != as.Date("2005-09-06"),]
#ordering the dataset based on date
card.not.sept <- card.not.sept[order(card.not.sept$id),]

#splitting card dataset into dataset which has sept
card.sept <- card[card$month == as.Date("2005-09-06"),]
#ordering the dataset based on date
card.sept <- card.sept[order(card.sept$id),]

#calculating the total bill amount and order it based on date
total.bill.amt <- aggregate(card.not.sept$bill_amt, by=list(card.not.sept$id), sum, na.rm = TRUE)
total.bill.amt <- total.bill.amt[order(total.bill.amt$Group.1),]

#calculating the total amount paid and order it based on date
total.amount.paid <- aggregate(card.not.sept$amout_paid, by=list(card.not.sept$id), sum, na.rm = TRUE)
total.amount.paid <- total.amount.paid[order(total.amount.paid$Group.1),]

#assigning the calcualted total bil amount and amount paid to the card.sept dataset
card.sept$bill_amt <- total.bill.amt$x
card.sept$amout_paid <- total.amount.paid$x

#adding the minimum.due column to the main dataset
card.sept <- left_join(card.sept, min_due, by = c("id"="id"))
names(card.sept)[names(card.sept) == 'n'] <- 'count.min.due'
card.sept <- card.sept[ , !(names(card.sept) %in% c("repayment_status.y"))]
#replace na with zero's
card.sept[is.na(card.sept$count.min.due),"count.min.due"] = 0

#adding the one.month.due column to the main dataset
card.sept <- left_join(card.sept, one.month.due, by = c("id"="id"))
names(card.sept)[names(card.sept) == 'n'] <- 'count.one.month.due'
card.sept <- card.sept[ , !(names(card.sept) %in% c("repayment_status"))]
#replace na with zero's
card.sept[is.na(card.sept$count.one.month.due),"count.one.month.due"] = 0

#adding the two.month.due column to the main dataset
card.sept <- left_join(card.sept, two.month.due, by = c("id"="id"))
names(card.sept)[names(card.sept) == 'n'] <- 'count.two.month.due'
card.sept <- card.sept[ , !(names(card.sept) %in% c("repayment_status"))]
#replace na with zero's
card.sept[is.na(card.sept$count.two.month.due),"count.two.month.due"] = 0

#adding the three.month.due column to the main dataset
card.sept <- left_join(card.sept, three.month.dude, by = c("id"="id"))
names(card.sept)[names(card.sept) == 'n'] <- 'count.three.month.due'
card.sept <- card.sept[ , !(names(card.sept) %in% c("month","repayment_status"))]
#replace na with zero's
card.sept[is.na(card.sept$count.three.month.due),"count.three.month.due"] = 0

#View the dataset after cleaning
#View(card.sept)

#computing the new status of the repayment for biasing the model
repayment.status <- function(x) {
  
  #if the repayment.status contains 0, there is a penalty of 2 for each minimum due
  #if the repayment.status contains 1, there is a penalty of 3 for each time he delay the payment by one month
  #if the repayment.status contains 2, there is a penalty of 4 for each time he delay the payment by two month
  #if the repayment.status contains > 2, there is a penalty of 5 for each time he delay the payment by more than two month
  return  ((2 * card.sept$count.min.due) + (3 * card.sept$count.one.month.due) +
            (4 * card.sept$count.two.month.due) + (5 * card.sept$count.three.month.due))
   
   #return ((card.sept$count.min.due ^ card.sept$count.min.due) + (card.sept$count.one.month.due ^ card.sept$count.one.month.due) +
          #(card.sept$count.two.month.due ^ card.sept$count.two.month.due) + (card.sept$count.three.month.due ^ card.sept$count.three.month.due))
}

#applying the repayment.status function for each row in the card dataset
status <- lapply(seq_along(x), repayment.status)
names(status) <- c("a", "b", "c")

#binding the status column with the card dataset
#it will act as a new repyament status or a factor which will be useful in fitting the model
card.sept <- cbind(card.sept, status = status$a)

#normalize function, to normalize any column
normalize <- function(x) {
  
  return ((x - 0)/(3 - 0)) }
  #return ((x - min(card.sept$status)) / (max(card.sept$status) - min(card.sept$status))) }

#applying normalize function on every status column of card dataset
repayment.status <- lapply(card.sept$status, normalize)
card.sept$status <- repayment.status

#converting character to numeric for status column
card.sept$status <- as.numeric(card.sept$status)
#View(card.sept)

#drop month, repayment status and credit.limit.cutoff and other columns which are presented in the vector nelow
drops <- c("repayment_status.x", "month", "count.min.due", "count.one.month.due", "count.two.month.due", "count.three.month.due", "credit.limit.cuttoff")
card.sept <- card.sept[ , !(names(card.sept) %in% drops)]

#assigning the card.sept back to the card dataset
card <- card.sept

#View(credit_card[card$id == 452,])
#View(card[card$id == 452,])

#summary of card dataset
#reformatted credit card dataset
#View(summary(card))
#View(card)

#write file to csv
write.csv(card, file="/Users/hpanjwani/R Workspace/Mid Term/Problem 1/Output/credit.csv", row.names = F)

#reading the credit card dataset to the CreditData, prepared in the "Credit Wrangling.R" file
CreditData <- card
nrow(CreditData)

#75% of the sample size
class_smp_size <- floor(0.75 * nrow(CreditData))

#Set the seed to make your partition reproductible
set.seed(1)
train_logistic <- sample(seq_len(nrow(CreditData)), size = class_smp_size)

#Split the data into training and testing
train <- CreditData[train_logistic, ]
test <- CreditData[-train_logistic,]

#### 1. Logistic Regression##################
######## START OF LOGISTIC REGRESSION #######
#A. Logistic Regression steps

#fitting the logistic regression model, based on the important parameters
#credit_limit, sex, education, marriage, age, bill amount, amount paid and status are important parameters amongst all others
modelLogit <- glm(default_payment ~ credit_limit + education + age + sex + marriage + bill_amt + amout_paid + status, family=binomial(link='logit'), data=train)

#summary of the logistic regression
summary(modelLogit)

#analyzing the table of deviance
anova(modelLogit, test="Chisq")

#r2 in logit, we see McFadden which is similar to R2. Closer to zero implies model is good
#install.packages("pscl")
library(pscl)
pR2(modelLogit)

# Plot the performance of the model applied to the evaluation set as an ROC curve.
#install.packages("ROCR")
library(ROCR)
detach("package:neuralnet", unload=TRUE)
#prediction of the test data by the logistic model
predict.Logit.Credit <- predict(modelLogit, newdata=test, type="response")
pr <- prediction(predict.Logit.Credit, test$default_payment)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")

#plotting ROC Curve
plot(prf, main="ROC curve", colorize=T)

#plot the lift curve
library(caret)
test$prob <- predict.Logit.Credit
test$prob <- sort(test$prob, decreasing = T)
lift.logit.model <- lift( factor(default_payment) ~ prob, data = test)
lift.logit.model
xyplot(lift.logit.model, plot = "gain")

#Validation of Predicted Values, Confusion Matrix
library(e1071)
#Create predicted target value as 1 or 0 based on threshold of 0.3
pred.resp.level  <- ifelse(predict.Logit.Credit >0.3,1,0)
#confusion matrix for logistic regression
#sensitivity = 0.47
#specificity = 0.87
#accuracy = 0.79
confusionMatrix(data=factor(pred.resp.level),reference=factor(test$default_payment),positive='1')

#computing the overall error - 0.21
credit.logistic.error <- 1- sum(pred.resp.level==test$default_payment)/length(test$default_payment)
credit.logistic.error

#plotting the regression
plot(modelLogit, uniform=TRUE, main="Logistic Regression for credit")

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
#training the decision tree based on train dataset
credit.tree = tree(default_payment ~ credit_limit + sex + education + marriage + age + bill_amt + amout_paid + status, data = train, method = "class")

#summary of the decision tree
summary (credit.tree)

#display the tree structure and node levels of the tree
plot (credit.tree)
text (credit.tree, pretty = 0)

#calling cross-validation to choose tree complexity
cv.credit = cv.tree (credit.tree, FUN = prune.tree)
plot (cv.credit$size, cv.credit$dev, type='b')

#predict the 'default_payment' and then based on threshold classify the values as defaulter or non-defaulter
dec.prob = predict(credit.tree, test)
dec.pred = ifelse(dec.prob > 0.4, 1, 0)

#pruning the credit.tree based on the cross-validation
prune.credit.dec = prune.tree(credit.tree, best = 4)

#plot the prune tree
plot(prune.credit.dec)
text(prune.credit.dec, pretty = 0)

#calling cross-validation to choose tree complexity, to further optimize
#but after observation it seems, further optimization is going to impact the mode in negative way
cv.prune.credit = cv.tree (prune.credit.dec)
plot (cv.prune.credit$size, cv.prune.credit$dev, type='b')

prune.dec.probs = predict (prune.credit.dec, newdata =test)
prune.dec.pred = ifelse(prune.dec.probs > 0.2, 1, 0)

#Validation of Predicted Values, Confusion Matrix
#Create predicted target value as 1 or 0
library(e1071)
library(caret)
#confusion Matrix for decision tree
#sensitivity = 0.67
#specificity = 0.70
#accuracy = 0.69
confusionMatrix(data=factor(prune.dec.pred),reference=factor(test$default_payment), positive='1')

#computing the overall error - 0.31
credit.dec.error <- 1- sum(prune.dec.pred==test$default_payment)/length(test$default_payment)
credit.dec.error

#performance evaluation of the model
#ploting the rocr curve
library(ROCR)
roc_pred <- prediction(prune.dec.pred, as.numeric(test$default_payment))
credit.dec.model <- performance(roc_pred, measure="tpr", x.measure="fpr")
plot(credit.dec.model, colorize=TRUE)

#plot the lift curve
library(caret)
test$pred <- prune.dec.pred
test$pred <- sort(test$pred, decreasing = T)
lift.dec.model <- lift(factor(default_payment) ~ pred, data = test)
lift.dec.model
xyplot(lift.dec.model, plot = "gain")

######## END OF DECISION TREES #######
#####################################

#### 3. Neural Network  ##########
######## START OF NEURAL NETWORK #######
#C. Neural Network for classification
#assigning the card dataset to datannet, since we need to normalize in
#neural network we are assiging to different dataset
datannet <- card

#normalizing the data in the datannet, so that we can implement nnet
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

#normalizing the datannet
datannet_n <- as.data.frame(lapply(datannet, normalize))
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

#Sampling of the dataset
#75% of the sample size
class_new1 <- floor(0.75 * nrow(datannet_n))

#set the seed to make your partition reproductible
set.seed(13)
train_datannet <- sample(seq_len(nrow(datannet_n)), size = class_new1)

#split the data into training and testing
traindatannet <- datannet_n[train_datannet, ]
testdatannet  <- datannet_n[-train_datannet,]
#testdatannet <- datannet_n[-train_datannet,c('default_payment','status') ]

#applying the neural net algorithm
#install.packages("NeuralNetTools")
library(NeuralNetTools)
#training the neural network with size = 10 and 1 hidden layers
fitnn <- nnet(default_payment ~ credit_limit + sex + marriage + bill_amt + amout_paid+ status, traindatannet, size=10, 
              maxit = 90, entropy = TRUE, softmax = FALSE,censored = FALSE, skip = FALSE, 
              rang = 0.7, Hess = FALSE, trace = TRUE, MaxNWts = 1000, abstol = 1.0e-4, 
              decay = 15e-4, reltol = 1.0e-8, hidden = 1,threshold = 0.01,act.fct="tanh")
fitnn
#summary of the neural net
summary(fitnn)

#plotting the neural net, to visualize the nodes
library(scales)
library(reshape)
plot.nnet(fitnn, pos.col='darkgreen',neg.col='darkblue', alpha.val=0.7, rel.rsc=10, circle.cex=5, cex=1.4,circle.col='brown')

#predicting the default_payment using nnet model
predict.neural.Credit <- predict(fitnn, newdata=testdatannet)
#View(predict.neural.Credit)
#converting the prediction to vector
predict.neural.Credit <- as.vector(predict.neural.Credit)

#changing matrix to numeric
#Create predicted target value as 1 or 0 based on threshold of 0.2
pred.resp.level.nnet1  <- ifelse(predict.neural.Credit > 0.2, 1, 0)
#View(predict.neural.Credit)

#Validation of Predicted Values, Confusion Matrix
#Create predicted target value as 1 or 0
library(e1071)
library(caret)
#confusion Matrix for decision tree
#sensitivity = 0.65
#specificity = 0.74
#accuracy = 0.73
pred.resp.nnet.factor <- factor(pred.resp.level.nnet1)
confusionMatrix(data=pred.resp.nnet.factor,reference=factor(testdatannet$default_payment), positive='1')

#computing the overall error - 0.32
credit.neural.error <- 1- sum(pred.resp.nnet.factor==testdatannet$default_payment)/length(testdatannet$default_payment)
credit.neural.error

#plot the ROC curve
#install.packages("ROCR")
library(ROCR)
pred.resp.nnet.num <- as.numeric(pred.resp.level.nnet1)
roc_pred.nnnet <- prediction(pred.resp.nnet.num, testdatannet["default_payment"])
#roc_pred.nnnet
perf.nnet.model <- performance(roc_pred.nnnet, measure="tpr", x.measure="fpr")
plot(perf.nnet.model, colorize=TRUE)

#plot the lift curve
testdatannet$pred <- predict.neural.Credit
testdatannet$pred <- sort(testdatannet$pred, decreasing = T)
lift <- lift( factor(default_payment) ~ pred, data = testdatannet)
lift
xyplot(lift, plot = "gain")

#Sensitivity/specificity curve and precision/recall curve:
plot(performance(roc_pred.nnnet, measure="sens", x.measure="spec"), colorize=TRUE)
plot(performance(roc_pred.nnnet, measure="prec", x.measure="rec"), colorize=TRUE)

##################### END OF NEURAL NETWORK ####################
################################################################

#writing prediction performance metrics to csv files
filename = "/Users/hpanjwani/R Workspace/Mid Term/Problem 1/PredictionPerformanceMetrics_Problem1.csv"
account <- c("Defaulter of credit card")
write.table(account, file=filename, row.names = F, col.names = F, qmethod = "double")

logist <- confusionMatrix(data=factor(pred.resp.level),reference=factor(test$default_payment),positive='1')

#writing the confusion matrix of logistic regression to the file
write.table("\n", file=filename, append = T, row.names = F, col.names = F)
write.table("Confusion Matrix for Logistic Regression", file=filename, append = T, row.names = F, col.names = F)
write.table(logist$table, file=filename, append = T, col.names = F, qmethod = "double")
write.table("Confusion Matrix Accuracy", file=filename, append = T, row.names = F, col.names = F)
write.table(logist$overall, file=filename, append = T, col.names = F, qmethod = "double")

#writing the overall error of logistic error to the file
write.table("\n", file=filename, append = T, row.names = F, col.names = F)
write.table("Overall Error of Logistic Regression", file=filename, append = T, row.names = F, col.names = F)
write.table(credit.logistic.error, file=filename, append = T, col.names = F, qmethod = "double")

dec <- confusionMatrix(data=factor(prune.dec.pred),reference=factor(test$default_payment), positive='1')

#writing the confusion matrix of decision tree to the file
write.table("\n", file=filename, append = T, row.names = F, col.names = F)
write.table("Confusion Matrix for Decision Tree", file=filename, append = T, row.names = F, col.names = F)
write.table(dec$table, file=filename, append = T, row.names = F, col.names = F, qmethod = "double")
write.table("Confusion Matrix Accuracy", file=filename, append = T, row.names = F, col.names = F)
write.table(dec$overall, file=filename, append = T, row.names = F, col.names = F, qmethod = "double")

#writing the overall error of decision tree to the file
write.table("\n", file=filename, append = T, row.names = F, col.names = F)
write.table("Overall Error of Decision Tree", file=filename, append = T, row.names = F, col.names = F)
write.table(credit.dec.error, file=filename, append = T, row.names = F, col.names = F, qmethod = "double")

neural <- confusionMatrix(data=pred.resp.nnet.factor,reference=factor(testdatannet$default_payment), positive='1')

#writing the confusion matrix of neural net to the file
write.table("\n", file=filename, append = T, row.names = F, col.names = F)
write.table("Confusion Matrix for Neural Network", file=filename, append = T, row.names = F, col.names = F)
write.table(neural$table, file=filename, append = T, row.names = F, col.names = F, qmethod = "double")
write.table("Confusion Matrix Accuracy", file=filename, append = T, row.names = F, col.names = F)
write.table(neural$overall, file=filename, append = T, row.names = F, col.names = F, qmethod = "double")

#writing the overall error of neural net to the file
write.table("\n", file=filename, append = T, row.names = F, col.names = F)
write.table("Overall Error of Neural Net", file=filename, append = T, row.names = F, col.names = F)
write.table(credit.neural.error, file=filename, append = T, row.names = F, col.names = F, qmethod = "double")