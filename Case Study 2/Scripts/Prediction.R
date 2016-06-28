#install.packages("tree")
#install.packages("MASS")
#install.packages("ISLR")
#install.packages("mcclust")
#install.packages("readxl")
#install.packages("MASS")
#install.packages("grid")
#install.packages("neuralnet")
#install.packages("nnet")
#install.packages("clusterGeneration")

library (tree)
library (MASS)
library (ISLR)
library(mcclust)
library(readxl)
library (grid)
library (neuralnet)

#A. Regression Tree for prediction
#Reading the best "Filled Dataset"
best_dataset <- read.csv("/Users/hpanjwani/R Workspace/Assignment 2/Output/Hourly_filled_data.csv")
#View(best_dataset)

set.seed (1)
train = sample (1:nrow(best_dataset), nrow(best_dataset)/2)

tree.best_dataset = tree(formula = kWh ~   as.numeric(month) + as.numeric(day)  + as.numeric(Day.of.Week) + as.numeric(Weekday) + as.numeric(hour) + as.numeric(Peakhour) + as.numeric(Temp),best_dataset,subset=train)
summary (tree.best_dataset)
plot (tree.best_dataset)
text (tree.best_dataset, pretty = 0)
cv.boston = cv.tree (tree.best_dataset)
plot (cv.boston$size, cv.boston$dev, type='b')

yhat=predict(tree.best_dataset, newdata = best_dataset [-train,])
#View(as.data.frame(yhat))
newdata <- best_dataset [-train,"kWh"]
accuracy(yhat,newdata)

boston.test=best_dataset [-train,"kWh"]
plot(yhat,boston.test)
abline (0,1)
mean((yhat -boston.test)^2)

#not to be use without pruning best results are coming
#pruning of the tree
prune.boston =prune.tree(tree.best_dataset, best = 8)
plot(prune.boston)
text(prune.boston, pretty = 0)
cv.prune = cv.tree (prune.boston)
plot (cv.prune$size, cv.prune$dev, type='b')

yhat=predict (prune.boston, newdata =best_dataset [-train,])
boston.test=best_dataset [-train,"kWh"]
plot(yhat,boston.test)
abline (0,1)
mean((yhat -boston.test)^2)

#B. Neural Network for Prediction
neural_dataset <- read.csv("/Users/hpanjwani/R Workspace/Assignment 2/Output/Hourly_filled_data.csv")

#neural_dataset$Account <- as.numeric(neural_dataset$Account)
#neural_dataset$Date <- as.numeric(neural_dataset$Date)
#neural_dataset$year <- as.numeric(neural_dataset$year)
neural_dataset$month <- as.numeric(neural_dataset$month)
neural_dataset$day <- as.numeric(neural_dataset$day)
neural_dataset$Day.of.Week <- as.numeric(neural_dataset$Day.of.Week)
neural_dataset$Weekday <- as.numeric(neural_dataset$Weekday)
neural_dataset$hour <- as.numeric(neural_dataset$hour)
neural_dataset$Peakhour <- as.numeric(neural_dataset$Peakhour)
neural_dataset$Temp <- as.numeric(neural_dataset$Temp)
neural_dataset$kWh <- as.numeric(neural_dataset$kWh)
#View(neural_dataset)

drops <- c("Account","Date", "year")
dfnnet <- neural_dataset[ , !(names(neural_dataset) %in% drops)]
#View(dfnnet)

#Sampling the data
#normalizing the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

#selecting my new subset
datannet_n <- as.data.frame(lapply(dfnnet, normalize))
#View(datannet_n)

train <- sample(1:nrow(datannet_n),round(0.75*nrow(datannet_n)))
traindata <- datannet_n[train,]
testdata <- datannet_n[-train,]
View(testdata)

#training neural net for th model
net.sqrt <- neuralnet(kWh ~ Peakhour + Day.of.Week  + hour + Weekday + Temp + day + month, data=traindata, hidden=c(7,5,5), threshold=0.5, linear.output = F)

#print the error result
print(net.sqrt)

#Plot the neural network
plot(net.sqrt)

#Test the neural network on some training data
acutal <- testdata$kWh
drops <- c("kWh")
testdata <- testdata[ , !(names(testdata) %in% drops)]
net.results <- compute(net.sqrt, testdata) #Run them through the neural network
#View(net.results)

#accuracy of the algorithm
accuracy(net.sqrt, acutal)

#Lets see the results
print(net.results$net.result)

#changing matrix to numeric
net.results$net.result <- as.numeric(net.results$net.result)

#denormalized the data
denormalize <- function(x) {
  #return (((x - min(x)) / (max(x) - min(x)))*((max(x)-min(x))+min(x)))
  return ((x * (max(neural_dataset$kWh) - min(neural_dataset$kWh))) + min(neural_dataset$kWh))
}

results <- as.data.frame(sapply(net.results$net.result, denormalize))
#View(results)


#computing performance of the algorithm
results <- as.numeric(results$`sapply(net.results$net.result, denormalize)`)
error = (results - neural_dataset$kWh)

# Function that returns Root Mean Squared Error
rmse <- function(error) {
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)  {
  mean(abs(error))
}

# Function that returns Mean Absolute Percentage Error
mape <- function(error)  {
  mean(abs(error/neural_dataset$kWh) * 100)
}

#calculating mean square value
rmse(error)
rms <- c("RMS", rmse(error))
mae(error)
ma <- c("MAE", mae(error))
mape(error)
map <- c("MAPE", mape(error))

neural_performance <- NULL
neural_performance <- rbind(rms, ma, map, deparse.level = 0)
neural_performance

#writing prediction performance metrics to csv files
account <- c("Account No", unique(neural_dataset$Account))
write.table(t(account), file="/Users/hpanjwani/R Workspace/Assignment 2/Output/PredictionPerformanceMetrics.csv", row.names = F, col.names = F, qmethod = "double")

write.table("\n", file="/Users/hpanjwani/R Workspace/Assignment 2/Output/PredictionPerformanceMetrics.csv", append = T, row.names = F, col.names = F)
write.table("Regression Tree", file="/Users/hpanjwani/R Workspace/Assignment 2/Output/PredictionPerformanceMetrics.csv", append = T, row.names = F, col.names = F)

write.table(t(accuracy(yhat,newdata)), file="/Users/hpanjwani/R Workspace/Assignment 2/Output/PredictionPerformanceMetrics.csv", append = T, col.names = F, qmethod = "double")

write.table("\n", file="/Users/hpanjwani/R Workspace/Assignment 2/Output/PredictionPerformanceMetrics.csv", append = T, row.names = F, col.names = F)
write.table("Neural Network", file="/Users/hpanjwani/R Workspace/Assignment 2/Output/PredictionPerformanceMetrics.csv", append = T, row.names = F, col.names = F)

write.table(neural_performance, file="/Users/hpanjwani/R Workspace/Assignment 2/Output/PredictionPerformanceMetrics.csv", append = T, row.names = F, col.names = F, qmethod = "double")