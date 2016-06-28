#forecast
#prepare the input file
forecast <- read.csv("/Users/hpanjwani/R Workspace/Assignment 2/Problem Statement/forecastNewData2.csv");
forecastInput <- forecast
colnames(forecast) <- c("Day", "Hour", "Temp");

#Rearranging the column
library(lubridate)
forecast$Date <- as.Date(forecast$Day, "%m/%d/%Y")
forecast <- forecast[order(forecast$Date),]

forecast$month <- month(forecast$Date)
forecast$day <- day(forecast$Date)
forecast$year <- year(forecast$Date)

#finding day of week
#compute Day of week 
forecast["Day of Week"] <- wday(forecast$Date) - 1;

#Weekday
#install.packages("timeDate")
library("timeDate")
weekdayList <- sapply(forecast$Date, function(x) {
  if(isWeekday(x, wday=1:5)) {
    forecast["Weekday"] <- 1
  } else  {
    forecast["Weekday"] <- 0 
  }
})
forecast$Weekday <- weekdayList

#Evaluating the peak hour
#assigning the peak hour to the list
forecast$Peakhour <- 0
peakHourList <- sapply(forecast$Hour, function(x) {
  if(x > 6 && x < 20) { forecast$Peakhour <- 1  } else {  forecast$Peakhour <- 0  }
})
forecast$Peakhour <- peakHourList

#Removing unneccesary column
forecast <- forecast[ , -which(names(forecast) %in% c("Day"))]
#View(forecast)
str(forecast)
#writing input file to csv
write.csv(forecast, file="/Users/hpanjwani/R Workspace/Assignment 2/Output/Forecast CSV/forecastInput.csv")

forecast$hour <- as.numeric(forecast$Hour)
forecast$day <- as.numeric(forecast$day)
forecast$Date <- as.numeric(forecast$Date)

str(forecast)

drops <- c("Hour", "Day of Week", "year", "Date")
forecast <- forecast[ , !(names(forecast) %in% drops)]
#View(forecast)

#forecast Classification through neural network
library(nnet)
#library(neuralnet)
pred_classification = predict(fitnn, forecast)
View(pred_classification)

#changing matrix to numeric
KWH <- as.numeric(pred_classification)

forecastInput$KWH <- NULL
classList <- NULL
classList <- sapply(pred_classification, function(x) {
  if(x > 0.5) {forecastInput$KWH <- "Above_Normal"} 
  else { forecastInput$KWH <- "Optimal"} })

forecastInput$KWH <- classList
View(forecastInput)

#writing the data to forecastData.csv
filename <- paste("/Users/hpanjwani/R Workspace/Assignment 2/Output/Forecast CSV/forecastOutput_Account_",unique(neural_dataset$Account), "_neuralNetworkClassification.csv", sep = "")
filename;
write.csv(forecastInput, file=filename)

##############################################
#forecast for logistic regression tree
#classify the power usage in KWH for each Hour

#changing forecast$Hour to forecast$hour
#forecast$hour <- as.numeric(forecast$Hour)
drops <- c("Hour", "kWh");
forecast <- forecast[ , !(names(forecast) %in% drops)]

library(forecast)
pred_class = predict(modelLogit, newdata = forecast)
#View(as.data.frame(pred_class))

forecastInput$KWH <- NULL
classList <- NULL
classList <- sapply(pred_class, function(x) {
  if(x > mean(pred_class)) {forecastInput$KWH <- "Above_Normal"} 
  else { forecastInput$KWH <- "Optimal"} })

forecastInput$KWH <- classList

#writing the data to forecastData.csv
filename <- paste("/Users/hpanjwani/R Workspace/Assignment 2/Output/Forecast CSV/forecastOutput_Account_",unique(best_dataset$Account), "_regressionTreeClassification.csv", sep = "")
filename;
write.csv(forecastInput, file=filename)


##############################################
#forecast for decision tree
#classify the power usage in KWH for each Hour
#View(forecast)
library(forecast)
pred_decision = predict(pfit1, newdata = forecast)
pred_decision <- as.data.frame(pred_decision)
#View(pred_decision[1])

forecastInput$KWH <- NULL
classList <- NULL
classList <- sapply(pred_decision[,1], function(x) {
  if(x > 0.5) {forecastInput$KWH <- "Optimal"} 
  else { forecastInput$KWH <- "Above_Normal"} })

forecastInput$KWH <- classList

#writing the data to forecastData.csv
filename <- paste("/Users/hpanjwani/R Workspace/Assignment 2/Output/Forecast CSV/forecastOutput_Account_",unique(best_dataset$Account), "_classificationTree.csv", sep = "")
filename;
write.csv(forecastInput, file=filename)