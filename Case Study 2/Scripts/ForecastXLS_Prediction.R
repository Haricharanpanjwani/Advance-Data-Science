#forecast
#prepare the input file
library(gdata)
forecast <- read.xls("/Users/hpanjwani/R Workspace/Assignment 2/Problem Statement/forecastNewData.xlsx")
forecastInput <- forecast
colnames(forecast) <- c("Day", "Hour", "Temp");

#Rearranging the column
library(lubridate)
#forecast$Date <- as.Date(forecast$Day, "%Y%m%d")
forecast$Date <- as.Date(strptime(forecast$Day, "%Y%m%d"))
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

#writing input file to csv
write.csv(forecast, file="/Users/hpanjwani/R Workspace/Assignment 2/Output/Forecast XLS/forecastInput.csv")

#forecast for neural network
#predict the power usage in KWH for each Hour.
library(forecast)
library(neuralnet)
pred = compute(net.sqrt, forecast[,-c(3,6)])
#accuracy(pred, test$kWh)
KWH <- pred$net.result
#View(pred$net.result)

#changing matrix to numeric
KWH <- as.numeric(KWH)

#denormalized the data
denormalize <- function(x) {
  return ((x * (max(neural_dataset$kWh) - min(neural_dataset$kWh))) + min(neural_dataset$kWh))
}

kWHList <- as.data.frame(sapply(KWH, denormalize))

forecastOutput <- cbind(forecastInput,kWHList)
colnames(forecastOutput) <- c("Day", "Hour", "Temp", "kWh");
#View(forecastOutput)

#writing the data to forecastData.csv
filename <- paste("/Users/hpanjwani/R Workspace/Assignment 2/Output/Forecast XLS/forecastOutput_Account_",unique(neural_dataset$Account), "_neuralNetwork.csv", sep = "")
filename;
write.csv(forecastOutput, file=filename)

#############################
#forecast for regression tree
#predict the power usage in KWH for each Hour

#changing forecast$Hour to forecast$hour
forecast$hour <- as.numeric(forecast$Hour)
drops <- c("Hour");
forecast <- forecast[ , !(names(forecast) %in% drops)]

library(forecast)
pred_Reg = predict(tree.best_dataset, newdata = forecast)

#binding the output with input
forecastOutput <- cbind(forecastInput,pred_Reg)
colnames(forecastOutput) <- c("Day", "Hour", "Temp", "kWh");
#View(forecastOutput)

#writing the data to forecastData.csv
filename <- paste("/Users/hpanjwani/R Workspace/Assignment 2/Output/Forecast XLS/forecastOutput_Account_",unique(best_dataset$Account), "_regressionTree.csv", sep = "")
filename;
write.csv(forecastOutput, file=filename)