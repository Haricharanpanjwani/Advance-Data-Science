setwd("/Users/Prateek/Documents/Summer 16/TA/Summer 16/Assignment/Assignment 2/Assignment2/Part 1/Data/ForecastNewData2")

forecast_DS <- read.csv("forecastNewData2.csv")

head(forecast_DS)

library(data.table)
setnames(forecast_DS, old = c('Day','Hour','Temp'), new = c('Date','hour', 'Temperature'))
head(forecast_DS)

actual_date <- forecast_DS$Date

forecast_DS$Date <- as.Date(forecast_DS$Date, format ="%m/%d/%Y")


require(lubridate)
forecast_DS$day <- day(forecast_DS$Date)
forecast_DS$month <- month(forecast_DS$Date)
forecast_DS$year <- year(forecast_DS$Date)
forecast_DS$DayOfWeek <- wday(forecast_DS$Date) - 1


library(chron)
WeekdayOrWeekend <- is.weekend(forecast_DS$Date)
forecast_DS$Weekday <- ifelse(WeekdayOrWeekend == "TRUE", 0, 
                                     ifelse(WeekdayOrWeekend == "FALSE", 1, "NA"))


forecast_DS$PeakHour <- ifelse(as.numeric(forecast_DS$hour) >= 6 &  as.numeric(forecast_DS$hour) <= 19 , 1 , 0)

forecast_DS$Date <- actual_date

forecast_DS <- forecast_DS[c("Date", "month", "day", "year", "hour", "DayOfWeek", "Weekday", "PeakHour", "Temperature")]
forecast_DS$Weekday <- as.numeric(forecast_DS$Weekday)

head(forecast_DS)
dim(forecast_DS)

write.csv(forecast_DS, "forecastInput2.csv", row.names=FALSE)

###################################################################################################

#NEURAL Network

forecast_DS2 <- read.csv("forecastInput2.csv", header = TRUE)
dim(forecast_DS2)

#To convert columns type into Numeric values
columnNames <- colnames(forecast_DS2)
for (i in columnNames)
{forecast_DS2[[i]] <- as.numeric(forecast_DS2[[i]])
}


#Scaling the inputs of neural network
#Scaled values are preferred over normalized Values
max <- apply(forecast_DS2, 2, max)
min <- apply(forecast_DS2, 2, min)
scaledd <- as.data.frame(scale(forecast_DS2, center = min, scale = max - min))


#Prediction of test 
scaledd <- scaledd[c("Temperature", "PeakHour", "Weekday", "hour")]
net.resultss <- compute(nn, scaledd)


#descaling the data
deScale <- function(x) {
  return ((x * (max(hourly_Data$kWh) - min(hourly_Data$kWh))) + min(hourly_Data$kWh))
}
resultss <- as.data.frame(sapply(net.resultss$net.result, deScale))

length(resultss)
forecastOutput <- cbind(forecast_DS, resultss)
colnames(forecastOutput)[10] <- "kWh"
head(forecastOutput)
write.csv(forecastOutput, "forecastOutput_999999999_NeuralNetworks.csv", row.names = FALSE)

###################################################################################################

#Regression Tree

kWh = predict (tree_pruned_Model, forecast_DS2)

forecastOutput <- cbind(forecast_DS2, kWh)
write.csv(forecastOutput, "forecastOutput_999999999_regressionTree.csv", row.names = FALSE)


