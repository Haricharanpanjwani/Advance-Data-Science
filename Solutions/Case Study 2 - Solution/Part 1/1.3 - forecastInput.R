setwd("/Users/Prateek/Documents/Summer 16/TA/Summer 16/Assignment/Assignment 2/Assignment2/Part 1/Data/ForecastNewData")

library(readxl)
forecast_DS <- read_excel("forecastNewData.xlsx")
head(forecast_DS)

colnames(forecast_DS)=c("Date","hour","Temperature")

actual_date <- forecast_DS$Date

forecast_DS$Date <- as.Date(as.character(forecast_DS$Date,"%Y%m%d"),"%Y%m%d")
forecast_DS$Date <- as.Date(forecast_DS$Date, format = "%m%d%Y")
forecast_DS$month = format(forecast_DS$Date, format = "%m")
forecast_DS$day = format(forecast_DS$Date, format = "%d")
forecast_DS$year = format(forecast_DS$Date, format = "%Y")

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

write.csv(forecast_DS, "forecastInput.csv", row.names=FALSE)

###################################################################################################

#NEURAL Network

forecast_DS <- read.csv("forecastInput.csv", header = TRUE)
head(forecast_DS)

#To convert columns type into Numeric values
columnNames <- colnames(forecast_DS)
for (i in columnNames)
{forecast_DS[[i]] <- as.numeric(forecast_DS[[i]])
}


#Scaling the inputs of neural network
#Scaled values are preferred over normalized Values
max <- apply(forecast_DS, 2, max)
min <- apply(forecast_DS, 2, min)
scaled <- as.data.frame(scale(forecast_DS, center = min, scale = max - min))


#Prediction of test 
scaled <- scaled[c("Temperature", "PeakHour", "Weekday", "hour")]
net.results <- compute(nn, scaled)


#descaling the data
deScale <- function(x) {
  return ((x * (max(hourly_Data$kWh) - min(hourly_Data$kWh))) + min(hourly_Data$kWh))
}

results <- as.data.frame(sapply(net.results$net.result, deScale))


forecastOutput <- cbind(forecast_DS, results)
colnames(forecastOutput)[10] <- "kWh"
head(forecastOutput)
write.csv(forecastOutput, "forecastOutput_999999999_NeuralNetworks.csv", row.names = FALSE)

###################################################################################################

#Regression Tree

kWh = predict (tree_pruned_Model, forecast_DS)
  
forecastOutput <- cbind(forecast_DS, kWh)
head(forecastOutput)
write.csv(forecastOutput, "forecastOutput_999999999_regressionTree.csv", row.names = FALSE)

