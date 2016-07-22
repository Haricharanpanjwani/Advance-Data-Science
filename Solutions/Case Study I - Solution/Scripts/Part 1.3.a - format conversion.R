#forecast_DS <- read.csv(file.choose(),header = T)
forecast_DS <- read.csv("/Users/Prateek/Documents/Summer 16/TA/Summer 16/Assignment/Assignment 1/Part 2/forecastData.csv")

head(forecast_DS)

library(data.table)
setnames(forecast_DS, old = c('Day','Hr','Temp'), new = c('Date','hour', 'Temperature'))
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

write.csv(forecast_DS, "/Users/Prateek/Documents/Summer 16/TA/Summer 16/Assignment/Assignment 1/Part 2/Forecast.csv")


library(forecast)
summary(lm.fit)
pred <- predict(lm.fit, forecast_DS)
forecast_DS$kWh <- pred
dim(forecast_DS)

write.csv(forecast_DS, "/Users/Prateek/Documents/Summer 16/TA/Summer 16/Assignment/Assignment 1/Part 2/forecastOutput_26435791004.csv")


