#Data Wrangling of cleanning Dataset 1 - Energy
energy <- read.csv("/Users/hpanjwani/Documents/NEU/3rd Sem/ADS/Assignment 1/Part 2/NewData.csv")
#View(energy)

df <- NULL
kWHList <- sapply (energy$Units, function(x) x == 'kWh')
df <- energy[kWHList,]

nrow(df)
#View(df)

#Transforming and changing
x <- df
#View(x)
trans <- t(x[4:292])
#View(trans)

#install.packages("reshape")
library("reshape")
md <- melt(df, id=(c("Account", "Date", "Channel", "Units")))
md <- md[order(md$Date),]
View(md)

#105120
#8760
#hourlySum$kWh <- aggregate(md$value, by=list(), sum, na.rm = FALSE)

k <- 1
hour <- 0
hourlySum <- NULL
modified <- NULL

for(i in seq(from=1, to=nrow(md), by=12)) {
  
  j <- i+11;
  
  if(hour == 24)
    hour = 0;
  
  if(hour > 6 && hour < 20)
    hourlySum$Peakhour[k] <- 1
  else
    hourlySum$Peakhour[k] <- 0
  
  hourlySum$Hour[k] <- hour;
  hourlySum$kWh[k] <- sum(md$value[i:j])
  #hold the requried rows and delete the rest
  modified <- rbind(modified, md[i,])
  
  hour <- hour + 1;
  k <- k + 1;
}

#View(hourlySum)
#View(modified)

#Consolidating the dataset hourly data with other fields
energy <- cbind(modified, hourlySum)
#View(energy)

#splitting the date
library(lubridate)
energy$Date <- as.Date(energy$Date, "%m/%d/%Y")

energy$month <- month(energy$Date)
energy$day <- day(energy$Date)
energy$year <- year(energy$Date)

energy["Day of Week"] <- wday(energy$Date) - 1;

#Weekday
#install.packages("timeDate")
library("timeDate")
weekdayList <- sapply(energy$Date, function(x) {
  if(isWeekday(x, wday=1:5)) {
    energy["Weekday"] <- 1
  } else  {
    energy["Weekday"] <- 0 
  }
})
energy$Weekday <- weekdayList
#View(energy$Weekday)

#energy["Weekday"] <- NULL
#energy["Weekday"] <- factor(isWeekday(energy$Date, wday = 1:5), labels = c("1", "0"), levels = c(TRUE, FALSE))

#Removing unneccesary column
energy <- energy[ , -which(names(energy) %in% c("Channel","Units", "variable", "value"))]

#Finding and replacing NA values with Zero from kWh
for(i in 1:nrow(energy)) {
  a <- is.na(energy$kWh)
  
  if(is.na(energy$kWh[i]))
    energy$kWh[i] <- energy$kWh[i-1];
}

#Detecting the outliers in kWH
for(i in 1:nrow(energy)) {
  if((energy$kWh[i]) < (mean(energy$kWh) - (1.5)*sd(energy$kWh)) &&
     (energy$kWh[i]) > (mean(energy$kWh) + (1.5)*sd(energy$kWh))) {
    
    b <- energy[i,]
    #energy$kWh[i] <- mean(energy$kWh);
    energy$kWh[i] <- energy$kWh[i-1];
  }
}

#finding the minimum but greater tha zero
minkWh <- energy[energy$kWh > min(energy$kWh),]
minkWh <- min(minkWh$kWh)

#Log Transformation for dealing with thousands of zero's
energy$kWh <- sapply(energy$kWh, function(x) {if(x == 0) { x <-log1p(minkWh) } else { x <- x}})

#Display energy dataset
View(energy)
View(summary(energy))

#write clean energy dataset to csv file
#write.csv(energy, file="Output/energy.csv")


##Dataset 2 - Temperature (Pulling from weatherData)
#install.packages("weatherData")
#remove.packages("weatherData")
#install.packages("devtools")
#install_github("ozagordi/weatherData")
library("devtools")
library(weatherData)
getStationCode("Boston")

tempData <- getWeatherForDate("KBOS", start_date=min(energy$Date),
                              end_date = max(energy$Date),
                              opt_detailed=T,
                              opt_custom_columns=T, custom_columns=c(2))

#View(tempData)
temperature <- tempData
#View(temperature)

tempData <- NULL
tempData <- temperature

library("lubridate")
tempData$Date <- NULL
tempData$hour <- NULL
tempData$Date <- as.Date(tempData$Time, "%Y-%m-%d", tz = "EST")
tempData$hour <- hour(tempData$Time)
tempData <- tempData[order(tempData$Date),]
#View(tempData)

aggdata <- aggregate(tempData$TemperatureF, by=list(tempData$Date, tempData$hour), mean, na.rm = FALSE)
aggdata <- aggdata[order(aggdata$Group.1),]

#displaying the dataset
#View(aggdata)
#View(summary(aggdata))

#removing the outlier
#10th October 2014, 4th Hour
aggdata[which(aggdata$x < 0),]
aggdata <- aggdata[-which(aggdata$x < 0),]

#aggdata <- reviseTemp

View(summary(aggdata))

#Finding and replacing NA values with Zero from kWh
#naList <- sapply (aggdata$x, function(x) is.na(x))
#aggdata[naList,]

for(i in 1:nrow(aggdata)) {
  a <- is.na(aggdata$x)
  
  if(is.na(aggdata$x[i]))
    aggdata$x[i] <- aggdata$x[i-1];
}

#Detecting the outliers in Temperature
for(i in 1:nrow(aggdata)) {
  if((aggdata$x[i]) < (mean(aggdata$x) - (1.5)*sd(aggdata$x)) &&
     (aggdata$x[i]) > (mean(aggdata$x) + (1.5)*sd(aggdata$x))) {
    b <- aggdata[i,]
    aggdata$x[i] <- aggdata$x[i-1];
  }
}

#renaming the columns to the desired value
colnames(aggdata) <- c("Date", "Hour", "Temp");

#View(aggdata)

#write clean temp to csv file
#write.csv(aggdata, file="Output/temperature.csv")


##Merging of Energy and Temperature
library(dplyr)
consolidate <- left_join(energy,aggdata, by = c("Date"="Date", "Hour"= "Hour"))

#plot(consolidate$kWh, consolidate$Temp)

## Working on energy for outlier
#View(energy)

#Outlier detected for 9th March, 2nd Hour
#energy[which(energy$kWh == ''),]

#Finding and replacing NA values with Zero from kWh
for(i in 1:nrow(consolidate)) {
  #a <- is.na(consolidate$kWh)
  if(is.na(consolidate$kWh[i]))
    consolidate$kWh[i] <- consolidate$kWh[i-1]
  
  if(is.na(consolidate$Temp[i])) {
    #naMonth <- month(consolidate$Date[i])
    #consolidate %>% group_by(month) %>% filter(month == naMonth) %>% filter(kWh > 350)
    #consolidate$Temp[i] <- mean(consolidate$Temp, na.rm = TRUE)
    consolidate$Temp[i] <- consolidate$Temp[i-1];
  }
}

#Detecting the outliers in kWH
for(i in 1:nrow(consolidate)) {
  if((consolidate$kWh[i]) < (mean(consolidate$kWh) - (1.5)*sd(consolidate$kWh)) &&
     (consolidate$kWh[i]) > (mean(consolidate$kWh) + (1.5)*sd(consolidate$kWh))) {
    #a <- consolidate[i,]
    #consolidate[i,5] <- mean(consolidate$kWh);
    consolidate$kWh[i] <- consolidate$kWh[i-1];
  }
}

#Detecting the outliers in Temperature
for(i in 1:nrow(consolidate)) {
  if((consolidate$Temp[i]) < (mean(consolidate$Temp) - (1.5)*sd(consolidate$Temp)) &&
     (consolidate$Temp[i]) > (mean(consolidate$Temp) + (1.5)*sd(consolidate$Temp))) {
    #b <- consolidate[i,]
    #consolidate[i,11] <- mean(consolidate$Temp);
    consolidate$Temp[i] <- consolidate$Temp[i-1];
  }
}

#Displaying the summary and dataset
View(consolidate)
View(summary(consolidate))

#Writing it to csv
write.csv(consolidate, file = "Output/merge.csv")
