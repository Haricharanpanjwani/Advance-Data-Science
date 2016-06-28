#Reading the Energy Dataset - newdata.csv
energy <- read.csv("/Users/hpanjwani/R Workspace/Assignment 2/Problem Statement/NewData.csv")

#Keeping the rows which has units in 'kWh' and discarding others
kWHList <- sapply (energy$Units, function(x) x == 'kWh')
energy <- energy[kWHList,]
nrow(energy)

#Transforming and changing
x <- energy
trans <- t(x[4:292])
#View(trans)

#install.packages("reshape")
library("reshape")
md <- melt(energy, id=(c("Account", "Date", "Channel", "Units")))
md <- md[order(md$Date),]

#assinging changed matrix to energy again
energy <- md

#splitting the date
#install.packages("lubridate")
library(lubridate)
energy$Date <- as.Date(energy$Date, "%m/%d/%Y")

energy$month <- month(energy$Date)
energy$day <- day(energy$Date)
energy$year <- year(energy$Date)

#compute Day of week 
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

#aggregating the rows to calculate the hourly sum
#install.packages("dplyr")
library(dplyr)
md <- subset(energy, grepl(".05", variable))
md$kWh <- colSums(matrix(energy$value, nrow=12), na.rm = TRUE)
md$hour <- 0
hourList <- sapply(md$variable, function(x) {
  if(x == 'X1.05'){ md$hour <- 1 }        else if(x == 'X2.05'){ md$hour <- 2 }
  else if(x == 'X3.05'){ md$hour <- 3 }   else if(x == 'X4.05'){ md$hour <- 4 }
  else if(x == 'X5.05'){ md$hour <- 5 }   else if(x == 'X6.05'){ md$hour <- 6 }
  else if(x == 'X7.05'){ md$hour <- 7 }   else if(x == 'X8.05'){ md$hour <- 8 }
  else if(x == 'X9.05'){ md$hour <- 9 }   else if(x == 'X10.05'){ md$hour <- 10 }
  else if(x == 'X11.05'){ md$hour <- 11 } else if(x == 'X12.05'){ md$hour <- 12 }
  else if(x == 'X13.05'){ md$hour <- 13 } else if(x == 'X14.05'){ md$hour <- 14 }
  else if(x == 'X15.05'){ md$hour <- 15 } else if(x == 'X16.05'){ md$hour <- 16 }
  else if(x == 'X17.05'){ md$hour <- 17 } else if(x == 'X18.05'){ md$hour <- 18 }
  else if(x == 'X19.05'){ md$hour <- 19 } else if(x == 'X20.05'){ md$hour <- 20 }
  else if(x == 'X21.05'){ md$hour <- 21 } else if(x == 'X22.05'){ md$hour <- 22 }
  else if(x == 'X23.05'){ md$hour <- 23 } else { md$hour <- 0 }
})

md$hour <- hourList

#assigning the peak hour to the list
md$Peakhour <- 0
peakHourList <- sapply(md$hour, function(x) {
  if(x > 6 && x < 20) { md$Peakhour <- 1  } else {  md$Peakhour <- 0  }
})
md$Peakhour <- peakHourList

#Removing unneccesary column
energy <- md
energy <- energy[ , -which(names(energy) %in% c("Channel","Units", "variable", "value"))]

#Splitting rows which has zero's power consumption(kWh)
zeroKWHEnergy <- energy[energy$kWh == 0,]
nonZeroKWHEnergy <- energy[energy$kWh != 0,]

#splitting nonZeroKWH dataset into peak hour and non peak hour
nonPeakNonZero <- nonZeroKWHEnergy[nonZeroKWHEnergy$Peakhour == 0,]
peakNonZero <- nonZeroKWHEnergy[nonZeroKWHEnergy$Peakhour == 1,]

#vector is a list of outliers
#26 outliers are detected
vector <- NULL
vector <- c()
for(i in 1:nrow(nonPeakNonZero)) {
  if((nonPeakNonZero$kWh[i]) < (mean(nonPeakNonZero$kWh) - (3)*sd(nonPeakNonZero$kWh)) ||
     (nonPeakNonZero$kWh[i]) > (mean(nonPeakNonZero$kWh) + (3)*sd(nonPeakNonZero$kWh))) {
    vector <- c(vector, i)
    #nonPeakNonZero$kWh[i] <- nonPeakNonZero$kWh[i-1];
  }
}

#finding the mean of the outlier
outlier_nonPeakNonZero <- nonPeakNonZero[-vector,]
mean(outlier_nonPeakNonZero$kWh)

#replacing the value of outliers with the mean
nonPeakNonZero[vector,]$kWh <- mean(outlier_nonPeakNonZero$kWh)
nonPeakNonZero[vector,]

#binding the non Peak hour and peak hour dataset to complete the nonZero KWH energy dataset
nonZeroKWHEnergy <- rbind(nonPeakNonZero, peakNonZero)

energy <- rbind(nonZeroKWHEnergy, zeroKWHEnergy)

#finding the minimum but greater tha zero
minkWh <- energy[energy$kWh > min(energy$kWh),]
minkWh <- min(minkWh$kWh)

#Log Transformation for dealing with thousands of zero's
energy$kWh <- sapply(energy$kWh, function(x) {if(x == 0) { x <-log1p(minkWh) } else { x <- x}}) 

#summary of the whole dataset
#View(summary(energy))
#View(energy)

write.csv(energy, file="/Users/hpanjwani/R Workspace/Assignment 2/Output/Part b/energy.csv")

#Working on temperature dataset
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

library("lubridate")
tempData$Date <- NULL
tempData$hour <- NULL
tempData$Date <- as.Date(tempData$Time, "%Y-%m-%d", tz = "EST")
tempData$hour <- hour(tempData$Time)
tempData <- tempData[order(tempData$Date),]
#View(tempData)

aggdata <- aggregate(tempData$TemperatureF, by=list(tempData$Date, tempData$hour), mean, na.rm = FALSE)
aggdata <- aggdata[order(aggdata$Group.1),]

#renaming the columns to the desired value
colnames(aggdata) <- c("Date", "Hour", "Temp");

#displaying the dataset
#View(summary(aggdata))

#removing the outlier
#17th October 2014, 4th Hour
#vector is a list of outliers
vector <- NULL
vector <- c()
for(i in 1:nrow(aggdata)) {
  if((aggdata$Temp[i]) < (mean(aggdata$Temp) - (3)*sd(aggdata$Temp)) ||
     (aggdata$Temp[i]) > (mean(aggdata$Temp) + (3)*sd(aggdata$Temp))) {
    vector <- c(vector, i)
  }
}

#replacing the outlier
aggdata$Temp[vector] <- (((aggdata$Temp[vector[1]-1]) + (aggdata$Temp[vector[1]+2])) / 2)

#View(summary(aggdata))

#write to csv file
write.csv(aggdata, file="/Users/hpanjwani/R Workspace/Assignment 2/Output/Part b/Temperature.csv")

# merging the dataset
library(dplyr)
consolidate <- left_join(nonZeroKWHEnergy, aggdata, by = c("Date"="Date", "hour"= "Hour"))

#Summary of consolidated data
#View(consolidate)
#View(summary(consolidate))

#removing NA from temp
library(zoo)
consolidate$Temp <- na.locf(consolidate$Temp)

#Writing it to csv
write.csv(consolidate, file="/Users/hpanjwani/R Workspace/Assignment 2/Output/Part b/Hourly_filled_data.csv")

#75% of the sample size
smp_size <- floor(0.75 * nrow(consolidate))

#Set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(consolidate)), size = smp_size)

#Split the data into training and testing
train <- consolidate[train_ind, ]
test <- consolidate[-train_ind, ]

##Linear Model
lm.fit = lm(kWh ~ ., data = train)

#Modified Linear Model
lm.fit <- lm(kWh ~ Temp + Peakhour + hour + Weekday  + month + day + Date, data = train)

#Summary of the fit
summary(lm.fit)

#Measures of predictive accuracy
#install.packages("forecast")
library(forecast)
pred = predict(lm.fit, test)
accuracy(pred, test$kWh)
#View(pred)

#write prediction to the csv
#write.csv(pred, file="/Users/hpanjwani/R Workspace/Assignment 2/Output/Part b/Prediction.csv")

#writing data to the csv files
library(devtools)
#install_github("dgrtwo/broom")
library(broom)
tidy_lmfit <- tidy(coef(lm.fit))
tidy_lmfit[,1:2]
account <- c("Account No", unique(energy$Account))
tidy_lmfit <- rbind(account,(tidy_lmfit[,1:2]))
tidy_lmfit[,1:2]
write.csv(tidy_lmfit[,1:2], file="/Users/hpanjwani/R Workspace/Assignment 2/Output/Part b/RegressionOutputs.csv")

#install.packages("ROCR")
library(ROCR)
account <- c("Account No", unique(energy$Account))
account <- setNames(as.data.frame(account[2]), c("Account"))
performance <- t(cbind(account, accuracy(pred, test$kWh)))
write.csv(performance, file="/Users/hpanjwani/R Workspace/Assignment 2/Output/Part b/PerformanceMetrics.csv")

