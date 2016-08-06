library(dplyr)
library(caret)
library(rpart)

setwd("/Users/hpanjwani/Dropbox/Assignment 3")

# Note that raw data was pre-processed to exclude non-English content
sfo <- read.csv("train.csv")

sfo$Category <- as.factor(sfo$Category)
sfo$DayOfWeek <- as.factor(sfo$DayOfWeek)
sfo$PdDistrict <- as.factor(sfo$PdDistrict)
sfo$Resolution <- as.factor(sfo$Resolution)
sfo$Address <- as.character(sfo$Address)
sfo$Dates <- as.POSIXct(sfo$Dates, format = "%Y-%m-%d %H:%M:%S")
sfo$Date <- (format(sfo$Dates, "%d"))
sfo$Year <- format(sfo$Dates, "%Y")
sfo$months <- (format(sfo$Dates, "%m"))
sfo$Hours <- (format(sfo$Dates, "%H"))

#converting the columns to numeric for the year, date, months and hours
sfo$Year <- as.numeric(sfo$Year)
sfo$Date <- as.numeric(sfo$Date)
sfo$months <- as.numeric(sfo$months)
sfo$Hours <- as.numeric(sfo$Hours)

boxplot(sfo$X, main = "Plot of San Frnacisco Latitude")
boxplot(sfo$Y, main = "Plot of San Frnacisco Longitude")

#removing outliers of latitude and longitude
lat <- which(sfo$Y == 90, arr.ind = T)
nrow(sfo)
length(lat)
sfo <- sfo[-lat,]
nrow(sfo)

#removing 47 rows where date is NA
sfo <- sfo[-which(is.na(sfo$Dates)),]
nrow(sfo)

#removing na from the dataset using na.omit of zoo package
library(zoo)
sfo <- na.omit(sfo)

#storing the crime description in another dataset,
#before removing from the main dataset
col.names <- c("Dates", "Descript", "Resolution")
sfo.description <- sfo[,(names(sfo) %in% col.names)]

#removing unneccessary column from the dataset
sfo <- sfo[,!(names(sfo) %in% col.names)]

library(plyr)
#calculating the frequency of the each category in the dataset
category <- data.frame(table(sfo$Category))

#sorting the category based on frequency in ascending order
arrange(category, desc(Freq))

#calculating the frequency percentage
CategoryFreqPercentage <- (category$Freq/nrow(sfo))*100

#putting the new category with the normal distribution in the new data.frame
new.category <- data.frame(category, round(CategoryFreqPercentage))

#sorting the data frame using arrange function
arrange(new.category, desc(Freq))

#computing categorgies name which has zero frequency value
category.zero.freq <- category$Var1[which(new.category$round.CategoryFreqPercentage. == 0)]

#removing categorgies having zero value from the main dataset
library(dplyr)
nrow(sfo)
sfo$Category[(sfo$Category %in% category.zero.freq)] <- "OTHER OFFENSES"
#View(as.data.frame(unique(sfo$CategoryMap)))

#converting category to factor from character
sfo$Category <- as.factor(sfo$Category)

#adding a column as CategoryMap which will have factor values for category
sfo$CategoryMap <- sfo$Category

levels(sfo$CategoryMap) <- gsub("LARCENY/THEFT", 1, levels(sfo$CategoryMap))
levels(sfo$CategoryMap) <- gsub("OTHER OFFENSES", 2, levels(sfo$CategoryMap))
levels(sfo$CategoryMap) <- gsub("NON-CRIMINAL", 3, levels(sfo$CategoryMap))
levels(sfo$CategoryMap) <- gsub("ASSAULT", 4, levels(sfo$CategoryMap))
levels(sfo$CategoryMap) <- gsub("DRUG/NARCOTIC", 5, levels(sfo$CategoryMap))
levels(sfo$CategoryMap) <- gsub("VEHICLE THEFT", 6, levels(sfo$CategoryMap))
levels(sfo$CategoryMap) <- gsub("VANDALISM", 7, levels(sfo$CategoryMap))
levels(sfo$CategoryMap) <- gsub("WARRANTS", 8, levels(sfo$CategoryMap))
levels(sfo$CategoryMap) <- gsub("BURGLARY", 9, levels(sfo$CategoryMap))
levels(sfo$CategoryMap) <- gsub("SUSPICIOUS OCC", 10, levels(sfo$CategoryMap))
levels(sfo$CategoryMap) <- gsub("MISSING PERSON", 11, levels(sfo$CategoryMap))
levels(sfo$CategoryMap) <- gsub("ROBBERY", 12, levels(sfo$CategoryMap))
levels(sfo$CategoryMap) <- gsub("FRAUD", 13, levels(sfo$CategoryMap))
levels(sfo$CategoryMap) <- gsub("FORGERY/COUNTERFEITING", 14, levels(sfo$CategoryMap))
levels(sfo$CategoryMap) <- gsub("SECONDARY CODES", 15, levels(sfo$CategoryMap))
levels(sfo$CategoryMap) <- gsub("WEAPON LAWS", 16, levels(sfo$CategoryMap))
levels(sfo$CategoryMap) <- gsub("PROSTITUTION", 17, levels(sfo$CategoryMap))
levels(sfo$CategoryMap) <- gsub("TRESPASS", 18, levels(sfo$CategoryMap))
levels(sfo$CategoryMap) <- gsub("STOLEN PROPERTY", 19, levels(sfo$CategoryMap))

#removing unsed levels
sfo$CategoryMap <- droplevels(sfo$CategoryMap)

#removing Cateory column from the dataset
sfo <- sfo[,!(names(sfo) %in% c("Category"))]

sfo$DayOfWeekMap <- sfo$DayOfWeek

levels(sfo$DayOfWeekMap) <- gsub("Sunday", 1, levels(sfo$DayOfWeekMap))
levels(sfo$DayOfWeekMap) <- gsub("Saturday", 1, levels(sfo$DayOfWeekMap))
levels(sfo$DayOfWeekMap) <- gsub("Monday", 0, levels(sfo$DayOfWeekMap))
levels(sfo$DayOfWeekMap) <- gsub("Tuesday", 0, levels(sfo$DayOfWeekMap))
levels(sfo$DayOfWeekMap) <- gsub("Wednesday", 0, levels(sfo$DayOfWeekMap))
levels(sfo$DayOfWeekMap) <- gsub("Thursday", 0, levels(sfo$DayOfWeekMap))
levels(sfo$DayOfWeekMap) <- gsub("Friday", 1, levels(sfo$DayOfWeekMap))

sfo$AddressMap <- sfo$Address

#View(as.data.frame(summary(sfo$CategoryMap)))

sfo$AddressMap[!grepl("/", sfo$Address)] = "0"
sfo$AddressMap[grepl("/", sfo$Address)] = "1"

sfo$AddressMap <- as.factor(sfo$AddressMap)

#removing Days of Week and Address from the dataset
sfo <- sfo[,!(names(sfo) %in% c("DayOfWeek", "Address"))]

#View(sfo)
View(summary(sfo))

write.csv(sfo, file="sfo_19.csv", row.names = F)
