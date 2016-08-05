school_DS <- read.csv(file.choose(),header = T)
#school_DS <- read.csv("/Users/Prateek/Documents/Summer 16/TA/Summer 16/Assignment/Assignment 1/Part 1/Part 2/NewData.csv")
#school_DS <- read.csv("/Users/Prateek/Documents/Summer 16/TA/Summer 16/Assignment/Assignment 1/Part 1/RawData.csv")

school_DS_kWh <- subset(school_DS,  (Units =="kWh"))
school_DS_kWh <- school_DS_kWh[order(as.Date(school_DS_kWh$Date , format="%m/%d/%Y")), ]

library(reshape)
school_DS_kWh_Melt <- melt(school_DS_kWh, id=c("Account","Date","Channel","Units"))
school_DS_kWh_Melt <- school_DS_kWh_Melt[order(as.Date(school_DS_kWh_Melt$Date , format="%m/%d/%Y")), ]


library("stringi")
s <- stri_split_fixed(school_DS_kWh_Melt$variable, ".", 2, simplify = TRUE)
sd <- s[,1]
p <- structure(gsub("X", "", sd), dim=dim(sd))


library(stringr)        
newSeason <- substring(school_DS_kWh_Melt$variable,  2 , str_length(school_DS_kWh_Melt$variable))
newSeasonn1 <- ifelse(newSeason == "1.00", 0, 
                      ifelse(newSeason == "2.00", 1, 
                             ifelse(newSeason == "3.00", 2, 
                                    ifelse(newSeason == "4.00", 3,
                                           ifelse(newSeason == "5.00", 4,    
                                                  ifelse(newSeason == "6.00", 5,
                                                         ifelse(newSeason == "7.00", 6,
                                                                ifelse(newSeason == "8.00", 7,
                                                                       ifelse(newSeason == "9.00", 8,
                                                                              ifelse(newSeason == "10.00", 9,
                                                                                     ifelse(newSeason == "11.00", 10,
                                                                                            ifelse(newSeason == "12.00", 11,
                                                                                                   ifelse(newSeason == "13.00", 12,
                                                                                                          ifelse(newSeason == "14.00", 13,
                                                                                                                 ifelse(newSeason == "15.00", 14,
                                                                                                                        ifelse(newSeason == "16.00", 15,
                                                                                                                               ifelse(newSeason == "17.00", 16,
                                                                                                                                      ifelse(newSeason == "18.00", 17,
                                                                                                                                             ifelse(newSeason == "19.00", 18,
                                                                                                                                                    ifelse(newSeason == "20.00", 19,
                                                                                                                                                           ifelse(newSeason == "21.00", 20,
                                                                                                                                                                  ifelse(newSeason == "22.00", 21,
                                                                                                                                                                         ifelse(newSeason == "23.00", 22,
                                                                                                                                                                                ifelse(newSeason == "24.00", 23, p))))))))))))))))))))))))

school_DS_kWh_Melt$season <- paste(newSeasonn1, as.Date(school_DS_kWh_Melt$Date , format="%m/%d/%Y"))
school_DS_kWh_Melt$join_Date <- str_replace_all(string=as.Date(school_DS_kWh_Melt$Date , format="%m/%d/%Y"), pattern="-", repl="")
school_DS_kWh_Melt <- cbind(school_DS_kWh_Melt)


school_DS_kWh_Melt_tmp <- school_DS_kWh_Melt


library(sqldf)
school_DS_kWh_Melt_tmp <- sqldf('SELECT Account, Date, Channel, Units, season, join_Date, SUM(value) AS kWh FROM school_DS_kWh_Melt_tmp GROUP BY season, join_Date, Account, Date, Channel, Units')
school_DS_kWh_Melt_tmp <- school_DS_kWh_Melt_tmp[order(as.numeric(school_DS_kWh_Melt_tmp$Date , format="%m/%d/%Y"), school_DS_kWh_Melt_tmp$season), ]


require(lubridate)
aaa <- as.Date(school_DS_kWh_Melt_tmp$Date, format ="%m/%d/%Y")
school_DS_kWh_Melt_tmp$day <- day(aaa)
school_DS_kWh_Melt_tmp$month <- month(aaa)
school_DS_kWh_Melt_tmp$year <- year(aaa)
school_DS_kWh_Melt_tmp$DayOfWeek <- wday(aaa) - 1
head(school_DS_kWh_Melt_tmp)


school_DS_kWh_Melt_tmp$hour <- gsub(" .*$", "", school_DS_kWh_Melt_tmp$season)


library(chron)
WeekdayOrWeekend <- is.weekend(aaa)
school_DS_kWh_Melt_tmp$Weekday <- ifelse(WeekdayOrWeekend == "TRUE", 0, 
                                     ifelse(WeekdayOrWeekend == "FALSE", 1, "NA"))


school_DS_kWh_Melt_tmp$PeakHour <- ifelse(as.numeric(school_DS_kWh_Melt_tmp$hour) >= 6 &  as.numeric(school_DS_kWh_Melt_tmp$hour) <= 19 , 1 , 0)


school_DS_kWh_Melt_tmp$season <- gsub("\\s", "", str_replace_all(string=school_DS_kWh_Melt_tmp$season, pattern="-", repl=""))  


school_DS_kWh_Melt <- cbind(school_DS_kWh_Melt_tmp)
school_DS_kWh_Melt$season <- NULL
#write.csv(school_DS_kWh_Melt, "/Users/Prateek/Documents/Summer 16/TA/Summer 16/Assignment/Assignment 1/Part 1/bbcfd1.csv", row.names=FALSE)
dim(school_DS_kWh_Melt)
head(school_DS_kWh_Melt)

Uniquedate <- as.Date(school_DS_kWh_Melt_tmp$Date, format='%m/%d/%Y')
Uniquedate <- as.character(Uniquedate)
Uniquedate <- data.frame(unique(Uniquedate))
colnames(Uniquedate)[1] <- "DAtes"
Uniquedate <- str_replace_all(string=Uniquedate$DAtes, pattern="-", repl="")
Uniquedate <- data.frame(Uniquedate)
colnames(Uniquedate)[1] <- "DAtes"

write.csv(Uniquedate, "/Users/Prateek/Documents/Summer 16/TA/Summer 16/Assignment/Assignment 1/Part 1/UNIQUEDATE.csv", row.names=TRUE)

unique_Date <- read.csv("/Users/Prateek/Documents/Summer 16/TA/Summer 16/Assignment/Assignment 1/Part 1/UNIQUEDATE.csv")


matrix_temperature_AllDates <- matrix(0, ncol = 3,)
matrix_temperature_AllDates <- data.frame(matrix_temperature_AllDates)
matrix_temperature_AllDates <-  matrix_temperature_AllDates[-1,]


put_delay <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}


library(jsonlite)
getTemperature <- function(var_date){
  put_delay(10)
  url <- paste0("http://api.wunderground.com/api/c60cdcdd011d2bcc/history_", var_date, "/q/MA/Boston.json")
  raw.data <- readLines(url, warn = "F")
  rd <- fromJSON(raw.data)
  
  rd$history$observations$date$year
  rd$history$observations$date$mon
  rd$history$observations$date$mday
  hourrr <- rd$history$observations$date$hour
  tempi <- rd$history$observations$tempi
  Datex <- c(var_date)
  
  temperature_DF <- data.frame(Datex,hourrr,tempi)
  
  temperature_DF$tempi <- as.numeric(as.character(temperature_DF$tempi)) 
  temperature_DF <- aggregate(x = temperature_DF["tempi"], 
                      by = list(temperature_DF$Datex, month = substr(temperature_DF$hour, 1, 7)), 
                      FUN = mean)
  
  print(temperature_DF)
  
  for (i in 1:nrow(temperature_DF)) 
  {
    matrix_temperature_AllDates[nrow(matrix_temperature_AllDates)+1,] <<- rbind(temperature_DF[i,1], temperature_DF[i,2], temperature_DF[i,3] )
  }
  
}

lapply(unique_Date$DAtes, getTemperature)


library(data.table)
setnames(matrix_temperature_AllDates, old = c('X1','X2','X3'), new = c('DAte','Hour', 'Temperature'))
head(matrix_temperature_AllDates)
head(school_DS_kWh_Melt)
dim(school_DS_kWh_Melt)
newSeason <- substring(matrix_temperature_AllDates$Hour,  1 , 1)
matrix_temperature_AllDates$Hour <- ifelse(newSeason == 0, substring(matrix_temperature_AllDates$Hour,  2 , 2), matrix_temperature_AllDates$Hour)

#matrix_temperature_AllDates <- read.csv("/Users/Prateek/Documents/Summer 16/TA/Summer 16/Assignment/Assignment 1/Part 1/TEMP/1-365.csv")
merge_school_DS_Temperature <- merge(school_DS_kWh_Melt, matrix_temperature_AllDates, by.x = c("join_Date","hour"), by.y=c("DAte","Hour"), all.x = TRUE)
head(merge_school_DS_Temperature)

merge_school_DS_Temperature$join_Date <- NULL
merge_school_DS_Temperature <- merge_school_DS_Temperature[c("Account", "Date", "kWh", "month", "day", "year", "hour", "DayOfWeek", "Weekday", "PeakHour", "Temperature")]
merge_school_DS_Temperature <- merge_school_DS_Temperature[order(merge_school_DS_Temperature$Date, merge_school_DS_Temperature$hour), ]
write.csv(merge_school_DS_Temperature, "/Users/Prateek/Documents/Summer 16/TA/Summer 16/Assignment/Assignment 2/Assignment2/Part 1/Data/NewData_SampleFormat/NewData_SampleFormat.csv", row.names=FALSE)

head(merge_school_DS_Temperature)
dim(merge_school_DS_Temperature)