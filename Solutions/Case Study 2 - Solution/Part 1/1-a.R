setwd("/Users/Prateek/Documents/Summer 16/TA/Summer 16/Assignment/Assignment 2/Assignment2/Part 1/Data/NewData_SampleFormat")
school_DS <- read.csv("NewData_SampleFormat.csv",  stringsAsFactors = FALSE)
head(school_DS)
dim(school_DS)

#Treating Outliers
#Finding Outliers
library(outliers)
boxplot(school_DS$Temperature,horizontal = TRUE)
boxplot.stats(school_DS$Temperature)


#Relacing outliers with NA 
#TREAT Temperature
outliers = boxplot(school_DS$Temperature, plot=FALSE)$out
school_DS[school_DS$Temperature %in% outliers,] = NA
summary(school_DS)
which(is.na(school_DS$Temperature)) 


##################################################################################################

#Non Missing Values in Temperature
school_DS_Temp_No_Missing <- subset(school_DS,  (school_DS$Temperature !="NA"))
dim(school_DS_Temp_No_Missing)
unique(school_DS_Temp_No_Missing)


#Missing Values in Temperature
school_DS_Temp_Missing<- subset(school_DS, is.na(school_DS$Temperature))
dim(school_DS_Temp_Missing)
school_DS_Temp_Missing$Temperature <- NULL
head(school_DS_Temp_Missing$Temperature)


#Model Built on Non Missing Values in Temperature
lm.fit <- lm(formula = Temperature ~ kWh + day + month + hour + DayOfWeek + Weekday + PeakHour, data = school_DS_Temp_No_Missing)
summary(lm.fit)


#Dropping day 
lm.fit <- lm(formula = Temperature ~ kWh + month + hour + DayOfWeek + Weekday + PeakHour, data = school_DS_Temp_No_Missing)
summary(lm.fit)


#Predict Missing Temperature
library(forecast)
missing_temperature_prediction = predict(lm.fit, school_DS_Temp_Missing)
head(missing_temperature_prediction)
which(is.na(missing_temperature_prediction)) 
school_DS_Temp_Missing$Temperature <- missing_temperature_prediction


#Merge school_DS_Temp_No_Missing & school_DS_Temp_Missing 
school_DS <- rbind(school_DS_Temp_No_Missing,school_DS_Temp_Missing)
dim(school_DS)
head(school_DS)
a <- which(is.na(school_DS$Temperature)) 
ifelse(length(a) == 0, school_DS , school_DS <- school_DS[-a,])

head(school_DS)
dim(school_DS)
which(is.na(school_DS)) 
which(school_DS$kWh == 0)
school_DS_raw <- school_DS

##################################################################################################
#TREAT KWH
library(outliers)
boxplot(school_DS$kWh,horizontal = TRUE)
boxplot.stats(school_DS$kWh)
outliers = boxplot(school_DS$kWh, plot=FALSE)$out
school_DS[school_DS$kWh %in% outliers, ]=NA
summary(school_DS)
which(is.na(school_DS$kWh)) 

school_DS_raw <- school_DS

#Non zerO KwH
school_DS_kWh_Non_Zero <- subset(school_DS,  ( school_DS$kWh != "0")) 
head(school_DS_kWh_Non_Zero)
dim(school_DS_kWh_Non_Zero)
unique(school_DS_kWh_Non_Zero)
which(is.na(school_DS_kWh_Non_Zero$kWh)) 

##################################################################################################

#Feature Selection

#Partition For Kwh Prediction 
indexes = sample(1:nrow(school_DS_kWh_Non_Zero), size=0.75*nrow(school_DS_kWh_Non_Zero))
test = school_DS_kWh_Non_Zero[-indexes,]
dim(test) 
train = school_DS_kWh_Non_Zero[indexes,]
dim(train)


require(leaps)
#Exhaustive Regression
regfit.full=regsubsets (kWh ~ month + day + year + hour + DayOfWeek + Weekday + PeakHour + Temperature,data=train ,nvmax=8,really.big = TRUE)
exhaustive_search_Summary = summary(regfit.full)
print(exhaustive_search_Summary)
names(exhaustive_search_Summary)
exhaustive_search_Summary$rss
exhaustive_search_Summary$adjr2
par(mfrow=c(2,2))
plot(exhaustive_search_Summary$rss ,xlab="Cnt Variables ",ylab="RSS", type="l")
plot(exhaustive_search_Summary$adjr2 ,xlab="Cnt Variables ", ylab="Adj Rsq",type="l")


#Forward Regression
regfit.fwd=regsubsets(kWh ~ month + day + year + hour + DayOfWeek + Weekday + PeakHour + Temperature,data=train ,nvmax=8, method="forward") 
forward_search_Summary=summary(regfit.fwd)
names(forward_search_Summary)
forward_search_Summary
forward_search_Summary$rss
forward_search_Summary$adjr2
coef(regfit.fwd,5)


##################################################################################################

#Create regression dataset for kWh
#Exculding Temperature
school_DS_Reg <- school_DS_kWh_Non_Zero[c("kWh", "month", "hour", "DayOfWeek", "Weekday", "PeakHour", "Temperature")]
dim(school_DS_Reg) 


#Partition For kWh Prediction 
indexes = sample(1:nrow(school_DS_Reg), size=0.70*nrow(school_DS_Reg))
school_DS_Reg_test = school_DS_Reg[-indexes,]
dim(school_DS_Reg_test) 
school_DS_Reg_train = school_DS_Reg[indexes,]
dim(school_DS_Reg_train)


#Multi-Linear Regression model to predict kWh
lm.fit <- lm(formula = (kWh) ~ . , data = school_DS_Reg_train)
summary(lm.fit)


# 1.a.a the Non Zero data set?
library(forecast)
pred <- predict(lm.fit, school_DS_kWh_Non_Zero)
length(pred)
acc <- accuracy(pred, school_DS_kWh_Non_Zero$kWh)
write.csv(t(acc), "PerformanceMetrics11.csv", row.names=TRUE)


# 1.a.b the “raw” (including zeros) data set?
pred <- predict(lm.fit, school_DS_raw)
length(pred)
acc <- accuracy(pred, school_DS_raw$kWh)
write.csv(t(acc), "PerformanceMetrics11.csv", row.names=TRUE)
