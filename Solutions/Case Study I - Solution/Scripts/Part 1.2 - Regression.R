school_DS <- read.csv("/Users/Prateek/Documents/Summer 16/TA/Summer 16/Assignment/Assignment 1/Part 1/bbcfd1.csv", stringsAsFactors = FALSE)
#school_DS <- read.csv(file.choose(),header = T)
head(school_DS)
dim(school_DS)


#Treating Outliers
#Finding Outliers
library(outliers)
boxplot(school_DS$kWh,horizontal = TRUE)
boxplot.stats(school_DS$kWh)

boxplot(school_DS$Temperature,horizontal = TRUE)
boxplot.stats(school_DS$Temperature)


#Relacing outliers with NA 
#TREAT KWH
outliers = boxplot(school_DS$kWh, plot=FALSE)$out
school_DS[school_DS$kWh %in% outliers,3]=NA
summary(school_DS)
which(is.na(school_DS$kWh)) 


#TREAT Temperature
outliers = boxplot(school_DS$Temperature, plot=FALSE)$out
school_DS[school_DS$Temperature %in% outliers,3]=NA
summary(school_DS)
which(is.na(school_DS$Temperature)) 


##################################################################################################

#Non zerO and Non NA Values in kWh
school_DS_kWh_Non_Zero <- subset(school_DS,  ( school_DS$kWh != "0"))  # | school_DS$kWh !="NA" 
dim(school_DS_kWh_Non_Zero)
unique(school_DS_kWh_Non_Zero)
which(is.na(school_DS_kWh_Zero)) 


#zerO and NA Values in kWh
school_DS_kWh_Zero <- subset(school_DS, ( school_DS$kWh == "0" | is.na(school_DS$kWh) ))
dim(school_DS_kWh_Zero)
unique(school_DS_kWh_Zero)
school_DS_kWh_Zero$kWh <- NULL
head(school_DS_kWh_Zero$kWh)


#Model Built on Non Missing Values in kWh
lm.fit <- lm(formula = kWh ~ day + month + hour + DayOfWeek + Weekday + PeakHour + Temperature, data = school_DS_Temp_No_Missing)
summary(lm.fit)


#Dropping day 
lm.fit <- lm(formula = kWh ~ month + hour + DayOfWeek + Weekday + PeakHour + Temperature, data = school_DS_Temp_No_Missing)
summary(lm.fit)


#Predict Missing kWh
library(forecast)
zerO_kWh_prediction = predict(lm.fit, school_DS_kWh_Zero)
head(zerO_kWh_prediction)
which(is.na(zerO_kWh_prediction)) 
school_DS_kWh_Zero$kWh <- zerO_kWh_prediction


#Merge school_DS_Temp_No_Missing & school_DS_Temp_Missing 
school_DS <- rbind(school_DS_kWh_Non_Zero,school_DS_kWh_Zero)
dim(school_DS)
head(school_DS)
a <- which(is.na(school_DS$kWh)) 
ifelse(length(a) == 0, school_DS , school_DS <- school_DS[-a,])


head(school_DS)
dim(school_DS)
which(is.na(school_DS$kWh)) 

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

##################################################################################################
#Feature Selection

#Partition For Kwh Prediction 
indexes = sample(1:nrow(school_DS), size=0.75*nrow(school_DS))
test = school_DS[-indexes,]
dim(test) 
train = school_DS[indexes,]
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

#################################################################################################

#Create regression dataset for kWh
school_DS_Reg <- school_DS[c("kWh", "month", "DayOfWeek", "Weekday", "PeakHour", "Temperature")]
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
library(forecast)
head(school_DS_Reg_test)
pred <- predict(lm.fit, school_DS_Reg_test)
acc <- accuracy(pred, school_DS_Reg_train$kWh)

#################################################################################################

#DOWNLOAD Coefficients
library(broom)
tidy_lmfit <- tidy(lm.fit)
RegressionOutput <- data.frame(tidy_lmfit$term, tidy_lmfit$estimate, stringsAsFactors=F)
RegressionOutput[1, 1] = "Constant"
library(data.table)
setnames(RegressionOutput, old = c('tidy_lmfit.term','tidy_lmfit.estimate'), new = c('Variables','Coefficient'))
write.csv(RegressionOutput, "/Users/Prateek/Documents/Summer 16/TA/Summer 16/Assignment/Assignment 1/Part 1/RegressionOutputs1.csv", col.names = TRUE, row.names=FALSE)


#DOWNLOAD METRICS
write.csv(t(acc), "/Users/Prateek/Documents/Summer 16/TA/Summer 16/Assignment/Assignment 1/Part 1/PerformanceMetrics11.csv", row.names=TRUE)

