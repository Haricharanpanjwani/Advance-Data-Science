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
lm.fit <- lm(kWh ~ `Day of Week` + Peakhour + Weekday + Temp + day + month + Hour, data = train)

#Summary of the fit
summary(lm.fit)

#Measures of predictive accuracy
#install.packages("forecast")
library(forecast)
pred = predict(lm.fit, test)
accuracy(pred, test$kWh)
View(pred)
#write.csv(pred, "Output/Prediction.csv")

#writing data to the csv files
library(devtools)
#install_github("dgrtwo/broom")
library(broom)
tidy_lmfit <- tidy(coef(lm.fit))
tidy_lmfit[,1:2]
account <- c("Account No", unique(energy$Account))
tidy_lmfit <- rbind(account,(tidy_lmfit[,1:2]))
write.csv(tidy_lmfit[,1:2], file = "Output/RegressionOutputs.csv")

#install.packages("ROCR")
library(ROCR)
account <- c("Account No", unique(energy$Account))
write.csv(accuracy(pred, test$kWh), file = "Output/PerformanceMetrics.csv")
