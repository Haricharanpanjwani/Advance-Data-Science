#reading train.csv file to take the wind power of each farm
library(data.table)
data.hist.dt <- read.csv("/Users/hpanjwani/R Workspace/Mid Term/Problem 3/train.csv")

#converting the date column to date format
data.hist.dt$date <- as.POSIXct(as.character(data.hist.dt$date), format = "%Y%m%d%H", tz = "GMT")

data.hist.dt <- data.table(data.hist.dt)
#converting the wp values from column to rows and adding the 
#farm column to assign the wp belong to which farm
data.hist.dt <- data.hist.dt[ ,list(farm = rep(1:7, each = length(wp1)), 
                              wp = c(wp1,wp2,wp3,wp4,wp5,wp6,wp7)), by="date"]
#View(data.hist.dt)

boxplot(wp~farm, data= data.hist.dt,col = "lightgray")

library(zoo)
#filling the na values of wp's with na.locf
data.hist.dt[data.hist.dt$wp == 0,]$wp <- NA
data.hist.dt$wp <- na.locf(data.hist.dt$wp)

#reading the wind forecasts file to consolidate the data in one data frame and 
data.forecast.all <- NULL

for (i in 1:7) {

  filename <- paste("/Users/hpanjwani/R Workspace/Mid Term/Problem 3/windforecasts_wf", i, ".csv", sep ="")
  data.forecast.cur <- read.csv(filename)
  str(data.forecast.cur)

  data.forecast.cur$date <- as.POSIXct(as.character(data.forecast.cur$date), format = "%Y%m%d%H", tz = "GMT")
  
  #adding hours in the date and making it a timestamp
  data.forecast.cur$pdate <- data.forecast.cur$date +  as.difftime(data.forecast.cur$hors, unit="hours")
  #data.forecast.cur <- data.forecast.cur[which(data.forecast.cur$pdate >=min.forecast), ]
  
  #converting date from POSIXct to character
  #data.forecast.cur$date <- as.character(data.forecast.cur$date)
  #data.forecast.cur$pdate <- as.character(data.forecast.cur$pdate)
  
  #making wd into the range of 30
  data.forecast.cur$wd_cut <- cut(data.forecast.cur$wd, seq(0,360,30), include.lowest = T)
  
  #saving missing dates in the data frame
  data.missing.cur <- data.forecast.cur[is.na(data.forecast.cur$ws),]
  
  #removing na's from the dataset
  data.forecast.cur <- data.forecast.cur[!is.na(data.forecast.cur$ws),]
  
  data.forecast.cur$start <- data.forecast.cur$date
  data.forecast.cur$date <- data.forecast.cur$pdate
  #data.forecast.cur$dist <- data.forecast.cur$hors
  #data.forecast.cur$dist <- as.factor(sprintf("%02d", data.forecast.cur$hors))
  
  data.forecast.cur$start <- as.POSIXct(data.forecast.cur$start, tz = "GMT")
  data.forecast.cur$turn <- as.factor(format(data.forecast.cur$start, "%H"))
  #data.forecast.cur$start <- as.character(data.forecast.cur$start)

  #View(data.forecast.cur)
  
  #aggregating the data
  data.forecast.farm <- aggregate(cbind(u = data.forecast.cur$u, v = data.forecast.cur$v, 
                                        ws = data.forecast.cur$ws, wd = data.forecast.cur$wd), 
                                  by=list(date = data.forecast.cur$date, turn = data.forecast.cur$turn), 
                                  mean, na.rm = FALSE)
  
  #assigning the farm value
  data.forecast.farm$farm <- i
  
  #making wd into the range of 30
  #data.forecast.farm$wd_cut <- cut(data.forecast.farm$wd, seq(0,360,30), include.lowest = T)
  
  #retrieving the hour value from date
  data.forecast.farm$hour <- as.factor(format(data.forecast.farm$date, "%H"))
  
  boxplot(u~farm, data= data.forecast.farm,col = "lightgray")
  boxplot(v~farm, data= data.forecast.farm,col = "lightgray")
  boxplot(ws~farm, data= data.forecast.farm,col = "lightgray")
  boxplot(wd~farm, data= data.forecast.farm,col = "lightgray")
  
  #filling the na values of u and v with the na.locf
  data.forecast.farm[data.forecast.farm$u == 0,]$u <- NA
  data.forecast.farm$u <- na.locf(data.forecast.farm$u)
  
  data.forecast.farm[data.forecast.farm$v == 0,]$v <- NA
  data.forecast.farm$v <- na.locf(data.forecast.farm$v)
  
  data.forecast.all <- rbind(data.forecast.all, data.forecast.farm)
}

#View(data.forecast.all)
#View(summary(data.forecast.all))

library(dplyr)
data.wind.power.all <- left_join(data.forecast.all, data.hist.dt, by = c("date" = "date", "farm" = "farm"))

forecast.date <- as.POSIXct(as.character("2011-01-01"), tz = "GMT")
forecast.date
data.wind.power <- data.wind.power.all[data.wind.power.all$date < forecast.date,]
#View(data.wind.power)
#View(summary(data.hist.dt))

#write to csv
write.csv(data.wind.power, file="/Users/hpanjwani/R Workspace/Mid Term/Problem 3/windfarm.csv", row.names = F)

#functions which are used for computing the performance of the algorithm
# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}
# Function that returns Mean Absolute Percentage Error
mape <- function(error)  {
  mean(abs(error/test.wind$wp) * 100)
}

#sample model fitting
#75% of the sample size
class_smp_size <- floor(0.75 * nrow(data.wind.power))

#Set the seed to make your partition reproductible
set.seed(543)
train.indx <- sample(seq_len(nrow(data.wind.power)), size = class_smp_size)

#Split the data into training and testing
train.wind <- data.wind.power[train.indx, ]
test.wind <- data.wind.power[-train.indx,]

#### 1. Logistic Regression##########
#A. Logistic Regression steps
#logit code
wind.logit <- glm(wp ~ date + u + v + ws + wd + farm + hour, data=train.wind)

#summary
summary(wind.logit)

#analyzing the table of deviance
anova(wind.logit, test="Chisq")

#r2 in logit
#install.packages("pscl")
library(pscl)
pR2(wind.logit)

#prediction of wind power based on the model
pr.wind <- predict(wind.logit, newdata=test.wind, type="response")

#error
error.wind <- test.wind$wp - pr.wind
#error.wind

#getting summary of our model performance
#rmse
wind.rmse.logit <- rmse(error.wind) 
wind.rmse.logit
#calculating mean square value
rms.logit <- c("RMS", rmse(error.wind))

#mae
wind.mae.logit <- mae(error.wind)
wind.mae.logit
ma.logit <- c("MAE", mae(error.wind))

#mape
wind.mape.logit <- mape(error.wind)
wind.mape.logit
map.logit <- c("MAPE", mape(error.wind))


#"RMS"  - "0.173800019410756"
#"MAE"  - "0.134119414288033"
#"MAPE" - "193.857101513402" 
#performance metrics of model Logit
logit.wind.performance <- NULL
logit.wind.performance <- rbind(rms.logit, ma.logit, map.logit, deparse.level = 0)
logit.wind.performance

######## END OF LOGISTIC REGRESSION #######
###########################################

#### 2. Classification - Decision Trees ##########
#Decision Tree model
library(tree)
library(caret)
library (ISLR)
library(mcclust)
library(readxl)
library (grid)
#train.wind$wp ~ train.wind$date + train.wind$turn + train.wind$u + train.wind$v + train.wind$ws + train.wind$wd + train.wind$hour, data.wind.power, subset = train.wind)
wind.dec = tree(wp ~ date + u + v + ws + wd + farm + hour, data = train.wind)

#summary of the decision tree
summary (wind.dec)

#display the tree structure and node levels
plot (wind.dec)
text (wind.dec, pretty = 0)

cv.wind = cv.tree (wind.dec, FUN = prune.tree)
plot (cv.wind$size, cv.wind$dev, type='b')

#predict the wp for the test dataset
yhat=predict(wind.dec, test.wind)
##################accuracy in decision tree
plot(yhat, test.wind$wp)
abline(0,1)

#mean square error - 0.03431809
mean((yhat - test.wind$wp)^2)

#not to be use without pruning best results are coming
#pruning of the tree
prune.wind.dec =prune.tree(wind.dec, best = 4)

plot(prune.wind.dec)
text(prune.wind.dec, pretty = 0)

cv.prune.wind = cv.tree (prune.wind.dec)
plot (cv.prune.wind$size, cv.prune.wind$dev, type='b')

yhat=predict (prune.wind.dec, newdata =test.wind)

plot(yhat,test.wind$wp)
abline (0,1)

#mean square error - 0.03589378
mean((yhat - test.wind$wp)^2)

################Prediction error of our model#########
error.wind.dec <- test.wind$wp - yhat

### Model Evaluation####
#calculating mean square value and others
rmse(error.wind.dec)
rms <- c("RMS", rmse(error.wind.dec))
mae(error.wind.dec)
ma <- c("MAE", mae(error.wind.dec))
mape(error.wind.dec)
map <- c("MAPE", mape(error.wind.dec))

#"RMS" - "0.189456549192582"
#"MAE" - "0.145069094568582"
#"MAPE" - "228.30202357159"

dec_performance <- NULL
dec_performance <- rbind(rms, ma, map, deparse.level = 0)
dec_performance

######## END OF DECISION TREES #######
######################################

#### 3. Neural Network  ##########
#C. Neural Network for classification
library(nnet)
#converting hour to numeric from factor
data.wind.power$hour <- as.numeric(data.wind.power$hour)
#normalizing the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

#selecting my new subset (date, turn)
data.wind.normalize <- NULL
data.wind.normalize <- as.data.frame(lapply(data.wind.power[,-c(1,2)], normalize))
#View(data.wind.normalize)

#binding the date with the normalized date
data.wind.normalize <- cbind(data.wind.normalize, date = data.wind.power$date)

#Sampling the data
train.index <- sample(1:nrow(data.wind.normalize),round(0.75*nrow(data.wind.normalize)))
train.wind <- data.wind.normalize[train.index,]
test.wind <- data.wind.normalize[-train.index,]

#training neural net for th model
net.wind <- nnet(wp ~ farm + u + v + ws + wd + hour, data=train.wind, maxit = 150,
                 size=7, MaxNWts = 1000, rang = 0.5, hidden = 3, threshold = 0.01,, decay = 5e-4,
                 Hess = FALSE, entropy = TRUE, softmax = FALSE, abstol = 1e-4, reltol = 1e-8)

#summary of the neural net
summary(net.wind)

#Plot the neural network
library(scales)
library(reshape)
plot.nnet(net.wind, pos.col='darkgreen',neg.col='darkblue', alpha.val=0.7, rel.rsc=10, circle.cex=5, cex=1.4,circle.col='brown')

#prediction based on neural net
predict.wind.neural <- predict(net.wind, newdata=test.wind)
#View(predict.wind.neural)

#denormalized the data
denormalize <- function(x) {
  #return (((x - min(x)) / (max(x) - min(x)))*((max(x)-min(x))+min(x)))
  return ((x * (max(data.wind.power$wp) - min(data.wind.power$wp))) + min(data.wind.power$wp))
}

#computing performance of the algorithm
results <- as.numeric(sapply(predict.wind.neural, denormalize))

# Function that returns Mean Absolute Percentage Error
mape <- function(error)  {
  mean(abs(error/test.wind$wp) * 100)
}

#computing the error
error.nnet = (results - test.wind$wp)

#calculating mean square value
rmse(error.nnet)
rms <- c("RMS", rmse(error.nnet))
mae(error.nnet)
ma <- c("MAE", mae(error.nnet))
mape(error.nnet)
map <- c("MAPE", mape(error.nnet))

#"RMS"  "0.1609"
#"MAE"  "0.1188"
#"MAPE" "Inf"   
wind.neural.performance <- NULL
wind.neural.performance <- rbind(rms, ma, map, deparse.level = 0)
wind.neural.performance

##################### END OF NEURAL NETWORK ################
############################################################

#writing prediction performance metrics to csv files
filename = "/Users/hpanjwani/R Workspace/Mid Term/Problem 3/PredictionPerformanceMetrics.csv"
account <- c("Global Wind Energy Forecasting")
write.table(account, file=filename, row.names = F, col.names = F, qmethod = "double")

write.table("\n", file=filename, append = T, row.names = F, col.names = F)
write.table("Logistic Regression", file=filename, append = T, row.names = F, col.names = F)

write.table(logit.wind.performance, file=filename, append = T, col.names = F, qmethod = "double")

write.table("\n", file=filename, append = T, row.names = F, col.names = F)
write.table("Decision Tree", file=filename, append = T, row.names = F, col.names = F)

write.table(dec_performance, file=filename, append = T, row.names = F, col.names = F, qmethod = "double")

write.table("\n", file=filename, append = T, row.names = F, col.names = F)
write.table("Neural Network", file=filename, append = T, row.names = F, col.names = F)

write.table(wind.neural.performance, file=filename, append = T, row.names = F, col.names = F, qmethod = "double")


#########################################
###3. Forecasting of the wind power
#########################################
library(data.table)
data.test.dt <- read.csv("/Users/hpanjwani/R Workspace/Mid Term/Problem 3/benchmark.csv")

data.test.dt$date <- as.POSIXct(as.character(data.test.dt$date), format = "%Y%m%d%H", tz = "GMT")

#nrow(data.test.dt)
#str(data.test.dt)

#drop other columns apart from date
data.test.dt <- data.test.dt[, c("id","date"), drop = F]
#View(data.test.dt)

#appending columns in the required format to make farm wise distribution
mkRow <- function(nCol, i) {
  # make row mixed types by changing first column to string
  data.test.dt$date <- data.test.dt$date
  data.test.dt$id <- data.test.dt$id
  
  if(i == 1) data.test.dt$farm <- 1
  else if(i == 2) data.test.dt$farm <- 2
  else if(i == 3) data.test.dt$farm <- 3
  else if(i == 4) data.test.dt$farm <- 4
  else if(i == 5) data.test.dt$farm <- 5
  else if(i == 6) data.test.dt$farm <- 6
  else if(i == 7) data.test.dt$farm <- 7
  
  do.call(cbind.data.frame, data.test.dt)
}

makeFarm <- function(nRow,nCol) {
  d <- lapply(seq_len(nRow),function(i) {
    windFarm <- mkRow(nCol, i)
    data.frame(windFarm, stringsAsFactors=FALSE)
  })
  do.call(rbind,d)
}

#assigning the test data in the appropriate format
data.test.dt <- makeFarm(7, 0)

#retrieving the hour value from date
data.test.dt$hour <- as.factor(format(data.test.dt$date, "%H"))
str(data.test.dt)

#full dataset with all the values which we created earlier
#View(data.wind.power.all)

# we are going to utilize the full dataset to retrieve the value of test data
#forecaset date we kept it as '2011-01-01'
data.test.wind <- data.wind.power.all[data.wind.power.all$date > forecast.date,]
#View(data.test.wind)

data.test.final <- merge(data.test.wind[, c("date","hour", "farm", "u", "v", "ws", "wd")], data.test.dt, by = c("date", "hour", "farm"))
str(data.test.final)
#View(data.test.final)

data.test <-  aggregate(cbind(u = data.test.final$u, v = data.test.final$v, 
                              ws = data.test.final$ws, wd = data.test.final$wd), 
                        by=list(date = data.test.final$date, hour = data.test.final$hour, 
                                farm = data.test.final$farm, id = data.test.final$id), mean, na.rm = FALSE)
#View(data.test)

#logistic forecast
wp.forecast.logistic <- predict(wind.logit, newdata=data.test, type="response")
#View(as.data.frame(wp.forecast.logistic))
#binding of forecast with the data
wind.forecast <- NULL
wind.forecast <- cbind(data.test, wp = wp.forecast.logistic)
wind.forecast <- wind.forecast[,!names(wind.forecast) %in% c("hour", "u", "v", "ws", "wd")]
wind.forecast <- reshape(wind.forecast, idvar="date", v.names = "wp", timevar = "farm", direction="wide")
wind.forecast$date <- as.character(wind.forecast$date, format="%Y%m%d%H")
#View(wind.forecast)
write.csv(wind.forecast, file="/Users/hpanjwani/R Workspace/Mid Term/Problem 3/benchmark_logistic.csv", row.names = F)

#decision tree forecast
library(forecast)
wp.forecast.decision = predict(wind.dec, newdata = data.test)
wp.forecast.decision <- as.data.frame(wp.forecast.decision)
View(wp.forecast.decision)
#binding of forecast with the data
wind.forecast <- NULL
wind.forecast <- cbind(data.test, (wp = wp.forecast.decision))
colnames(wind.forecast)[colnames(wind.forecast) == "wp.forecast.decision"] <- "wp"
wind.forecast <- wind.forecast[,!names(wind.forecast) %in% c("hour", "u", "v", "ws", "wd")]
wind.forecast <- reshape(wind.forecast, idvar="date", v.names = "wp", timevar = "farm", direction="wide")
wind.forecast$date <- as.character(wind.forecast$date, format="%Y%m%d%H")
#View(wind.forecast)
write.csv(wind.forecast, file="/Users/hpanjwani/R Workspace/Mid Term/Problem 3/benchmark_decision.csv", row.names = F)

#neural network forecast
library(nnet)
library(reshape2)
data.test$hour <- as.numeric(data.test$hour)
wp.forecast.neural = predict(net.wind, data.test)
#View(wp.forecast.neural)
#binding of forecast with the data
wind.forecast <- NULL
wind.forecast <- cbind(data.test, wp.forecast.neural)
colnames(wind.forecast)[colnames(wind.forecast) == "wp.forecast.neural"] <- "wp"
wind.forecast <- wind.forecast[,!names(wind.forecast) %in% c("hour", "u", "v", "ws", "wd")]
wind.forecast <- reshape(wind.forecast, idvar="date", v.names = "wp", timevar = "farm", direction="wide")
wind.forecast$date <- as.character(wind.forecast$date, format="%Y%m%d%H")
#View(wind.forecast)
write.csv(wind.forecast, file="/Users/hpanjwani/R Workspace/Mid Term/Problem 3/benchmark_neural.csv", row.names = F)
