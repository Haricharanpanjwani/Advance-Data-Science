setwd("/Users/Prateek/Documents/Summer 16/TA/Summer 16/Assignment/Assignment 2/Assignment2/Part 1/Data/")

hourly_Data <- read.csv("Hourly_filled_data.csv",  stringsAsFactors = FALSE)
head(hourly_Data)

library(forecast)


#NEURAL NETWORKS

library(neuralnet)

#To convert columns type into Numeric values
columnNames <- colnames(hourly_Data)
for (i in columnNames)
{hourly_Data[[i]] <- as.numeric(hourly_Data[[i]])
}


# To verify that no numeric columns with 0s exist
apply(hourly_Data,2,function(x) sum(is.na(x)))


#Scaling the inputs of neural network
#Scaled values are preferred over normalized Values
max <- apply(hourly_Data, 2, max)
min <- apply(hourly_Data, 2, min)
scaled <- as.data.frame(scale(hourly_Data, center = min, scale = max - min))


#To detect and treat any presenence of 0 values as a result of scaling
scaled$kWh <- ifelse((scaled$kWh == 0), NA, scaled$kWh)
library(zoo)
scaled$kWh <- zoo(scaled$kWh)
scaled$kWh=na.fill(scaled$kWh, "extend")


#partioning the dataset
index <- sample(1:nrow(hourly_Data), (0.80 * nrow(hourly_Data)))
train <- scaled[index, ]
test <- scaled[-index, ]

n <- names(train)
f <- as.formula(kWh ~ Temperature + hour + Weekday + PeakHour)

#Building the Neural Model on Trainign Data
nn <- neuralnet(f, data = train, hidden = c(3, 2), linear.output = T, threshold = 0.5, stepmax=1e6)
summary (nn)
plot(nn)


#Prediction of test 
test1 <- test[c("Temperature", "PeakHour", "Weekday", "hour")]
net.results <- compute(nn, test1)


#descaling the data
deScale <- function(x) {
  return ((x * (max(hourly_Data$kWh) - min(hourly_Data$kWh))) + min(hourly_Data$kWh))
}

results <- as.data.frame(sapply(net.results$net.result, deScale))

acc <- accuracy(net.results$net.result[, 1], test$kWh)
acc

write.csv(t(acc),"PredictionPerformanceMetrics_Neural_Tree.csv")

###################################################################################################

#Regression TREE

library(tree)

set.seed(1)
train = sample(1:nrow(hourly_Data),nrow(hourly_Data)/2)
test = hourly_Data[-train,]
tree_Model = tree(kWh ~ month + day + hour + DayOfWeek + Weekday + PeakHour + Temperature , hourly_Data, subset = train)

plot(tree_Model)
text(tree_Model,pretty = 0)

cv.sf = cv.tree(tree_Model)
plot (cv.sf$size, cv.sf$dev, type='b')

tree_pruned_Model = prune.tree(tree_Model,best = 6)
plot(tree_pruned_Model)
text(tree_pruned_Model, pretty = 0)

tree.pred <- predict(tree_pruned_Model, test)
acc <- accuracy(tree.pred, test$kWh)
acc

write.csv(t(acc),"PredictionPerformanceMetrics_Regression_Tree.csv")
