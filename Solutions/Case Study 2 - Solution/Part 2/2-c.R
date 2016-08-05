setwd("/Users/Prateek/Documents/Summer 16/TA/Summer 16/Assignment/Assignment 2/Assignment2/Part 1/Output/1.3/ForecastInput")

forecastInput <- read.csv("forecastInput.csv", header = TRUE)
head(forecastInput)

forecastInput_Model_features <- forecastInput[c( "Temperature", "PeakHour", "Weekday", "hour", "DayOfWeek", "month", "day")]



#Predict Power Usage - Classification Tree
KWh_Class = predict(prune.Hourly_filled_DS, forecastInput_Model_features, type = "class")
forecastInput_tree <- cbind(forecastInput, KWh_Class)
head(forecastInput_tree)
write.csv(forecastInput_tree, "forecastNewData-KWH_Class-ClassificationTree.csv")


#Predict Power Usage - Neural Networks
KWh_Class <- predict(nett, forecastInput_Model_features, type = ("class"))
forecastInput_Neural <- cbind(forecastInput, KWh_Class)
head(forecastInput_Neural)
write.csv(forecastInput_Neural, "forecastNewData-KWH_Class-Neural.csv")

##################################################################################################

setwd("/Users/Prateek/Documents/Summer 16/TA/Summer 16/Assignment/Assignment 2/Assignment2/Part 1/Output/1.3/ForecastInput2")

forecastInput2 <- read.csv("forecastInput2.csv", header = TRUE)
head(forecastInput2)


forecastInput_Model_features <- forecastInput2[c( "Temperature", "PeakHour", "Weekday", "hour", "DayOfWeek", "month", "day")]


#Predict Power Usage - Classification Tree
KWh_Class = predict(prune.Hourly_filled_DS, forecastInput_Model_features, type = "class")
forecastInput_tree <- cbind(forecastInput2, KWh_Class)
head(forecastInput_tree)
write.csv(forecastInput_tree, "forecastNewData2-KWH_Class-ClassificationTree.csv")


#Predict Power Usage - Neural Networks
KWh_Class <- predict(nett, forecastInput_Model_features, type = ("class"))
forecastInput_Neural <- cbind(forecastInput2, KWh_Class)
head(forecastInput_Neural)
write.csv(forecastInput_Neural, "forecastNewData2-KWH_Class-Neural.csv")
