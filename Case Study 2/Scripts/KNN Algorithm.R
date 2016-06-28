
# KNN algo
library(class)
file3 <- read.csv("E:\\Study Files\\INFO 7390 - ADS\\Assignments\\Assignment 2\\consolidateNew.csv",stringsAsFactors = FALSE)

str(file3)

#View(file3)

dataknn <- file3
dataknn$KWH_Class <- NULL

classLists <- sapply(dataknn$kWh, function(x) {
  if(x > mean(dataknn$kWh)) {dataknn$KWH_Class <- "Above_Normal"} 
  else { dataknn$KWH_Class <- "Optimal"} })

dataknn$KWH_Class <- classLists; classLists
table(dataknn$KWH_Class)

dataknn$KWH_factor <- factor(dataknn$KWH_Class, levels = c("Above_Normal", "Optimal"), labels = c("1", "0"))

#rounding percentage

rounded <- round(prop.table(table(dataknn$KWH_factor)) * 100, digits = 1)

rounded
#normalizing the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

#d <- c(4,5,6,7,8,9,10,11,12,14)
dfknn <- subset(dataknn, select=c(4,5,7:12,14))
View(dfknn)

class(dfknn$KWH_factor)
dfknn$KWH_factor<- as.numeric(dfknn$KWH_factor)
dataknn_n <- as.data.frame(lapply(dfknn, normalize))

nrow(dataknn)
nrow(dataknn_n)
View(dataknn_n)

#sampling knn
knn_train <- dataknn_n[1:4008,]
knn_test <- dataknn_n[4009:8016,]

#labelling the factor
knn_train_labels <- dataknn_n[1:4008, 9]
knn_test_labels <- dataknn_n[4009:8016,9]

library(class)

#applying knn with k as square root of observations
dataknn_test_pred <- knn(train = knn_train, test = knn_test,cl = knn_train_labels, k=63)

#Evaluate the model performance
install.packages("gmodels")

library(gmodels)

#confusion matrix
CrossTable(x=knn_test_labels, y=dataknn_test_pred, prop.chisq = FALSE)
