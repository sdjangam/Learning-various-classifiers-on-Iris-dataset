#Free  the environment
rm(list=ls(all=TRUE))

#install required packages
#install.packages("FNN") #"Fast Nearest Neighbours" for knn regression
#install.packages("Metrics") #to calculate error metrics for regression
library(FNN)
library(Metrics)

#set.seed()
set.seed(12345) #to get same random numbers generated every time

#Create a dataframe of 100 rows and 25 columns
data <- data.frame(matrix(data = runif(2500, 24,65), nrow = 100, ncol = 25))

# target attribute is x25
### Applying KNN

## Excluding Target Variable 
testData <- data[sample(81:100),1:24]
trainData <- data[1:80,1:24]
train.tgt <- data[1:80,25]
test.tgt <- data[sample(81:100),25]

# Run the model
pred <- knn.reg(train = trainData, test = testData, y = train.tgt, k = 1 )
actual <- test.tgt
pred <- data.frame(pred$pred)
result2 <- rmse(actual = actual, predicted = pred)
result2

# Run the model
pred <- knn.reg(train = trainData, test = testData, y = train.tgt, k = 3 )
actual <- test.tgt
pred <- data.frame(pred$pred)
result2 <- rmse(actual = actual, predicted = pred)
result2

# Run the model
pred <- knn.reg(train = trainData, test = testData, y = train.tgt, k = 5)
actual <- test.tgt
pred <- data.frame(pred$pred)
result2 <- rmse(actual = actual, predicted = pred)
result2
# Run the model
pred <- knn.reg(train = trainData, test = testData, y = train.tgt, k = 7)
actual <- test.tgt
pred <- data.frame(pred$pred)
result2 <- rmse(actual = actual, predicted = pred)
result2

