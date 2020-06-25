setwd("C:/Users/gmanish/Dropbox/latest/openminds/slides/MachineLearning/2.Decision_Trees/")
# setting the working directory
rm(list=ls(all=TRUE))

univ = read.table('UnivBank.csv', header=T,sep=',',
                  col.names=c('ID','age','exp','inc',
                              'zip','family','ccavg',
                              'edu','mortgage','loan',
                              'securities','cd',
                              'online','cc'))

# removing the id, experience and Zip, 
# experience is correlated to age
univ=univ[,-c(1,3,5)]

univ$family=as.factor(univ$family)
univ$edu=as.factor(univ$edu)
univ$mortgage=as.factor(univ$mortgage)
univ$loan=as.factor(univ$loan)
univ$securities=as.factor(univ$securities)
univ$cd=as.factor(univ$cd)
univ$online=as.factor(univ$online)
univ$cc=as.factor(univ$cc)

#convert mortgage as numeric
univ$mortgage=as.numeric(univ$mortgage)

set.seed(123)
rows = seq(1, nrow(univ), 1)
trainRows = sample(rows, nrow(univ) * .6)
testRows = rows[-(trainRows)]


train = univ[trainRows,] 
test=univ[testRows,] 

rm(univ,rows,testRows,trainRows)

# Classification Trees using C5.0 

library(C50)
dtC50 = C5.0(loan ~ ., data = train, rules=TRUE)
summary(dtC50)
#no of rules
dtC50$size
#Train Data dimensions
dtC50$dims
#Attribute names
dtC50$predictors
#category levels of attributes
dtC50$xlevels
dtC50 = C5.0(loan ~ ., data = train, rules=FALSE)
summary(dtC50)
plot(dtC50)
predict(dtC50, test)
C5imp(dtC50, pct=TRUE)

a=table(train$loan, predict(dtC50, newdata=train, type="class"))
rcTrain=(a[2,2])/(a[2,1]+a[2,2])*100
a=table(test$loan, predict(dtC50, newdata=test, type="class"))
rcTest=(a[2,2])/(a[2,1]+a[2,2])*100
rcTest

cat("Recall in Training", rcTrain, '\n',
    "Recall in Testing", rcTest)
rm(a,rcTest,rcTrain)
######################################################################################################
data(churn)

treeModel <- C5.0(x = churnTrain[, -20], y = churnTrain$churn)
treeModel
summary(treeModel)

ruleModel <- C5.0(churn ~ ., data = churnTrain, rules = TRUE)
ruleModel
summary(ruleModel)

######################################################################################################

library(rpart)

# Classification Trees using CART 
dtCart=rpart(loan~., data=train, method="class")    
plot(dtCart,main="Classification Tree for loan Class",margin=0.15,uniform=TRUE)
text(dtCart,use.n=T)
summary(dtCart)

a = table(train$loan, predict(dtCart, newdata=train, type="class"))
(a[2,2])/(a[2,1]+a[2,2])*100

# Regression Trees using CART 
dtCart = rpart(inc ~., data=train, method="anova")    
plot(dtCart, main="Decision Tree for Income", margin=0.15, uniform=TRUE,)
text(dtCart, use.n=T)

predCartTrain = predict(dtCart, newdata=train, type="vector")
predCartTest = predict(dtCart, newdata=test, type="vector")

library(DMwR)
regr.eval(train[,"inc"], predCartTrain, train.y = train[,"inc"])
regr.eval(test[,"inc"], predCartTest, train.y = train[,"inc"])

######################################################################################################