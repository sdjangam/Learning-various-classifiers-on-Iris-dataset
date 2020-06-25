rm(list=ls(all=TRUE))
setwd("C:/Users/gmanish/Dropbox/latest/handson/KNN")

library(class)
library(dummies)
library(vegan)

#Bank problem

bankdata=read.csv(file="UniversalBank.csv", header=TRUE, sep=",")

summary(bankdata)
sum(is.na(bankdata))
bankdata2=subset(bankdata, select=-c(ID,ZIP.Code)) # to remove the columns ID & ZIP Code from the data
bankdata2$Education = as.factor(as.character(bankdata2$Education))
Education=dummy(bankdata2$Education)
bankdata3=subset(bankdata2,select=-c(Education)) 
bankdata4=cbind(bankdata3,Education)


set.seed(123) # to get same data in each time
train = sample(1:5000,3000) # to take a random sample of  60% of the records for train data 
bankdata_train = bankdata4[train,] 
bankdata_test = bankdata4[-train,] 

table(bankdata4$Personal.Loan)
table(bankdata_train$Personal.Loan)
table(bankdata_test$Personal.Loan)

# bankdata_trainwithclass = bankdata_train
# bankdata_testwithclass = bankdata_test
bankdata_trainwithoutclass = subset(bankdata_train,select=-c(Personal.Loan))
bankdata_testwithoutclass = subset(bankdata_test,select=-c(Personal.Loan))


#N=1
pred=knn(bankdata_trainwithoutclass, bankdata_testwithoutclass, bankdata_train$Personal.Loan, k = 1)
a=table(pred,bankdata_test$Personal.Loan)
a
accu= sum(diag(a))/nrow(bankdata_testwithoutclass)
accu

#N=3
pred=knn(bankdata_trainwithoutclass, bankdata_testwithoutclass, bankdata_train$Personal.Loan, k = 3)
a=table(pred,bankdata_test$Personal.Loan)
a
accu=sum(diag(a))/nrow(bankdata_test)
accu

#N=5
pred=knn(bankdata_trainwithoutclass, bankdata_testwithoutclass, bankdata_train$Personal.Loan, k = 5)
a=table(pred,bankdata_test$Personal.Loan)
a
accu=sum(diag(a))/nrow(bankdata_test)
accu



#N=7
pred=knn(bankdata_trainwithoutclass, bankdata_testwithoutclass, bankdata_train$Personal.Loan, k = 7)
a=table(pred,bankdata_test$Personal.Loan)
a
accu=sum(diag(a))/nrow(bankdata_test)
accu

library(vegan)
bankdata5=decostand(bankdata4,"range") # to standardize the data using 'Range' method
set.seed(123) # to get same data in each time
train = sample(1:5000,3000) # to take a random sample of  60% of the records for train data 
bankdata_train = bankdata5[train,] 
bankdata_test = bankdata5[-train,] 

# bankdata_trainwithclass = bankdata_train
# bankdata_testwithclass = bankdata_test
bankdata_trainwithoutclass = subset(bankdata_train,select=-c(Personal.Loan))
bankdata_testwithoutclass = subset(bankdata_test,select=-c(Personal.Loan))


#N=1
pred=knn(bankdata_trainwithoutclass, bankdata_testwithoutclass, bankdata_train$Personal.Loan, k = 1)
a=table(pred,bankdata_test$Personal.Loan)
a
accu= sum(diag(a))/nrow(bankdata_test)
accu



#N=3
pred=knn(bankdata_trainwithoutclass, bankdata_testwithoutclass, bankdata_train$Personal.Loan, k = 3)
a=table(pred,bankdata_test$Personal.Loan)
a
accu=sum(diag(a))/nrow(bankdata_test)
accu

#N=5
pred=knn(bankdata_trainwithoutclass, bankdata_testwithoutclass, bankdata_train$Personal.Loan, k = 5)
a=table(pred,bankdata_test$Personal.Loan)
a
accu=sum(diag(a))/nrow(bankdata_test)
accu

#N=7
pred=knn(bankdata_trainwithoutclass, bankdata_testwithoutclass, bankdata_train$Personal.Loan, k = 7)
a=table(pred,bankdata_test$Personal.Loan)
a
accu=sum(diag(a))/nrow(bankdata_test)
accu

#N=7, Majority=5

pred=knn(bankdata_trainwithoutclass, bankdata_testwithoutclass, bankdata_train$Personal.Loan, k =7,l=5)
table(pred,bankdata_test$Personal.Loan)

# Condensing to reduce the complexity of the model - 
# condensinng the number of records to compute distances from a test record 

keep=condense(bankdata_trainwithoutclass, bankdata_train$Personal.Loan)
keep

# take condensed data and run the model

pred=knn(bankdata_trainwithoutclass [keep, , drop=FALSE], bankdata_testwithoutclass, 
         bankdata_train$Personal.Loan[keep],k=5)
a <- table(pred,bankdata_test$Personal.Loan)
a
accu=sum(diag(a))/nrow(bankdata_testwithoutclass)
accu

summary(bankdata)

# run the model using FNN library
library(FNN)
pred=FNN::knn(bankdata_trainwithoutclass [keep, , drop=FALSE], bankdata_testwithoutclass, 
         bankdata_train$Personal.Loan[keep],k=5)
a <- table(pred,bankdata_test$Personal.Loan)
a
accu=sum(diag(a))/nrow(bankdata_test)
accu

indices = knnx.index(bankdata_trainwithoutclass [keep, , drop=FALSE], bankdata_testwithoutclass, 
           k=5)

# If you want the indices of the 5 nearest neighbors to row 20 of test dataset :
print(indices[20, ])
