rm(list=ls(all=TRUE))


library(tidyverse)
library(caret)
library(rpart)
library(DMwR)
library(randomForest)
library(rpart.plot)


###set working directory###
setwd("C:/Users/User/Desktop/data science project/internship/classification")


#reading
#Loading data
data<-read.csv(file="iris.csv",header=T,sep=",")

dim(data)
str(data)
summary(data)

data=data[,2:6]
#checking missin vaalue##
sum(is.na(data))      

data=na.omit(data)


##########Exploratory data analysis###########
library(DataExplorer)
create_report(data)




#data partiotioin###
set.seed(214)
crows <- sample(x = 1:nrow(data), size = 0.75*nrow(data))
train<- data[crows, ]
train
test <- data[-crows, ]
test

#build model#                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     

dt=rpart(Species~.,data=train)
dt
print(dt)

#visualize data##
rpart.plot(dt)

#predicting data##

pred = predict(dt, test,type="class")
pred

install.packages("e1071")
confusionMatrix(test$Species,pred)

#############################################



Confusion Matrix and Statistics

Reference
Prediction        Iris-setosa Iris-versicolor Iris-virginica
Iris-setosa              11               0              0
Iris-versicolor           0              14              0
Iris-virginica            0               2             11

Overall Statistics

Accuracy : 0.9474          
95% CI : (0.8225, 0.9936)
No Information Rate : 0.4211          
P-Value [Acc > NIR] : 7.335e-12       

Kappa : 0.9205          

Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: Iris-setosa Class: Iris-versicolor Class: Iris-virginica
Sensitivity                      1.0000                 0.8750                1.0000
Specificity                      1.0000                 1.0000                0.9259
Pos Pred Value                   1.0000                 1.0000                0.8462
Neg Pred Value                   1.0000                 0.9167                1.0000
Prevalence                       0.2895                 0.4211                0.2895
Detection Rate                   0.2895                 0.3684                0.2895
Detection Prevalence             0.2895                 0.3684                0.3421
Balanced Accuracy                1.0000                 0.9375                0.9630
