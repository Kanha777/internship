rm(list=ls(all=TRUE))


library(tidyverse)
library(caret)
library(rpart)
library(DMwR)
library(randomForest)


###set working directory###
setwd("C:/Users/User/Desktop/data science project/internship/linear")


#reading
#Loading data
data<-read.csv(file="scores.csv",header=T,sep=",")

dim(data)
str(data)
summary(data)

#checking missin vaalue##
sum(is.na(data))      

data=na.omit(data)


##########Exploratory data analysis###########
library(DataExplorer)
create_report(data)


boxplot(data$Scores)
boxplot(data$Hours)

#data partiotioin###
set.seed(214)
crows <- sample(x = 1:nrow(data), size = 0.75*nrow(data))
train<- data[crows, ]
train
test <- data[-crows, ]
test



#build  model###


lm_model <- lm(Scores~ Hours, data= train)
plot(lm_model)
summary(lm_model)


pred1=predict(lm_model,test)
regr.eval(train$Scores,pred1)


saveRDS(lm_model,"study.rds")

#predict with new data###

predict(lm_model,data.frame(Hours=9.5))
1 
95.99132 
