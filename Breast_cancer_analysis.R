library(dplyr)
library(ggplot2)
library(caret)
library(tidyverse)
library(corrplot)
library(e1071)
library(Metrics)

load_data <- read.csv('Breast Cancer.csv')
summary(load_data)
set.seed(1)
load_data$diagnosis <- as.factor(load_data$diagnosis)
ggplot(load_data,aes(x = diagnosis,fill = diagnosis)) + geom_bar()

b<-load_data %>% select(-c('X','id'))

#Split the data into train and test

data_split <- createDataPartition(b$diagnosis,times = 1,p=0.1,list = FALSE)

Train<- b %>% slice(-data_split)
Test<- b %>% slice(data_split)
Train<-preProcess(Train)

ggplot(Test,aes(x=diagnosis,fill = diagnosis)) + geom_bar()
#Remove diagnosis for Test data
Test1<- select(Test,-c('diagnosis'))
#Applying correlation function and plots

c<- b %>% select(-c('diagnosis'))
D<-cor(c)
corrplot(D,c('circle'))

#Apply Different algorithms

#knn

KNN_model <- knn3(diagnosis~.,data = Train,k=25)
KNN_test<-predict(KNN_model,Test,type = "class")

#Accuracy of 25 neighbor KNN model
confusionMatrix(KNN_test, Test$diagnosis)$overall['Accuracy']

#> confusionMatrix(KNN_test, Test$diagnosis)$overall['Accuracy']
#Accuracy 
#0.9482759 

#Naive Bayes Classifier 

Naive_train <- naiveBayes(diagnosis~.,data = Train)
Naive_test <- predict(Naive_train,Test,type = "class")
confusionMatrix(Naive_test,Test$diagnosis)$overall['Accuracy']

#> confusionMatrix(Naive_test,Test$diagnosis)$overall['Accuracy']
#Accuracy 
#0.9310345 

#To compute RMSE, we must convert string to numeric value
KNN_test <- as.numeric(KNN_test)
Naive_test<- as.numeric(Naive_test)
Test$diagnosis <- as.numeric(Test$diagnosis)

RMSE(Naive_test,Test$diagnosis)

#> RMSE(Naive_test,Test$diagnosis)
#[1] 0.2626129

RMSE(KNN_test,Test$diagnosis)

#> RMSE(KNN_test,Test$diagnosis)
#[1] 0.2274294

