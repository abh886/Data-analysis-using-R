library(dplyr)
library(ggplot2)
library(caret)

help("createDataPartition")
help("slice")

#Load the dataa
get_data <- read.csv("R codes/data.csv")

#Summarize the data from input csv file
summary(get_data)

#Partitioning the data into training and testing based on diagnosis

x<-get_data$diagnosis
test_data<-createDataPartition(x,times=1,p=0.4,list=FALSE)
train_set<-get_data %>% slice(-test_data)
test_set<-get_data %>% slice(test_data)

#Simple plot via ggplot2

g1<-ggplot(get_data,aes(x=diagnosis,fill=diagnosis))+geom_bar()
g1
dim(get_data)

#Plot linear model

g2<-ggplot(get_data,aes(x=texture_mean,y=radius_mean))+geom_point()+stat_smooth(formula = y~x)
g2

