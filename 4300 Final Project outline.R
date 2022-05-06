library(dslabs)
library(tidyverse)
library(readr)
#reading the file and creating a plot 
survey <- read_csv("survey.csv")
surveyDF <- as.data.frame(survey)
surveyDF$Sex <- as.factor(surveyDF$Sex)
p <- surveyDF %>% ggplot()
p + geom_point(aes(Height,Wr.Hnd,col = Sex)) +
  geom_vline(xintercept = 175)
surveyDF$predicted <- ifelse(surveyDF$Height < 174,0,1)
surveyDF
T <- table(Predict = surveyDF$predicted,Actual = surveyDF$Sex)
T
T <- as.vector(T)
# Accuracy of the prediction is (num. correctly predicted)/(total)
accuracy <- (T[1]+T[4])/(T[1]+T[2]+T[3]+T[4]) # correct/total
# Sensitivity is Pr(predict 1 given actually 1) = 
sensitivity <- T[4]/(T[3]+T[4])
# Specificity is Pr(predict 0 given actually 0) = 
specificity <- T[1]/(T[1]+T[2])
metric <- c("Accuracy","Sensitivity","Specificity")
value <- c(accuracy,sensitivity,specificity)
data.frame(Metric = metric,Value = round(value,3))
# adding an x-intercept to our original plot
p <- surveyDF %>% ggplot()
p + geom_point(aes(Height,Wr.Hnd,col = Sex)) +
  geom_hline(yintercept= 20)+
  geom_vline(xintercept = 174)
surveyDF$prediction <- ifelse(surveyDF$Height <= 174 & surveyDF$Wr.Hnd <= 20,"Female","Male")
T <- table(Predicted = surveyDF$prediction,Actual=surveyDF$Sex)
T
T <- as.vector(T)
# Accuracy of the prediction is (num. correctly predicted)/(total)
accuracy <- (T[1]+T[4])/(T[1]+T[2]+T[3]+T[4]) # correct/total
# Sensitivity is Pr(predict 1 given actually 1) = 
sensitivity <- T[4]/(T[3]+T[4])
# Specificity is Pr(predict 0 given actually 0) = 
specificity <- T[1]/(T[1]+T[2])
metric <- c("Accuracy","Sensitivity","Specificity")
value <- c(accuracy,sensitivity,specificity)
data.frame(Metric = metric,Value = round(value,3))

