survey_data = read.csv("D:/College Lectures/Data Mining/Project Folder/Final Datasets/european-social-survey-ess-8-ed21-201617/Euro Survey.csv")

survey_data2 = subset(survey_data, select = -c(1:5,10:12,14,17,19:332))

table(survey_data2$News.Time)
table(survey_data2$Internet.Use)
table(survey_data2$Internet.Time)
table(survey_data2$Political.Interest)
table(survey_data2$Trust.in.Parliament)

survey_data2$Internet.Time[survey_data2$Internet.Time == ""] = NA
survey_data2$Trust.in.Parliament[survey_data2$Trust.in.Parliament == "99"] = NA

survey_data2$News.Time[which(is.na(survey_data2$News.Time))] = mean(survey_data2$News.Time, replace = TRUE)
survey_data2$Internet.Time[which(is.na(survey_data2$Internet.Time))] = mean(survey_data2$Internet.Time, replace = TRUE)
survey_data2$Trust.in.Parliament[which(is.na(survey_data2$Trust.in.Parliament))] = mean(survey_data2$Trust.in.Parliament)

for(i in 1:ncol(survey_data2)){
  survey_data2$Internet.Time[is.na(survey_data2$Internet.Time)] <- mean(survey_data2$Internet.Time, na.rm = TRUE)
}
for(i in 1:ncol(survey_data2)){
  survey_data2$News.Time[is.na(survey_data2$News.Time)] <- mean(survey_data2$News.Time, na.rm = TRUE)
}
survey_data2$Trust.in.Parliament = as.factor(survey_data2$Trust.in.Parliament)

most = tail(names(sort(table(survey_data2$Trust.in.Parliament))),10)
survey_data2$Trust.in.Parliament[which(is.na(survey_data2$Trust.in.Parliament))] = sample(most[1:10], replace = T)

write.csv(survey_data2,"SURVEY_DATA.csv")

breaks = c(0,3,11)
# specify interval/bin labels
tags <- c("Not Interested", "Interested")
# bucketing values into bins
survey_data2$Political.Interest.bin <- cut(survey_data2$Political.Interest, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)
# inspect bins
summary(group_tags)

install.packages('tidyverse')
library(tidyverse)
install.packages('ggplot2')
library(ggplot2)
install.packages('caret')
library(caret)
install.packages('caretEnsemble')
library(caretEnsemble)
install.packages('psych')
library(psych)
install.packages('Amelia')
library(Amelia)
install.packages('mice')
library(mice)
install.packages('GGally')
library(GGally)
install.packages('rpart')
library(rpart)
install.packages('randomForest')
library(randomForest)
install.packages("GGally")
library(GGally)
install.packages("utilities")
library(utilities)

missmap(survey_data2)
backup_survey = survey_data2

shuffled_survey = sample(1:nrow(survey_data2))
survey_data2 = survey_data2[shuffled_survey,]
survey_data3 = subset(survey_data2, select = -c(1))

head = head(survey_data2, 10000)

ggplot(head, aes(News.Time, colour = Political.Interest.bin)) +
geom_freqpoly(binwidth = 1) + labs(title="Interest by news")


c = ggplot(head, aes(x=Internet.Time, fill=Political.Interest.bin, color=Political.Interest.bin)) +
  geom_histogram(binwidth = 1) + labs(title="Interest by Internet time")
c + theme_bw()

ggpairs(survey_data3)

indxTrain <- createDataPartition(y = survey_data3$Political.Interest.bin,p = 0.75,list = FALSE)
training = survey_data3[indxTrain,]
testing = survey_data3[-indxTrain,]

prop.table(table(survey_data3$Political.Interest.bin))*100
prop.table(table(training$Political.Interest.bin))*100
prop.table(table(testing$Political.Interest.bin))*100

x = training[,-9]
y = training$Political.Interest.bin

library(e1071)

model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
model

Predict = predict(model, newdata = testing)
Predict
confusionMatrix(Predict, testing$Political.Interest.bin)

x = varImp(model)
plot(x)
  