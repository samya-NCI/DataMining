#CAR CRASH DATASET

car_data = read.csv("CAR_DATA.csv", header = T, sep = ",")
table(car_data$Surface.Condition.x)
Missing_values = sum(is.na(car_data$Route.Type.x))

library(ggplot2)

install.packages("Amelia")
install.packages("missMDA")
install.packages("klaR")
library(Amelia)
library(dplyr)
library(missMDA)
library(klaR)


summary(car_data)
table(car_data$Vehicle.Make)
most = tail(names(sort(table(car_data$Weather.x))),11)
car_Company = c("VOLK","DODG","CHRYSLER","VOLVO","AUDI","GILL","MERCEDES","MERZ","SUBARU","HYUN","THOMAS","KIA","MAZDA","GMC","NISS","LEXUS","ACURA","BMW","HYUNDAI","JEEP","CHEVY","CHEV","CHEVROLET","DODGE","HOND","NISSAN","TOYT","FORD","HONDA","TOYOTA")

backup1 = car_data

#backup1 = backup1 %>% filter(.data[backup1$Vehicle.Make == for(i in range(car_Company){car_Company[i]})])
backup1 = backup1 %>% filter(backup1$Vehicle.Make == for(i in 1:range(car_Company)){car_Company[i]})


backup1$Surface.Condition.x[which(is.na(backup1$Surface.Condition.x))] = sample(most[1:11], replace = T)
backup1$Weather.x[which(is.na(backup1$Weather.x))] = sample(most[1:11], replace = T)

summary(backup1)
table(backup1$Vehicle.Year)
backup2 = backup1
backup1 = na.omit(backup1, cols = "Report.Number")
write.csv(backup1, "CAR_DATA_NO_NA.csv", sep=",")


###### K-Modes
table(backup1$Surface.Condition.x)
crash.torun = subset(backup1, select = -c(1,2,3,4,5,7,6,10,11,12,13))
crash.torun2 = subset(backup1, select = -c(1,2,3,4,5,7,11,12,13))


result.kmode <- kmodes(crash.torun, 10, iter.max = 100, weighted = FALSE)
result.kmode.mm <- table(backup1$Surface.Condition.x, result.kmode$cluster)
purity.kmode <- sum(apply(result.kmode.mm, 2, max)) / nrow(crash.torun)
purity.kmode
  plot(result.kmode.mm, las = 2)

result_DF = as.matrix(result.kmode.mm)
x = subset(result_DF, select = -c(1))

plot(jitter(result_DF), col = result.kmode$cluster, las =2)
points(result.kmode$modes, col = 1:5, pch = 8)

result.kmode <- kmodes(crash.torun2, 10, iter.max = 100, weighted = FALSE)
result.kmode.mm <- table(backup1$Weather.x, result.kmode$cluster)
purity.kmode <- sum(apply(result.kmode.mm, 2, max)) / nrow(crash.torun)
purity.kmode
plot(result.kmode.mm, las = 2)

##### K-Modes end

#####Decision Tree

tree_dataset = subset(backup1, select = -c(1,2,3,8:11,13))
shuffled = sample(1:nrow(tree_dataset))
tree_dataset = tree_dataset[shuffled,]
tree_dataset['Fatal.Accident'] = sample(c(0,1),replace = TRUE,size = nrow(tree_dataset))
tree_dataset$Fatal.Accident = factor(tree_dataset$Fatal.Accident, levels = c(0,1), labels = c("No","Yes") )
tree_dataset['Time.of.Day'] = sample(c(0,1),replace = TRUE,size = nrow(tree_dataset))
tree_dataset$Time.of.Day = factor(tree_dataset$Time.of.Day, levels = c(0,1), labels = c("Night","Day") )
tree_dataset2 = subset(tree_dataset, select = -c(1,2))

ohe = as.data.frame(model.matrix(~.-1, data=tree_dataset))
str(ohe)

create_train_test = function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample = 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

data_train <- create_train_test(ohe, 0.8, train = TRUE)
data_test <- create_train_test(ohe, 0.8, train = FALSE)

install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
fit = rpart(Fatal.AccidentYes~.,data = data_train, method = 'class')
plot(fit, extra = 106)

####Decision Tree end

#### Logistic Regression

LR_dataset = tree_dataset
LR_dataset$Fatal.Accident = factor(LR_dataset$Fatal.Accident, levels = c("No","Yes"),labels = c(0,1))
table(LR_dataset$Fatal.Accident)
input_ones = LR_dataset[which(LR_dataset$Fatal.Accident == 1),]
input_zeros = LR_dataset[which(LR_dataset$Fatal.Accident == 0),]
set.seed(100)
input_training_ones = sample(1:nrow(input_ones), 0.7 * nrow(input_ones))
input_training_zeros = sample(1:nrow(input_zeros), 0.7 * nrow(input_zeros))
training_ones = input_ones[input_training_ones,]
training_zeros = input_zeros[input_training_zeros,]
training_data = rbind(training_ones, training_zeros)

test_ones = input_ones[-input_training_ones,]
test_zeros = input_zeros[-input_training_zeros,]
test_data = rbind(test_ones,test_zeros)

backup6 = LR_dataset

logitMOD = glm(Fatal.Accident ~ Route.Type.x + Weather.x + Surface.Condition.x + Traffic.Control.x + Time.of.Day, data = training_data,
               family = binomial(link = "logit"))
predicted = plogis(predict(logitMOD, test_data))

library(InformationValue)

optCutOff = optimalCutoff(test_data$Fatal.Accident, predicted)
optCutOff

library(tidyverse)
install.packages('car')
library(car)

summary(logitMOD)
car::vif(logitMOD)

misClassError(LR_dataset$Fatal.Accident, predicted, threshold = optCutOff)

plotROC(LR_dataset$Fatal.Accident, predicted)

####Logistic end

####LR 2nd try

LR_dataset2 = subset(tree_dataset, select = -c(1:4,6,7))
ohe2 = as.data.frame(model.matrix(~.-1, data=LR_dataset2))
LR_dataset2 = cbind(tree_dataset, ohe2)
LR_dataset2 = subset(LR_dataset2, select = -c(5))

input_ones = LR_dataset2[which(LR_dataset2$Road.AlignmentSTRAIGHT == 1),]
input_zeros = LR_dataset2[which(LR_dataset2$Road.AlignmentSTRAIGHT == 0),]
set.seed(100)
input_training_ones = sample(1:nrow(input_ones), 0.7 * nrow(input_ones))
input_training_zeros = sample(1:nrow(input_zeros), 0.7 * nrow(input_zeros))
training_ones = input_ones[input_training_ones,]
training_zeros = input_zeros[input_training_zeros,]
training_data = rbind(training_ones, training_zeros)


test_ones = input_ones[-input_training_ones,]
test_zeros = input_zeros[-input_training_zeros,]
test_data = rbind(test_ones,test_zeros)

logitMOD = glm(Road.AlignmentSTRAIGHT ~ Route.Type.x + Weather.x + Traffic.Control.x + Time.of.Day, data = training_data,
               family = binomial(link = "logit"))
predicted = plogis(predict(logitMOD, test_data))

optCutOff = optimalCutoff(test_data$Road.AlignmentSTRAIGHT, predicted)
optCutOff
summary(logitMOD)
abc = misClassError(LR_dataset2$Road.AlignmentSTRAIGHT, predicted, threshold = optCutOff)
plotROC(LR_dataset2$Road.AlignmentSTRAIGHT, predicted)

abc

####Logistic2 end

#### Random Forest
install.packages("randomForest")
library(randomForest)
str(tree_dataset)

set.seed(100)
train_tree = sample(nrow(tree_dataset), 0.7*nrow(tree_dataset), replace = FALSE)
TrainingSet = tree_dataset[train_tree,]
ValidationSet = tree_dataset[-train_tree,]
summary(TrainingSet)
summary(ValidationSet)

model_Forest1 = randomForest(Road.Alignment ~ ., data = TrainingSet, importance = TRUE)
model_Forest1

model_Forest1 = randomForest(Road.Alignment ~ ., data = TrainingSet, ntree = 500, mtry = 8, importance = TRUE)
model_Forest1

importance(model_Forest1)
varImpPlot(model_Forest1)

a=c()
i=5
for (i in 3:8) {
  model3 = randomForest(Road.Alignment ~ ., data = TrainingSet, ntree = 500, mtry = i, importance = TRUE)
  predValid = predict(model3, ValidationSet, type = "class")
  a[i-2] = mean(predValid == ValidationSet$Road.Alignment)
}

a

plot(3:8,a)

install.packages("party")
library(party)
x <- ctree(Road.Alignment ~ ., data=TrainingSet)
plot(x, type="simple")

x <- ctree(Road.Alignment ~ ., data=ValidationSet)
plot(x, type="simple")
