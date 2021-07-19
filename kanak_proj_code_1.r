## 1. read data and pca
setwd("~/eas595/project")
data = read.csv('GSE89_series_matrix_2.csv')
p_data = read.csv('original_gsedata.csv')
cancer.pca <- prcomp(p_data[,c(2:5725)], center = TRUE,scale. = TRUE)
summary(cancer.pca)
library(ggplot2)
library(ggfortify)


## 2. elimination of highly correlated features
# https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/
set.seed(3)
# load the library
library(mlbench)
library(caret)
# calculate correlation matrix
correlationMatrix <- cor(p_data[,2:5725])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# remove highly correlated attributes
length(highlyCorrelated)
nhc = highlyCorrelated*-1
p_data_wo_red = p_data[,nhc] # data without redundant columns


## 3. check the most important variables
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(class~., data=p_data, method="rf", preProcess="scale", trControl=control)
# estimate variable importance 
importance <- varImp(model, scale=FALSE)


# 4. variable importance approach 2 using boruta
# install.packages('Boruta')
library(Boruta)


#library("healthcareai")
#op = split_train_test(p_data, class, percent_train = 0.8, seed=100)
#train = scale(op$train[c(2:5725)])
#test = scale(op$test[c(2:5725)])
# https://www.r-bloggers.com/2020/04/automl-frameworks-in-r-python/

