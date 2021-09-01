options(java.parameters = "-Xmx24g")
library(xlsx)
library(readxl)
library(hydroGOF)
library(randomForest)
library(ggplot2)
library(circlize)
library(RColorBrewer)
library(dplyr)
library(randomForestExplainer)
library(pdp)
library(tcltk)
library(patchwork)
library(caret)
library(ggrepel)
library(data.table)
library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer) 
library(pdp)
library(Rcpp)
library(randomForest)
library(randomForestExplainer)
library(caret)
library(networkD3)
library(shiny)
library(tidyverse)
library(xgboost)
library(lightgbm)
library(EIX)
library(Matrix)

colindex<-c('FR','AL','CL','DC','FP','SM','WC','SC','IC','CT','AFR','CFR','BP','Pt_Util')
r2_general <-function(preds,actual){ 
  return(1- sum((preds - actual) ^ 2)/sum((actual - mean(actual))^2))
}
nthmax<-function(x,n){
  y<-as.numeric(x)
  y<-order(y,decreasing=TRUE)
  return(x[y[n]])
}
##################################################
data_PC=read.csv("datatest_PC.csv",head=T)
data_PC_train=read.csv("training_data_PC.csv",head=T)
data_PC_test=read.csv("test_data_PC.csv",head=T)
#set.seed(9)
#train <- sample(nrow(data_PC), 0.85*nrow(data_PC))
#data_PC_train=data_PC[train,]
#data_PC_test=data_PC[-train,]
#head(data_PC_train)
#head(data_PC_test)
num_trees<-200
##################################################
set.seed(3)

model_matrix_all<-model.matrix(Pt_Util ~ . - 1, data_PC)
model_martix_train <- model.matrix(Pt_Util ~ . - 1, data_PC_train)
model_martix_test <- model.matrix(Pt_Util ~ . - 1, data_PC_test)
data_train <- xgb.DMatrix(model_martix_train, label = data_PC_train$Pt_Util)
data_test <- xgb.DMatrix(model_martix_test, label = data_PC_test$Pt_Util)

param <- list(max_depth = 7, eta = 0.14, subsample=0.4,silent = 1,lambda=0,alpha=1e-5,objective = "reg:squarederror")

xgb_model <- xgb.train(param, data_train, nrounds = num_trees)

##################################################
train_predict <- predict(xgb_model, data_train)
plot(data_PC_train$Pt_Util, train_predict, main = 'Training Set',
     xlab = 'Real_Pt_Consumption_per_kW@0.65V (mgpt kW-1)', ylab = 'Predict_Pt_Consumption_per_kW@0.65V (mgpt kW-1)')
abline(0, 1)
rmse_train<-rmse(data_PC_train$Pt_Util, train_predict)
corr_train<-cor(data_PC_train$Pt_Util, train_predict)
mae_train<-mae(data_PC_train$Pt_Util, train_predict)
mse_train<-mse(data_PC_train$Pt_Util, train_predict)
cov_train<-cov(data_PC_train$Pt_Util, train_predict)
rsq_train <- r2_general(data_PC_train$Pt_Util, train_predict)
##################################################
test_predict <- predict(xgb_model, data_test)
plot(data_PC_test$Pt_Util, test_predict, main = 'Test Set',
     xlab = 'Real_Pt_Consumption_per_kW@0.65V (mgpt kW-1)', ylab = 'Predict_Pt_Consumption_per_kW@0.65V (mgpt kW-1)')
abline(0, 1)
rmse_test<-rmse(data_PC_test$Pt_Util, test_predict)
corr_test<-cor(data_PC_test$Pt_Util, test_predict)
mae_test<-mae(data_PC_test$Pt_Util, test_predict)
mse_test<-mse(data_PC_test$Pt_Util, test_predict)
rsq_test <- r2_general(data_PC_test$Pt_Util, test_predict)

lolli<-lollipop(xgb_model,model_matrix_all)
plot(lolli,labels='topAll',log_scale=T,threshold = 0.03)

imp<-importance(xgb_model,model_matrix_all,option='both')
plot(imp,top = 10)
inter<-interactions(xgb_model,model_matrix_all,option='interactions')
??inter
plot(inter)
