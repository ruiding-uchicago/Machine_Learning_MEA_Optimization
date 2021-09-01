options(java.parameters = "-Xmx24g")

library(xgboost)
library(lightgbm)
library(EIX)
library(Matrix)
library(ModelMetrics)
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
num_trees<-100
##################################################
set.seed(3)

num_features<-dim(data_PC_train)[2]-1
#LGBM_train<-as.matrix(data_PC[,1:num_features])
LGBM_train<-as.matrix(data_PC_train[,1:num_features])
LGBM_test<-as.matrix(data_PC_test[,1:num_features])

params <- list(objective="regression", metric="l2")
lgbm_model <- lightgbm(
  params = params
  , data = LGBM_train
  , nrounds = num_trees
  , boosting='gbdt'
  , learning_rate = 0.12
  , max_depth = 5
  , label=data_PC_train$Pt_Util
)
??lightgbm


##################################################
train_predict <- predict(lgbm_model, LGBM_train)
plot(data_PC_train$Pt_Util, train_predict, main = 'Training Set',
     xlab = 'Real_Pt_Consumption_per_kW@0.65V (mgpt kW-1)', ylab = 'Predict_Pt_Consumption_per_kW@0.65V (mgpt kW-1)')
abline(0, 1)
rmse_train<-rmse(data_PC_train$Pt_Util, train_predict)
mae_train<-mae(data_PC_train$Pt_Util, train_predict)
mse_train<-mse(data_PC_train$Pt_Util, train_predict)
cov_train<-cov(data_PC_train$Pt_Util, train_predict)
rsq_train <- r2_general(data_PC_train$Pt_Util, train_predict)
##################################################
test_predict <- predict(lgbm_model, LGBM_test)
plot(data_PC_test$Pt_Util, test_predict, main = 'Test Set',
     xlab = 'Real_Pt_Consumption_per_kW@0.65V (mgpt kW-1)', ylab = 'Predict_Pt_Consumption_per_kW@0.65V (mgpt kW-1)')
abline(0, 1)
rmse_test<-rmse(data_PC_test$Pt_Util, test_predict)
corr_test<-cor(data_PC_test$Pt_Util, test_predict)
mae_test<-mae(data_PC_test$Pt_Util, test_predict)
mse_test<-mse(data_PC_test$Pt_Util, test_predict)
rsq_test <- r2_general(data_PC_test$Pt_Util, test_predict)

lolli<-lollipop(lgbm_model,model_matrix_all)
plot(lolli,labels='topAll',log_scale=T,threshold = 0.02)

imp<-importance(lgbm_model,model_matrix_all,option='both')
plot(imp,top = 10)
inter<-interactions(lgbm_model,model_matrix_all,option='interactions')
plot(inter)
