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

source('min_depth_distribution.R')
source('measure_importance.R')
source('min_depth_interactions.R')
source('interaction.R')
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
#data_PC=read.csv("datatest_PC.csv",head=T)
data_PC_train=read.csv("training_data_PC.csv",head=T)
data_PC_test=read.csv("test_data_PC.csv",head=T)
#set.seed(39)
#train <- sample(nrow(data_PC), 0.85*nrow(data_PC))
#data_PC_train=data_PC[train,]
#data_PC_test=data_PC[-train,]
#head(data_PC_train)
#head(data_PC_test)
num_trees<-500
##################################################
set.seed(1)
forest_huge <- randomForest(Pt_Util ~ .,type=regression, data = data_PC_train, ntree=10000, importance = TRUE)
plot(forest_huge, main = "Learning curve of the forest")
forest <- randomForest(Pt_Util ~ .,type=regression, data = data_PC_train, ntree=num_trees, importance = TRUE)

##################################################
train_predict <- predict(forest, data_PC_train)
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
test_predict <- predict(forest, data_PC_test)
plot(data_PC_test$Pt_Util, test_predict, main = 'Test Set',
     xlab = 'Real_Pt_Consumption_per_kW@0.65V (mgpt kW-1)', ylab = 'Predict_Pt_Consumption_per_kW@0.65V (mgpt kW-1)')
abline(0, 1)
rmse_test<-rmse(data_PC_test$Pt_Util, test_predict)
corr_test<-cor(data_PC_test$Pt_Util, test_predict)
mae_test<-mae(data_PC_test$Pt_Util, test_predict)
mse_test<-mse(data_PC_test$Pt_Util, test_predict)
rsq_test <- r2_general(data_PC_test$Pt_Util, test_predict)





##################################################
im_frame<-measure_importance(forest)
im_frame[4]<-im_frame[4]/max(im_frame[4])
im_frame[5]<-im_frame[5]/max(im_frame[5])



##################################################
c1<-rep(1:length(colindex),each=length(colindex))
c11<-colindex[c1]
c2<-rep(1:length(colindex),length(colindex))
c22<-colindex[c2]
rd_frame<-data.frame(c11,c22)
colnames(rd_frame)=c('variable','root_variable')
rd_frame<-merge(rd_frame,im_frame[c(1,6)],all.x=T)
pb1 <- tkProgressBar("??????","?????? %", 0, 100) 
for (j in 1:forest$ntree){
  info1<- sprintf("?????? %d%%", round(j*100/forest$ntree)) 
  D<-calculate_max_depth(forest,j)
  interactions_frame_single<-min_depth_interactions_single(forest,j,colindex)
  rD<-calculate_rD(D,interactions_frame_single,j)
  rD<-cbind(interactions_frame_single[1:2],rD)
  rd_frame<-merge(rd_frame,rD,by=c('variable','root_variable'),all=T)
  setTkProgressBar(pb1, j*100/forest$ntree, sprintf("???? (%s)", info1),info1) 
}
close(pb1)
rd_frame[is.na(rd_frame)]<-0



for (k in 1:nrow(rd_frame)){
  rd_frame[k,num_trees+4]<-sum(rd_frame[k,4:num_trees+3])/rd_frame[k,3]
}
r_frame<-rd_frame[c(1,2,num_trees+4)]
colnames(r_frame)<-c("variable" , "root_variable" ,"r")
##################################################
type<-data.frame(label=c(colindex[-14],'Pt_Util'),
                 type=c(rep('MEA_param',3),rep('Slurry_Param',6),rep('Operating_param',4),rep('Target',1)),
                 color=c(rep('#98dbef',3),rep('#a4e192',6),rep('#ffc177',4),
                         rep('#ffb6d4',1)))
nodes<-data.frame(id=c(1:length(colindex)),label=c(colindex[-14],'Pt_Util'))
nodes<-merge(nodes,type,all.x=T)
nodes<-arrange(nodes,nodes['id'])
edges<-cbind(r_frame,c(rep('x-x',nrow(r_frame))))
colnames(edges)<-c('Source','Target','Weight','Type')
edges[is.na(edges)]<-0
edges[3]<-edges[3]/max(edges[3])
edges[3][edges[3]<0.5]<-0
edges<-edges[-which(edges[3]==0),]
edges<-edges[-which(edges[1]==edges[2]),]
for (j in 1:nrow(edges)){
  j1<-which(edges[j,1]==edges[2])
  j2<-which(edges[j,2]==edges[1])
  j3<-intersect(j1,j2)
  if (length(j3)!=0){
    edges[j,3]<-mean(c(edges[j,3],edges[j3,3]))
    edges<-edges[-j3,]
  }
}
x_y<-data.frame(Source=c(rep('Pt_Util',13)),
                  Target=im_frame$variable,
                  Weight=im_frame[4],
                  Type=c(rep('x-y',13)))
colnames(x_y)<-c('Source','Target','Weight','Type')
edges<-rbind(edges,x_y)
edges[3][edges[3]<=0]<-0
for (j in 1:nrow(nodes[1])){
  edges[edges==nodes[j,1]]<-nodes[j,2]
}
##################################################
interactions_frame_rel <- min_depth_interactions(forest,mean_sample = "relevant_trees", uncond_mean_sample = "relevant_trees")
interactions_frame <- min_depth_interactions(forest)


plot_min_depth_distribution(forest)
plot_min_depth_interactions(interactions_frame)
plot_min_depth_interactions(interactions_frame_rel)
##################################################
write.csv(interactions_frame,paste0('test/inter_sum','.csv'),row.names=FALSE,fileEncoding='UTF-8')
##################################################
total_inter_counts<-c(3162, 2894, 3174, 2380, 2062, 1212, 0, 2585, 2758, 1636, 723, 672, 1101)
total_inter_counts<-100*(total_inter_counts/max(total_inter_counts))*(total_inter_counts/max(total_inter_counts))*(total_inter_counts/max(total_inter_counts))*(total_inter_counts/max(total_inter_counts))
nodes$total_inter<-c(total_inter_counts,max(total_inter_counts))
##################################################
#write.csv(nodes,paste0('test/nodes----','Pt_Util','.csv'),row.names=FALSE,fileEncoding='UTF-8')
#write.csv(edges,paste0('test/edges----','Pt_Util','.csv'),row.names=FALSE,fileEncoding='UTF-8')
##################################################
#EDG<-read_csv('test/edges----Pt_Util.csv')
#NDS<-read_csv('test/nodes----Pt_Util.csv')
##################################################
EDG<-edges
NDS<-nodes
EDG[1]<-sapply(EDG[1],as.numeric)
EDG[2]<-sapply(EDG[2],as.numeric)
EDG[1]<-EDG[1]-1
NDS[2]<-NDS[2]-1
EDG[3]<-10*EDG[3]*EDG[3]*EDG[3]*EDG[3]
red_index<-((EDG $ Weight> (nthmax(EDG$Weight,21)))&(EDG$Source!=13)&(EDG$Target!=13))[,1]
##################################################
NETWORK <- forceNetwork(Links = EDG,#线性质数据�?  
                        Nodes = NDS,#节点性质数据�?  
                        Source = "Source",#连线的源变量  
                        Target = "Target",#连线的目标变�?  
                        Value = "Weight",#连线的粗细�?  
                        NodeID = "label",#节点名称  
                        Group = "type",#节点的分�?  
                        Nodesize = "total_inter" ,#节点大小，节点数据框�?  
                        ###美化部分 
                        charge=-4000,
                        fontFamily="Arial",#字体设置�?"华文行楷" �?  
                        fontSize = 40, #节点文本标签的数字字体大小（以像素为单位）�?  
                        linkColour=ifelse(red_index,"red","black"),#连线颜色,black,red,blue,    
                        colourScale=JS("d3.scaleOrdinal(d3.schemeCategory10);"), #节点颜色,red，蓝色blue,cyan,yellow�?  
                        #linkWidth,#节点颜色,red，蓝色blue,cyan,yellow�?  
                        #charge = -100,#数值表示节点排斥强度（负值）或吸引力（正值）    
                        opacity = 0.9,  
                        legend=F,#显示节点分组的颜色标�?  
                        arrows=F,#是否带方�?  
                        #bounded=F,#是否启用限制图像的边�?  
                        #opacityNoHover=1.0,#当鼠标悬停在其上时，节点标签文本的不透明度比例的数�? 
                        opacityNoHover=TRUE, #显示节点标签文本
                        #zoom = T#允许放缩，双击放�? 
                        width = 1200, height = 1200
)
NETWORK
saveNetwork(NETWORK,"networkRF500.html",selfcontained=TRUE)#save HTML

