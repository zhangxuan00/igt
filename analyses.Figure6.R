###Classification of non -DM and DM status by relative abundance of candidate microbial species and serum metabolites marker after 4-year follow-up
###analyses.Figure6
library(randomForest)
library(datasets)
library(caret)
sp <- read.csv("data/igt_data_speciese.csv",header = T,row.names = 1)
met <- read.csv("data/igt_data_met.csv",row.names =1,header = T)
met =data.frame(t(met))
data0 = cbind(sp,met[match(rownames(sp),rownames(met)),])
data0<- scale(data0,center = T,scale = T)
data = cbind(group,data0[match(rownames(group),rownames(data0)),])
data$group <- as.factor(data$group)
table(data$group)
set.seed(123)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.6, 0.4))
train <- data[ind==1,]
test <- data[ind==2,]
set.seed(123)
rf<- randomForest(group~., data=train, proximity=TRUE,ntree=500,importance = TRUE) 
print(rf)
traim_predict <- predict(rf, test,type = "prob")
traim_predict
#使用测试集，评估预测性能
test_predict <- predict(rf, test)
confusionMatrix(test_predict, test$group)
compare_test <- table(length(traim_predict),length(test$group),  dnn = c('Actual', 'Predicted'))
compare_test

imp= as.data.frame(rf$importance)
imp = imp[order(imp[,1],decreasing = T),]
head(imp)
imp = data.frame(importance(rf),MDA.p = rf$importanceSD[3])
head(imp) 
imp_select =imp[order(1:30,decreasing = T),]
imp_select $speciese =rownames(imp_select)
library(mgsub)
library(ggplot2)
library(pROC)
p=ggplot(data =imp_select, mapping = aes(x=reorder(id,MeanDecreaseGini),y=MeanDecreaseGini)) +geom_bar(position = position_dodge(),width = 0.5,stat = "identity",fill="steelblue")+coord_flip()
p+theme_classic() +theme(
  axis.title = element_text(size = 15),
  axis.text = element_text(size = 14),
  legend.text = element_text(size = 14))
ROC_rf <- roc(test$group, traim_predict[,2],ci=T)
ROC_rf

ROC_rf_auc <- auc(ROC_rf)
ROC_rf_auc
data <- data.frame(Specificity=ROC_rf$specificities,Sensitivity=ROC_rf$sensitivities)
data <- cbind(data, c(rep('met_sp',17)))
p<-ggplot()+
  geom_path(data=data,aes(x = 1-Specificity, y=Sensitivity),color="#D73027")+annotate(geom="text", x=0.7, y=0.3,label=paste0("AUC:",signif(ROC_rf_auc,2)," (95% CI:0.5835-1)"),color="#D73027")+geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), colour='grey', linetype = 'dotdash')
p
p4<-p+theme(panel.background = element_rect(fill='white', colour='black',size = 0.5, linetype = 1),rect = element_rect(fill = "white",colour = "black", size = 0.5, linetype = 1),
            line = element_line(colour = "black", size = 0.5, linetype = 1, 
                                lineend = "butt"),
            axis.title.x=element_text(size=12),
            axis.title.y=element_text(size=12),
            legend.text=element_text(size=12),
            legend.key=element_blank(),legend.position = c(),
            axis.text.y = element_text(size=12),
            axis.text.x  = element_text(size=12))
p4