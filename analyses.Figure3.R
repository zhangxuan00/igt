###Serum metabolomics characteristics among DM, IGT, and NGT groups
###analyses.Figure3
met_group <- read.csv("data/Serum_metabolomics.csv",row.names = 1,header = T)
met<- scale(met_group[,2:281],center = T,scale = T)
group <- read.csv("data/group.csv",row.names = 1,header = T)
library("FactoMineR")
library(factoextra)
library("vegan")
otu.adonis <- adonis2(met_group[,2:281]~ group$group,  method = "bray",permutations = 999)
otu.adonis
library(ggplot2)
library(ggrepel)

pca1 <- prcomp(met,center = F,scale. = F)
df1 <- pca1$x 
df1 <- as.data.frame(df1) 
summ1 <- summary(pca1)
xlab1 <- paste0("PCA1(",round(summ1$importance[2,1]*100,2),"%)")
ylab1 <- paste0("PCA2(",round(summ1$importance[2,2]*100,2),"%)")
pca=data.frame(df1[,1:2])
colnames(pca)=c("PCA1","PCA2")
pca_data=cbind(group,pca[match(rownames(group),rownames(pca)),])
pca_data$group <- factor(pca_data$group,  c("NGT","IGT","DM"))
library(ggpubr)
library(ggplot2)
library(ggrepel)

p1 <- ggscatter(pca_data, x= "PCA1", y = "PCA2", color = "group", 
                ellipse.alpha = 0.36,ellipse.level = 0.6,
                palette = c("#3f71a2","#9d1e1e","#328332"),
                ellipse = F,mean.point = F,star.plot = F,
                size = 2, ggtheme = theme_minimal())
p1
p2 <- p1 +
  stat_ellipse(aes(x = PCA1, y = PCA2, color = group), level = 0.95, linetype = 2, show.legend = FALSE) +labs(x = xlab1,y = ylab1)
p2

p4<-p2+theme(panel.background = element_rect(fill='white', colour='black'),
             panel.border = element_blank(),
             title = element_text(face = 'bold',size=15),
             axis.line = element_line(colour = "black"), 
             axis.title = element_text(face = 'bold',size=12),
             axis.ticks = element_line(color='black'),
             axis.title.x=element_text(colour='black', size=12),
             axis.title.y=element_text(face = 'bold', size=12),
             axis.text=element_text(colour='black',size=12),
             legend.title=element_blank(),
             legend.text=element_text(face = 'bold',size=12),
             legend.key=element_blank(),legend.position = c(),
             axis.text.y = element_text(face = 'bold',size=12),
             axis.text.x  = element_text(face = 'bold',size=12))
p4
ggsave("pca.pdf",width = 6,height = 4,units = "in")
dev.off()

library(ropls)
mest=cbind(group,met[match(rownames(group),rownames(met)),])
df<- scale(mest[,2:281],center = F,scale = F)
#使用opls函数进行PLS-DA分析
plsda <- opls(df,mest$group,orthoI=0,predI=3,fig.pdfC="plsda.pdf")
#提取结果利用ggplot进行绘图展示
pdata <- data.frame(Sample=rownames(plsda@scoreMN),Group=mest$group,component1=plsda@scoreMN[,1],component2=plsda@scoreMN[,2],component3=plsda@scoreMN[,3])
pdata$Group <- factor(pdata$Group,
                      c("NGT","IGT","DM"))
xlab <- paste("component1(",round(plsda@modelDF[1,1]*100,2),"%)")
ylab <- paste("component2(",round(plsda@modelDF[2,1]*100,2),"%)")
library(ggplot2)
library(ggpubr)
library(ggplot2)
library(ggrepel)
p1 <- ggscatter(pdata, x= "component1", y = "component2", color = "Group", ellipse.alpha = 0.36,ellipse.level = 0.6,palette = c("#3f71a2","#9d1e1e","#328332","#864ea2"),ellipse = F,mean.point = F,star.plot = F,size = 2, ggtheme = theme_minimal())
p1
p2 <- p1 +
  stat_ellipse(aes(x = component1, y = component2, color = Group), level = 0.95, linetype = 2, show.legend = FALSE) 
p2

p3=p2+theme(panel.background = element_rect(fill='white', colour='black'),panel.border = element_blank())+geom_vline(xintercept=0,colour="grey",linetype="dashed")+geom_hline(yintercept=0,colour="grey",linetype="dashed")+labs(x=xlab,y=ylab)
p3
p4<-p3+theme(panel.background = element_rect(fill='white', colour='black'),
             panel.border = element_blank(),
             title = element_text(face = 'bold',size=15),
             axis.line = element_line(colour = "black"), 
             axis.title = element_text(face = 'bold',size=12),
             axis.ticks = element_line(color='black'),
             axis.title.x=element_text(colour='black', size=12),
             axis.title.y=element_text(face = 'bold', size=12),
             axis.text=element_text(colour='black',size=12),
             legend.title=element_blank(),
             legend.text=element_text(face = 'bold',size=12),
             legend.key=element_blank(),legend.position = c(),
             axis.text.y = element_text(face = 'bold',size=12),
             axis.text.x  = element_text(face = 'bold',size=12))
p4
ggsave("PLS-DA.pdf",width = 6,height = 4,units = "in")
dev.off()
vip <- data.frame(Variable=names(plsda@vipVn),VIP=plsda@vipVn)
vip <- vip[order(vip$VIP,decreasing=T),]
vip$Variable <- factor(vip$Variable,levels=vip$Variable)
vip <- vip[which(vip$VIP >= 1), ]
write.csv(vip,"vip.csv")
