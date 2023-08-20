###Gut microbial characteristics among DM, IGT, and NGT groups##
##analyses.Figure1
speciese <- read.csv("data/speciesmg.csv",header=T,row.names = 1)
speciese <- data.frame(speciese[,-1:-6])
group <- read.csv("data/design.csv",header = T,row.names = 1)
library(vegan)
speciese=data.frame(t(speciese))
alpha <- function(speciese){
  est <- estimateR(speciese*1000000000)
  Chao1 <- est[2, ]
  Shannon <- diversity(speciese,index = "shannon")
  result <- data.frame(Chao1,Shannon)
}
alpha_all <- alpha(speciese)
index =cbind(alpha_all,group[match(rownames(alpha_all),rownames(group)),])
colnames(index) <- c("Chao1","Shannon","group")
library(ggplot2)
library(ggpubr)
#######Figure1A
wt<- compare_means(Shannon~group,data = index,method = "wilcox.test",ref.group = "IGT")
write.csv(wt,"Shannon_comapre_wt.csv")
kt <- compare_means(Shannon~group,data = index,method = "kruskal.test")
write.csv(kt,"Shannon_comapre_kt.csv")
p1 <- ggviolin(data = index, x="group",y="Shannon",color="group",main="Shannon index",  palette = c("#3f71a2","#9d1e1e","#328332"),add = "boxplot",order = c("NGT","IGT","DM"))
p1
data1<-list( c("DM", "IGT"),c("NGT","IGT"))
p2<-p1+stat_compare_means(method="wilcox.test",comparisons =data1,hide.ns = T,
                          label =  "p.signif",label.y = c(4.2,4.3),
                          alternative = "greater")+ylab("Shannon index")
p2
p4<-p2+theme(panel.background = element_rect(fill='white', colour='black'),
             panel.grid=element_blank(), 
             axis.title = element_text(color='black',size=10),
             axis.ticks = element_line(color='black'),
             axis.line = element_line(colour = "black"), 
             axis.title.x=element_text(face="bold",colour='black', size=10),
             axis.title.y=element_text(face="bold",colour='black', size=10),
             axis.text=element_text(colour='black',size=10),
             legend.title=element_blank(),
             legend.text=element_text(face="bold",size=10),
             legend.key=element_blank(),legend.position = c())
p4

ggsave("Shannon.pdf",width = 5,height = 4,units = "in")
dev.off()

####Figure1B
bray_dis <- vegdist(speciese, method = 'bray')
#PCoA 排序，详情 ?cmdscale
pcoa <- cmdscale(bray_dis, k = (nrow(speciese) - 1), eig = TRUE)
#提取前两轴的贡献度
pcoa_exp <- pcoa$eig/sum(pcoa$eig)
pcoa1 <- paste('PCoA axis1 :', round(100*pcoa_exp[1], 2), '%')
pcoa2 <- paste('PCoA axis2 :', round(100*pcoa_exp[2], 2), '%')
#提取前两轴的的坐标，并添加样本的分组信息
site <- data.frame(pcoa$point)[1:2]
plotdata = cbind(site, group[match(rownames(site), rownames(group)), ])
names(plotdata)[1:3] <- c('pcoa1', 'pcoa2','group')
#ggplot2 绘制二维平面图展示 PCoA 结果
library(ggplot2)
library(ggrepel)

library(ggpubr)
library(ggplot2)
library(ggrepel)
plotdata$group <- factor(plotdata$group,c("NGT","IGT","DM"))

p1 <- ggscatter(plotdata, x= "pcoa1", y = "pcoa2", color = "group", 
                ellipse.alpha = 0.36,ellipse.level = 0.95,
                palette = c("#3f71a2","#9d1e1e","#328332"),
                ellipse =F,mean.point = F,star.plot = F,
                size = 2, ggtheme = theme_minimal())
p1
group_average <- aggregate(cbind(pcoa1, pcoa2)~group, data = plotdata, FUN = mean)
p2 <- p1 +
  stat_ellipse(aes(x = pcoa1, y = pcoa2, color = group), level = 0.95, linetype = 2, show.legend = FALSE) 
p2
p3 <- p2 +
  annotate('text', label = 'PERMANOVA', x = 0.35, y = 0.5, size = 3) +
  annotate('text', label = sprintf(adonis_group$P[1,5]), x = 0.35, y = 0.45, size = 3, parse = TRUE)
p3
p4 <- p3+labs(x = paste(pcoa1),y = paste( pcoa2))+
  geom_hline(yintercept = 0,lty=4,lwd=0.6,alpha=0.8)+
  geom_vline(xintercept = 0,lty=4,lwd=0.6,alpha=0.8)
p4
p5<-p4+theme(panel.background = element_rect(fill='white', colour='black'),
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
p5
#ggsave("PCoA.pdf",width = 6,height = 4,units = "in")
dev.off()


