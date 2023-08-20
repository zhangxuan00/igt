###Significant differences in species abundance among DM, IGT, and NGT groups
##analyses.Figure2
speciese <- read.csv("data/speciesmg.csv",header = T,row.names = 1)
speciese <- data.frame(speciese[,-1:-6])
speciese=data.frame(t(speciese))
group <- read.csv("data/group.csv",header = T,row.names = 1)
diff = cbind(group, speciese[match(rownames(group), rownames(speciese)), ]) 
library(ggpubr)
df<-data.frame()
v <- colnames(diff)[-1]
for(i in v){
  a <- compare_means(as.formula(paste(i, "~group")), diff, "wilcox.test")
  df <- rbind(df, a)
}
wilcox <- data.frame(df)
inData0 <- data.frame(wilcox[which(wilcox$p.adj < 0.05), ])
head(inData0)
library(dplyr)
diff = cbind(group, data[match(rownames(group), rownames(data)), ]) 
library(dplyr)
df2 <- group_by(diff,group)%>%summarise_all(funs(mean))
df2 <- data.frame(df2)
rownames(df2)<- df2[,1]
df2 <- df2[,-1]
df2 <- data.frame(t(df2))
order = c('NGT','IGT','DM')
df2=df2[order]
library(pheatmap)
mycol<-colorRampPalette(c("navy","white","firebrick3"))(100)
p=pheatmap(df2,scale = "row",cluster_row =T, 
         cluster_col = F,border_color=NA,
         cellwidth =15, cellheight =10,color=mycol,
         fontface="bold",fontsize_row=8,fontsize_col = 8,angle_col=0)
p

