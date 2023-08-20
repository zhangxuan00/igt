###Significant differential multi-omics characteristics of IGT development to DM after 4-year follow-up.
###analyses.Figure5
library(ggplot2)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(ggbreak)
library(ggprism)
data= read.csv("data/igt_data.csv",row.names = 1,header = T)
yanse=c("#6cc396","#9896ca")
H = max(data$mean)*2
p4 = ggplot(data,aes(x =key,y = mean,fill = group))+ 
  geom_bar(stat = "identity" ,width=0.5, position = position_dodge(0.6)) + 
  geom_errorbar(aes(ymin = mean - se,ymax = mean+se),
                position = position_dodge(0.6), width = 0.3,color="black",lwd=1)+
  labs(x='',y='Relative Abundance(%)')+coord_flip()+
  scale_y_continuous(expand = c(0,0),limits = c(0,H))+
  scale_fill_manual(values = yanse, guide = guide_legend(title = NULL))+
  ggprism::theme_prism()+theme(axis.text.x = element_text(angle = 90))
p4
