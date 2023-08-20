###Multi-omics correlation analysis of DM, IGT, and NGT groups.
###analyses.Figure4AB
library(ggplot2)
library(tidyverse)
library(see)
head(cor.long)
head(fdr.long)
library(paletteer)
cor= read.csv("data/cor.csv",row.names = 1,header = T)
p=ggplot()+geom_point(data=cor,
                      aes(x=id,y=variable,
                          size=8,
                          color=value))+
  geom_text(data=fdr.long,
            aes(x=id,y=variable,label=Significant))+
  scale_color_paletteer_c(palette = "ggthemes::Classic Red-Blue")

p
p4<-p+theme(panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_rect(color = "black",fill = NA),
            title = element_text(face = 'bold',size=15),
            axis.line = element_line(colour = "black"), 
            axis.title = element_text(face = 'bold',size=10),
            axis.ticks = element_line(color='black'),
            axis.title.x=element_text(colour='black', size=10),
            axis.title.y=element_text(face = 'bold', size=10),
            axis.text=element_text(colour='black',size=10),
            legend.title=element_blank(),
            legend.text=element_text(face = 'bold',size=10),
            legend.key=element_blank(),legend.position = c(),
            axis.text.y = element_text(face = 'bold',size=10),
            axis.text.x  = element_text(face = 'bold',size=10,angle = 90,hjust = 1,vjust = 0.3))
p4
