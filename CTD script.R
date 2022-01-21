
library(ggplot2)


tiff("CTD_data", units="in", width=10, height=5, res=300)

bp<-ggplot(CTD_Data) +
  geom_path (aes(x=X, y=Djup, color=Parameter), size=2)+
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line.x = element_line(),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.ticks.x=element_blank(),
        axis.line.y=element_line(),
        axis.title=element_text(size=16,face="bold"),
        strip.text.x=element_text(size=24),
        legend.text=element_text(size=14),
        legend.position="right")+
  ylab("Djup (m)")+
  ylim(0,0) +
  scale_x_continuous(position = "top")
 bp 
  
 dev.off()



