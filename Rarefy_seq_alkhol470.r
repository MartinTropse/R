#Segment for sequence rarefaction (normalizing sequence depth to minimum level)
library(vegan)

setwd("P:/eDNA/eDNA BilagaScript/NMDS")

df=read.csv("Etanol470_Sekvenser.csv", sep=",", fileEncoding = "UTF-8-BOM")
df=df[1:17,]

row.names(df) = df$Species
df[is.na(df)] = 0
df=subset(df,select = -Species)
df=as.data.frame(t(df))
minRare=min(rowSums(df))
sum(rowSums(df))
check1<-rrarefy(df, minRare)

write.csv(check1, "seqDataRarefied_AlkoholPek.csv", fileEncoding = "UTF-8")


###NMDS###
library(vegan)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(gapminder)
library(reshape2)

df1=read.csv("seqDataRarefied_AlkoholPek1.csv", fileEncoding = "UTF-8-BOM", row.names = 1)

env = df1[,19]
spc[spc>0] = 1
spcPA=spc
spc = df1[,1:17]

#Standard NMDS function. Check the content in spcCA and env data to see ~suitable data format. 
NMDS1=metaMDS(spcPA, distance = "bray", k = 2)
NMDS_Scr = as.data.frame(scores(NMDS1))
NMDS_Scr$Env=env
NMDS1$stress

numSeq=c(1,2,3,4,5,6,7,9,11,12,13,14,15,17,19) #A value for each sample that is duplicated, used in order to group the samples 
NMDS_Scr$ID_Hook = rep(numSeq, each=2)

NMDS_Scr$Env=gsub("A","Day 1", as.character(NMDS_Scr$Env))
NMDS_Scr$Env=gsub("B","Day 470", as.character(NMDS_Scr$Env))
NMDS_Scr$Env=as.factor(NMDS_Scr$Env)     
NMDS_Scr$ID_label = ifelse(NMDS_Scr$Env == "Day 1", NMDS_Scr$ID_Hook, NA)

#NMDS_Scr$Sample=gsub("(_|_(B|Y))", "",NMDS_Scr$Sample)
labX=tapply(NMDS_Scr$NMDS1, NMDS_Scr$ID_Hook,function(x)sum(x)/2)
labY=tapply(NMDS_Scr$NMDS2, NMDS_Scr$ID_Hook,function(x)sum(x)/2)
xList = list()
yList = list()

for(x in labX){
  xList=append(xList,x)
  xList=append(xList,NA)
}

for(y in labY){
  yList=append(yList,y)
  yList=append(yList,NA)
}

NMDS_Scr$LabelX = unlist(xList)
NMDS_Scr$LabelY = unlist(yList)

plotNm = ggplot(data=NMDS_Scr)
plotNm = plotNm + geom_line(aes(x=NMDS1,y=NMDS2,group = ID_Hook),color = "grey", size = 0.4)
plotNm = plotNm + geom_point(aes(x=NMDS1,y=NMDS2, color = Env), size =4)
plotNm = plotNm + labs(subtitle = "NMDS fish community, presence/absence")
plotNm = plotNm + geom_label(aes(x=LabelX, y=LabelY, label = ID_label),size = 2, label.padding = unit(0.10, "lines"),)+theme_bw(13)
plotNm = plotNm + guides(color=guide_legend(title = "Conservation treatments"))
plotNm = plotNm + theme(legend.position = "bottom", legend.title = element_text(size = 14), plot.subtitle = element_text(size = 15))+labs(x="Dimension 1", y="Dimension 2") 