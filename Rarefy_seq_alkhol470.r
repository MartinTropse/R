library(vegan)

setwd("C:/Bas/AquaBiota/Projekt/Life DNAquatics/EtanolPek")

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

df1=read.csv("seqDataRarefied_AlkoholPek1.csv", fileEncoding = "UTF-8-BOM", row.names = 1)

tapply(df1$SpeciesCount, df1$Category, median)


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

#spcPA$Env = env
#spcPA$Sample = row.names(spc)
#spcPA_Melt=melt(spcPA, var.id = c("env", "Sample"))
#chkDf=as.data.frame(tapply(spcPA_Melt$value, list(spcPA_Melt$variable,spcPA_Melt$Sample), sum))
# chkDf$BV_01_A == chkDf$BV_01_B
# chkDf$BV_05_A == chkDf$BV_03_B


NMDS1=metaMDS(spcPA, distance = "bray", k = 2)
NMDS_Scr = as.data.frame(scores(NMDS1))
NMDS_Scr$Env=env
NMDS1$stress

#NMDSRow=row.names(NMDS_Scr)
#NMDSRow=gsub("BV_","", NMDSRow)
#NMDSRow=as.factor(gsub("A","", NMDSRow))
#NMDS_Scr$Sample = env
numSeq=c(1,2,3,4,5,6,7,9,11,12,13,14,15,17,19)
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


?guides

df = gapminder %>%
  filter(year %in% c(1952,2007)) %>%
  filter(continent %in% c("Asia")) %>%
  select(country,year,lifeExp, gdpPercap)%>%
  mutate(paired = rep(1:(n()/2),each=2),
         year=factor(year))


DF <- data.frame(Time = seq(0,24,2),
                 conc= c(0, 9, 7, 6, 5.5, 5, 4, 3.75, 3.5, 2.75,2.5, 2.25, 2,
                         0, 8, 7.5, 6, 5, 4.75, 4.5, 3.2, 2.75, 2.25, 2.05, 1.5, 1.03,
                         0, 10, 9.5, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0.25),
                 Dose = rep(c(3.1, 4.4, 5.5), each = 13))
DF <- mutate(DF, Label = ifelse(Time == 24, Dose, NA),
             Dose = as.character(Dose))
ggplot(data = DF, aes(Time, conc,  group = Dose)) +
  geom_line(aes(color = Dose), size = 1.2) +
  geom_label(aes(label = Label), nudge_x = 0.35, size = 4) 

