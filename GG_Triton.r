library(vegan)
library(ggplot2)
library(ggrepel)
library(janitor)
library(viridis)
library(reshape2)
library(stats)

setwd("C:/Bas/AquaBiota/Projekt/OX2/Triton/eDNA/eDNA")

#df=read.csv("Triton_eDNAFish_NMDS.csv", row.names = 1, col.names = 1)
df1 = read.csv("Triton_Spec_1.csv")
df1env = read.csv("Triton_eDNAEnv_NMDS.csv", row.names = 1)



names(df1env)[1] = "Name"

df1=as.data.frame(t(df1))
#tritonDf = df %>% row_to_names(row_number = 1)

names(df1) = df1[1,]
df1 = df1[-1,]
names(df1) = specName

colRange=seq(1,dim(df1)[2],1)

for(aCol in colRange){
  df1[,aCol]= as.numeric(as.character(df1[,aCol]))
}

nmdsTr_1=metaMDS(df1, distance= 'bray', trymax = 200)
GG_Scr1=scores(nmdsTr_1)

#DNA_GG2=merge(dfAug, dfFeb, by = "Species", all.y = TRUE)

NMDS_tGG=merge(GG_Scr1, df1env, by=0)
BigFrame = merge(df1, df1env, by=0)

meltX=melt(BigFrame, by.id = Djup)

for(x in unique(meltX$variable)){
  aBase=meltX[which(meltX$variable == x),]
  yBase = aBase[which(aBase$Djup == "Y"),]
  bBase = aBase[which(aBase$Djup == "B"),]
  print(x)
  print(t.test(yBase$value, bBase$value))
}

sill=meltX[which(meltX$variable == "Sill"),]
silly = sill[which(sill$Djup == "Y"),]
sillb = sill[which(sill$Djup == "B"),]

t.test(silly$value, sillb$value)

tapply(meltX$value, list(meltX$Djup, meltX$variable), mean)



levels(NMDS_tGG$Djup) = c("Botten", "Djup")

tapply(NMDS_tGG

gg2 = ggplot(NMDS_tGG, aes(x=NMDS1, y=NMDS2,shape=as.factor(Djup), colour=Salinitet))
gg2 = gg2 + geom_point(aes(colour=Salinitet),size=3) + theme_bw() + labs(x="Dimension 1", y="Dimension 2", title="Fisksamhälle Triton 2021 (NMDS, Djup & Salinitet)") + theme(legend.title = element_blank())+theme(plot.title = element_text(hjust = 0.5, size = 12), legend.position = "bottom")
gg2 = gg2 + scale_color_viridis()


gg2 = ggplot(NMDS_tGG, aes(x=NMDS1, y=NMDS2,shape=as.factor(Djup), colour=Y))
gg2 = gg2 + geom_point(aes(colour=Y),size=3) + theme_bw() + labs(x="Dimension 1", y="Dimension 2", title="Fisksamhälle Triton 2021 (NMDS, Djup & Salinitet)") + theme(legend.title = element_blank())+theme(plot.title = element_text(hjust = 0.5, size = 12), legend.position = "bottom")
gg2 = gg2 + scale_color_viridis()

#gg2 = gg2 + scale_color_manual(values = c("#001df7","#63ccf2"))
  
  














##GG NMDM## 
#DNA_GG=merge(dfAug, dfFeb, by = "Species")

df=as.data.frame(t(df))
names(DNA_GG) = DNA_GG[1,]
DNA_GG = DNA_GG[-1,]

colRng=seq(1,dim(DNA_GG)[2],1)

for(yCol in colRng){
  DNA_GG[,yCol]=gsub(",",".", DNA_GG[,yCol])
  DNA_GG[,yCol]=as.numeric(DNA_GG[,yCol])
}



nmdsGG_1=metaMDS(DNA_GG, distance= 'bray', trymax = 200)
GG_Scr1=scores(nmdsGG_1)

for(x in row.names(GG_Scr1)){
  aMonth=substr(row.names(GG_Scr1), start=2, stop=2) 
  aDepth=substr(row.names(GG_Scr1), start=5, stop=5)
  aSample=substr(row.names(GG_Scr1), start=1, stop=4)
}

GG_Scr1 = as.data.frame(GG_Scr1)
GG_Scr1$Month = as.factor(aMonth)
GG_Scr1$Depth = as.factor(aDepth)
GG_Scr1$Sample = as.factor(aSample)

levels(GG_Scr1$Month) = c("Aug-2020", "Feb-2021")
levels(GG_Scr1$Depth) = c("Botten", "Yta")

gg1 = ggplot(GG_Scr1, aes(x=NMDS1, y=NMDS2,shape=as.factor(Month), colour=Depth))
gg1=gg1 + geom_label_repel(aes(label = Sample),
                           box.padding   = 0.35, 
                           point.padding = 0.5,
                           segment.color = 'grey50', size=2)
gg1 = gg1 + geom_point(aes(colour=Depth),size=3) + theme_bw() + labs(x="Dimension 1", y="Dimension 2", title="Fisksamhälle GG 2020-2021 (NMDS)") + theme(legend.title = element_blank())
gg1 = gg1 + scale_color_manual(values = c("#001df7","#63ccf2"))+theme(plot.title = element_text(hjust = 0.5))

gg2 = ggplot(GG_Scr1, aes(x=NMDS1, y=NMDS2,shape=as.factor(Month), colour=Depth))
gg2 = gg2 + geom_point(aes(colour=Depth),size=3) + theme_bw() + labs(x="Dimension 1", y="Dimension 2", title="Fisksamhälle GG 2020-2021 (NMDS)") + theme(legend.title = element_blank())
gg2 = gg2 + scale_color_manual(values = c("#001df7","#63ccf2"))+theme(plot.title = element_text(hjust = 0.5))


####NMDS GG All Species####
dfAug=read.csv("Aug2020_GG.csv")
dfFeb=read.csv("Feb2021_GG.csv")

DNA_GG2=merge(dfAug, dfFeb, by = "Species", all.y = TRUE)
DNA_GG2[is.na(DNA_GG2)] = 0  

DNA_GG2=as.data.frame(t(DNA_GG2))


names(DNA_GG2) = DNA_GG2[1,]
DNA_GG2 = DNA_GG2[-1,]

colRng=seq(1,dim(DNA_GG2)[2],1)

for(yCol in colRng){
  DNA_GG2[,yCol]=gsub(",",".", DNA_GG2[,yCol])
  DNA_GG2[,yCol]=as.numeric(DNA_GG2[,yCol])
}

DNA_GG2

nmdsGG_2=metaMDS(DNA_GG2, distance= 'bray', trymax = 200)
GG_Scr2=scores(nmdsGG_2)

for(x in row.names(GG_Scr1)){
  aMonth2=substr(row.names(GG_Scr2), start=2, stop=2) 
  aDepth2=substr(row.names(GG_Scr2), start=5, stop=5)
  aSample2=substr(row.names(GG_Scr2), start=1, stop=4)
}

GG_Scr2 = as.data.frame(GG_Scr2)
GG_Scr2$Month = as.factor(aMonth2)
GG_Scr2$Depth = as.factor(aDepth2)
GG_Scr2$Sample = as.factor(aSample2)

levels(GG_Scr2$Month) = c("Aug-2020", "Feb-2021")
levels(GG_Scr2$Depth) = c("Botten", "Yta")

gg3 = ggplot(GG_Scr2, aes(x=NMDS1, y=NMDS2,shape=as.factor(Month), colour=Depth))
gg3=gg3 + geom_label_repel(aes(label = Sample),
                           box.padding   = 0.35, 
                           point.padding = 0.5,
                           segment.color = 'grey50', size=2)
gg3 = gg3 + geom_point(aes(colour=Depth),size=3) + theme_bw() + labs(x="Dimension 1", y="Dimension 2", title="Fisksamhälle GG 2020-2021 (NMDS)") + theme(legend.title = element_blank())
gg3 = gg3 + scale_color_manual(values = c("#001df7","#63ccf2"))+theme(plot.title = element_text(hjust = 0.5))

gg4 = ggplot(GG_Scr2, aes(x=NMDS1, y=NMDS2,shape=as.factor(Month), colour=Depth))
gg4 = gg4 + geom_point(aes(colour=Depth),size=3) + theme_bw() + labs(x="Dimension 1", y="Dimension 2", title="Fisksamhälle GG 2020-2021 (NMDS)") + theme(legend.title = element_blank())
gg4 = gg4 + scale_color_manual(values = c("#001df7","#63ccf2"))+theme(plot.title = element_text(hjust = 0.5))
