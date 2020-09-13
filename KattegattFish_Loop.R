library(ggplot2)
library(reshape2)
library(plyr)
library(ggpubr)
library (car)
#library(sp)
#library(raster)

setwd("E:/DataAnalys/OX2_SeasonPatternFish")
df=read.csv("Kattegatt_FishBits.csv", header = TRUE, sep=',')
levels(df)=c("Quarter 1", "Quarter 4")
o=gsub("1","Quarter 1",df$Quarter)
df$Quarter=gsub("4","Quarter 4",o)

#Exclude 2011 & 2019, which lacks quarter sampling
idY = grep("2010|2012|2013|2014|2015|2016|2017|2018", df$Year)
dfY = df[idY,]

###Report Loop###
specVec=c("Gadus morhua","Pleuronectes platessa","Clupea harengus","Scomber scombrus")

for(art in specVec){
  id = grep(art, dfY$Species)
  dfSpec=dfY[id,]
  #Create Decimal depth
  dfSpec$decDepth=round_any(dfSpec$Depth, 10)  
  #Split Data by quarter
  idQ=grep("Quarter 1", dfSpec$Quarter)
  idQ4=grep("Quarter 4", dfSpec$Quarter)
  dfQ1=dfSpec[idQ,]
  dfQ4=dfSpec[idQ4,]
  dfSpec$decDepth=round_any(dfSpec$Depth, 10)
  
  #Summary of CPUE per quarter and decimal depth 
  Q1DecDpt=melt(tapply(dfQ1$CPUE_number_per_hour, dfQ1$decDepth, mean))
  Q1DecDpt$Quarter = rep("Quarter 1",length(Q1DecDpt$Var1))
  Q4DecDpt=melt(tapply(dfQ4$CPUE_number_per_hour, dfQ4$decDepth, mean))
  Q4DecDpt$Quarter = rep("Quarter 4",length(Q4DecDpt$Var1))
  DecDptCPUE=rbind(Q4DecDpt,Q1DecDpt)
  DecDptCPUE$value=as.integer(DecDptCPUE$value)
  
  #Depth Date graph
  x<-paste("2020",substring(dfSpec$DateTime,1,5),sep = "/")
  dfSpec$NewDate= as.Date(x, "%Y/%d/%m")

  #Depth Decimal graph
  pltDQC=ggplot(data=DecDptCPUE, aes(x=as.factor(Var1), y=value, fill=as.factor(value)))
  pltDQC = pltDQC + geom_bar(stat="identity")
  pltDQC = pltDQC + facet_grid(.~Quarter)
  pltDQC = pltDQC + theme_bw(13)+ylab("Mean CPUE")+xlab("Depth")
  pltDQC = pltDQC + theme(legend.title = element_blank())
  pltDQC = pltDQC + ggtitle(paste0(art, ", Mean CPUE by depth"))
  
  pltQ_CPUE2 = ggplot(data=dfSpec, aes(y=CPUE_number_per_hour, x=NewDate, color=as.factor(Quarter)))
  pltQ_CPUE2 = pltQ_CPUE2 + geom_point()
  pltQ_CPUE2 = pltQ_CPUE2 + theme_bw(13)
  pltQ_CPUE2 = pltQ_CPUE2 + xlab("")+ylab("CPUE per hour")+theme(legend.title = element_blank())
  pltQ_CPUE2 = pltQ_CPUE2 + ggtitle(paste0(art, ", Hour CPUE by date and quarter"))
  
  mean_QCPUE=melt(tapply(dfSpec$CPUE_number_per_hour, dfSpec$Quarter, mean), value.name = "M_CPUE")
  sd_QCPUE=melt(tapply(dfSpec$CPUE_number_per_hour, dfSpec$Quarter, sd), value.name = "SD_CPUE")
  mean_QCPUE$SD_CPUE = sd_QCPUE$SD_CPUE
  
  pltQ_CPUE = ggplot(data=dfSpec, aes(x=as.factor(Quarter), y=log(CPUE_number_per_hour), fill=as.factor(Quarter)))
  pltQ_CPUE = pltQ_CPUE+geom_violin(trim = FALSE) + geom_boxplot(width=0.02,fill="white",lwd=0.1) 
  pltQ_CPUE = pltQ_CPUE + theme_bw(13)
  pltQ_CPUE = pltQ_CPUE + theme(legend.title = element_blank())+xlab("")+ylab("LogScale CPUE per hour")
  pltQ_CPUE = pltQ_CPUE + ggtitle(paste0(art, ", Hour CPUE by quarter"))  
  
  #CPUE Longitude
  longiDf=rbind(dfQ1, dfQ4)
  pltLong=ggplot(data=longiDf, aes(x=ShootLong, y=CPUE_number_per_hour, color=Quarter))
  pltLong = pltLong + geom_point(stat="identity")
  pltLong = pltLong + facet_grid(~Quarter)
  pltLong = pltLong + theme_bw(13)+xlab("Longitude")+ylab("CPUE per hour")+theme(legend.position = "none")   
  pltLong = pltLong + ggtitle(paste0(art, ", CPUE by longitude"))
  
  pltLat=ggplot(data=longiDf, aes(x=ShootLat, y=CPUE_number_per_hour, color=Quarter))
  pltLat = pltLat + geom_point(stat="identity")
  pltLat = pltLat + facet_grid(~Quarter)
  pltLat = pltLat + theme_bw(13)+xlab("Lattitude")+ylab("CPUE per hour")+theme(legend.position = "none")
  pltLat = pltLat + ggtitle(paste0(art, ", CPUE by latitude"))
  
  myPdf=ggarrange(pltLong, pltLat, pltDQC, nrow = 1, ncol = 1)
  myPdf2=ggarrange(pltQ_CPUE2, pltQ_CPUE,nrow = 2, ncol = 1)
  
  X=paste0(art,".pdf")
  ggexport(myPdf, myPdf2, filename = X)        
}


#CPUE plot Quarter
#QCPUE=melt(tapply(dfSpec$CPUE_number_per_hour, dfSpec$Quarter, sum))
#pltQC=ggplot(data=QCPUE, aes(x=as.factor(Var1), y=value, fill=as.factor(Var1)))
#pltQC = pltQC + geom_bar(stat="identity")
#pltQC = pltQC + theme_bw(12)+ylab("CPUE")+xlab("Season")
#pltQC = pltQC + theme(legend.title = element_blank())
#pltQ_CPUE = ggplot(data=mean_QCPUE, aes(x=as.factor(Var1), y=M_CPUE, fill=as.factor(Var1)))
#pltQ_CPUE = pltQ_CPUE+geom_bar(stat="identity") +  
#geom_errorbar(aes(ymin=M_CPUE-SD_CPUE, ymax=M_CPUE+SD_CPUE), width=.1,position = position_dodge(.9))  
#pltDptDate=ggplot(data=dfSpec, aes(x=NewDate, y=Depth))
#pltDptDate = pltDptDate + geom_point()
#pltDptDate = pltDptDate + theme_bw(13)+xlab("")
#pltDptDate = pltDptDate + ggtitle(paste0(art, ", CPUE by depth"))