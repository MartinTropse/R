library(ggplot2)
library(reshape2)

###Automatised script for semiseperated CTD-csv file###
rm(list=ls())
setwd("C:/Base/LittlePrjct/CTD/December_2021/CTD-data/Excel-filer")

wndPark=readline() #Enter the project windpark

df=read.csv("FC201326_99.CSV", sep =';')
ID=which(grepl("Vbatt", df[,1])) #Identify line with headnames by looking for "Vbatt"

df=df[ID[2]:dim(df)[1],]
headList = list()

for(aPos in seq(1, dim(df)[2],1)){ #Combines the two rows into one headline row
  val1=gsub(" ","",df[1,aPos])
  val2=gsub(" ","",df[2,aPos])
  headList=append(headList,paste(val1, val2, sep="_"))
}

newFrame=df[3:dim(df)[1],]
colnames(newFrame) = headList
#Convert all columns to numeric which does not include Int, i.e. date and time.
ID=which(!(grepl("Int", colnames(newFrame)))) 
newFrame[ID] <- sapply(newFrame[ID],as.numeric) 
newFrame=newFrame[which(newFrame$`Press_[dbar]`>15),] #Removes lines with salinity under 1.5 promile, change in freshwater areas.   

maxBar=max(newFrame$`Press_[dbar]`)
posVec = list()
#Loop that identifies the turning and bottom position
for(thPos in seq(1, length(newFrame$`Press_[dbar]`),1)){
  if(newFrame$`Press_[dbar]`[thPos] != maxBar){
    posVec = append(posVec, thPos)
  }
  else 
  {posVec = append(posVec, thPos)
  break
  } 
}

graphDf=newFrame[unlist(posVec),]
theTime = paste(graphDf$`IntD_[Date]`[1],graphDf$`IntT_[Time]`[1])

colList = c("Press_[dbar]","SALIN_[PSU]","Temp_[°C]","DO_ml_[ml/l]")
subName <- names(newFrame)[(names(newFrame) %in% colList)]
graphDf=graphDf[,c(subName)]

graphMelt=melt(graphDf, id.vars = c("Press_[dbar]"))

bp<-ggplot(graphMelt) + geom_path(aes(y=`Press_[dbar]`, x=value, color=variable), size=1.7)
bp=bp+ylim((max(graphMelt$`Press_[dbar]`)),0)+scale_x_continuous(position = "top")+theme_bw()
bp=bp+labs(x="Salinitet (PSU)",y="Djup (m)", title=paste(wndPark,theTime))
bp=bp+theme(legend.title = element_blank(),panel.grid.major = element_line(colour = "#888888", size=0.2), panel.grid.minor = element_line(colour = "#888888", size=0.2),axis.ticks = element_blank())
bp=bp+scale_color_manual(labels = c("°C", "Salinitet (PSU)", "Syre (ml/l)"),
                         values = c("#009688", "#b3dca0", "#83d0c9"))
#"#258a63", "#ff0000", "#0a75ad" Difference color scheme. 


###Script for CTD-csv file that had a mixed of separators, including comma and multiple spaces. 
setwd("C:/Base/LittlePrjct/CTD")

df=read.csv("FC201326_100.CSV", sep =',')
aDate=df[2,]
df=as.data.frame(df[25:dim(df)[1],1])
df=as.data.frame(gsub(";","",df[,1]))

headV=strsplit(df[1,], '   ')[[1]]
headL = list()

for(nm in headV){
  nm=gsub(" ", "", nm)
  if(nchar(nm)>0){
    headL=append(headL, nm)
  }
}

head2=strsplit(gsub(" ","",df[2,1]),"]")[[1]]
headP = list()

for(np in head2){
  np=gsub("\\[","",np)
  if(nchar(np)>0){
    headP=append(headP,np)
  }
}

theHead = list()
for(aNum in seq(1, length(headP),1)){
  theHead=append(theHead, paste(headL[aNum], headP[aNum], sep="__"))
}

x=gsub(" ","",df[,1])
valList = x[4:length(x)]

newFrame=data.frame(matrix(ncol=length(theHead), nrow=(length(valList)/length(theHead))))
colnames(newFrame) = theHead
theVal = 0

for(aRow in seq(1, dim(newFrame)[1])){
  for(aCol in seq(1, dim(newFrame)[2])){
    theVal = theVal + 1
    newFrame[aRow, aCol] = valList[theVal]
  }  
}  

myCol=colnames(newFrame) 
subCol=myCol[c(1:8,11:length(myCol))]
newFrame[subCol] <- sapply(newFrame[subCol],as.numeric)
newFrame=newFrame[which(newFrame$SALIN__PSU>1),]

maxSal=max(newFrame$SALIN__PSU)
posVec = list()

for(thPos in seq(1, length(newFrame$SALIN__PSU),1)){
  if(newFrame$SALIN__PSU[thPos] != maxSal){
    posVec = append(posVec, thPos)
  }
  else 
  {posVec = append(posVec, thPos)
  break
  } 
}

graphDf=newFrame[unlist(posVec),]
graphDf=graphDf[,c(2:3,7,15)]
graphMelt=melt(graphDf, id.vars = c("Press__dbar"))

bp<-ggplot(graphMelt) + geom_path(aes(y=Press__dbar, x=value, color=variable), size=2)
bp=bp+ylim(65,0)+scale_x_continuous(position = "top")+theme_bw()
bp=bp+labs(x="Salinitet (PSU)",y="Djup (m)", title=aDate)
bp=bp+theme(legend.title = element_blank())#, legend.position = "bottom")
bp=bp+scale_color_manual(labels = c("°C", "Salinitet (PSU)", "Syre (ml/l)"),
                         values = c("#009688", "#b3dca0", "#83d0c9"))



#Stored leftover code
# rm(list =ls())
# 
# library(ggplot2)
# library(foreach)
# library(reshape2)
# 
# setwd("C:/Base/LittlePrjct/CTD/December_2021/CTD-data/Excel-filer")
# 
# df=read.csv("FC201326_94.CSV", sep =';')
# ID=which(grepl("Vbatt", df[,1]))
# 
# df=df[ID[2]:dim(df)[1],]
# headList = list()
# 
# for(aPos in seq(1, dim(df)[2],1)){
#   val1=gsub(" ","",df[1,aPos])
#   val2=gsub(" ","",df[2,aPos])
#   headList=append(headList,paste(val1, val2, sep="_"))
# }
# 
# newFrame=df[3:dim(df)[1],]
# colnames(newFrame) = headList
# 
# ID=which(!(grepl("Int", colnames(newFrame))))
# newFrame[ID] <- sapply(newFrame[ID],as.numeric)
# newFrame=newFrame[which(newFrame$`SALIN_[PSU]`>1.5),]
# 
# maxBar=max(newFrame$`Press_[dbar]`)
# posVec = list()
# 
# for(thPos in seq(1, length(newFrame$`Press_[dbar]`),1)){
#   if(newFrame$`Press_[dbar]`[thPos] != maxBar){
#     posVec = append(posVec, thPos)
#   }
#   else 
#   {posVec = append(posVec, thPos)
#   break
#   } 
# }
# 
# graphDf=newFrame[unlist(posVec),]
# theTime = paste(graphDf$`IntD_[Date]`[1],graphDf$`IntT_[Time]`[1])
# 
# 
# colList = c("Press_[dbar]","SALIN_[PSU]","Temp_[°C]","DO_ml_[ml/l]")
# subName <- names(newFrame)[(names(newFrame) %in% colList)]
# graphDf=graphDf[,c(subName)]
# 
# graphMelt=melt(graphDf, id.vars = c("Press_[dbar]"))
# 
# bp<-ggplot(graphMelt) + geom_path(aes(y=`Press_[dbar]`, x=value, color=variable), size=1.8)
# bp=bp+ylim((max(graphMelt$`Press_[dbar]`)),0)+scale_x_continuous(position = "top")+theme_bw()
# bp=bp+labs(x="Salinitet (PSU)",y="Djup (m)", title=theTime)
# bp=bp+theme(legend.title = element_blank())
# bp=bp+scale_color_manual(labels = c("°C", "Salinitet (PSU)", "Syre (ml/l)"),
#                          values = c("#258a63", "#ff0000", "#0a75ad"))
