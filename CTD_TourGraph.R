library(ggplot2)
library(reshape2)
library(stringr)
library(viridis)

###Automatised script for semiseperated CTD-csv file###
rm(list=ls())
setwd("C:/Base/LittlePrjct/CTD/December_2021/CTD-data/Excel-filer")

dfStack=data.frame(matrix(nrow=0,ncol = 4))
colnames(dfStack) = c("Press_[dbar]","TimeID","variable","value")

wndPark=readline() #Enter the project windpark

files=list.files()
fileStr=files[grepl("(.csv$|.CSV$)", files, perl = TRUE)]

for(file in fileStr){
  df=read.csv(file, sep =';')
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
  newFrame=newFrame[which(newFrame$`SALIN_[PSU]`>1.5),] #Removes lines with salinity under 1.5 promile, change in freshwater areas.   
  
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
  graphDf$TimeID=rep(theTime, dim(graphDf)[1]) 
  graphMelt=melt(graphDf, id.vars = c("Press_[dbar]","TimeID"))
  dfStack=rbind(graphMelt, dfStack)
}  
  
bp<-ggplot(graphMelt) + geom_path(aes(y=`Press_[dbar]`, x=value, color=variable), size=1.7)
bp=bp+ylim((max(graphMelt$`Press_[dbar]`)),0)+scale_x_continuous(position = "top")+theme_bw()
bp=bp+labs(x="Salinitet (PSU)",y="Djup (m)", title=paste(wndPark,theTime))
bp=bp+theme(legend.title = element_blank(),panel.grid.major = element_line(colour = "#888888", size=0.2), panel.grid.minor = element_line(colour = "#888888", size=0.2),axis.ticks = element_blank())
#bp=bp+scale_color_manual(labels = c("°C", "Salinitet (PSU)", "Syre (ml/l)")
                         
np=ggplot(dfStack) + geom_path(aes(y=`Press_[dbar]`, x=value, color=variable),size=1.1)
np=np+ylim((max(dfStack$`Press_[dbar]`)),0)+scale_x_continuous(position = "top")+theme_bw()
np=np+facet_wrap(~TimeID)
np=np+labs(x="Salinitet (PSU)",y="Djup (m)", subtitle=paste("CTD-profiler: Aurora december"))
np=np+theme(legend.title = element_blank(),panel.grid.major = element_line(colour = "#888888", size=0.2), panel.grid.minor = element_line(colour = "#888888", size=0.2),axis.ticks = element_blank(), axis.text.x = element_text(size = 5), strip.text =  element_text(size = 6))

gp=ggplot(dfStack) + geom_path(aes(y=`Press_[dbar]`, x=value, color=TimeID),size=0.8)
gp=gp+ylim((max(dfStack$`Press_[dbar]`)),0)+scale_x_continuous(position = "top")+theme_bw()
gp=gp+facet_wrap(~variable)
gp=gp+labs(x="Salinitet (PSU)",y="Djup (m)", subtitle=paste("CTD-profiler: Aurora december"))
gp=gp+theme(legend.title = element_blank(),panel.grid.major = element_line(colour = "#888888", size=0.2), panel.grid.minor = element_line(colour = "#888888", size=0.2),axis.ticks = element_blank(), axis.text.x = element_text(size = 6), strip.text =  element_text(size = 7))
gp=gp+scale_color_viridis(discrete = TRUE)
#np=np+scale_color_manual(labels = c("°C", "Salinitet (PSU)", "Syre (ml/l)"), values = c("#258a63", "#ff0000", "#0a75ad"))


#values = c("#009688", "#b3dca0", "#83d0c9"))
#"#258a63", "#ff0000", "#0a75ad" Difference color scheme. 
