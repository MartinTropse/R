library(ggplot2)
library(reshape2)
library(stringr)
library(viridis)
library(tidyr)
library(foreach)
library(dplyr)
library(lubridate)

rm(list=ls())
setwd("C:/Bas/CTD/CTD_Data/Aurora_Sep2021")

dfStack=data.frame(matrix(nrow=0,ncol = 4))
colnames(dfStack) = c("Press_[dbar]","TimeID","variable","value")

wndPark=readline() #Enter the name for project, e.g. "Aurora sep 2021"

files=list.files()
fileStr=files[grepl("(.csv$|.CSV$)", files, perl = TRUE)]


#Section that attempts to rebuild CTD-data  into a consistent format
#First rebuild attempt
for(file in fileStr){
  df=read.csv(file, sep =',')
  if(dim(df)[1]<400){
    print(paste("Skipping file:", file))
    next
  }
  if(dim(df)[2]<2){
    df=read.csv(file, sep =';')
  }
  if(dim(df)[2]<2){
    print(paste0("We got a tricky one: ", file))
    df=read.csv(file, sep =',')
    aDate=df[2,]
    df=as.data.frame(df[25:dim(df)[1],1])
    df=as.data.frame(gsub(";","",df[,1]))
    
    headV=strsplit(as.character(df[1,]),"   ")[[1]]
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
    
    write.table(newFrame,file,fileEncoding = "UTF-8", row.names = FALSE, sep = ";")
    print(paste("Exported file", file))
  }
}

#Remove files that were not altered in the folder, then continue with second loop

#Second rebuild attempt 
files=list.files()
fileStr=files[grepl("(.csv$|.CSV$)", files, perl = TRUE)]

for(file in fileStr){
  print(paste("Running file: ", file))
  df=read.csv(file, sep =';')
  if(dim(df)[2] > 10 & grepl("Vbatt.+", colnames(df)[1], ignore.case = TRUE)){
    print(paste("Dude, this one is already fantastic:", file))
    next
  }
  if(dim(df)[2] <2){
    print(paste("Differnt data format in:",file))
    df=read.csv(file, sep=',')
  }
  ID=which(grepl("Vbatt", df[,1])) #Identify line with headnames by looking for "Vbatt"
  df=df[ID[2]:dim(df)[1],]
  headList = list()
  subList = list()
  print("Starting head loop!")
  for(aPos in seq(1, dim(df)[2],1)){ #Combines the two rows into one headline row
    if(nchar(as.character(df[1,aPos]))>21){
      print("Found a long one!")
      subHead1=lapply(as.list(strsplit(as.character(df[1,aPos]), "    ")[[1]]),function(x) gsub(" ", "",x)) 
      subHead2=lapply(as.list(strsplit(as.character(gsub("\\]","]!",df[2,aPos])), "!")[[1]]),function(x) gsub(" ", "",x))
      foreach(x = subHead1, y = subHead2) %do% {
        subList = append(subList, paste(x, y, sep = "_"))
      }
      headList = append(headList, subList)
    } else {
      val1=gsub(" ","",df[1,aPos])
      val2=gsub(" ","",df[2,aPos])
      headList=append(headList,paste(val1, val2, sep="_"))
    }
  }
  newFrame=df[3:dim(df)[1],]
  colChk=apply(newFrame[1,], 2, function(x) grepl("[0-9]+\\.[0-9]+\\,[0-9]+:[0-9]+:[0-9]+\\,[0-9]+-[0-9]+-[0-9]+\\,",x))
  if(sum(colChk>0)){
    kolNamn=names(newFrame[colChk])
    newFrame=tidyr::separate(data=newFrame, col=kolNamn, sep = ",", into = c("O_P_[hPa]", "IntT_[Time]", "IntD_[Date]"))
  }
  colnames(newFrame) = headList
  newFrame=as.data.frame(apply(newFrame, 2, function(x) gsub(",","",x)))
  print(paste("Exporting file:", file))
  write.table(newFrame,file,fileEncoding = "UTF-8", row.names = FALSE, sep = ";")
}



#Create and export individual CTD-graphs for each file in the folder
for(file in fileStr){
  df=read.csv(file, sep =';')
  print(paste("Dataset dimensions:",file,dim(df)[1],dim(df)[2]))
  Sys.sleep(0.3)
  if(dim(df)[1]>200){
    names(df)=gsub("Press.+","Press_[dbar]",names(df), ignore.case = TRUE)
    names(df)=gsub("SALIN.+","SALIN_[PSU]",names(df), ignore.case = TRUE)
    names(df)=gsub("Temp.+","Temp_[°C]",names(df), ignore.case = TRUE)
    names(df)=gsub("DO_ml.+","DO_ml_[ml/l]",names(df), ignore.case = TRUE)
    names(df)=gsub("intt.+","IntT_[Time]",names(df), ignore.case = TRUE)
    names(df)=gsub("intd.+","IntD_[Date]",names(df), ignore.case = TRUE)
    newName=str_extract(file,"^[^.csv]*")
    pdfName=paste0(newName,"_",df$`IntD_[Date]`[1],"_",df$`IntT_[Time]`[1],".pdf")
    pdfName=gsub("(-|:)","",pdfName)
    graphDf=df[c("Press_[dbar]","SALIN_[PSU]","DO_ml_[ml/l]","Temp_[°C]")]
    graphMelt=melt(graphDf, id.vars = c("Press_[dbar]"))
    bp<-ggplot(graphMelt) + geom_path(aes(y=`Press_[dbar]`, x=value, color=variable), size=1.7)
    bp=bp+ylim((max(graphMelt$`Press_[dbar]`)),0)+scale_x_continuous(position = "top")+theme_bw()
    bp=bp+labs(x="Salinitet (PSU)",y="Djup (m)", subtitle="")
    bp=bp+theme(legend.title = element_blank(),panel.grid.major = element_line(colour = "#888888", size=0.2), panel.grid.minor = element_line(colour = "#888888", size=0.2),axis.ticks = element_blank())
    bp=bp+scale_color_manual(labels = c("°C", "Salinitet (PSU)", "Syre (ml/l)"),
                             values = c("#009688", "#b3dca0", "#83d0c9"))
    ggsave(bp, filename = pdfName, device = cairo_pdf, 
           width = 9, height = 16, units = "in")
  }
  else{
    print(paste("Uncomplete measurement in ,",file," skipping to next!"))
    next
  }
}


#Stack the data into one data frame for summary graphs
for(file in fileStr){
  newFrame = read.csv(file, sep=";", fileEncoding = "UTF-8")
  Sys.sleep(0.3)
  if(dim(newFrame)[1]<50){
    print(paste("Uncomplete measurement in ,",file," skipping to next!"))
    next
  }
  names(newFrame)=gsub("Press.+","Press_[dbar]",names(newFrame), ignore.case = TRUE)
  names(newFrame)=gsub("SALIN.+","SALIN_[PSU]",names(newFrame), ignore.case = TRUE)
  names(newFrame)=gsub("Temp.+","Temp_[°C]",names(newFrame), ignore.case = TRUE)
  names(newFrame)=gsub("DO_ml.+","DO_ml_[ml/l]",names(newFrame), ignore.case = TRUE)
  names(newFrame)=gsub("intt.+","IntT_[Time]",names(newFrame), ignore.case = TRUE)
  names(newFrame)=gsub("intd.+","IntD_[Date]",names(newFrame), ignore.case = TRUE)
  ID=which(!(grepl("^int.+", colnames(newFrame),ignore.case = TRUE))) 
  newFrame[ID] <- sapply(newFrame[ID],as.numeric) 
  newFrame=newFrame[which(newFrame$`Press_[dbar]`>1.0),] #Removes lines with salinity under 1.5 promile, change in freshwater areas.   
  maxBar=max(newFrame$`Press_[dbar]`)
  posVec = list()
  #Loop that identifies the turning and bottom position
  for(thPos in seq(1, length(newFrame$`Press_[dbar]`),1)){
    if(newFrame$`Press_[dbar]`[thPos] < maxBar-1.0){
      posVec = append(posVec, thPos)
    } else {
      posVec = append(posVec, thPos)
      break
    } 
  }    
  myDf=newFrame[unlist(posVec),]
  theTime = paste(myDf$`IntD_[Date]`[1],myDf$`IntT_[Time]`[1])
  colList = c("Press_[dbar]","SALIN_[PSU]","Temp_[°C]","DO_ml_[ml/l]")
  subName <- names(newFrame)[(names(newFrame) %in% colList)]
  myDf=myDf[,c(subName)]
  myDf$TimeID=rep(theTime, dim(myDf)[1]) 
  myMelt=melt(myDf, id.vars = c("Press_[dbar]","TimeID"))
  print(paste("Stacking data", file))
  dfStack=rbind(myMelt, dfStack)
}


#Optional rows for subsetting the data frame into smaller sections
unique(dfStack$variable)
subStack=dfStack[dfStack$variable == "SALIN_[PSU]",] #Line if you want to separate one CTD-variable 

dfStack$TimeID=strptime(dfStack$TimeID, "%Y-%m-%d %H:%M:%S") #Line if you want split data from a time point
subStack=dfStack %>% 
  filter(TimeID > ymd_hms("2021-09-08 20:00:00"))


#Section for creatign summary graphs 
gp=ggplot(subStack) + geom_path(aes(y=`Press_[dbar]`, x=value, color=TimeID),size=0.3)
gp=gp+ylim((max(subStack$`Press_[dbar]`)),0)+scale_x_continuous(position = "top")+theme_bw()
gp=gp+facet_wrap(~variable)
gp=gp+labs(x="Salinitet (PSU)",y="Djup (m)", subtitle=paste("CTD-profiler:", wndPark))
gp=gp+theme(legend.position = "none",legend.title = element_blank(),panel.grid.major = element_line(colour = "#888888", size=0.2), panel.grid.minor = element_line(colour = "#888888", size=0.2),axis.ticks = element_blank(), axis.text.x = element_text(size = 6), strip.text =  element_text(size = 7))
gp=gp+scale_color_viridis(discrete = TRUE)

np=ggplot(subStack) + geom_path(aes(y=`Press_[dbar]`, x=value, color=variable),size=0.6)
np=np+ylim((max(subStack$`Press_[dbar]`)),0)+scale_x_continuous(position = "top")+theme_bw()
np=np+facet_wrap(~as.factor(TimeID))
np=np+labs(x="Salinitet (PSU)",y="Djup (m)", subtitle=paste("CTD-profiler: ", wndPark))
np=np+theme(legend.title = element_blank(),legend.position = "bottom",panel.grid.major = element_line(colour = "#888888", size=0.2), panel.grid.minor = element_line(colour = "#888888", size=0.2),axis.ticks = element_blank(), axis.text = element_text(size = 8),  axis.title = element_text(size = 10),strip.text =  element_text(size = 6))
np=np+scale_color_manual(values = c("#ffad60", "#3b5998", "#88d8b0"), labels = c("°C", "Salinitet (PSU)", "Syre ml/l"))





#Example of possible code for replacing time points with respective stations names. 
# timePoint=c("2021-09-08 12:22:26","2021-09-08 13:02:29","2021-09-08 13:56:02","2021-09-08 16:32:06","2021-09-08 18:10:36","2021-09-09 09:47:21","2021-09-09 14:37:49","2021-09-09 16:30:31","2021-09-09 17:20:06","2021-09-10 11:19:22","2021-09-10 12:09:42","2021-09-10 13:52:32","2021-09-10 14:43:51", "2021-09-10 16:06:32","2021-09-10 17:06:51")
# stationID=c("GKK 22","GKK 20","GKK 18","GKK 16","GKK 17","GKK 40","GKK 38","GKK 50","GKK 32","GKK 49","GKK 43","GKK 29","GKK 46","GKK 26","GKK 23")
# 
# timePoint=c("2021-09-08 14:49:30","2021-09-08 15:38:09","2021-09-11 13:04:23","2021-09-11 13:59:30")
# stationID=c("GKK 13","GKK 10", "GKK 08","GKK 04")
# dfStack$TimeID=gsub("08-09-2021 12:22:26", "2021-09-08 12:22:26", dfStack$TimeID)
# 
# subStack = data.frame(matrix(nrow=0, ncol=5))
# names(subStack) =  names(dfStack)
# 
# foreach(aTime = timePoint, station = stationID) %do% {
#    atDf = dfStack[which(dfStack$TimeID == aTime),]
#    atDf$Station = rep(station, dim(atDf)[1])
#    subStack = rbind(atDf, subStack)
# }
# unique(subStack$variable)