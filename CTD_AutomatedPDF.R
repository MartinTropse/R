rm(list=ls())

library(ggplot2)
library(reshape2)
library(viridis)

monthYear = readline()
park = readline()

path = "P:/CTD_Vattenprovtagning_Skript_Grafer/Aurora_September_2021/Konverterat_till_CSV"
setwd(path)
files = list.files()
fileStr=files[grepl("(.csv$|.CSV$)", files, perl = TRUE)]

for(file in fileStr){
  df=read.csv(file, sep =',')
  cat(paste("Processing: ",file,"\n"))
  Sys.sleep(0.5)
  ID=which(grepl("Vbatt", df[,1])) #Identify line with headnames by looking for "Vbatt"
  df=df[ID[2]:dim(df)[1],1]
  headList = list()
  df=as.data.frame(gsub(";","",df))
  x=as.character(df[1,])
  headV=strsplit(x,"   ")[[1]]
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
  y=gsub(" ","",df[,1])
  valList = y[4:length(y)]
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
  ID=which(!(grepl("Int", colnames(newFrame)))) 
  newFrame[ID] <- sapply(newFrame[ID],as.numeric) 
  newFrame=newFrame[which(newFrame$Press__dbar>1.5),]
  if(dim(newFrame)[1]==0){
    pdf(paste0(file,"Failed",".pdf"))
    print(paste0(file, " does not contain relevant data. Skipping to next file."))
    dev.off()
  } else{
    maxDepth=max(newFrame$Press__dbar)
    posVec = list()
    for(thPos in seq(1, length(newFrame$SALIN__PSU),1)){
      if(newFrame$Press__dbar[thPos] != maxDepth){
        posVec = append(posVec, thPos)
      }
      else 
      {posVec = append(posVec, thPos)
      break
      } 
    }
    aTime=substr(newFrame$IntT__Time[1],1,nchar(newFrame$IntT__Time[1])-3)
    pdfName=paste0(newFrame$IntD__Date[1],"_",aTime,".pdf")
    pdfNamex=gsub("(-|:)","",pdfName[1])
    fileName=paste0(path,"/",pdfNamex)
    graphDf=newFrame[unlist(posVec),]
    graphDf=graphDf[,c(2:3,7,15)]
    graphMelt=melt(graphDf, id.vars = c("Press__dbar"))
    
    bp<-ggplot(graphMelt) + geom_path(aes(y=Press__dbar, x=value, color=variable), size=1.7)
    bp=bp+ylim((max(graphMelt$Press__dbar)),0)+scale_x_continuous(position = "top")+theme_bw(24)
    bp=bp+labs(x="Temperatur, salinitet och syre",y="Djup (m)", title=paste(park, monthYear))
    bp=bp+theme(legend.title = element_blank(),panel.grid.major = element_line(colour = "#888888", size=0.2), panel.grid.minor = element_line(colour = "#888888", size=0.2),axis.ticks = element_blank())
    bp=bp+scale_color_manual(labels = c("°C", "Salinitet (PSU)", "Syre (ml/l)"),
                             values = c("#ffad60", "#3b5998", "#88d8b0"))
    ggsave(bp, filename = fileName, device = cairo_pdf, 
           width = 9, height = 16, units = "in")
    Sys.sleep(0.5)
  }
}

##88d8b0 Teal
