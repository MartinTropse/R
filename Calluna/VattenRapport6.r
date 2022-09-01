rm(list = ls())
#install.packages("rJava")
library(foreach)
library(data.table)
library(tibble)
library(xlsx)
library(dplyr)
library(stringr)
#https://stackoverflow.com/questions/18511249/excel-cell-coloring-using-xlsx

setwd("C:/Calluna/Projekt/VattenprovRapport_MON")

list.files()

dfIn = read.csv("JärfällaOrdinarieVattenkemi_2021_10_01_1039.csv", fileEncoding = "UTF-8-BOM")
dfOut = read.csv("Järfälla 2021_tom sept.csv",fileEncoding = "UTF-8-BOM")

names(dfOut)=dfOut[1,]

#Clean dfOut
dfOut=dfOut[,1:97]
dfOut=dfOut[grepl("^.+$",dfOut$Provtagningsdatum),]    
dfOut=dfOut[-1,]

dfOut$ID=paste(dfOut$Märkning, dfOut$Provpunkt, sep ="|")

dfOut <- dfOut %>%
  select(ID, everything())

#dfOut=dfOut[,!(names(dfOut) %in% c("Märkning","Provpunkt"))]

###dfOut upprensning slut###
nameDf=gsub("\\.+","\\.",names(dfIn))
nameDf=gsub("\\.", "_",nameDf)
names(dfIn)=gsub("^X$", "X_0",nameDf)

xID=!(grepl("^X_[0-9]{1-2}",names(dfIn)))
colList = list()

#Builds together row 1-2 to the header (with some variation between columns) 
foreach(id = xID, aPos = seq(1, length(xID),1)) %do%{
  firstNm = names(dfIn)[aPos]
  secNm = dfIn[1,aPos] 
  if(id){
    colList = append(colList,paste(firstNm, secNm, sep='|'))
    backPos = aPos
  }
  else  {
    colList = append(colList, paste(names(dfIn)[backPos],secNm, sep='|'))
  }
}

dfIn=dfIn[-1,] #Removing the previous header row
names(dfIn) = colList #Adding the new header 

###Check columns against list to detect deviance###
baseColLst=read.csv("KolumnLista_EurofinCalluna.csv", fileEncoding = "Latin1")

posCount = 0
negList = list()
multList = list()

for(kolNamn in names(dfIn)[6:dim(dfIn)[2]]){
  aBool=grepl(kolNamn, baseColLst$EurofinColumn, fixed = TRUE)   
  sumbo=sum(aBool) 
  if(sumbo == 0){
    negList = append(negList, kolNamn)
  }
  else if(sumbo == 1){
    posCount = posCount + 1
  }
  else if(sumbo > 1){
    multList = append(multList, kolNamn)
  }
}
 
print(paste((posCount), "positive columns"))
print(paste(length(negList), "negative columns"))
print(paste(length(multList), "multi hit columns"))

#Remove "utgår" parameters, which seems to be the oil parameters 
utGarBol=apply(dfIn, 2, function(x) grepl("Utgår", x[1]))
utGarDf = dfIn[,utGarBol]#
dfIn = dfIn[,!(utGarBol)]#

#Remove "utgår" parameters, which seems to be the oil parameters 
utdfOut=apply(dfOut, 2, function(x) grepl("Utgår", x))
oilOut=dfOut[,colSums(utdfOut) >0]
dfOut=dfOut[,!(colSums(utdfOut) >0)]



#dfRaw = dfIn
#dfIn = dfRaw

#x = dfIn$`Provpunkt|`

dfIn$`Provpunkt|`=gsub("¦","|", dfIn$`Provpunkt|`)
dfIn$`Provpunkt|`=gsub("Jär|","", dfIn$`Provpunkt|`)
dfIn$`Provpunkt|`=gsub("^\\|","", dfIn$`Provpunkt|`, perl = TRUE)
dfIn$`Provpunkt|`=gsub("\\s((?=[0-9]{2}\\|)|(?=[0-9]{3}\\|))","", dfIn$`Provpunkt|`, perl=TRUE)

sort(unique(dfIn$`Provpunkt|`))
sort(unique(dfOut$ID))

dfOut$ID=gsub("(?<=[A-ZÅÄÖ][a-zåäö])\\s", "", dfOut$ID, perl = TRUE)
dfIn$`Provpunkt|` = gsub("(?<=[A-ZÅÄÖ][a-zåäö])\\s", "", dfIn$`Provpunkt|`, perl = TRUE)

unique(dfIn$`Provpunkt|`)
unique(dfOut$ID)


###Put together names that lacks on of the two "name" positions in dfOut###
for(aRnm in grep("^\\|.+",dfOut$ID, perl=TRUE)){
  print(dfOut$ID[aRnm])
  IDs=grep(paste0("^.+","\\",dfOut$ID[aRnm],"$"), dfOut$ID, perl=TRUE)
  for(id in IDs){
    if(nchar(dfOut$ID[id]) > nchar(dfOut$ID[aRnm])){
      dfOut$ID[aRnm] = dfOut$ID[id]
      break
    }
  } 
}
dfOut$ID=gsub("(^\\||\\|$)", "", dfOut$ID)



unique(dfOut$ID)
unique(dfIn$`Provpunkt|`)

for(aChk in names(dfOut)[6:dim(dfOut)[2]]){
  x=unlist(strsplit(aChk, split = " "))[1] 
  print(x)
  print(grep(x, names(dfIn)))
  Sys.sleep(1)
}


str_match(names(dfOut)[6],)

names(dfIn)
names(dfOut)

oprList = list()
posList = list()
minList = list()
nmList = list()
#dfIn[,33]

#Finds columns with "<" and store the position in list
for(nm in names(dfIn)[6:dim(dfIn)[2]]){
  logicGate = TRUE
  for(aRow in seq(dim(dfIn)[1])){
    if(substring(dfIn[aRow,nm],1,1) == "<" && logicGate){
      oprList = append(oprList, "<")
      posList = append(posList,which(colnames(dfIn)==nm))
      nmList = append(nmList, nm)
      dfIn[,nm]= gsub("<", "", dfIn[,nm])
      dfIn[,nm]= gsub(",", "\\.", dfIn[,nm])
      dfIn[,nm]= as.numeric(dfIn[,nm])
      minList = append(minList, min(dfIn[,nm]))
      logicGate = FALSE
    }
  }
}

for(nm in names(dfIn)[6:dim(dfIn)[2]]){
  logicGate = TRUE
  for(aRow in seq(dim(dfIn)[1])){
    if(substring(dfIn[aRow,nm],1,1) == "<" && logicGate){
      oprList = append(oprList, "<")
      posList = append(posList,which(colnames(dfIn)==nm))
      dfIn[,nm]= gsub("<", "", dfIn[,nm])
      dfIn[,nm]= gsub(",", "\\.", dfIn[,nm])
      dfIn[,nm]= as.numeric(dfIn[,nm])
      minList = append(minList, min(dfIn[,nm]))
      logicGate = FALSE
     }
  }
}

oprList = unlist(oprList)
posList = unlist(posList)
minList = unlist(minList)

length(oprList)
length(posList)
length(minList)

dfNum = dfIn[,6:length(dfIn)]
dfNum=as.data.frame(sapply(dfNum, function(x) as.numeric(x)))
dfIn[,6:length(dfIn)] = dfNum


#Divided the values below detection limit by half 
dfHalf = dfIn

pos = 0
for(theCol in posList){
  pos = pos+1
  dfHalf[,theCol]=ifelse(dfHalf[,theCol] == minList[pos], dfHalf[,theCol]/2, dfHalf[,theCol]/1)  
}



#Calculate the max, min, mean, median for each sample 
newFrame = as.data.frame(matrix(ncol = dim(dfHalf)[2],nrow=0))
names(newFrame) = names(dfHalf)
newFrame$Type <- character(0)

for(lokal in unique(dfHalf$`Provpunkt|`)){
  lokalID=grep(lokal, dfHalf$`Provpunkt|`, fixed = TRUE)
  subDf=dfHalf[lokalID,]
  mX=apply(subDf[,6:dim(subDf)[2]], 2, max)
  mI=apply(subDf[,6:dim(subDf)[2]], 2, min)
  mN=apply(subDf[,6:dim(subDf)[2]], 2, mean)
  mD=apply(subDf[,6:dim(subDf)[2]], 2, median)
  metaFrame = subDf[,1:5]
  calList=list(mX, mI, mN,mD)
  calFrame=as.data.frame(do.call(cbind, calList))
  calFrameT=transpose(calFrame)
  rownames(calFrameT) = colnames(calFrame)
  colnames(calFrameT) = rownames(calFrame)
  calFrameT$Type = c("Max", "Min","Medel","Median")
  base = metaFrame
  for(aVal in seq(4)){
    metaFrame[aVal,] = base
  }
  x=cbind(metaFrame,calFrameT)
  subDf$Type = "Value"
  addFrame=rbind(subDf, x)
  newFrame=rbind(newFrame, addFrame)
}

#dfHalf[,names(dfHalf)[39]]
#dfIn[,names(dfIn)[39]]
#newFrame[,names(newFrame)[39]]

#storeFrame = newFrame
#newFrame = storeFrame

#Restore "<" and dubbels the dection limit values
row.names(newFrame) = seq(dim(newFrame)[1])
#write.csv(newFrame, "newFrame_raw.csv", fileEncoding = "UTF-8")

for(aSmp in unique(newFrame$`Provpunkt|`)){
  frameSub=newFrame[grepl(aSmp, newFrame$`Provpunkt|`, fixed =TRUE),]
  print(paste("Sample:", aSmp))
  frameSlice=frameSub[grepl("Value", frameSub$Type),]
  listPos = 0
  Sys.sleep(1)
  for(colVal in posList){
    listPos = listPos + 1
    print(paste("Column position:", colVal))
    print(paste("List position:", listPos))
    print(paste("RowName FrmSlice:",row.names(frameSlice)))
    print(paste("Column Name:", names(frameSlice)[colVal]))
    Sys.sleep(1)
    rowID=grepl(minList[listPos]/2, frameSlice[,colVal])
    if(sum(rowID) > 0)
      {
    mltiVal=as.character(as.numeric(frameSlice[rowID,colVal])*2)
    frameSlice[rowID,colVal]=paste(oprList[listPos],mltiVal)
    frmRowNm=row.names(frameSlice)
    newFrame[as.numeric(frmRowNm),colVal] = frameSlice[rowID,colVal]
      }
    else {
      next
          }
  }
}


newFrame=tibble::add_column(newFrame, newFrame$Type, .after = 4)
names(newFrame)[5] = "Värde_kategori"
newFrame=newFrame[,-which(names(newFrame) == "Type")]

endFrame=as.data.frame(matrix(ncol = 1, nrow = dim(newFrame)[1]))

for(unqCols in seq(dim(utGarDf)[2])){
  x=as.data.frame(rep(unique(utGarDf[,unqCols]),1,dim(newFrame)[1]))
  endFrame = cbind(endFrame,x)
}

endFrame=endFrame[,2:3]
names(endFrame) = names(utGarDf)
expFrame=cbind(newFrame,endFrame)

write.csv(expFrame, "exportFrame.csv", fileEncoding = "UTF-8",row.names = FALSE)