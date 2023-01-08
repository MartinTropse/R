rm(list = ls())

library(foreach)
library(data.table)
library(tibble)
library(openxlsx)
library(dplyr)
library(stringr)
library(readxl)

#Steg 1: Skapa loop för de första sex konstanta faktor kolumnerna
setwd("C:/Calluna/Projekt/VattenprovRapport_MON/Automate")
list.files()

newFile = "Jarfalla_NewDfIn.xlsx" # Change to set report input file
archvFile = "JarfallaReportColumns.xlsx" # Change to set archive file
  
dfIn = openxlsx::readWorkbook(newFile)
dfOut = openxlsx::readWorkbook(archvFile)

# An ad-hoc solution to get the correct date format using readxl package 
dfInDate = readxl::read_xlsx(newFile) 
dfOutDate = readxl::read_xlsx(archvFile) 
dfIn$Ankomstdatum = dfInDate$Ankomstdatum
dfOut$Ankomstdatum = dfOutDate$Ankomstdatum

nameDf=gsub("\\.", "_",names(dfIn)) #Replace dot with underscore
names(dfIn)=gsub("^X$", "X_0",nameDf)

xID=grepl("^X[0-9]{1,3}",names(dfIn), perl = TRUE) #Find all columns with"X." at the start
names(dfIn)[xID]
sID=is.na(dfIn[1,]) # Finds columns with NA 
dfIn=dfIn[,!((sID+xID) == 2)] #Removes columns that lacks both headers, if any. 

colList = list()

#Builds together row 1-2 to the header (with some variation between columns) 
foreach(id = !(xID), aPos = seq(1, length(xID),1)) %do%{
  firstNm = names(dfIn)[aPos]
  secNm = dfIn[1,aPos] 
  if(id){
    #print(paste("This is the first Name:",firstNm))
    #print(paste("This is the second Name:",secNm))
    colList = append(colList,paste(firstNm, secNm, sep='|'))
    backPos = aPos
  }
  else  {
    colList = append(colList, paste(names(dfIn)[backPos],secNm, sep='|'))
  }
}

dfIn=dfIn[-1,] #Removing the previous header row
names(dfIn) = colList #Adding the new header 
names(dfIn) = gsub("(\\|NA)", "", names(dfIn))

###Section for ensuring consistent factor columns 
facRmvID= grepl("(^Kund$|(^Eurofins kommentar$|^Eurofins_kommentar$))", names(dfIn), ignore.case = TRUE)
dfIn=dfIn[,!(facRmvID)]

facShtID=grepl("(^Eurofins_provnummer$|^Ankomstdatum$|^Provmärkning$|^Märkning$|^Provpunkt$)",names(dfIn), ignore.case = TRUE)
facLngID=grepl("(^Eurofins_provnummer$|^Ankomstdatum$|^Provmärkning$|^Märkning$|^Provpunkt$|^Provets_status$|(^Flöde kommentar$|^Flöde_kommentar$))",names(dfIn), ignore.case = TRUE)

facShtNam=names(dfIn[facShtID])  
facLngNam=names(dfIn[facLngID])  

if(length(facLngNam) == 6){ 
  dfFac = dfIn[,c("Provpunkt", "Eurofins_provnummer", "Ankomstdatum", "Provmärkning", 
                  "Provets_status", "Flöde_kommentar")]
  dfIn=cbind(dfFac,dfIn[,!(facLngID)])
  lastFact = sum(facLngID)
  print("Found all six factor columns")
}  else if(length(facShtNam) == 4){
    dfFac = dfIn[,c("Provpunkt", "Eurofins_provnummer", "Ankomstdatum", "Provmärkning")]
    dfFac = dfFac %>% add_column(Provets_status = rep("NULL", dim(dfFac)[1]),
                                 .after = "Provmärkning")
    dfFac = dfFac %>% add_column(Flöde_kommentar  = rep("NULL", dim(dfFac)[1]),
                                 .after = "Provets_status")
    dfIn=cbind(dfFac,dfIn[,!(facShtID)])
    lastFact = sum(facShtID)
    print("Found the four requried factor columns, added empty column Prov_status & Flöde_kommentar")
}  else{
    print("Could not find correct factor columns. Check dataset and re-run")
}

#Adding the column StatKategori and sets the factor columns to the correct class
dfIn = dfIn %>% add_column(StatKategori = rep("Value", dim(dfIn)[1]), .after = "Flöde_kommentar") 
dfIn[,c(1,2,4,5,6,7)]=apply(dfIn[,c(1,2,4,5,6,7)], 2, function(x) as.character(x))

FacList=1:7

###Check water chemistry columns against report data to detect deviance###
names(dfOut)=gsub("(\\.|,| |-)","_",names(dfOut))
names(dfOut)=gsub("(__|___)","_", names(dfOut))

names(dfIn)=gsub("(\\.|,| |-)","_",names(dfIn))
names(dfIn)=gsub("(__|___)","_", names(dfIn))

#Remove columns including "utgår" which seems to often be oil measurements
utGarBol=apply(dfIn, 2, function(x) grepl("Utgår", x[1]))
dfIn = dfIn[,!(utGarBol)]#

#Print columns that misses in respective dataset, then proceed to create matching empty columns. 
#Check if the missing columns seems reasonable! 
print(paste("Columns in archieve missing in new report:", 
               names(dfOut)[!(names(dfOut) %in% names(dfIn))]))
misColIn=names(dfOut)[!(names(dfOut) %in% names(dfIn))] #
dfMisColIn=setNames(data.frame(matrix(ncol = length(misColIn), nrow = dim(dfIn)[1])), c(misColIn)) # Add the empty columns that exist in archieve
dfIn=cbind(dfIn, dfMisColIn)

print(paste("Columns in report missing in archieve:",
            names(dfIn)[!(names(dfIn) %in% names(dfOut))]))
misColOut=names(dfIn)[!(names(dfIn) %in% names(dfOut))]
dfMisColOut=setNames(data.frame(matrix(ncol = length(misColOut), nrow = dim(dfOut)[1])), c(misColOut)) # Add the empty columns that exist in archieve
dfOut = cbind(dfOut, dfMisColOut)

#Replace different regex patterns in provpunkt
dfIn$Provpunkt=gsub("¦","|", dfIn$Provpunkt)
dfIn$Provpunkt=gsub("Jär|","", dfIn$Provpunkt)
dfIn$Provpunkt=gsub("^\\|","", dfIn$Provpunkt, perl = TRUE)
dfIn$Provpunkt=gsub("\\s((?=[0-9]{2}\\|)|(?=[0-9]{3}\\|))","", dfIn$Provpunkt, perl=TRUE)
dfIn$Provpunkt = gsub("(?<=[A-ZÅÄÖ][a-zåäö])\\s", "", dfIn$Provpunkt, perl = TRUE) #Same for Provpunkt column 

#Attempts to homogenize the naming along locations 
prior=c("\\butl\\b", "uppströms", "nedströms", "kulvert", "norr", "(söder|\\bsö\\b)","öster", "väster", "\\(f,d, .+\\)","(vägtru|vägtrumma)")
newer=c("utlopp", "uppstr", "nedstr", "kulv", "N", "S", "Ö", "V", "", "vägtr") 

for(aOne in seq(length(prior))){
  dfIn$Provpunkt = gsub(prior[aOne], newer[aOne],dfIn$Provpunkt, ignore.case = TRUE)
}

###Indata independent report section### 
numList=max(FacList+1):dim(dfIn)[2]

dfIn=dfIn[order(dfIn$Provpunkt),]  #Sort dfNew according to provpunkt-column 

dfOri = dfIn #Stores the unaltered results

#Store the positions for larger and smaller then signs #Analysis file can go in here
smlrThen=apply(dfIn[,numList], 2, function(x) grepl("<",x))
bigrThen=apply(dfIn[,numList], 2, function(x) grepl(">",x))

dfIn[,numList]=apply(dfIn[,numList], 2, function(x) gsub(">", "", x))
dfIn[,numList]=apply(dfIn[,numList], 2, function(x) gsub("<", "", x))
dfIn[,numList]=apply(dfIn[,numList], 2, function(x) gsub(",", ".", x))
dfIn[,numList]=apply(dfIn[,numList], 2, function(x) as.numeric(x))


dfRaw = dfIn
dfNum = dfIn[,numList]

#Divide the values of smlr then positions for downstream calculations 
for(aCol in seq(dim(smlrThen)[2])){
  for(aRow in seq(dim(smlrThen)[1])){
    if(smlrThen[aRow, aCol]){
      dfNum[aRow, aCol] = dfNum[aRow, aCol]/2 
    }
  }
}

#Multiply the values of greater then positions for downstream calculations 
for(aCol in seq(dim(bigrThen)[2])){
  for(aRow in seq(dim(bigrThen)[1])){
    if(bigrThen[aRow, aCol]){
      dfNum[aRow, aCol] = dfNum[aRow, aCol]*2 
    }
  }
}

smlrThen=cbind(dfIn[,FacList],smlrThen)
bigrThen=cbind(dfIn[,FacList],bigrThen)

dfIn = cbind(dfIn[,FacList], dfNum)
calStore=matrix(ncol = dim(dfIn)[2], nrow = 0)

###Loop to create summary stat table###
for(unqName in unique(dfIn$Provpunkt)){
  subDf=dfIn[grep(unqName, dfIn$Provpunkt, fixed = TRUE),]
  itrNames=subDf[1,names(subDf)[FacList]]
  meanRow=apply(subDf[,numList], 2, function(x) mean(x,na.rm=TRUE))
  maxRow=apply(subDf[,numList], 2, function(x) max(x,na.rm=TRUE))
  minRow=apply(subDf[,numList], 2, function(x) min(x,na.rm=TRUE))
  itrNames$StatKategori = "Mean"
  meanRow = c(itrNames, meanRow)
  calStore = rbind(calStore,unlist(meanRow))# date conversion error
  itrNames$StatKategori = "Max"
  maxRow = c(itrNames, maxRow)
  calStore = rbind(calStore,unlist(maxRow))
  itrNames$StatKategori = "Min"
  minRow = c(itrNames, minRow)
  calStore = rbind(calStore,unlist(minRow))
}

#Cleaning steps 
calStore=as.data.frame(calStore)

calStore$Ankomstdatum = NA 
calStore$Ankomstdatum=as.POSIXct(calStore$Ankomstdatum)

numStore=apply(calStore[,numList], 2, function(x) as.numeric(x))
numStore[is.infinite(numStore)] <- NA
numStore[is.na(numStore)] <- NA
numStore=round(numStore, 5)

calStore=cbind(calStore[,FacList], numStore)


#Combine the calculated tables with the original values with and without operator symbols
dfSymbl=rbind(calStore, dfOri) #Operator values
dfExprt=rbind(calStore, dfRaw) #Raw values without operator

#Clean SymbolExcel before export 
dfSymbl=dfSymbl[order(dfSymbl$Provpunkt, dfSymbl$StatKategori),]
dfSymbl$Provpunkt=gsub("(\\|$|\\($| )", "",dfSymbl$Provpunkt)
dfSymbl[is.na(dfSymbl)] = NA

dfSymbl$Provets_status[dfSymbl$Provets_status == "NULL"] = NA
dfSymbl$Flöde_kommentar[dfSymbl$Flöde_kommentar == "NULL"] = NA
dfSymbl$Ankomstdatum = lubridate::as_date(dfSymbl$Ankomstdatum)
dfSymbl$Ankomstdatum = as.character(dfSymbl$Ankomstdatum)

#Changes Excel before export 
dfExprt=dfExprt[order(dfExprt$Provpunkt, dfExprt$StatKategori),]
dfExprt$Provpunkt=gsub("(\\|$|\\($| )", "",dfExprt$Provpunkt)
dfExprt[is.na(dfExprt)] = NA

dfExprt$Provets_status[dfExprt$Provets_status == "NULL"] = NA
dfExprt$Flöde_kommentar[dfExprt$Flöde_kommentar == "NULL"] = NA

#Create matching sorting in the 3 data frames 
dfExprt=dfExprt[order(dfExprt$StatKategori, dfExprt$Provpunkt, decreasing = TRUE),]
bigrThen=bigrThen[order(bigrThen$Provpunkt, decreasing = TRUE),]
smlrThen=smlrThen[order(smlrThen$Provpunkt, decreasing = TRUE),]

###Openxlsx Section###
wb=createWorkbook()

#Create timeStamp string for file name 
maxMnth = lubridate::month(max(dfExprt$Ankomstdatum[!is.na(dfExprt$Ankomstdatum)]), label = TRUE)
minMnth = lubridate::month(min(dfExprt$Ankomstdatum[!is.na(dfExprt$Ankomstdatum)]),label = TRUE)
meanYr = lubridate::year(median(dfExprt$Ankomstdatum[!is.na(dfExprt$Ankomstdatum)]))

exprtNm=paste0("JarfallaRapport","_",meanYr, "_",minMnth,maxMnth) 
sheetNm=paste0(meanYr, "_",minMnth,maxMnth) 

#transforms time data to date & character to prevent Excel-transformation
dfExprt$Ankomstdatum = lubridate::as_date(dfExprt$Ankomstdatum)
dfExprt$Ankomstdatum = as.character(dfExprt$Ankomstdatum)

#Add data to excel workbook
addWorksheet(wb, sheetNm)
writeData(wb, sheetNm, dfExprt, keepNA = TRUE)

greenSt = createStyle(bgFill = "#32a852")
redSt = createStyle(bgFill = "#a83253")  
lightBlueSt = createStyle(bgFill = "#5fbef5")
darkBlueSt = createStyle(bgFill = "#0b2299")    

###Conditonal formating loop for top 5% high and low values###
for(colPs in numList){
  x1=paste0('>',quantile(dfExprt[,colPs], na.rm=TRUE, probs = seq(0,1,0.05))[20])
  
  x2=paste0('<',quantile(dfExprt[,colPs], na.rm=TRUE, probs = seq(0,1,0.05))[2])
  conditionalFormatting(wb, sheetNm, cols = colPs, rows = 2:(dim(dfExprt)[1]+1),
                        rule = x1, style = redSt)
  conditionalFormatting(wb, sheetNm, cols = colPs, rows = 2:(dim(dfExprt)[1]+1),
                        rule = x2, style = greenSt)
}

###Conditional formating for values below detection limit###
for(colPs in numList){
  sumCol=sum(smlrThen[,colPs])
  if(sumCol > 0){
    minVal=paste0('==',min(dfExprt[which(dfExprt$StatKategori == "Value"),colPs],na.rm = TRUE))
    conditionalFormatting(wb, sheetNm, cols = colPs, rows = 2:dim(smlrThen)[1]+1,
                          rule = minVal, style = lightBlueSt)
  }
  else{next
  }
}

###Conditional formating for above detection limit###
for(colPs in numList){
  sumCol=sum(bigrThen[,colPs])
  if(sumCol > 0){
    maxVal=paste0('==',max(dfExprt[which(dfExprt$StatKategori == "Value"),colPs],na.rm = TRUE))
    conditionalFormatting(wb, sheetNm, cols = colPs, rows = 1:dim(bigrThen)[1]+1,
                          rule = maxVal, style = darkBlueSt)
  }
  else{next
  }
}

setColWidths(wb, sheetNm, cols = 1:2, widths = 42)
setColWidths(wb, sheetNm, cols = 3:dim(dfExprt)[2], widths = 25)

headSt=createStyle(textDecoration = "Bold",halign = "left", valign = "center")
addStyle(wb, sheetNm, headSt, rows = 1, cols = 1:dim(dfExprt)[2])

numSt=createStyle(halign = "center", valign = "center")
numBase=createStyle(halign = "left", valign = "center")
addStyle(wb, sheetNm, numBase, rows = 2:(dim(dfExprt)[1]+1), cols = 1:2, gridExpand = T)
addStyle(wb, sheetNm, numSt, rows = 2:(dim(dfExprt)[1]+1), cols = 3:dim(dfExprt)[2], gridExpand = T)

openxlsx::groupColumns(wb, sheetNm, 2:6)

ArchivePth=file.path(getwd(), paste0("Archive",as.Date(Sys.time())))
dir.create(ArchivePth)

saveWorkbook(wb, file.path(ArchivePth,paste0(exprtNm,".xlsx")), overwrite = TRUE)

#Prepare data for next analysis step 
dfOut$Ankomstdatum = lubridate::as_date(dfOut$Ankomstdatum)
dfOut$Ankomstdatum = as.character(dfOut$Ankomstdatum)

dfOut[,c(1,2,4,5,6,7)]=apply(dfOut[,c(1,2,4,5,6,7)], 2, function(x) as.character(x))

wbSym=createWorkbook()
addWorksheet(wbSym, "inFil")
writeData(wbSym, "inFil", dfSymbl, keepNA = TRUE)
saveWorkbook(wbSym, file.path(ArchivePth,"InFil_Orginal.xlsx"), overwrite = TRUE)

wbOut=createWorkbook()
addWorksheet(wbOut, "utFil")
writeData(wbOut, "utFil", dfOut, keepNA = TRUE)
saveWorkbook(wbOut, file.path(ArchivePth,"ArkivFil_Orginal.xlsx"), overwrite = TRUE)

###Update Archive File Section###
"""
Create an archieve file,  i.e. a stacked version of the input file. This file 
Can then later be used to create reports from selected time intervall. 
"""
rm(list=ls())

library(dplyr)
library(openxlsx)

setwd("C:/Calluna/Projekt/VattenprovRapport_MON/Automate")
ArchivePth=file.path(getwd(), paste0("Archive",as.Date(Sys.time())))

dfOld=openxlsx::readWorkbook(file.path(ArchivePth,"ArkivFil.xlsx"), sheet = "utFil")
dfNew=openxlsx::readWorkbook(file.path(ArchivePth,"InFil_Orginal.xlsx"), sheet = "inFil")

dfOld=dfOld[dfOld$StatKategori == "Value",]
dfNew=dfNew[dfNew$StatKategori == "Value",]

dfOld[,1:dim(dfOld)[2]] = sapply(dfOld[,1:dim(dfOld)[2]], function(x) as.character(x))
dfNew[,1:dim(dfNew)[2]] = sapply(dfNew[,1:dim(dfNew)[2]], function(x) as.character(x))

###
#Include section for sample match section between previous and archive file
###

dfArc=bind_rows(dfOld, dfNew)

arcWb=createWorkbook()
addWorksheet(arcWb, "Arkiv")
writeData(arcWb, "Arkiv", dfArc)
saveWorkbook(arcWb, file.path(ArchivePth, paste0("Arkiv",as.Date(Sys.time()),".xlsx")), overwrite = TRUE)

###Section for report analysis based on archive time selection###
rm(list=ls())

library(dplyr)
library(openxlsx)
library(lubridate)

setwd("C:/Calluna/Projekt/VattenprovRapport_MON/Automate")
ArchivePth=file.path(getwd(),"Archive2023-01-08")

dfArc=openxlsx::readWorkbook(file.path(ArchivePth,"Arkiv2023-01-08.xlsx"), sheet = "Arkiv")

FacList=1:7
numList=seq(max(FacList+1),dim(dfArc)[2])

dfArc$Ankomstdatum=as.Date(dfArc$Ankomstdatum)

#Set the time selection for the report
timeInt <- interval(ymd("2021-01-01"), ymd("2022-11-30"))
dfArc=dfArc[dfArc$Ankomstdatum %within% timeInt,]


#Store the positions for larger and smaller then signs #Analysis file can go in here
smlrThen=apply(dfArc[,numList], 2, function(x) grepl("<",x))
bigrThen=apply(dfArc[,numList], 2, function(x) grepl(">",x))

dfArc[,numList]=apply(dfArc[,numList], 2, function(x) gsub(">", "", x))
dfArc[,numList]=apply(dfArc[,numList], 2, function(x) gsub("<", "", x))
dfArc[,numList]=apply(dfArc[,numList], 2, function(x) gsub(",", ".", x))
dfArc[,numList]=apply(dfArc[,numList], 2, function(x) as.numeric(x))


dfRaw = dfArc
dfNum = dfArc[,numList]

#Divide the values of smlr then positions for downstream calculations 
for(aCol in seq(dim(smlrThen)[2])){
  for(aRow in seq(dim(smlrThen)[1])){
    if(smlrThen[aRow, aCol]){
      dfNum[aRow, aCol] = dfNum[aRow, aCol]/2 
    }
  }
}

#Multiply the values of greater then positions for downstream calculations 
for(aCol in seq(dim(bigrThen)[2])){
  for(aRow in seq(dim(bigrThen)[1])){
    if(bigrThen[aRow, aCol]){
      dfNum[aRow, aCol] = dfNum[aRow, aCol]*2 
    }
  }
}

smlrThen=cbind(dfArc[,FacList],smlrThen)
bigrThen=cbind(dfArc[,FacList],bigrThen)

dfArc = cbind(dfArc[,FacList], dfNum)
calStore=matrix(ncol = dim(dfArc)[2], nrow = 0)

###Loop to create summary stat table###
for(unqName in unique(dfArc$Provpunkt)){
  subDf=dfArc[grep(unqName, dfArc$Provpunkt, fixed = TRUE),]
  itrNames=subDf[1,names(subDf)[FacList]]
  meanRow=apply(subDf[,numList], 2, function(x) mean(x,na.rm=TRUE))
  maxRow=apply(subDf[,numList], 2, function(x) max(x,na.rm=TRUE))
  minRow=apply(subDf[,numList], 2, function(x) min(x,na.rm=TRUE))
  itrNames$StatKategori = "Mean"
  meanRow = c(itrNames, meanRow)
  calStore = rbind(calStore,unlist(meanRow))# date conversion error
  itrNames$StatKategori = "Max"
  maxRow = c(itrNames, maxRow)
  calStore = rbind(calStore,unlist(maxRow))
  itrNames$StatKategori = "Min"
  minRow = c(itrNames, minRow)
  calStore = rbind(calStore,unlist(minRow))
}

#Cleaning steps 
calStore=as.data.frame(calStore)

calStore$Ankomstdatum = NA 
calStore$Ankomstdatum=as.POSIXct(calStore$Ankomstdatum)

numStore=apply(calStore[,numList], 2, function(x) as.numeric(x))
numStore[is.infinite(numStore)] <- NA
numStore[is.na(numStore)] <- NA
numStore=round(numStore, 5)

calStore=cbind(calStore[,FacList], numStore)

#Combine the calculated tables with the original values with and without operator symbols
dfExprt=rbind(calStore, dfRaw) #Raw values without operator

#Changes Excel before export 
dfExprt=dfExprt[order(dfExprt$Provpunkt, dfExprt$StatKategori),]
dfExprt$Provpunkt=gsub("(\\|$|\\($| )", "",dfExprt$Provpunkt)
dfExprt[is.na(dfExprt)] = NA

dfExprt$Provets_status[dfExprt$Provets_status == "NULL"] = NA
dfExprt$Flöde_kommentar[dfExprt$Flöde_kommentar == "NULL"] = NA

#Create matching sorting in the 3 data frames 
dfExprt=dfExprt[order(dfExprt$StatKategori, dfExprt$Provpunkt, decreasing = TRUE),]
bigrThen=bigrThen[order(bigrThen$Provpunkt, decreasing = TRUE),]
smlrThen=smlrThen[order(smlrThen$Provpunkt, decreasing = TRUE),]

###Openxlsx Section###
wb=createWorkbook()

#Create timeStamp string for file name 
maxMnth = lubridate::month(max(dfExprt$Ankomstdatum[!is.na(dfExprt$Ankomstdatum)]), label = TRUE)
minMnth = lubridate::month(min(dfExprt$Ankomstdatum[!is.na(dfExprt$Ankomstdatum)]),label = TRUE)
maxYr = lubridate::year(max(dfExprt$Ankomstdatum[!is.na(dfExprt$Ankomstdatum)]))
minYr = lubridate::year(min(dfExprt$Ankomstdatum[!is.na(dfExprt$Ankomstdatum)]))

if(maxYr == minYr){
  exprtNm=paste0("JarfallaRapport","_",minYr, "_",minMnth,maxMnth) 
} else{
  exprtNm=paste0("JarfallaRapport","_",minYr,"_",maxYr, "_",minMnth,maxMnth) 
}

sheetNm=paste0("ArkivRapport") 

#transforms time data to date & character to prevent Excel-transformation
dfExprt$Ankomstdatum = lubridate::as_date(dfExprt$Ankomstdatum)
dfExprt$Ankomstdatum = as.character(dfExprt$Ankomstdatum)

#Add data to excel workbook
addWorksheet(wb, sheetNm)
writeData(wb, sheetNm, dfExprt, keepNA = TRUE)

greenSt = createStyle(bgFill = "#32a852")
redSt = createStyle(bgFill = "#a83253")  
lightBlueSt = createStyle(bgFill = "#5fbef5")
darkBlueSt = createStyle(bgFill = "#0b2299")    

###Conditonal formating loop for top 5% high and low values###
for(colPs in numList){
  x1=paste0('>',quantile(dfExprt[,colPs], na.rm=TRUE, probs = seq(0,1,0.05))[20])
  
  x2=paste0('<',quantile(dfExprt[,colPs], na.rm=TRUE, probs = seq(0,1,0.05))[2])
  conditionalFormatting(wb, sheetNm, cols = colPs, rows = 2:(dim(dfExprt)[1]+1),
                        rule = x1, style = redSt)
  conditionalFormatting(wb, sheetNm, cols = colPs, rows = 2:(dim(dfExprt)[1]+1),
                        rule = x2, style = greenSt)
}

###Conditional formating for values below detection limit###
for(colPs in numList){
  sumCol=sum(smlrThen[,colPs])
  if(sumCol > 0){
    minVal=paste0('==',min(dfExprt[which(dfExprt$StatKategori == "Value"),colPs],na.rm = TRUE))
    conditionalFormatting(wb, sheetNm, cols = colPs, rows = 1:dim(smlrThen)[1]+1,
                          rule = minVal, style = lightBlueSt)
  }
  else{next
  }
}

###Conditional formating for above detection limit###
for(colPs in numList){
  sumCol=sum(bigrThen[,colPs])
  if(sumCol > 0){
    maxVal=paste0('==',max(dfExprt[which(dfExprt$StatKategori == "Value"),colPs],na.rm = TRUE))
    conditionalFormatting(wb, sheetNm, cols = colPs, rows = 1:dim(bigrThen)[1]+1,
                          rule = maxVal, style = darkBlueSt)
  }
  else{next
  }
}

setColWidths(wb, sheetNm, cols = 1:2, widths = 42)
setColWidths(wb, sheetNm, cols = 3:dim(dfExprt)[2], widths = 25)

headSt=createStyle(textDecoration = "Bold",halign = "left", valign = "center")
addStyle(wb, sheetNm, headSt, rows = 1, cols = 1:dim(dfExprt)[2])

numSt=createStyle(halign = "center", valign = "center")
numBase=createStyle(halign = "left", valign = "center")
addStyle(wb, sheetNm, numBase, rows = 2:(dim(dfExprt)[1]+1), cols = 1:2, gridExpand = T)
addStyle(wb, sheetNm, numSt, rows = 2:(dim(dfExprt)[1]+1), cols = 3:dim(dfExprt)[2], gridExpand = T)

openxlsx::groupColumns(wb, sheetNm, 2:6)

ArchivePth=file.path(getwd(), paste0("Archive",as.Date(Sys.time())))
dir.create(ArchivePth)

saveWorkbook(wb, file.path(ArchivePth,paste0(exprtNm,".xlsx")), overwrite = TRUE)