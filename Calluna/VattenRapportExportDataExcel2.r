rm(list = ls())
#install.packages("rJava")
library(foreach)
library(data.table)
library(tibble)
library(openxlsx)
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
#dfOut=dfOut[,!(names(dfOut) %in% c("M?rkning","Provpunkt"))]
###dfOut upprensning slut###


nameDf=gsub("\\.+","\\.",names(dfIn)) #Replace several dots with one 
nameDf=gsub("\\.", "_",nameDf) #Replace dot with underscore
names(dfIn)=gsub("^X$", "X_0",nameDf)

xID=!(grepl("^X_[0-9]{1-2}",names(dfIn))) #Find all columns without "X_." at the start

sID=!(is.na(dfIn[1,])) # Finds columns without NA 
dfIn=dfIn[,!((sID+xID) == 0)] #Removes columns that lacks both headers, if any. 
#xID=!(grepl("^X_[0-9]{1-2}",names(dfIn))) #Renews the xID to the matching length (Can be removed?)


colList = list()

#Builds together row 1-2 to the header (with some variation between columns) 
foreach(id = xID, aPos = seq(1, length(xID),1)) %do%{
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

###Check columns against list to detect deviance###
baseColLst=read.csv("KolumnLista_EurofinCalluna.csv", fileEncoding = "Latin1")

posCount = 0
negList = list()
multList = list()
cVal = 5
aX = dfIn

for(kolNamn in names(dfIn)[6:dim(dfIn)[2]]){
  aBool=grepl(kolNamn, baseColLst$EurofinColumn, fixed = TRUE)   
  sumbo=sum(aBool)
  cVal = cVal+1
  if(sumbo == 0){
    negList = append(negList, kolNamn)
  }
  else if(sumbo == 1){
    posCount = posCount + 1
    names(dfIn)[cVal] = baseColLst$CallunaColumn[aBool]
  }
  else if(sumbo > 1){
    multList = append(multList, kolNamn)
  }
}

#Rework this validation block next time title issue occurs with if checks.   
print(paste((posCount), "positive columns")) #Proceed if all columns are available, else check 
print(paste(length(negList), "negative columns"))
print(paste(length(multList), "multi hit columns")) 

#Remove results rows with "utgår" in dfIn, which seems to be the oil parameters 
utGarBol=apply(dfIn, 2, function(x) grepl("Utgår", x[1]))
utGarDf = dfIn[,utGarBol]#
dfIn = dfIn[,!(utGarBol)]#

#Remove results rows with "utgår" in dfOut, which seems to be the oil parameters 
utdfOut=apply(dfOut, 2, function(x) grepl("Utgår", x))
oilOut=dfOut[,colSums(utdfOut) >0]
dfOut=dfOut[,!(colSums(utdfOut) >0)]

dfRaw = dfIn
# dfIn = dfRaw #Uncomment to get back the dfIn from this point
 
#Replace different regex patterns in provpunkt
dfIn$`Provpunkt|`=gsub("¦","|", dfIn$`Provpunkt|`)
dfIn$`Provpunkt|`=gsub("Jär|","", dfIn$`Provpunkt|`)
dfIn$`Provpunkt|`=gsub("^\\|","", dfIn$`Provpunkt|`, perl = TRUE)
dfIn$`Provpunkt|`=gsub("\\s((?=[0-9]{2}\\|)|(?=[0-9]{3}\\|))","", dfIn$`Provpunkt|`, perl=TRUE)

dfOut$ID=gsub("(?<=[A-ZÅÄÖ][a-zåäö])\\s", "", dfOut$ID, perl = TRUE) #Think it replaces the first whitespace after the first word
dfIn$`Provpunkt|` = gsub("(?<=[A-ZÅÄÖ][a-zåäö])\\s", "", dfIn$`Provpunkt|`, perl = TRUE) #Same for Provpunkt column 

###Put together names that lacks on of the two "name" positions in dfOut###
for(aRnm in grep("^\\|.+",dfOut$ID, perl=TRUE)){
  print(dfOut$ID[aRnm])
  IDs=grep(paste0("^.+","\\",dfOut$ID[aRnm],"$"), dfOut$ID, perl=TRUE) #Creates a regex using the previous regex ID-hit (aRnm), and sees if there is the same ID but with a different start of the string 
  for(id in IDs){ #Goes through the ID-regex hits, and if they are longer then the previous ID, it replaces the previous ID with the new longer ID.
    if(nchar(dfOut$ID[id]) > nchar(dfOut$ID[aRnm])){ 
      print(cat(paste("Replacing first ID with second row:",dfOut$ID[aRnm],dfOut$ID[id], sep = "\n")))
      dfOut$ID[aRnm] = dfOut$ID[id]
      break
    }
  } 
}

dfOut$ID=gsub("(^\\||\\|$)", "", dfOut$ID) #Replace "|" at beginninng or end of the ID string

###Safety checkpoint
#myStore = dfOut 
#dfOut = myStore #Uncomment to return dfOut from this position 



#Attempts to homogenize the naming along locations 
prior=c("\\butl\\b", "uppströms", "nedströms", "kulvert", "norr", "(söder|\\bsö\\b)","öster", "väster", "\\(f,d, .+\\)" )
newer=c("utlopp", "uppstr", "nedstr", "kulv", "N", "S", "Ö", "V", "") 

for(aOne in seq(length(prior))){
  dfIn$`Provpunkt|` = gsub(prior[aOne], newer[aOne],dfIn$`Provpunkt|`, ignore.case = TRUE)
  dfOut$ID = gsub(prior[aOne], newer[aOne], dfOut$ID, ignore.case = TRUE)
}

#Manually replace remaining ID which are inconsistent
dfOut$ID=gsub("^IgSäTå40\\|Tånglötsbäcken, skjutbana \\(uppstr\\)$", "IgSäTå40|Tånglötsbäcken, skjutbana (uppstr vägtrumma)", dfOut$ID)

"""
Run script until here. 
Load the JärfällaRapportID_Lista & JärfällaArkivID_Lista
Compare the names of the two list and create updated table that matches the format of: JärfällaArkivID_Lista_SAN3

The table should have the columns that shows which rapport name is equivalent to what arkiv name.     
If a new location is added it only exist in the Rapport column. 

#Lines for exporting tables
#write.table(unique(dfIn$`Provpunkt|`), 'JärfällaRapportID_Lista202212.csv',sep =",", row.names = FALSE, col.names = FALSE, fileEncoding = "UTF-8")
#write.table(unique(dfOut$ID), 'JärfällaArkivID_Lista202212.csv',sep=",",  col.names=FALSE, row.names = FALSE, fileEncoding = "UTF-8")
"""

sampleNams=read.table("JärfällaArkivID_Lista_SAN3.csv",sep=",", fileEncoding = "UTF-8-BOM", nrow = -1, header = 1)
namnRows=grep(paste(dfIn$`Provpunkt|`, collapse = "|"), sampleNams$NamnRapport) #Pick out matching samples  

#Check if all provpunkter in dfIn exists in sampleNams, otherwise return non matching samples 
if(sum(sampleNams$NamnRapport[namnRows] %in% unique(dfIn$`Provpunkt|`)) == length(unique(dfIn$`Provpunkt|`))){ 
  print("Alla provpunkter in dfIn were matched") # Swenglish for the win!
} else {
    print(cat(paste("These dfIn provpunkter are missing:",
                    dfIn$`Provpunkt|`[!(sampleNams$NamnRapport[namnRows] %in% unique(dfIn$`Provpunkt|`))], sep = "\n")))
}

chkdSample = sampleNams$NamnArkiv[namnRows] #This creates a mismatch since one sample is new
dfIn$TidigareLokal=unlist(lapply(chkdSample, function(x) nchar(x) >0)) #Identifies the matching and none matching samples

#This section adds then names from sampleNams to equivalent position in dfIn Provpunkt (make sense?)
foreach(dfNum = seq(length(chkdSample)), smpNum = namnRows) %do% {
  if(dfIn$TidigareLokal[dfNum] == TRUE){
    dfIn$`Provpunkt|`[dfNum] = sampleNams$NamnArkiv[smpNum] 
  }
  else{
    dfIn$`Provpunkt|`[dfNum] = paste0(sampleNams$NamnRapport[smpNum],"|") # Adds an  "|" in order to match pattern of other locations 
    cat(paste("Replaced the following location name:", dfIn$`Provpunkt|`[dfNum], sampleNams$NamnRapport[smpNum], sep = "\n"))
    sink("") #Catches a print of all rows, use sink again below to close the print sink. 
  }
}
sink()

#Next ~30 lines renames and reorder the two dataframes with identical order/naming 
dfIn$`Provmärkning|NA`=str_extract_all(dfIn$`Provpunkt|`, "^.+(?=\\|)") # Creates provmarkning values, not sure if it should remain
names(dfIn)[1:5] = c("Provnummer","Provtagningsdatum", "Märkning","Provpunkt","Provets_status") #Might be good to check if this fits
  
dfOut$Provpunkt=dfOut$ID
dfOut=dfOut[,names(dfOut) != "ID"]
dfIn=dfIn[,names(dfIn) != "TidigareLokal"]
Provets_status=rep("IckeNoterat", dim(dfOut)[1])
dfOut=add_column(dfOut, Provets_status, .after = (grep("Provets_status", colnames(dfIn), fixed = TRUE)-1)) 
dfOut$'Temperatur luft (°C)' = rep("", dim(dfOut)[1])

newNameLen=length(names(dfOut)[!(names(dfOut) %in% names(dfIn))])

for(colNm in names(dfOut)[!(names(dfOut) %in% names(dfIn))]){
  dfIn=add_column(dfIn, rep("", dim(dfIn)[1]), .after = dim(dfIn)[2])
}

names(dfIn)[(length(names(dfIn))-(newNameLen-1)):length(names(dfIn))] = names(dfOut)[!(names(dfOut) %in% names(dfIn))]
dfOut <- dfOut %>% relocate(Labstatus, `Flöde kommentar`,  .after = Provets_status)
dfIn <- dfIn %>% relocate(Labstatus, `Flöde kommentar`,  .after = Provets_status)

newInOrder=sort(colnames(dfIn)[8:dim(dfIn)[2]])
dfIn = dfIn[,c(colnames(dfIn)[1:7], newInOrder)]

newOutOrder=sort(colnames(dfOut)[8:dim(dfOut)[2]])
dfOut = dfOut[,c(colnames(dfOut)[1:7], newOutOrder)]
dfIn$Märkning = unlist(dfIn$Märkning)

dfNew=dplyr::bind_rows(dfIn, dfOut) # Merge data frames
#write.csv(dfNew, "ReportData_forCalculation.csv", fileEncoding = "UTF-8") #Data format that fits with next calculation section
#End of 'naming' section 


"""
Start of calculation section
"""
dfNew=add_column(dfNew, rep("", dim(dfNew)[1]), .after = "Flöde kommentar") #Add new StatKategori column at col 8 position 
dfNew[,NumList]=apply(dfNew[,NumList], 2, function(x) gsub(",", ".", x)) # Replace "," with "."
names(dfNew)[8] = "StatKategori"

NumList=9:dim(dfNew)[2] #Set how many of the first columns should be excluded as none-numeric
FacList=1:8

dfNew=dfNew[order(dfNew$Provpunkt),]  #Sort dfNew according to provpunkt-column 
dfOri = dfNew #Stores the unaltered results
dfOri$StatKategori = rep("Value", dim(dfOri)[1])

#Store the positions for larger and smaller then signs
smlrThen=apply(dfNew[,NumList], 2, function(x) grepl("<",x))
bigrThen=apply(dfNew[,NumList], 2, function(x) grepl(">",x))

#Removes smaller and greater then signs
dfNew[,NumList]=apply(dfNew[,NumList], 2, function(x) gsub("<", "", x))
dfNew[,NumList]=as.data.frame(apply(dfNew[,NumList], 2, function(x) gsub(">", "", x)))
dfNew[,NumList]=sapply(dfNew[,NumList],function(x) as.numeric(x)) 

Provpunkt=dfNew$Provpunkt
dfNew=dfNew[,!(grepl("Provpunkt", names(dfNew)))]
dfNew=add_column(dfNew, Provpunkt, .before = "Provnummer")

dfNum=dfNew[,NumList]
dfRaw = dfNew #Stores the raw numeric data without ">|<" symbols (matching dim to bigrThen dataframe)
dfRaw$StatKategori = rep("Value", dim(dfRaw)[1])

#Divide the values of smlr then positions for downstream calculations 
for(aCol in seq(dim(smlrThen)[1])){
  for(aRow in seq(dim(smlrThen)[1])){
    if(smlrThen[aRow, aCol]){
      dfNum[aRow, aCol] = dfNum[aRow, aCol]/2 
    }
  }
}

#Multiply the values of greater then positions for downstream calculations 
for(aCol in seq(dim(bigrThen)[1])){
  for(aRow in seq(dim(bigrThen)[1])){
    if(bigrThen[aRow, aCol]){
      dfNum[aRow, aCol] = dfNum[aRow, aCol]*2 
    }
  }
}

smlrThen=cbind(dfNew[,FacList],smlrThen)
bigrThen=cbind(dfNew[,FacList],bigrThen)

dfNew = cbind(dfNew[,FacList], dfNum)
calStore=matrix(ncol = 98, nrow = 0)


###Loop to create the summary table###
for(unqName in unique(dfNew$Provpunkt)){
  subDf=dfNew[grep(unqName, dfNew$Provpunkt, fixed = TRUE),]
  itrNames=subDf[1,names(subDf)[FacList]]
  meanRow=apply(subDf[,NumList], 2, function(x) mean(x,na.rm=TRUE))
  maxRow=apply(subDf[,NumList], 2, function(x) max(x,na.rm=TRUE))
  minRow=apply(subDf[,NumList], 2, function(x) min(x,na.rm=TRUE))
  itrNames$StatKategori = "Mean"
  meanRow = c(itrNames, meanRow)
  calStore = rbind(calStore,unlist(meanRow))
  itrNames$StatKategori = "Max"
  maxRow = c(itrNames, maxRow)
  calStore = rbind(calStore,unlist(maxRow))
  itrNames$StatKategori = "Min"
  minRow = c(itrNames, minRow)
  calStore = rbind(calStore,unlist(minRow))
}

#Cleaning steps 
calStore=as.data.frame(calStore)
names(calStore) = names(dfNew)
numStore=apply(calStore[,NumList], 2, function(x) as.numeric(x))

numStore[is.infinite(numStore)] <- NaN
numStore[is.na(numStore)] <- NaN
numStore=round(numStore, 5)

calStore=cbind(calStore[,FacList], numStore)

#Combine the calculated table with the original values with symbology
dfSymbl=rbind(calStore, dfOri)
dfExprt=rbind(calStore, dfRaw)

#Changes Excel before export 
dfExprt=dfExprt[order(dfExprt$Provpunkt, dfExprt$StatKategori),]
dfExprt$Provpunkt=gsub("\\|$", "",dfExprt$Provpunkt)
dfExprt[is.na(dfExprt)] = NaN



#greenSt = createStyle(bgFill = "#32a852")
#redSt = createStyle(bgFill = "#a83253")  
#lightBlueSt = createStyle(bgFill = "#5fbef5")
#darkBlueSt = createStyle(bgFill = "#0b2299")    

#quantile(dfExprt$`Absorbans 420/5, filtr, Recipientvatten (A,U,)`, na.rm=TRUE, probs = seq(0,1,0.05))[19]
#quantile(dfExprt$`Absorbans 420/5, filtr, Recipientvatten (A,U,)`, na.rm=TRUE, probs = seq(0,1,0.05))[3]

#x1=paste0('>',mean(dfExprt$`Arsenik As (filtrerat) (mg/l)`, na.rm=TRUE))
#x2=paste0('<',mean(dfExprt$`Arsenik As (filtrerat) (mg/l)`, na.rm=TRUE))


#conditionalFormatting(wb, "JarfallaReport", cols = 10, rows = 2:dim(dfExprt)[1]+1,
#                       rule = x1, style = redSt)
#conditionalFormatting(wb, "JarfallaReport", cols = 10, rows = 2:dim(dfExprt)[1]+1,
#                      rule = x2, style = greenSt)

#Make the three data frames have matching sorting


dfExprt=dfExprt[order(dfExprt$StatKategori, dfExprt$Provpunkt, decreasing = TRUE),]
bigrThen=bigrThen[order(bigrThen$Provpunkt, decreasing = TRUE),]
smlrThen=smlrThen[order(smlrThen$Provpunkt, decreasing = TRUE),]

###Openxlsx Section###
wb=createWorkbook()
addWorksheet(wb, "JarfallaReport")
writeData(wb, "JarfallaReport", dfExprt)

greenSt = createStyle(bgFill = "#32a852")
redSt = createStyle(bgFill = "#a83253")  
lightBlueSt = createStyle(bgFill = "#5fbef5")
darkBlueSt = createStyle(bgFill = "#0b2299")    



###Conditonal formating loop for high|low values###
for(colPs in NumList){
  x1=paste0('>',quantile(dfExprt[,colPs], na.rm=TRUE, probs = seq(0,1,0.05))[20])
  x2=paste0('<',quantile(dfExprt[,colPs], na.rm=TRUE, probs = seq(0,1,0.05))[2])
  conditionalFormatting(wb, "JarfallaReport", cols = colPs, rows = 2:dim(dfExprt)[1]+1,
                        rule = x1, style = redSt)
  conditionalFormatting(wb, "JarfallaReport", cols = colPs, rows = 2:dim(dfExprt)[1]+1,
                        rule = x2, style = greenSt)
}

###Conditional formating for below detection limit###
for(colPs in NumList){
  sumCol=sum(smlrThen[,colPs])
  if(sumCol > 0){
    minVal=paste0('==',min(dfExprt[which(dfExprt$StatKategori == "Value"),colPs],na.rm = TRUE))
    conditionalFormatting(wb, "JarfallaReport", cols = colPs, rows = 2:dim(smlrThen)[1]+1,
                          rule = minVal, style = lightBlueSt)
  }
  else{next
  }
}

###Conditional formating for above detection limit###
for(colPs in NumList){
  sumCol=sum(bigrThen[,colPs])
  if(sumCol > 0){
    maxVal=paste0('==',max(dfExprt[which(dfExprt$StatKategori == "Value"),colPs],na.rm = TRUE))
    conditionalFormatting(wb, "JarfallaReport", cols = colPs, rows = 2:dim(bigrThen)[1]+1,
                          rule = maxVal, style = darkBlueSt)
  }
  else{next
  }
}

headSt=createStyle(textDecoration = "Bold")
addStyle(wb, "JarfallaReport", headSt, rows = 1, cols = 1:dim(dfExprt)[1])

saveWorkbook(wb, "JarfallaReport.xlsx", overwrite = TRUE)







openxlsx::write.xlsx(dfExprt, "Jarfalla_Arkiv_2021.xlsx", overwrite = TRUE) # Export data 

calStore[,unlist(lapply(calStore, is.numeric))]=
  apply(calStore[,unlist(lapply(calStore, is.numeric))], 2, function(x) as.character(x))

dfExprt[dfExprt == "NaN"] = NA



str(dfExprt)

#dfExprt=as.data.frame(apply(dfExprt, 2, function(x) gsub("00000", "0", x)))



