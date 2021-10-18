library(readxl)
library(reshape2)
library(dplyr)
library(tidyverse)
library(foreach)

#Change the working directory and input file data. 
#Check that columns are in correct order with script below 
#Then run the script and follow remaining instructions :-)!

setwd("C:/Bas/AquaBiota/Projekt/MEAD/Njord")

print("Please provide project ID:")
prjID=readline()

print("Please provide output file name (without .csv) :")
outName = readline()

#df=read.csv("FishNjord.csv", encoding = "UTF-8")
df=as.data.frame(read_excel("101808.client_OTUtable1.xlsx", skip = 2,sheet = 2))

colnames(df)
colnames(df)[1] = "Sequence" 
#df=subset(df, select=-Contaminant)

###Rename, remove and columns until they match the OrderCol format###
OrderCol = c("Sequence","Phylum","Class","Order","Family","Genus","Species")
vecNm=seq(1,7,1)

for(pos in vecNm){
  if(colnames(df)[pos] == OrderCol[pos]){
    print(paste("Column",colnames(df)[pos],"is correct"))
  } else {print(paste(colnames(df)[pos], "at position", pos, "is not correct")) }
}
print(paste("The next column,", colnames(df)[8], "should be a sample"))
#If correct run script. Otherwise keep at it!

###Translate species to Swedish###
#Extracts species and genus strings, and melt it to a dataframe with the highest resolved taxa   
conflictID=grepl("(?<=\\().+(?=\\/)", df$Species, perl = TRUE)
conflictList=as.vector(str_extract_all(df$Species,"(?<=\\().+(?=\\/)"))
specList=as.vector(str_extract_all(df$Species,"(?<=\\().+?.+?(?=\\))"))
specList[conflictID] = conflictList[conflictID]

dfSpc=melt(specList, value.name = "HigResTaxa")
dfSpc$HigResTaxa=gsub("sp.","", dfSpc$HigResTaxa)
dfSpc$HigResTaxa=gsub("\\W$","",dfSpc$HigResTaxa)

write.csv(dfSpc$HigResTaxa, "ArtListaTillArtfaktaVertebrate.csv", row.names = FALSE)
#Add the list from the exported csv to the following link: 
#https://namnochslaktskap.artfakta.se/match 

# print("Translate the result and download the csv back to the working directory.")
# print("Add the data to read.csv. Then press any key to continue")
# readline()

###Download the translated data and add it to the OTU-table.###
dfTrns=read.csv("MatchResult 2021-10-18 09.56.csv", sep = ";", encoding = "UTF-8")
names(dfTrns)[1] = "Sökterm"

#Removes subspecies and add "_conflict" to species names that had more then one annotation 
dfTrns=dfTrns[!grepl("Underart", dfTrns$Kategori),] 
dfTrns$Svenskt.namn=as.character(dfTrns$Svenskt.namn)
dfTrns[conflictID,5] =  paste0(dfTrns[conflictID,5], "_konflikt")

#Efficent lines that find duplicates in a column and select those rows in the dataframe
n_occur <- data.frame(table(dfSpc$HigResTaxa))
dfDupl=dfSpc[dfSpc$HigResTaxa %in% n_occur$Var1[n_occur$Freq > 1],]

#Function to add new row to specific index position
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

#Find duplicates in species column and add copies of translated rows at the right position 
#in the translated df. 
for(x in unique(dfDupl$HigResTaxa)){
  subDf=dfSpc[grep(x, dfSpc$HigResTaxa),]
  if(dim(subDf)[1]>1){
    for(y in seq(2,dim(subDf)[1],1))
      newRow=as.integer(row.names(subDf)[y])
      copyRow=as.integer(row.names(subDf)[1])
      dfTrns=insertRow(dfTrns, dfTrns[copyRow,],newRow)  
  } 
}

#Checks that all rows in dfTrns and dfSpc matches 
countVal = 0
for(x in seq(1,length(dfSpc$HigResTaxa),1)){
  if(dfSpc$HigResTaxa[x] != dfTrns$Sökterm[x]){
     print(x)
     countVal = countVal + 1
  }
}
print(paste(countVal, "rows had an error! ZOMG!"))

df$SweName = dfTrns$Svenskt.namn  
df$Category = dfTrns$Kategori
df$Presence = dfTrns$Svensk.förekomst
df$URL = dfTrns$Url

###Format data###
#Melt function, add the correct dataframe 
meltDf=melt(df, by.var = c("Sequence","Phylum","Class","Order","Family","Genus","Species", "SweName", "Category", "Presence", "URL"), value.name = "SequenceCount", variable.name = "SampleID")
meltDf$SampleID = as.character(meltDf$SampleID)
meltDf$SampleID=gsub("_","", meltDf$SampleID)

#Add sequence proportion 
seqProp=tapply(meltDf$SequenceCount, meltDf$SampleID, function(x) x/sum(x))
dfProp=as.data.frame(do.call(rbind, seqProp))
dfProp$RowName = row.names(seqProp)
dfProp=melt(dfProp, by.var = "RowName")
dfProp=dfProp[order(dfProp$RowName),]

meltDf$SeqProportion = dfProp$value
#meltDf$Name = dfProp$RowName

meltDf = meltDf %>% select(-SampleID, SampleID) #Moves set column to last position
prjIDs = rep(prjID, dim(meltDf)[1])
meltDf$SampleID = paste(meltDf$SampleID, prjIDs, sep = "_")
meltDf$ProjectID = prjIDs
meltDf = meltDf %>% select(-ProjectID, ProjectID) #Moves set column to last position

#Export the OTU data. Continue to add the data to the access table 
write.csv(meltDf, paste0(outName, "MEAD.csv"), row.names = FALSE, fileEncoding = "UTF-8")