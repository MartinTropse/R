
"""
Prior to script, resolve all OTU:s to one taxonomic level. 
Load the data and read the comments. 

Change the working directory and input file data. 
Check that columns are in correct order with script below.

Resolve rows in the OTU-table that have more then one species prior to running 
the script. By deciding for one species or replace by a common genus/family.
Only one taxon can be inside the ()

Format should be: English name (Latin name)
This is possible: Haddock/Whiting (Gadidae). But will only be translated family. 
This is possible: Wrasse species (Labrus sp.)
NOT this: Sculpin (Myoxocephalus scorpius/quadricornis).  

Then run the script and follow remaining instructions :-)!
"""

library(readxl)
library(reshape2)
library(dplyr)
library(tidyverse)
library(foreach)
library(tibble)



setwd("P:/eDNA/MEAD/Metadata/Sveavind/Gretas Klackar/2021/Maj/Sample/")
#setwd("C:/Users/MartinAndersson/AquaBiota/Gruppwebbplats för AquaBiota - Documents/PROJEKT/eDNA/MEAD/AqB_OTUtables_ReadCounts/AqB_OTUtables_ReadCounts")

print("Please provide project ID:")
prjID=readline()

#print("Please provide output file name (without OTU, MEAD or .csv) :")
#outName = readline()

#df=read.csv("102344.Fish_OTUtable.ReadCounts.xlsx", encoding = "UTF-8-BOM", skip=2)
df=as.data.frame(read_excel("102344.Fish_OTUtable.ReadCounts.xlsx", skip = 2,sheet = 1))

#The columns and their order that should be present, primer column is added below
OrderCol = c("Sequence","Primer","Phylum","Class","Order","Family","Genus","Species")
#####################################################################
###Rename, remove and columns until they match the OrderCol format###
colnames(df)

#colnames(df)[1] = "Sequence"
#df=subset(df, select=-OTU_ID)
# df=subset(df, select=-Contaminant)
#df=subset(df, select=-Note)
#df=subset(df, select=-Similarity)
#df=subset(df, select=-Target)
#df=subset(df, select=-Target.Taxa)
#df=subset(df, select=-Kingdom)
#df=subset(df, select=-Habitat)
#df=subset(df,select=-IUCN.Red.List)
#df=subset(df,select=-X)
#df=subset(df, select=-Comments)
#df=df[,-which(colnames(df)=="Threat status")]
#df=df[,-which(colnames(df)=="Common Name")]
#df=df[,-which(colnames(df)=="Target Taxa")]

#Select the primer that was used in the OTU-table, by changing the primerList value 
primerList = c("MiFish", "Kelly", "MarVer3", "MiFish/Kelly")
Primer = rep(primerList[3], times=dim(df)[1])
df=tibble::add_column(df, Primer, .after = 1)

#Check columns are correct
vecNm=seq(1,8,1)
for(pos in vecNm){
  if(colnames(df)[pos] == OrderCol[pos]){
    print(paste("Column",colnames(df)[pos],"is correct"))
  } else {print(paste(colnames(df)[pos], "at position", pos, "is not correct")) }
}
print(paste("The next column,", colnames(df)[9], "should be a sample"))
#If correct run script. Otherwise keep at it!

#Convert all samples to integer values
vecSm=seq(9,dim(df)[2],1)
for(thePos in vecSm){
  df[,thePos]=as.integer(df[,thePos])
}

#Checks if there are value in species and replaces empty with either genus or Family level taxa
sID=is.na(df$Species)
sum(sID) # If more then zero use code section beneath

# foreach(aBool = sID, aPos = seq(1,length(sID),1))%do%{
#   if(aBool == TRUE){
#     chk=is.na(df$Genus[aPos])
#     if(chk == FALSE){          
#       df$Species[aPos] = df$Genus[aPos]}
#       else{df$Species[aPos] = df$Family[aPos]}
#   }
# }

################################Translate To Swedish##################################
#Extracts species and genus strings, melt it to a dataframe with the highest resolved taxa   
#If species has format: "english (latin)" go to English & Latin!. If only latin then go to "Latin!"
df$Species

#English & Latin
specList=as.vector(str_extract_all(df$Species,"(?<=\\().+?.+?(?=\\))"))

#Latin!
#specList=gsub(".+\\/.+(?=\\()","/",df$Species, perl =TRUE)

#Continue
dfSpc=melt(specList, value.name = "x")
dfSpc$x=gsub("sp\\.","", dfSpc$x)
dfSpc$x=gsub("\\W$","",dfSpc$x)

write.csv(dfSpc$x, paste0(prjID,Primer[1], "_TillArtfaktaFisk.csv"), row.names = FALSE)
#Add the list from the exported csv to the following link: 
#https://namnochslaktskap.artfakta.se/match 

###Download the translated data and add it to the OTU-table.###
#dfSpc=read.csv("ArtListaTillArtfaktaTritonFisk1.csv", fileEncoding = "UTF-8-BOM") #Temp line
dfTrns=read.csv("MatchResult 2021-11-24 15.09.csv", sep = ";", fileEncoding = "UTF-8-BOM")
dfTrns$Filter = rep("", length(dfTrns$Vetenskapligt.namn)) #Adds empty filter column 

#Removes subspecies 
dfTrns=dfTrns[!grepl("Underart", dfTrns$Kategori),] 
dfTrns=dfTrns[!grepl("Morfotyp", dfTrns$Kategori),]

#Check if there are multiple translation, if so use section beneath
misDf=dfTrns[grepl("Information", dfTrns$Matchstatus),] 
print(misDf)
#Picks out one translation from species with multiple translations
for(SciName in unique(misDf$Vetenskapligt.namn[2:3])){
  subMis = misDf[grepl(SciName, misDf$Vetenskapligt.namn),]
  if(dim(subMis)[1] > 1){
    dfTrns=dfTrns[-as.integer(row.names(subMis)[2]),]
  }
}

#######################################################################################
###Species filter that flags for contamination & species not obeserved in the region###
region = c("BH", "BV", "EOS","EON","VK") #Bottenhavet, Bottenviken, Egentliga Östersjön Syd, Egentliga Österjön Norr, Västkusten
regionFilter = region[4] #Select region specific region filter 

path=paste0("P:/eDNA/MEAD/Artfilter/", regionFilter, "/")

list.files(path)
#filePaths = dir(path, pattern = "*.csv", full.names = TRUE) 

artFsh=read.csv(paste0(path,regionFilter,"_Fisk.csv"), fileEncoding = "UTF-8-BOM")
artVrt=read.csv(paste0(path, regionFilter,"_Daggdjur.csv"), fileEncoding = "UTF-8-BOM")
artFgl=read.csv(paste0(path, regionFilter,"_Faglar.csv"), fileEncoding = "UTF-8-BOM")

manFilterAd = read.csv(paste0(path, regionFilter, "_ManuelltFilter_Add.csv"), fileEncoding = "UTF-8-BOM")
manFilterRm = read.csv(paste0(path, regionFilter,"_ManuelltFilter_Remove.csv"), fileEncoding = "UTF-8-BOM")
genusFilter = read.csv(paste0(path, regionFilter,"_Genus.csv"), fileEncoding = "UTF-8-BOM")

#Check if bits exist, if so it loads it with the remaining species list if present.
bitsRegions = c("EOS","EON","VK")
if(regionFilter %in% bitsRegions){
  bits=read.csv(paste0(path,regionFilter,"_","BITS.csv"), fileEncoding = "UTF-8-BOM")
  artDf=rbind(artFsh, artVrt, artFgl, bits, manFilterAd,genusFilter)
  }else{
    artDf=rbind(artFsh,artVrt,artFgl, manAdd, genusFilter)
}

bigArt=sort(as.vector(unique(artDf$x))) #The species filter

contmnt=c("Sus scrofa", "Homo sapiens", "Bos taurus", "Rattus norvegicus", "Meleagris gallopavo", "Gallus gallus" #List of common contamination
          ,"Canis lupus", "Felis Catus", "Canis familiaris") 

#Add a filter/flag column depending on its matches against the OTU-filter. 
chkPos = dfTrns$Vetenskapligt.namn  %in% bigArt
chkNeg =!(dfTrns$Vetenskapligt.namn  %in% bigArt)
chkCnt = dfTrns$Vetenskapligt.namn %in% contmnt
chkMan = dfTrns$Vetenskapligt.namn %in% manFilterRm$x 

dfTrns$Filter[chkPos] = "Förekommande"
dfTrns$Filter[chkNeg] = "Otypisk"
dfTrns$Filter[chkMan] = "Otypisk_aktiv"
dfTrns$Filter[chkCnt] = "Kontaminering"

###########################
dfTrns$Svenskt.namn=as.character(dfTrns$Svenskt.namn)

#Efficent lines that find duplicates in a column and select those rows in the dataframe (check column value)
dfSpc$RowN = row.names(dfSpc)
n_occur <- data.frame(table(dfSpc$x))
dfDupl=dfSpc[dfSpc$x %in% n_occur$Var1[n_occur$Freq > 1],]

#Function to add new row to specific index position
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

tempSpc = dfSpc # Restore spcDf using this if you have to re-run scipt below. 
tempTrns = dfTrns
dim(dfSpc)
dim(dfTrns)

#Find duplicates in species column and add copies of translated rows at the right position 
#in the translated df. 
for(x in unique(dfDupl$x)){
  subDf=as.data.frame(dfSpc[grep(x, dfSpc$x),])
  if(dim(subDf)[1]>1){
      print("Check if we are here")
      for(y in seq(2,dim(subDf)[1],1))
        print(y)
        newRow=as.integer(row.names(subDf)[y])
        copyRow=as.integer(row.names(subDf)[1])
        dfTrns=insertRow(dfTrns, dfTrns[copyRow,],newRow)  
  }
  if(is.na(dfTrns$Auktor[dim(dfTrns)[1]])){ #Removes the extra row which occurs when insertRows add position at last row
    dfTrns=dfTrns[-dim(dfTrns)[1],]
  }
}

#Add the latin name for taxonomic groups that lacks Swedish name:
dfTrns$Svenskt.namn=as.character(dfTrns$Svenskt.namn)
dfTrns$Svenskt.namn[is.na(dfTrns$Svenskt.namn)] = ""
for(x in seq(1, dim(dfTrns)[1], 1)){
  if(nchar(dfTrns$Svenskt.namn[x])<1){
    dfTrns$Svenskt.namn[x] = as.character(dfTrns$Vetenskapligt.namn[x])
  }
}

#Checks that all rows in dfTrns and dfSpc matches 
countVal = 0
for(x in seq(1,length(dfSpc$x),1)){
  if(dfSpc$x[x] != dfTrns$Sökterm[x]){
     print(x)
     countVal = countVal + 1
  }
}
print(paste(countVal, "rows had an error! ZOMG!"))

df$SweName = dfTrns$Svenskt.namn
#df$Category = dfTrns$Kategori
#df$Presence = dfTrns$Svensk.förekomst
df$URL = dfTrns$Url
df$Filter = dfTrns$Filter

###Format data###
#Melt function, add the correct dataframe 
meltDf=melt(df, by.var = c("Sequence","Primer","Phylum","Class","Order","Family","Genus","Species", "SweName", "URL","Filter"), value.name = "SeqCount", variable.name = "SampleID")
meltDf$SampleID = as.character(meltDf$SampleID)
meltDf$SampleID=gsub("_","", meltDf$SampleID)

#Add percentage presence among samples, for each OTU/species 
meltDf$Presence=1*(meltDf$SeqCount > 0) #Count rows with sequence above 0 to get presence/absence 
meltDf$PresencePRCT = rep(NA, length(meltDf$SweName)) #Adds a column with NA values to be replaced

unique(meltDf$SampleID) #Add the negative sample lines
meltPos=meltDf[!(grepl("NEG", meltDf$SampleID)),] #Remove negative samples prior checking frequency
meltNeg=meltDf[grepl("NEG", meltDf$SampleID),]

seqFreq=tapply(meltPos$Presence, list(meltPos$SweName),function(x) table(x)/sum(table(x)))
seqFreqDf=as.data.frame(do.call(rbind, seqFreq))
seqFreqDf$SweName = row.names(seqFreqDf)

for(nme in unique(row.names(seqFreqDf))){#subset dataframe by species and replaces NA with correct percent presence value. 
  meltPos$PresencePRCT[which(meltPos$SweName == nme)] = seqFreqDf$'1'[which(row.names(seqFreqDf) == nme)]    
}
meltPos$PresencePRCT=meltPos$PresencePRCT*100 # Convert to percent presence

meltDf=rbind(meltPos, meltNeg)
meltDf = subset(meltDf, select = -Presence) #Removes presence column again 

#Add sequence proportion 
#seqProp=tapply(meltDf$SeqCount, meltDf$SampleID, function(x) (x/sum(x))*100)

seqProp=melt(tapply(meltDf$SeqCount, list(meltDf$SampleID,meltDf$SweName), 
               function(x) (x/sum(meltDf$SeqCount))*100))

seqList=list()
for(aID in unique(meltDf$SampleID)){
  meltDf_subset<-meltDf[which(meltDf$SampleID == aID),]
  seqList[[aID]]<-tapply(meltDf_subset$SeqCount, meltDf_subset$SweName, 
                          function(x)(x/sum(meltDf_subset$SeqCount))*100)
 }
seqX=melt(seqList)
seqX = seqX[order(seqX$L1,seqX$Var1,decreasing = TRUE),]
meltDf=meltDf[order(meltDf$SampleID,meltDf$SweName, decreasing = TRUE),]
meltDf$ProportionPRCT = seqX$value

#Rearrange columns before export
prjIDs = rep(prjID, dim(meltDf)[1])
meltDf$SampleID = paste(meltDf$SampleID, prjIDs, sep = "_")
meltDf$ProjectID = prjIDs
meltDf$ProportionPRCT = round(meltDf$ProportionPRCT,digits = 2) 
meltDf$PresencePRCT = round(meltDf$PresencePRCT,digits = 2)

aOrder = c("ProjectID","SampleID","Primer", "Species", "SweName", "Filter","SeqCount", "ProportionPRCT", "PresencePRCT","Phylum", "Class", "Order","Family", "Genus", "URL", "Sequence")
exportDf = meltDf[,aOrder]

#Export the OTU data. Continue to add the data to the access table 
write.csv(exportDf, paste0("OTU_",prjID,Primer[1],"_MEAD.csv"), row.names = FALSE, fileEncoding = "UTF-8")
print(paste0("OTU table was created: ", getwd(),"/OTU_",prjID,Primer[1],"_MEAD.csv"))

