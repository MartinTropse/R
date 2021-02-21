###Refine dropvideo aggregate###


#Set the folder where you file is
setwd("P:/PROJEKT/2020033 Naturvärdesklassning Gotland 2/GIS/Dataset_Naturvärdesbedömning/MA/Shp_Underlag")

#Add the name of the folder. Should be csv here but could be other format. If encoding is UTF-8 that is ideal. 
df=read.csv("AQBI_DROPVIDEO_Gotland_2018_Aggregate_UTF8.csv", encoding ="UTF-8", header=TRUE)

#Converts all factor columns to characters, usually not necessary but does not hurt. 
factLog=sapply(df, is.factor)
df[factLog] <- lapply(df[factLog], as.character)

#Removes rows that does not have a character name, also not necessary but can be good 
x=sapply(df$STATN, nchar) #Needs to be the column which includes the name of stations. 
id=x>0
df=df[id,]

#Write the first column from which you want to aggregate the data, also converts NA to 0 
df[is.na(df)] <- 0
colStart=grep("COVER_COBBLE_COARSE_200_600", colnames(df)) 


###The place where things gets done### 
C = dim(df)[2]-colStart+1
R = unique(df$STATN)
C2 = colStart-1
R = length(unique(df$STATN))

tomDf=as.data.frame(matrix(nrow = R, ncol = C))
names(tomDf) = names(df)[colStart:dim(df)[2]]

dfId=as.data.frame(matrix(nrow = R, ncol = C2))
names(dfId) = names(df)[1:colStart-1]

strVal = 1
rowPos = 1

idStat = unique(df$STATN) #Needs to be the column containing stations name 

for(id in idStat){
  aSet = df[which(df$STATN == id),colStart:dim(df)[2]] #Needs to be the column containing stations name  
  aggData = list(apply(aSet, 2, sum))
  a=do.call(rbind,aggData)
  tomDf[rowPos,] = a
  rowPos = rowPos + 1
}

idStat = unique(df$STATN)
rowPos = 1

for(id in idStat){
  subDf=df[which(df$STATN == id),1:colStart-1]
  dfId[rowPos,] = subDf[1,]
  rowPos = rowPos + 1
}

fullDf=cbind(dfId, tomDf)

#Write the name you want for the aggregated output file 
write.table(fullDf, "AQBI_DROPVIDEO_Gotland_2018_Aggregate.csv", fileEncoding = "UTF-8", sep=',')