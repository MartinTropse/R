rm(list = ls())

library(vegan)
library(dplyr)
library(reshape2)
library(sf)

setwd("C:/Bas/AquaBiota/Projekt/Life DNAquatics/Spatial_study")

###Formating Env & Spc Data###
df=read.csv("SeqData_Moalven.csv", fileEncoding = "UTF-8-BOM")
env=read.csv("EnvData_Moalven.csv", fileEncoding = "UTF-8-BOM")

df=df[1:114,]#Cuts away none-data rows 
env=env[1:114,]#Cuts away none-data rows 

env=env[which(env$Type != "Marine"),]
df=df[df$Sample.ID %in% env$Sample.ID,] #De-selects marine samples from speceis data. 

spc = df[,4:37]
row.names(spc) = df$Sample.ID
row.names(df) = df$Sample.ID
spc[is.na(spc)] = 0

spc=spc[,colSums(spc)>0]#Removes columns from species that lack presence after subsetting 


###Remove additional species 
names(spc)
rmvSpc=c("Gadus.morhua","Myoxocephalus.sp...Rötsimpa.","Gobio.gobio.sandkrypare", "Clupea.harengus")
spc=spc[,!(names(spc) %in% rmvSpc)]
spcList = names(spc)

lenSpc=length(spcList) 
spcList=sort(colSums(spc)) #Check the distrubution of species present 
spcList[(lenSpc-10):lenSpc]/sum(spcList) # Shows the 10 most common species and their percentage among total sequences

spc=as.data.frame(apply(spc, MARGIN = 2, function(x) ifelse(x>0, 1,0))) #Converts sequences to presence/absence 

###Species frequency###
df$Site.ID = as.factor(df$Site.ID)

numDf=df[,names(df) %in% names(spc)] #
numDf[is.na(numDf)] = 0
numDf[numDf>0] = 1

###Loop for random selection of samples until all species are found###
numDf=numDf[,which(colSums(numDf) >0)]
surveyCount = vector()
surveyCount2 = vector()
# dim(numDf)[2]*0.8 ### Check a certain percentage of the total diversity, and how many species it equals.

for(aLoop in seq(50)){
  spcList=names(numDf)
  sampleCount = 0
  for(aSample in sample.int(dim(numDf)[1],dim(numDf)[1], replace = FALSE)){
    mySample=numDf[aSample,]
    sampleCount = sampleCount + 1
    spcPrs=(colSums(mySample) == 1)
    newSpcNm=names(mySample)[spcPrs]
    spcID=!(spcList %in% newSpcNm) 
    spcList=spcList[spcID] # Removes the species that were found in the list 
    if(length(spcList) < 2){ #6 equals 80% of all species if spcCount ~= 30 
      surveyCount2 = append(surveyCount2, sampleCount)
      break
    }
  }
}

mean(surveyCount2) #Shows the mean amount of samples to reach set diversity through random selection 
sd(surveyCount2) #Shows the standard error for sample amount to reach set diversity through random selection  

###Finding maximum amount of species with minimum sample size###

row.names(env) <- env$Sample.ID # Make row names in env match numDf row.names
temDf = numDf #Create a none changing version of the dataset

"""
Loop for to identifying optimal sampling sequences with two random start samples.  
Each run gets stored as a layer in a geodatabase 'Stry_geo...' (will be overwritten if geodatabase name is not changed below)
Can the be loaded in to QGIS or other GIS-software.  
"""

for(aRun in seq(10)){  #Change the number, to set amount samples designed created
  spcList=names(numDf)
  #randVal=sample.int(dim(numDf)[1],2,  replace = FALSE) # Select two random numbers for start samples
  randVal=grep("LiSp_055", row.names(numDf)) #Use in the loop to start with the sample with the maximum species count
  mySample=numDf[randVal,] # Select the random sample
  print(row.names(mySample))
  coolVec = randVal
  spcPrs=(colSums(mySample) > 0) # Selects the species that were present
  newSpcNm=names(mySample)[spcPrs] #The names of species present
  spcID=!(spcList %in% newSpcNm)  
  spcList=spcList[spcID] # Removes the species that were found in the list 
  while(length(spcList)>1){
    sMax = 0
    for(qVal in seq(dim(numDf)[1])){
      #print(qVal)
      secSample=temDf[qVal,]
      spcPrs=(colSums(secSample) == 1)
      newSpcNm=names(secSample)[spcPrs]
      spcID= spcList %in% newSpcNm 
      if(sum(spcID)>sMax){
        sMax=sum(spcID)
        coolSmpl = qVal
        #print(paste("The really new cool sample is!:", coolSmpl))
        delteSmpl = newSpcNm
      }
      if(qVal == max(seq(dim(numDf)[1]))){
        #print(paste("Initial species length: ", length(spcList)))
        #print(paste("The greatest sample... ","is...", coolSmpl, "!!!"))
        spcList=spcList[!(spcList %in% delteSmpl)]
        #print(paste("Final species length: ", length(spcList)))
        randVal = append(randVal, coolSmpl)
        #Sys.sleep(3)
        break
      }
    }
  }
  strySeq=merge(numDf[randVal,], env, by = "row.names")
  crsObj = "+proj=longlat +datum=WGS84 +no_defs +type=crs"
  stryGeo = st_as_sf(strySeq, coords = c("Y","X"), crs = crsObj)
  gpkgPath = getwd()
  geoName = "stryGeo9.gpkg"
  st_write(stryGeo, paste0(gpkgPath,"/", geoName), paste0("stry_10_",aRun), append = TRUE)
}