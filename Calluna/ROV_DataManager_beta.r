#Useful stackOverFlow answers:
#https://stackoverflow.com/questions/51039704/faster-way-of-find-the-closest-dates-of-a-vector-to-an-element-of-another-vector  #Shows how to use data.table package to identify closest time point
#https://community.rstudio.com/t/convert-timestamp-by-milliseconds/22470/5 Example using milliseconds ##Milisecond example
#https://stackoverflow.com/questions/2150138/how-to-parse-milliseconds #Milisecond example

rm(list=ls()) #Clears the cache from all data/variable etc. Use it to get a "clear start" for the script. 

theName <- "220504_ankringskada" #Enter name to be included in the file exports
uniqueFolder = "220504_001_ankringsskada" #Enter the folder unique folder, remaining folder should be consistant 
filterPosList = c("", "-filtered")
mainOrFilter = filterPosList[1] #Enter 1 to get main position data, enter 2 to get filtered position data 

library(data.table)
library(dplyr)
library(lubridate) 
library(tidyr) 
library(stringr)
library(gsubfn)
library(foreach)
library(stringi)

options(digits.secs=3) #Tells the print command to include milliseconds

setwd(paste0("C:/Users/MartinAndersson/Calluna AB/JAG0092 Slite förstudie CCS 2021 - Dokument/JAG0092d, Batymetri/ROV/Surveys/",uniqueFolder, "/Actors/Rov"))
getwd()

filePosition = paste0('^position', mainOrFilter,'.+\\.+sofp4$')#Will cause an issue if there would be more then one position file in the folder
dirList = list.files()

#Convert position data
position_file=dirList[vapply(dirList, function(x) all(grepl(filePosition, x)), logical(1))]
{
  position_data <-read.table(position_file[1], fill = TRUE, as.is= TRUE, skip= 1)
  position_data<-transform(position_data, V1= as_datetime(V1, tz ="CET"), V2 = as.numeric(V2), V3 = as.numeric(V3), V4 = 0)
  timeStore=lubridate::as_datetime(position_data$V1, tz="CET")
  position_time<-data.frame(str_split_fixed(position_data$V1, " ", 2))
  position_data$Date<-position_time$X1
  position_data$Time<-position_time$X2
  position_data <- position_data[,c("Date","Time","V2","V3","V4")]
  position_data <- rename(position_data, Longitude =V2 ,  Latitude = V3 , Depth =V4)
  position_data$DateTime <- timeStore
  position_data$sDateTime = lubridate::as_datetime(as.character(strptime(timeStore, "%Y-%m-%d %H:%M:%S")), tz ="CET") #Weird line in order to prevent strptime to make an ~complicated timelist object. 
}
#Convert telemetry data
{
  fileTelemetry = 'telemetry.+\\.sogs1$'
  telemetry_file=dirList[vapply(dirList, function(x) all(grepl(fileTelemetry, x)), logical(1))]
  telemetry_data <-read.table(telemetry_file,sep=',', fill = TRUE, as.is= TRUE, skip= 1)
  telemetry_time<-data.table(gsub('[|$[:alpha:]]', ' ', telemetry_data$V1))
  telemetry_time<-transform(telemetry_time, V1 = as_datetime(V1))
  telemetry_depth<-gsub('[*]', ' ', telemetry_data$V5)
  telemetry_depth<-data.frame(str_split_fixed(telemetry_depth, " ", 2))
  telemetry_depth<-transform(telemetry_depth, X1 = as.numeric(X1))
  telemetry_data <- subset(telemetry_data, select = -c(V1,V5))
  telemetry_data<-transform(telemetry_data, V2 = as.numeric(V2),V3 = as.numeric(V3),
                            V4 = as.numeric(V4))
  telemetry_time<-data.frame(str_split_fixed(telemetry_time$V1, " ", 2))
  telemetry_data$Date<-telemetry_time$X1
  telemetry_data$Time<-telemetry_time$X2
  telemetry_data$Depth<-telemetry_depth$X1
  telemetry_data <- telemetry_data[,c("Date","Time","V2","V3","V4", "Depth")]
  telemetry_data <- rename(telemetry_data, Heading = V2 ,  Roll = V3 , pitch = V4)
  telemetry_data <- na.omit(telemetry_data)
}
#Convert attention point data
dirList <- list.files(pattern = paste0("[0-9]{2}",".*."), all.files = TRUE, full.names = FALSE)
fileAttention = '^attention.+\\.+soap1$'
attFile=dirList[vapply(dirList, function(x) all(grepl(fileAttention, x)), logical(1))]

out=read.pattern(attFile, "<AttentionPoint>.+")
attPoints=strsplit(out$V1, split="<AttentionPoint>")
attPoints=attPoints[[1]][2:length(attPoints[[1]])]

idList = list()
dateList = list()
timeList = list()
noteList = list()
catList = list()
  
for(pos in seq(1,length(attPoints),1)){
  aPoint=attPoints[pos]
  idList = append(idList, str_extract(aPoint,"(?<=<Id>).+(?=</Id>)"))
  dateList = append(dateList, str_extract(aPoint, "[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}"))
  timeList = append(timeList, str_extract(aPoint, "(?<=T)[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}\\.[0-9]{3}"))
  noteList = append(noteList, str_extract(aPoint, "(?<=<Note>).+(?=</Note>)"))
  catList = append(catList, str_extract(aPoint, "(?<=<Category>).+(?=</Category>)"))
}
  
attention_data=as.data.frame(matrix(nrow = length(idList), ncol = 5))
names(attention_data) = c("ID", "Date", "Time", "Note","Category")

attention_data$ID = idList
attention_data$Date = dateList
attention_data$Time = timeList
attention_data$Note = noteList
attention_data$Category = catList

###Match times and extract positions section###
attention_data$DateTime = paste(attention_data$Date, attention_data$Time, sep=" ")
telemetry_data$DateTime = paste(telemetry_data$Date, telemetry_data$Time, sep=" ")

position_data$DateTime=lubridate::as_datetime(position_data$DateTime,  tz = "CET")
attention_data$DateTime=lubridate::as_datetime(attention_data$DateTime,  tz = "CET")
telemetry_data$DateTime=lubridate::as_datetime(telemetry_data$DateTime,  tz = "CET")

attention_data$Latitude = NA
attention_data$Longitude = NA
attention_data$pDateTime = NA 
rowList = list()

#Loop that adds the position coordinates/data for attention points that has an exact time point match  
foreach(aChrTime = as.character(attention_data$DateTime), aPos = seq(1,length(attention_data$DateTime),1)) %do%{
  timeID=grep(aChrTime, as.character(position_data$DateTime))
  if(length(timeID) > 0){ 
    attention_data$Latitude[aPos]=position_data$Latitude[timeID]
    attention_data$Longitude[aPos]=position_data$Longitude[timeID]
    attention_data$pDateTime[aPos]=as.character(position_data$DateTime[timeID])
  }
    else {
      (rowList = append(rowList, aPos))
  }
}

#Loop that adds the position coordinates/data for all attention points that did not have an exact match 
#Code is rather slow, might need optimization if it becomes necessary to work with large number of attention points (~>50)
for(aRow in rowList[1:length(rowList)-1]){
  minVal = 1000000
  future = aRow+1
  for(pRow in seq(1, length(position_data$DateTime), 1)){
    timeInterval_a1 <- attention_data$DateTime[aRow] %--% position_data$DateTime[pRow] 
    timeInterval_a2 <- attention_data$DateTime[future] %--%  position_data$DateTime[pRow] 
    if(timeInterval_a1@.Data >= 0 & timeInterval_a2@.Data <0){
      aVal = abs(timeInterval_a1@.Data)
      if(aVal < minVal){
        minVal = aVal
        attention_data$Latitude[aRow] = position_data$Latitude[pRow]
        attention_data$Longitude[aRow] = position_data$Longitude[pRow]
        attention_data$pDateTime[aRow] = as.character(position_data$DateTime[pRow])
        print(paste("Adding position data from time point:",position_data$DateTime[pRow]))
      }
    }
  }
}

#Same purpose as above, but adds only the last row/point of the attention points. (could been incorporated above also)
for(lastRow in rowList[length(rowList)]){
  minVal = 1000000
  presence = aRow
  for(pRow in seq(1, length(position_data$DateTime), 1)){
    timeInterval_a1 <- attention_data$DateTime[lastRow] %--% position_data$DateTime[pRow] 
    timeInterval_a2 <- attention_data$DateTime[presence] %--%  position_data$DateTime[pRow] 
    if(timeInterval_a1@.Data >= 0){
      aVal = abs(timeInterval_a1@.Data)
      if(aVal < minVal){
        minVal = aVal
        attention_data$Latitude[lastRow] = position_data$Latitude[pRow]
        attention_data$Longitude[lastRow] = position_data$Longitude[pRow]
        attention_data$pDateTime[lastRow] = as.character(position_data$DateTime[pRow])
        print(paste("Adding position data from time point:",position_data$DateTime[pRow]))
      }
    }
  }
}

#For loop to save the converted data tables as csv files and save them under the converted file sub-directory
list2 <- c('attention_data', 'position_data', 'telemetry_data')

attention_data$Date = unlist(attention_data$Date)
attention_data$ID = unlist(attention_data$ID)
attention_data$Time = unlist(attention_data$Time)
attention_data$Note = unlist(attention_data$Note)
attention_data$Category = unlist(attention_data$Category)

for (i in 1:length(list2)){
  aData=get(list2[i]) 
  aName=paste0("converted_",list2[i],".csv")
  write.csv(aData, aName, row.names = FALSE)
} 

telemetry_data$DateTime=lubridate::as_datetime(as.character(strptime(telemetry_data$DateTime, "%Y-%m-%d %H:%M:%S")), tz = "CET")
attention_data$pDateTime=lubridate::as_datetime(as.character(strptime(attention_data$pDateTime, "%Y-%m-%d %H:%M:%S")), tz = "CET")

telTime <- structure(telemetry_data$DateTime, class = c("POSIXct", "POSIXt"), tzone = "CET") 
attTime <- structure(attention_data$pDateTime, class = c("POSIXct", "POSIXt"), tzone = "CET")

telposMrg = merge(telemetry_data, position_data, by.x = "DateTime", "sDateTime")
telposMrg = telposMrg[,names(telposMrg)[c(1:7,9:11)]] 

fullMrg = merge(telposMrg,attention_data, by.x = "DateTime", "pDateTime")

names(fullMrg)
selectMe = c("Date", "Time","Heading","Roll", "pitch", "Depth.x", "Longitude.x","Latitude.y", "ID", "Note", "Category")
subMrg=fullMrg[,names(fullMrg) %in% selectMe]

subMrg=subMrg %>%
  distinct(ID, .keep_all = TRUE)
#names(subMrg) = c("Heading", "Roll","Pitch", "Depth","Longitude", "ID", "Date", "Time", "Note", "Category", "DateTime","Latitude")
names(subMrg) = c("Heading", "Roll","Pitch", "Depth","Longitude", "ID", "Date", "Time", "Note", "Category", "Latitude")

expAttentionMrg=subMrg[,c("ID","Note", "Date", "Time","Depth", "Heading", "Longitude", "Latitude")]
write.csv(expAttentionMrg, paste0("attention_data_",theName,".csv"), row.names = FALSE)



###Match each video with its respective geolocation### 
sonrT_Rgx="(?<=SONAR-)[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}-[0-9]{2}-[0-9]{2}-[0-9]{3}" #Finds time in solar video
sonrF_Rgx=".+SONAR.+\\.mpg$" #Finds sonar video file

hdF_Rgx =".+HD_VIDEO.+\\.mpg$" #Finds HD-video file 
hdT_Rgx ="(?<=HD_VIDEO-)[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}-[0-9]{2}-[0-9]{2}-[0-9]{3}" #Finds time in HD_Video file

overF_Rgx=".+OVERLAY_VIDEO.+\\.mpg$" #Finds overlay video file
overT_Rgx="(?<=OVERLAY_VIDEO-)[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}-[0-9]{2}-[0-9]{2}-[0-9]{3}" #Finds overlay video time

ovrTime = list()
ovrFile = list()
HDFile = list()
HDTime = list()
sonTime = list()
sonFile = list()
theCat = list()

videoList=dirList[grep(".+\\.mpg$",dirList)]
videoDf = as.data.frame(matrix(nrow = length(videoList), ncol = 2))
#names(videoDf) = c("overFile","HdFile","sonarFile", "FileTime")
names(videoDf) = c("VideoFile", "FTime_Orig")

sink("Printout_Store.txt")
#Sorts videofiles and times into dataframe and seperate list
foreach(aFile = videoList, posFile = seq(1, length(videoList), 1)) %do% {
  hdF=str_extract(aFile, hdF_Rgx)
  ovrF=str_extract(aFile, overF_Rgx)
  sonF=str_extract(aFile, sonrF_Rgx)
  if(!(is.na(hdF))){
    hdT=str_extract(aFile, hdT_Rgx)
    videoDf[posFile,c("VideoFile")] = hdF
    videoDf[posFile,c("FTime_Orig")] = hdT
    HDFile = append(HDFile, hdF)
    HDTime = append(HDTime, gsub("T"," ", hdT))
    theCat = append(theCat, "HD_Video")
  } else if (!(is.na(ovrF))){
    ovrT=str_extract(aFile, overT_Rgx)
    videoDf[posFile,c("VideoFile")] = ovrF
    videoDf[posFile,c("FTime_Orig")] = ovrT
    ovrFile = append(ovrFile,ovrF)
    ovrTime = append(ovrTime,gsub("T"," ", ovrT))
    theCat = append(theCat, "Overlay_Video")
  } else if (!(is.na(sonF))){
    sonT=str_extract(aFile,sonrT_Rgx)  
    videoDf[posFile,c("VideoFile")] = sonF
    videoDf[posFile,c("FTime_Orig")] = sonT
    sonFile = append(sonFile, sonF)
    sonTime = append(sonTime, gsub("T"," ", sonT))
    theCat = append(theCat, "Multibeam_Video")
  }
}
sink()
videoDf[paste0('v', 2:3)] <- stri_list2matrix(strsplit(as.character(videoDf$FTime_Orig),'T', perl = TRUE),byrow =TRUE)

#videoDf$v3 = gsub("-[0-9]{3}$", "",videoDf$v3)
videoDf$v3 = gsub("-", ":",videoDf$v3)
videoDf$VTime = paste(videoDf$v2,videoDf$v3)
videoDf$VTime=gsub(":(?=[0-9]{3})", "\\.",videoDf$VTime, perl = TRUE)
videoDf$VTime = lubridate::ymd_hms(videoDf$VTime, tz="CET")

vTime=structure(videoDf$VTime, class = c("POSIXct", "POSIXt"), tzone = "CET")
pTime=structure(position_data$DateTime, class = c("POSIXct", "POSIXt"), tzone = "CET")

vDT = data.table(vTime) 
pDT = data.table(pTime)

videoPosIdx=pDT[vDT, on = .(pTime = vTime),roll = "nearest", which = TRUE ]

videoDf$Latitude = NA
videoDf$Longitude = NA

for(idx in seq(1,length(videoPosIdx),1)){ #This loop can have an issue if there a position data with identical time distance to video file, then remove one of the times
  videoDf$Latitude[idx] = position_data$Latitude[videoPosIdx[idx]]
  videoDf$Longitude[idx] = position_data$Longitude[videoPosIdx[idx]]
}

videoDf$Filepath=paste(getwd(), videoDf$VideoFile, sep = "/")
videoDf$Category = unlist(theCat)
names(videoDf)

expVideo = videoDf[,c("Category","VideoFile","Longitude","Latitude", "Filepath")]
write.csv(expVideo, paste0("videopaths_",theName, ".csv"), row.names = FALSE) #End of
#End of video_positioning

# Simplified Points -------------------------------------------------------
# Use the position data to create a simplified set of points for use in GIS
position <- read.csv('converted_position_data.csv')
{
  position <-data.frame(position)
  position$Time <-hms(position$Time)
  Longitude <- aggregate(position$Longitude, 
                         by = list(hours=position$Time@hour, minutes=position$Time@minute),mean)
  Latitude <- aggregate(position$Latitude, 
                        by = list(hours=position$Time@hour, minutes=position$Time@minute),mean)
  Longitude <- rename(Longitude, longitude = x)
  Latitude <- rename(Latitude, latitude = x)
  Longitude <- Longitude[order(Longitude$hours), ]
  Latitude <- Latitude[order(Latitude$hours), ]
  Longitude$latitude <- Latitude$latitude
  avg_line= Longitude
}


# Change the name in "file = " to an identifiable name for the specific position data
write.csv(avg_line,file = "simplified_testroute.csv", row.names = FALSE) #edit the name of the simplified line file