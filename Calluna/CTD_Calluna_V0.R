library(ggplot2)
library(viridis)
library(reshape2)
library(lubridate)
library(sf)

setwd("C:/Users/MartinAndersson/Calluna AB/JAG0092 Slite förstudie CCS 2021 - Dokument/JAG0092b, Hydrografi/GIS/Script/CTD")

df=read.csv("CTD_JAG0092b_081422_1136.csv", fileEncoding = "latin1") # Needs to be in latin1 to read rows

startChk = TRUE

for(rows in seq(dim(df)[1])){
  clpsRow=paste(df[rows,], collapse="")
  boolChk=grepl(".+Cond µS/cm.+", clpsRow)
  fileCreate = df[grep(".+FILE CREATED:+.", rows),]
  if(nchar(clpsRow) == 0){
    df=df[-rows,]
  }
  if(boolChk && startChk){
    startLine = rows
    print(paste("Title line at row:", startLine))
    names(df) = df[startLine,]
    startChk = FALSE
    df=df[-startLine,]
  }
  else if(boolChk && !(startChk)) {
    df=df[-startLine,]
  }
}

#df=df[-which(df$`Date (MM/DD/YYYY)` == "FILE CREATED:"),]

siteRows=grep("Site Name", df$`Site Name`,fixed =TRUE)

newDf=df[siteRows+1,]

geoDf=read.csv("Hydrografi_Stationer.csv", fileEncoding = "UTF-8")
geoDf$Station=gsub("MU3\\*","MU3",geoDf$Station)
geoDf=geoDf[,c("Station","Bottendjup","X","Y")]

newDf$DateTime=paste(newDf$`Date (MM/DD/YYYY)`,newDf$`Time (HH:mm:ss)`)
newDf=newDf[,!(names(newDf) %in% list("Time (HH:mm:ss)"))]
newDf=newDf[,c("Site Name","Date (MM/DD/YYYY)","Depth m","Temp °C","ODO mg/L","Sal psu")]
newDf$`Sal psu` = round(as.numeric(newDf$`Sal psu`),1)
newDf$`ODO mg/L` = round(as.numeric(newDf$`ODO mg/L`),1)
newDf$`Temp °C` = round(as.numeric(newDf$`Temp °C`),1)
newDf$`Site Name`=as.factor(newDf$`Site Name`)

cmbDf=merge(newDf, geoDf, by.x = "Site Name", by.y = "Station")

cmbDf$Date=lubridate::as_date(cmbDf$`Date (MM/DD/YYYY)`, "%d/%m/%Y", tz = "CET")
cmbDf=cmbDf[,!(names(cmbDf) %in% c("Date (MM/DD/YYYY)"))] 

#crsObj<-st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") #WGS84_Script
#cmbShp=st_as_sf(cmbDf, coords = c("Y","X"), crs = crsObj)
#myGpkg=paste0("CTD_GeoSlite",".gpkg")
#st_write(cmbShp,myGpkg, driver ="GPKG")
#st_write(cmbShp, myGpkg, paste0("CTD_GeoSlite",min(cmbDf$Date)), append = TRUE)

cmbMlt=melt(cmbDf, id.vars = c("Site Name","Date","Depth m", "Bottendjup","X","Y"))
cmbMlt$`Site Name`<-droplevels(as.factor(cmbMlt$`Site Name`))
cmbMlt$`Depth m` = as.numeric(cmbMlt$`Depth m`)

setwd("C:/Users/MartinAndersson/Calluna AB/JAG0092 Slite förstudie CCS 2021 - Dokument/JAG0092b, Hydrografi/GIS/Script/CTD/CTD_Graphs")

sttStr = "https://callunaab.sharepoint.com/teams/JAG0092/Delade%20dokument/Forms/AllItems.aspx?id=%2Fteams%2FJAG0092%2FDelade%20dokument%2FJAG0092b%2C%20Hydrografi%2FGIS%2FScript%2FCTD%2FCTD%5FGraphs%2FCTD%5FGraph%5F"
endStr = "%2Epdf&parent=%2Fteams%2FJAG0092%2FDelade%20dokument%2FJAG0092b%2C%20Hydrografi%2FGIS%2FScript%2FCTD%2FCTD%5FGraphs"

linkList = list()



#Subsets CTD data and creates respective depth profile, then export as pdf.
for(site in unique(cmbMlt$`Site Name`)){
  subMlt = cmbMlt[which(cmbMlt$`Site Name` == site),]
  names(subMlt) = c("Station","Datum", "Djup","Bottendjup","X","Y", "Parameter", "Tal")
  subMlt<-subMlt[order(subMlt$Djup),]
  levels(subMlt$Parameter) <- c("°C", "Syre mg/L", "Salinitet (PSU)") 
  a=ggplot(data = subMlt,aes(y=Djup, x=Tal, color=Parameter))
  a=a+geom_path(size=2)
  pdfTitle = paste0("CTD Profile: ",site," ",unique(subMlt$Datum)[1])
  a=a+scale_y_reverse()+labs(subtitle = pdfTitle)
  a=a+theme_bw(32)+theme(axis.text = element_text(size = 20), axis.title.x = element_blank(), legend.position = "bottom")
  a=a+facet_wrap(~Parameter,scales = "free")
  a=a+scale_color_manual(values =c("#ffad60", "#3b5998", "#88d8b0"))
  a=a+theme(legend.title = element_blank(), legend.text = element_text(),strip.text = element_text(), axis.title.y = element_text())
  unqStr=gsub("_", "%5F", substr(pdfName, 11, nchar(pdfName)-4)) 
  unqStr=gsub("-", "%2D", unqStr)
  linkList = unlist(append(linkList,paste0(sttStr, unqStr, endStr))) 
  pdfName = paste0("CTD_Graph_",site,"_",min(unique(subMlt$Datum)),".pdf")
  ggsave(a, filename = pdfName, device = cairo_pdf, 
         width = 14, height = 16, units = "in")
}

slimGeo=as.data.frame(matrix(ncol = 5, nrow = 0))
names(slimGeo) = c("Site","Bottendjup","X","Y","Date")

slimShp=st_as_sf(slimGeo, coords = c("Y","X"), crs = crsObj)
gpkgPath="C:/Users/MartinAndersson/Calluna AB/JAG0092 Slite förstudie CCS 2021 - Dokument/JAG0092b, Hydrografi/GIS/Script/CTD/"
st_write(slimShp, paste0(gpkgPath,myGpkg), paste0("CTD_GeoSliteSlim"), append = TRUE)


#Creates a matching gpkg layer that can link the PFD:s above
for(site in unique(cmbDf$`Site Name`)){
  first=cmbDf[grep(paste0("^",site,"$"), cmbDf$`Site Name`), c(1,6:9)]
  lowDate=min(unique(first$Date))
  secRow=as.data.frame(first[grep(paste0("^",lowDate,"$"), first$Date, perl = TRUE)[1],])
  slimGeo=rbind(slimGeo, secRow)
}

slimGeo$GraphLink = linkList




#From AquaBiota-CTD script, used for graph identification 

#Create and export individual CTD-graphs for each file in the folder
for(file in fileStr){
  df=read.csv(file, sep =';')
  print(paste("Dataset dimensions:",file,dim(df)[1],dim(df)[2]))
  Sys.sleep(0.3)
  if(dim(df)[1]>200){
    names(df)=gsub("Press.+","Press_[dbar]",names(df), ignore.case = TRUE)
    names(df)=gsub("SALIN.+","SALIN_[PSU]",names(df), ignore.case = TRUE)
    names(df)=gsub("Temp.+","Temp_[?C]",names(df), ignore.case = TRUE)
    names(df)=gsub("DO_ml.+","DO_ml_[ml/l]",names(df), ignore.case = TRUE)
    names(df)=gsub("intt.+","IntT_[Time]",names(df), ignore.case = TRUE)
    names(df)=gsub("intd.+","IntD_[Date]",names(df), ignore.case = TRUE)
    newName=str_extract(file,"^[^.csv]*")
    pdfName=paste0(newName,"_",df$`IntD_[Date]`[1],"_",df$`IntT_[Time]`[1],".pdf")
    pdfName=gsub("(-|:)","",pdfName)
    graphDf=df[c("Press_[dbar]","SALIN_[PSU]","DO_ml_[ml/l]","Temp_[?C]")]
    graphMelt=melt(graphDf, id.vars = c("Press_[dbar]"))
    bp<-ggplot(graphMelt) + geom_path(aes(y=`Press_[dbar]`, x=value, color=variable), size=1.7)
    bp=bp+ylim((max(graphMelt$`Press_[dbar]`)),0)+scale_x_continuous(position = "top")+theme_bw()
    bp=bp+labs(x="Salinitet (PSU)",y="Djup (m)", subtitle="")
    bp=bp+theme(legend.title = element_blank(),panel.grid.major = element_line(colour = "#888888", size=0.2), panel.grid.minor = element_line(colour = "#888888", size=0.2),axis.ticks = element_blank())
    bp=bp+scale_color_manual(labels = c("?C", "Salinitet (PSU)", "Syre (ml/l)"),
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
  names(newFrame)=gsub("Temp.+","Temp_[?C]",names(newFrame), ignore.case = TRUE)
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
  colList = c("Press_[dbar]","SALIN_[PSU]","Temp_[?C]","DO_ml_[ml/l]")
  subName <- names(newFrame)[(names(newFrame) %in% colList)]
  myDf=myDf[,c(subName)]
  myDf$TimeID=rep(theTime, dim(myDf)[1]) 
  myMelt=melt(myDf, id.vars = c("Press_[dbar]","TimeID"))
  print(paste("Stacking data", file))
  dfStack=rbind(myMelt, dfStack)
}