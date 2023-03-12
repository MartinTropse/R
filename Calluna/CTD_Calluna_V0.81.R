rm(list = ls())

library(ggplot2)
library(viridis)
library(reshape2)
library(lubridate)
library(sf)
library(ggpubr)

#Raw data folder
setwd("C:/Users/MartinAndersson/Calluna AB/JAG0092-1_CCS_2021 - Dokument/JAG0092b_Hydrografi/Arbetsdokument/CTD/Rawdata_CTD2022/New")
list.files()

df=read.csv("11aug_3nov_KorMeasurementFileExport_111422_145235.csv", fileEncoding = "latin1") # Needs to be in latin1 to read rows

startChk = TRUE

#Finds the "startpoint" of data in the file and sets/identify the headers
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


siteRows=grep("Site Name", df$`Site Name`,fixed =TRUE)
newDf=df[siteRows+1,]

#Loads dataframe with metadata for hydrografi stations
geoDf=read.csv("Hydrografi_Stationer1.csv", fileEncoding = "UTF-8")
geoDf$Station=gsub("MU3\\*","MU3",geoDf$Station)
geoDf=geoDf[,c("Station","Bottendjup","X","Y")]

newDf$DateTime=paste(newDf$`Date (MM/DD/YYYY)`,newDf$`Time (HH:mm:ss)`)
newDf=newDf[,!(names(newDf) %in% list("Time (HH:mm:ss)"))] # 
newDf=newDf[,c("Site Name","Date (MM/DD/YYYY)","Depth m","Temp °C","ODO mg/L","Sal psu")]
newDf$`Sal psu` = round(as.numeric(newDf$`Sal psu`),1)
newDf$`ODO mg/L` = round(as.numeric(newDf$`ODO mg/L`),1)
newDf$`Temp °C` = round(as.numeric(newDf$`Temp °C`),1)
newDf$`Site Name`= as.factor(newDf$`Site Name`)

cmbDf=merge(newDf, geoDf, by.x = "Site Name", by.y = "Station")
cmbDf$Date=lubridate::as_date(cmbDf$`Date (MM/DD/YYYY)`, "%m/%d/%Y", tz = "CET") # Creates new date col
cmbDf=cmbDf[,!(names(cmbDf) %in% c("Date (MM/DD/YYYY)"))]  # Removes previous one 

cmbMlt=melt(cmbDf, id.vars = c("Site Name","Date","Depth m", "Bottendjup","X","Y"))
cmbMlt$`Site Name`<-droplevels(as.factor(cmbMlt$`Site Name`))
cmbMlt$`Depth m` = as.numeric(cmbMlt$`Depth m`)

#Set names consistent columns names
names(cmbDf) <- c("Station", "Djup", "Temperatur","Syre_mgL", "Sal_PSU", "Bottendjup", "Y", "X", "Date") #Check X Y
cmbDf=cmbDf[,c("Station", "Djup", "Temperatur","Syre_mgL", "Sal_PSU", "Bottendjup", "X", "Y","Date")] #Check X Y

CTD_Path="C:/Users/MartinAndersson/Calluna AB/JAG0092-1_CCS_2021 - Dokument/JAG0092b_Hydrografi/Arbetsdokument/CTD"
dir.create(file.path(CTD_Path, "Graphs2023"))
setwd(file.path(CTD_Path, "Graphs2023"))

#These links needs a to pasted together with the intersecting file path  
#Sharepoint folder path 
newStart="https://callunaab.sharepoint.com/teams/JAG0092-1/Delade%20dokument/Forms/AllItems.aspx?id=%2Fteams%2FJAG0092%2D1%2FDelade%20dokument%2FJAG0092b%5FHydrografi%2FArbetsdokument%2FCTD%2FGraphs2023%2F"
#Sharepoint end path  
newEnd=  "%2Epdf&parent=%2Fteams%2FJAG0092%2D1%2FDelade%20dokument%2FJAG0092b%5FHydrografi%2FArbetsdokument%2FCTD%2FGraphs2023"
#newPath=paste0(newStart, newFile, newEnd)

linkList = list()

for(site in unique(cmbMlt$`Site Name`)){
  subMlt = cmbMlt[which(cmbMlt$`Site Name` == site),]
  print(dim(subMlt))
  dateVec=unlist(lapply(unique(subMlt$Date), function(x) as.character(x)))
  for(aDate in dateVec){
    dtMlt = subMlt[which(subMlt$Date == aDate),]
    names(dtMlt) = c("Station","Datum", "Djup","Bottendjup","X","Y", "Parameter", "Tal")
    dtMlt<-dtMlt[order(dtMlt$Parameter, dtMlt$Djup),]
    levels(dtMlt$Parameter) <- c("°C", "Syre mg/L", "Salinitet (PSU)") 
    a=ggplot(data = dtMlt,aes(y=Djup, x=Tal, color=Parameter))
    a=a+geom_path(size=2)
    pdfTitle = paste0("CTD Profile: ",site," ", aDate)
    a=a+scale_y_reverse()+labs(subtitle = pdfTitle)
    a=a+theme_bw(32)+theme(axis.text = element_text(size = 20), axis.title.x = element_blank(), legend.position = "bottom")
    a=a+facet_wrap(~Parameter,scales = "free")
    a=a+scale_color_manual(values =c("#ffad60", "#3b5998", "#88d8b0"))
    a=a+theme(legend.title = element_blank(), legend.text = element_text(),strip.text = element_text(), axis.title.y = element_text())
    pdfName = paste0("CTD_",site,"_",aDate,".pdf")
    unqStr = gsub("_", "%5F", substr(pdfName, 1, nchar(pdfName)-4)) 
    unqStr = gsub("-", "%2D", unqStr)
    linkList = unlist(append(linkList,paste0(newStart, unqStr, newEnd))) 
    ggsave(a, filename = pdfName, device = cairo_pdf, 
           width = 14, height = 16, units = "in")
  }
}

#Finds the year and month interval, and converts it to a string for geofile naming. 
srtMon=min(lubridate::month(cmbMlt$Date, label = TRUE))
endMon=max(lubridate::month(cmbMlt$Date, label = TRUE))
srtYr=min(lubridate::year(cmbMlt$Date))
endYr=max(lubridate::year(cmbMlt$Date))

moonName=gsub(" ", "", paste(as.character(unique(c(srtMon,endMon))), collapse  = " ")) 
yrName=gsub(" ", "_", paste(as.character(unique(c(srtYr,endYr))), collapse  = " ")) 
dateName = paste(yrName, moonName, sep = "_")

#Export the cmbDf for use in the scatterplot/merge section further down 
dataPath="C:/Users/MartinAndersson/Calluna AB/JAG0092-1_CCS_2021 - Dokument/JAG0092b_Hydrografi/Arbetsdokument/CTD/CTD_CCS_Exprtdata"
write.csv(cmbDf,paste0(dataPath,"/",dateName,".csv"), row.names = FALSE)

###Creates an empty gpkg frame###
slimGeo=as.data.frame(matrix(ncol = 5, nrow = 0))
names(slimGeo) = c("Site","Bottendjup","Y","X","Date")

#Creates a matching gpkg layer that can link the PFD:s above
for(site in unique(cmbDf$Station)){
  first=cmbDf[grep(paste0("^",site,"$"), cmbDf$Station), c(1,6:9)]
  for(secDate in unique(as.character(first$Date))){
    secRow = as.data.frame(first[grep(secDate, first$Date, perl = TRUE)[1],])
    slimGeo = rbind(slimGeo, secRow)
  }
}

slimGeo$GraphLink = linkList

##Creates a geolayer and writes it as an new layer in an existing geopackage 
crsObj<-st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") #WGS84_Script
slimShp=st_as_sf(slimGeo, coords = c("X","Y"), crs = crsObj)
gpkgPath="C:/Users/MartinAndersson/Calluna AB/JAG0092-1_CCS_2021 - Dokument/JAG0092b_Hydrografi/Arbetsdokument/CTD"
myGpkg=file.path(gpkgPath,"CTD_CCS.gpkg")
#This line both creates and store layer, if the geopackage exist it just updates with new layer
st_write(slimShp, myGpkg, paste0("CTD", dateName), append = TRUE)


#Merge with previous CTD-dataset 
setwd("C:/Users/MartinAndersson/Calluna AB/JAG0092-1_CCS_2021 - Dokument/JAG0092b_Hydrografi/Arbetsdokument/CTD/CTD_CCS_Exprtdata")
fileList = list.files()
fileList = fileList[grepl(".csv",fileList)] #Select files from the current folder ending with .csv

ctdMerge=as.data.frame(matrix(ncol = 5, nrow = 0), col.names = c("Station","Djup","Temperatur","Syre_mgL","Sal_PSU","Bottendjup","X","Y","Date"))

for(csvFile in fileList){
  priotDf = read.csv(csvFile,sep = ",")
  ctdMerge=rbind(ctdMerge,priotDf)
}

write.csv(ctdMerge,"CTD_AssembledData.csv", row.names = FALSE)

#Create summary CTD-graph
bigCTD = read.csv("CTD_AssembledData.csv", sep = ",")
bigCTD$Month=lubridate::month(as.Date(bigCTD$Date, "%m/%d/%Y"), label = TRUE)

bigCTD = bigCTD[,c("Station","Djup","Temperatur","Syre_mgL","Sal_PSU","Month")]

myMlt=melt(bigCTD,id.vars = c("Station","Djup","Month"))
#myMlt$Month = as.factor(myMlt$Month)
levels(myMlt$variable) <- c("°C", "Syre mg/L", "Salinitet (PSU)")

salDf = na.omit(myMlt[which(myMlt$variable == "Salinitet (PSU)"),])# Select salinity and remove NA-values
negSid = which(salDf$value == min(salDf$value))
salDf = salDf[-negSid,]

oxDf = na.omit(myMlt[which(myMlt$variable == "Syre mg/L"),])
negId=which(oxDf$value == min(oxDf$value))
oxDf=oxDf[-negId,]

temDf = na.omit(myMlt[which(myMlt$variable == "°C"),])
negId=which(temDf$value == min(temDf$value))
temDf=temDf[-negId,]


###Creates four summary graphs of the CTD data, using the different datasets above. 
sp= ggplot(data=salDf, aes(y=Djup, x=value, color = Month))
sp= sp + geom_point(size = 0.9)
sp= sp + theme_bw(14) + labs(x="Salinintet (PSU)")
sp= sp + scale_y_reverse()
sp= sp + scale_color_manual(values =c("#ffad60", "#3b5998", "#88d8b0", "#4c1130"))
sp= sp + theme(legend.position = "none")
#sp= sp + geom_smooth(fullrange = FALSE)

op= ggplot(data=oxDf, aes(y=Djup, x=value, color = Month))
op= op + geom_point(size = 0.9)
op= op + theme_bw(14) + labs(x="Syre mg/L")
op= op + scale_color_manual(values =c("#ffad60", "#3b5998", "#88d8b0", "#4c1130"))
op= op + theme(legend.position = "none")
op= op + scale_y_reverse()

tp= ggplot(data=temDf, aes(y=Djup, x=value, color = Month))
tp= tp + geom_point(size = 0.9)
tp= tp + theme_bw(14) + labs(x="°C")
tp= tp + scale_color_manual(values =c("#ffad60", "#3b5998", "#88d8b0"))
tp= tp + scale_y_reverse()
tp= tp + theme(legend.position = "none")

bx = ggplot(data=myMlt, aes(y = value, x = as.factor(variable), fill = Month))
bx = bx + geom_boxplot(lwd = 0.2)
bx = bx + theme_bw(14)
bx = bx + theme(legend.margin=margin(0,0,0,0), 
                legend.spacing.x = unit(2, "mm"), legend.spacing.y = unit(0, "mm"), legend.text = element_text(size = 18), legend.title = element_text(size = 20))
bx = bx + labs(y="Mätvärden", x = "")
bx = bx + scale_fill_manual(values =c("#ffad60", "#3b5998", "#88d8b0"))
bx = bx + guides(fill = guide_legend(title = "Månad",keywidth = unit(1.6, "cm"), keyheight = unit(1.6,"cm")))

setwd("C:/Users/MartinAndersson/Calluna AB/JAG0092-1_CCS_2021 - Dokument/JAG0092b_Hydrografi/Arbetsdokument/CTD/Graphs2023")


squarePlot=ggarrange(bx, tp, op, sp, ncol = 2, nrow = 2,common.legend = TRUE, legend = "bottom")
squarePlot=annotate_figure(squarePlot, top = text_grob("CTD-CSS: Utredningsområde 2022-2023", 
                                      color = "black", size = 20))
ggsave(squarePlot, filename = "Summary_CTD_CCS_2022-2023.pdf", device = cairo_pdf, 
       width = 14, height = 16, units = "in")