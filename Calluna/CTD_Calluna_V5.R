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

geoDf=read.csv("Hydrografi_Stationer.csv", fileEncoding = "UTF-8")
geoDf$Station=gsub("MU3\\*","MU3",geoDf$Station)
geoDf=geoDf[,c("Station","Bottendjup","X","Y")]

newDf$DateTime=paste(newDf$`Date (MM/DD/YYYY)`,newDf$`Time (HH:mm:ss)`)
newDf=newDf[,!(names(newDf) %in% list("Time (HH:mm:ss)"))]
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

#Export the cmbDf for use in the scatterplot/merge section further down 
names(cmbDf) <- c("Station", "Djup", "Temperatur","Syre_mgL", "Sal_PSU", "Bottendjup", "Y", "X", "Date") #Check X Y
cmbDf=cmbDf[,c("Station", "Djup", "Temperatur","Syre_mgL", "Sal_PSU", "Bottendjup", "X", "Y","Date")] #Check X Y

CTD_Path="C:/Users/MartinAndersson/Calluna AB/JAG0092-1_CCS_2021 - Dokument/JAG0092b_Hydrografi/Arbetsdokument/CTD"
dir.create(file.path(CTD_Path, "Graphs"))
setwd(file.path(CTD_Path, "Graphs"))

sttStr = "https://callunaab.sharepoint.com/teams/JAG0092-1/Delade%20dokument/Forms/AllItems.aspx?id=%2Fteams%2FJAG0092%2D1%2FDelade%20dokument%2FJAG0092b%5FHydrografi%2FArbetsdokument%2FCTD%2FGraphs%2FCTD%5FGraph%5F"
endStr = "%2Epdf&viewid=9f30aa4a%2Dbcbc%2D45f0%2D8284%2D065ac39dc79a&parent=%2Fteams%2FJAG0092%2D1%2FDelade%20dokument%2FJAG0092b%5FHydrografi%2FArbetsdokument%2FCTD%2FGraphs"

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
  pdfName = paste0("CTD_Graph_",site,"_",min(unique(subMlt$Datum)),".pdf")
  unqStr=gsub("_", "%5F", substr(pdfName, 11, nchar(pdfName)-4)) 
  unqStr=gsub("-", "%2D", unqStr)
  linkList = unlist(append(linkList,paste0(sttStr, unqStr, endStr))) 
  ggsave(a, filename = pdfName, device = cairo_pdf, 
         width = 14, height = 16, units = "in")
}

#Finds the year and month interval, and converts it to a string for geofile naming. 
srtMon=min(lubridate::month(cmbMlt$Date, label = TRUE))
endMon=max(lubridate::month(cmbMlt$Date, label = TRUE))
srtYr=min(lubridate::year(cmbMlt$Date))
endYr=max(lubridate::year(cmbMlt$Date))

moonName=gsub(" ", "", paste(as.character(unique(c(srtMon,endMon))), collapse  = " ")) 
yrName=gsub(" ", "_", paste(as.character(unique(c(srtYr,endYr))), collapse  = " ")) 
dateName = paste(yrName, moonName, sep = "_")

###Creates an empty gpkgPath###
slimGeo=as.data.frame(matrix(ncol = 5, nrow = 0))

#Creates a matching gpkg layer that can link the PFD:s above
for(site in unique(cmbDf$Station)){
  first=cmbDf[grep(paste0("^",site,"$"), cmbDf$Station), c(1,6:9)]
  lowDate=min(unique(first$Date))
  secRow=as.data.frame(first[grep(paste0("^",lowDate,"$"), first$Date, perl = TRUE)[1],])
  slimGeo=rbind(slimGeo, secRow)
}

names(slimGeo) = c("Site","Bottendjup","Y","X","Date")

slimGeo$GraphLink = linkList

#Link example #https://callunaab.sharepoint.com/teams/JAG0092-1/Delade%20dokument/Forms/AllItems.aspx?id=%2Fteams%2FJAG0092%2D1%2FDelade%20dokument%2FJAG0092b%5FHydrografi%2FArbetsdokument%2FCTD%2FGraphs%2FCTD%5FGraph%5FA%5F2022%2D08%2D11%2Epdf&viewid=9f30aa4a%2Dbcbc%2D45f0%2D8284%2D065ac39dc79a&parent=%2Fteams%2FJAG0092%2D1%2FDelade%20dokument%2FJAG0092b%5FHydrografi%2FArbetsdokument%2FCTD%2FGraphs

crsObj<-st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") #WGS84_Script
slimShp=st_as_sf(slimGeo, coords = c("X","Y"), crs = crsObj)
gpkgPath="C:/Users/MartinAndersson/Calluna AB/JAG0092-1_CCS_2021 - Dokument/JAG0092b_Hydrografi/Arbetsdokument/CTD"
myGpkg=file.path(gpkgPath,"CTD_GeoSlite1.gpkg")
st_write(slimShp, myGpkg, paste0("CTD", dateName), append = TRUE)

# """
# -Creating geodata from earlier hydrografi data. Merge it with new CTD-data. 
# -Create scatter plot based on the merged data, by depth and month.
# -Add the graph to a fake point within the dataframe. 
# """
setwd("C:/Users/MartinAndersson/Calluna AB/JAG0092-1_CCS_2021 - Dokument/JAG0092b_Hydrografi/Arbetsdokument/CTD/Script")
hsDf=read.csv("Hydrografidata Slite 2022_SMHI-mall_0628.csv", sep = ",", fileEncoding = "UTF-8-BOM")
old_cmbDf=read.csv("cmbDf.csv", sep = ",", fileEncoding = "UTF-8-BOM")

#X|Y differ, need to copy and add date to previous combined file

names(cmbDf) <-c("Station", "Djup", "Temperatur","Syre_mgL", "Sal_PSU", "Bottendjup", "Y", "X","Date")
cmbDf = cmbDf[,c("Station", "Djup", "Temperatur","Syre_mgL", "Sal_PSU", "Bottendjup", "X", "Y","Date")]

names(cmbDf)[3]<-"Temperatur"
names(hsDf)
colSet=c("Stationsnamn","Provtagningsdjup..m.", "Temperatur.CTD...C.","Syrgashalt.CTD..ml.l.","Salinitet.CTD..psu.","Vattendjup.vid.stationen..m.","Longitud..verklig..DDMM.MM..DD.dddd.","Latitud..verklig..DDMM.mm..DD.dddd.","Start.provtagningsdatum..YYYY.MM.DD.")
hsDf=hsDf[,colSet]
names(hsDf)=names(cmbDf)

hsDf$Temperatur = gsub(",","\\.",hsDf$Temperatur)
hsDf$Djup = gsub(",","\\.",hsDf$Djup)
hsDf$Syre_mgL = gsub(",","\\.",hsDf$Syre_mgL)

ID=!(hsDf$Sal_PSU == "")
sbDf=hsDf[ID,]

sbDf$Date=lubridate::as_datetime(sbDf$Date, tz = "CET")
sbDf$Bottendjup=gsub(",","\\.", sbDf$Bottendjup)
sbDf$Sal_PSU=gsub(",","\\.", sbDf$Sal_PSU)
sbDf[,2:6]=sapply(sbDf[,2:6], function(x) as.numeric(x))

myMrg=rbind(sbDf, cmbDf)
myMrg$Month=lubridate::month(myMrg$Date)
sctrDf = myMrg[,c(1:5,10)]

myMlt=melt(sctrDf,id.vars = c("Station", "Djup","Month"))
myMlt$Month = as.factor(myMlt$Month)
levels(myMlt$variable) <- c("°C", "Syre mg/L", "Salinitet (PSU)")

salDf = myMlt[which(myMlt$variable == "Salinitet (PSU)"),]
oxDf = myMlt[which(myMlt$variable == "Syre mg/L"),]
temDf = myMlt[which(myMlt$variable == "°C"),]

for(two in seq(2)){
  negSid=which(salDf$value == min(salDf$value))
  salDf=salDf[-negSid,]
}


###Creates four summary graphs of the CTD data, using the different datasets above. 
sp= ggplot(data=salDf, aes(y=Djup, x=value, color = Month))
sp= sp + geom_point(size = 0.75)
sp= sp + theme_bw() + labs(x="Salinintet (PSU)")
sp= sp + scale_y_reverse()
sp= sp + scale_color_manual(values =c("#ffad60", "#3b5998", "#88d8b0"))
sp= sp + theme(legend.position = "none")

negId=which(oxDf$value == min(oxDf$value))
oxDf=oxDf[-negId,]

op= ggplot(data=oxDf, aes(y=Djup, x=value, color = Month))
op= op + geom_point(size = 0.75)
op= op + theme_bw() + labs(x="Syre mg/L")
op= op + scale_color_manual(values =c("#ffad60", "#3b5998", "#88d8b0"))
op= op + theme(legend.position = "none")
op= op + scale_y_reverse()

tp= ggplot(data=temDf, aes(y=Djup, x=value, color = Month))
tp= tp + geom_point(size = 0.75)
tp= tp + theme_bw() + labs(x="°C")
tp= tp + scale_color_manual(values =c("#ffad60", "#3b5998", "#88d8b0"))
tp= tp + scale_y_reverse()
tp= tp + theme(legend.position = "none")

bx = ggplot(data=myMlt, aes(y = value, x = as.factor(variable), fill = Month))
bx = bx + geom_boxplot(lwd = 0.2)
bx = bx + theme_bw()
bx = bx + theme(legend.margin=margin(0,0,0,0), 
                legend.spacing.x = unit(0, "mm"), legend.spacing.y = unit(0, "mm"))
bx = bx + labs(y="Mätvärden", x = "")
bx = bx + scale_fill_manual(values =c("#ffad60", "#3b5998", "#88d8b0"))
bx = bx + guides(fill = guide_legend(title = "Månad"))

setwd("C:/Users/MartinAndersson/Calluna AB/JAG0092-1_CCS_2021 - Dokument/JAG0092b_Hydrografi/Arbetsdokument/CTD/Graphs/Sumry")
plot=ggarrange(bx, tp, op, sp, ncol = 2, nrow = 2,common.legend = TRUE, legend = "bottom")

annotate_figure(plot, top = text_grob("CTD-data Slite utredningsområde 2022", 
                                      color = "black", size = 13)) 

###Export manually with pdf 6*8 inches landscape###
###Export all geodata and attach summary graph
sumLink="https://callunaab.sharepoint.com/teams/JAG0092-1/Delade%20dokument/Forms/AllItems.aspx?id=%2Fteams%2FJAG0092%2D1%2FDelade%20dokument%2FJAG0092b%5FHydrografi%2FArbetsdokument%2FCTD%2FGraphs%2FSumry%2FCTD%5FSummary%2Epdf&viewid=9f30aa4a%2Dbcbc%2D45f0%2D8284%2D065ac39dc79a&parent=%2Fteams%2FJAG0092%2D1%2FDelade%20dokument%2FJAG0092b%5FHydrografi%2FArbetsdokument%2FCTD%2FGraphs%2FSumry"
myMrg=na.omit(myMrg)#Do NOT run second time after adding the NA sumlink column below ^.^

#Adds a fake point to the dataset to which the summary graph is linked
myMrg[dim(myMrg)[1]+1,1:6] = rep(NA, 1,6)
myMrg[dim(myMrg)[1],7:8] = c(18.85, 57.71) 
myMrg[dim(myMrg)[1],9] = lubridate::as_date(Sys.Date())
myMrg[dim(myMrg)[1],10:11] = c(NA, sumLink)

#Geodata Section
crsObj<-st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") #WGS84_Script
geoLayer=st_as_sf(myMrg, coords = c("X", "Y"), crs = crsObj)
gpkg="CTD_GeoSlite.gpkg"
st_write(geoLayer, paste0(gpkgPath, gpkg), "CTD_Sum",append = TRUE)



#write.csv(cmbDf, "cmbDf.csv", row.names = FALSE, fileEncoding = "UTF-8")

#Sometimes the data format changes, check it / check for 
# timeForm=do.call(rbind,lapply(as.character(cmbDf$`Date (MM/DD/YYYY)`), function(x) stringr::str_split_fixed(x, "/",3)))
# timeForm=apply(timeForm, 2, function(x) as.numeric(x))
# timeMax=apply(timeForm, 2, max)
# aVec = vector()
# 
# for(aNum in seq(length(timeMax))){
#   if(timeMax[aNum] < 13){
#     aVec= append(aVec, gsub(as.character(timeMax[aNum]), "%m",as.character(timeMax[aNum])))
#     print("happened")
#   }
#     else if(timeMax[aNum] > 12  && timeMax[aNum] < 1000){
#       aVec= append(aVec, gsub(as.character(timeMax[aNum]), "%d",as.character(timeMax[aNum])))
#   }
#     else {
#       timeMax[aNum]=gsub(as.character(timeMax[aNum]), "%Y",as.character(timeMax[aNum]))
#   }
# }