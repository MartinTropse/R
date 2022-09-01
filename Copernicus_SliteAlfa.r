library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting



setwd("C:/Calluna/Projekt/CementaSlite/Copernicus_BaltFys")
list.files()

#Prints data and store it into txt 
{
  sink("ndvi_nc.txt") #Contains the metadata, very useful
  print(nc_data) 
  sink()
}

lon <- ncvar_get(nc_data, "lon") #Extract the underlying attributes (note that not all parameters have all attributes)
lat <- ncvar_get(nc_data, "lat")
t <- ncvar_get(nc_data, "time")
depth = ncvar_get(nc_data, "depth")
fillval = ncatt_get(nc_data, "so", "_FillValue")  

so_array <- ncvar_get(nc_data, "so")#becomes a four dimensional array 
dim(so_array)

nc_close(nc_data)
so_slice = so_array[, , 1, 1] #Selects the data from the surface at the first day  (since time is the third attribute and depth is fourth)
dim(so_slice)

salRst=raster(t(so_slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),   
              crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")) #Note that lon/lat variables were created above and is now used here

plot(salRst)
writeRaster(r, "Sal2_SliteTime1Depth1.tif", "GTiff", overwrite=TRUE)



###Subset and Calculate specific layer###
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting

setwd("C:/Calluna/Projekt/CementaSlite/Copernicus_BaltFys/Monthly_BGC_003_007")
rm(list=ls())

#nc_dataDay=nc_open("dataset-bal-analysis-forecast-phy-dailymeans_1657797649987.nc") #Copernicus dataset: BALTICSEA_REANALYSIS_PHY_003_06  
nc_dataMth=nc_open("dataset-bal-analysis-forecast-bio-monthlymeans_1658132139997.nc") #Copernicus dataset  


{
  sink("BGC_003_007.txt")
  print(nc_dataMth)
  sink()
}

#no3, po4,chl, ph, nh4
#siconc_array=ncvar_get(nc_dataMth, "siconc") # Sea ice concentration
#sithick_array = ncvar_get(nc_dataMth, "sithick") #Sea ice thickness 
#btTm_array = ncvar_get(nc_dataMth, "bottomT")
#sob_array = ncvar_get(nc_dataMth, "sob")
#no3_array = ncvar_get(nc_dataMth, "no3")
#po4_array = ncvar_get(nc_dataMth, "po4")
#ph_array = ncvar_get(nc_dataMth, "ph")
#nh4_array = ncvar_get(nc_dataMth, "nh4")

theParm = "nh4"
data_array = ncvar_get(nc_dataMth, theParm)

lonMt = ncvar_get(nc_dataMth, "lon")
latMt = ncvar_get(nc_dataMth, "lat")
myMtTime = ncvar_get(nc_dataMth, "time")
myMtDepth = ncvar_get(nc_dataMth, "depth")

ymx = max(latMt)
ymn = min(latMt)
xmn = min(lonMt)
xmx = max(lonMt)

mntExt = extent(c(xmn, xmx, ymn, ymx))

mapSeq=seq(from=1, to=length(myMtTime), by=1)
depthMx = matrix(nrow = dim(data_array)[1], ncol=dim(data_array)[2])

#Finds the maximum depth that has a measured value for each coordinate. The value returned is the "depth category", not an actual depth,
for(y in seq(1,length(latMt),1)){
  #print(y)
  for(x in seq(1,length(lonMt),1)){
    logicGate = TRUE
    #print(x)
    #Sys.sleep(0.1)
    for(dpt in seq(1, length(myMtDepth), 1)){
      dptVal=data_array[x,y,dpt,1]
      if(dpt == 1 & is.na(data_array[x,y,dpt,1])){
        #print(paste(x,y,"Coordinate is NA"))
        break
      }
      if(is.na(dptVal) & logicGate == TRUE & dpt > 1){
        depthMx[x,y] = dpt-1
        logicGate = FALSE
      }
    }  
  }
}
##Creates geotiffs for each slice of the NC data. 
# for(aTime in mapSeq) {
#   timeMap=siconc_array[,,aTime]
#   print(timeMap)
#   Rst=raster(t(timeMap), crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
#   Rst=flip(Rst, direction = "y")
#   extent(Rst) = mntExt
#   myName = paste0("iceCover", aTime,".tif")
#   writeRaster(Rst, myName, "GTiff", overwrite=TRUE)  
# }

myMax=matrix(nrow = length(latMt), ncol=length(lonMt))
myMin=matrix(nrow = length(latMt), ncol=length(lonMt))
myMean=matrix(nrow = length(latMt), ncol=length(lonMt))

#Calculate highest 10% percentile, for 3D array.
for(y in seq(1,length(latMt),1)){
  for(x in seq(1,length(lonMt),1)){
    myQuant=quantile(data_array[x,y,], probs=seq(0,1,by=0.1),na.rm=TRUE)
    myMax[y,x]=myQuant[10] # 10% highest percentile
  }
}

#Calculate lowest 10% percentile, for 3D array.  
for(y in seq(1,length(latMt),1)){
  for(x in seq(1,length(lonMt),1)){
    myQuant=quantile(data_array[x,y,], probs=seq(0,1,by=0.1),na.rm=TRUE)
    myMin[y,x]=myQuant[1] # 10% lowest percentile
  }
}

#Calculate highest 10% percentile, for 4D array with varying depth
for(y in seq(1,length(latMt),1)){
  for(x in seq(1,length(lonMt),1)){
    print(paste("Checking coordinate",x,y))
    if(is.na(depthMx[x,y])){
      print("Skipping out, again!")
    }
      else {
    myQuant=quantile(data_array[x,y,1:depthMx[x,y],], probs=seq(0,1,by=0.1),na.rm=TRUE)
    print("Finally getting some work done!")
    print(paste("The current depth category:",depthMx[x,y]))
    print(myQuant[11])
    myMax[y,x]=myQuant[11] # 10% highest percentile
    Sys.sleep(0.1)
    }
  }
}

maxRst=raster(myMax, crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
maxRst=flip(maxRst, direction="y")
extent(maxRst) = mntExt
writeRaster(maxRst, paste0(theParm,"_Max10Quant.tif"),"GTiff", overwrite = TRUE)

#Calculate lowest 10% percentile, for 4D array with varying depth
for(y in seq(1,length(latMt),1)){
  for(x in seq(1,length(lonMt),1)){
    print(paste("Checking coordinate",x,y))
    if(is.na(depthMx[x,y])){
      print("Skipping out,again!")
    }
    else {
      myQuant=quantile(data_array[x,y,1:depthMx[x,y],], probs=seq(0,1,by=0.1),na.rm=TRUE)
      print("Finally getting some work done!")
      print(paste("The current depth category:",depthMx[x,y]))
      print(myQuant[1])
      myMin[y,x]=myQuant[1] # 10% lowest percentile
      Sys.sleep(0.1)
    }
  }
}

minRst=raster(myMin, crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
minRst=flip(minRst, direction="y")
extent(minRst) = mntExt

writeRaster(minRst, paste0(theParm,"_Min10Quant.tif"),"GTiff", overwrite = TRUE)

#Calculate mean array, for 4D array with varying depth
for(y in seq(1,length(latMt),1)){
  for(x in seq(1,length(lonMt),1)){
    print(paste("Checking coordinate",x,y))
    if(is.na(depthMx[x,y])){
      print("Skipping out, again!")
    }
    else {
      aMean=mean(data_array[x,y,1:depthMx[x,y],])
      print("Finally getting some work done!")
      print(paste("The current depth category:",depthMx[x,y]))
      myMean[y,x]=aMean # Mean value for each time 
      Sys.sleep(0.1)
    }
  }
}

meanRst=raster(myMean, crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
meanRst=flip(meanRst, direction="y")
extent(meanRst) = mntExt

writeRaster(meanRst, paste0(theParm,"_Mean.tif"),"GTiff", overwrite = TRUE)


###Get the max and min percentile per square over the timeperiod###
dimOut=dim(btmTStack)

latSeq=seq(from=1, to=dimOut[1], by=1)
lonSeq=seq(from=1, to=dimOut[2], by=1)

myMax=matrix(nrow = dimOut[1], ncol=dimOut[2])
myMin=matrix(nrow = dimOut[1], ncol=dimOut[2])

#Goes through all array index, for every position a quantile of all time periods are created, 
#the 10% percentile value is then added to a new matrix at the same index
for(y in latSeq){
  for(x in lonSeq){
    myQuant=quantile(btmTStack[y,x,], probs=seq(0,1,by=0.1),na.rm=TRUE)
    myMax[y,x]=myQuant[10] # 10% highest percentile
  }
}

for(y in latSeq){
  for(x in lonSeq){
    myQuant=quantile(so_slice1999[y,x,], probs=seq(0,1,by=0.1),na.rm=TRUE)
    myMin[y,x]=myQuant[2] # 10% lowest percentile
  }
}









# parmList = c("sob","vo","thetao","uo","bottomT","so","sithick","siconc")
# lon = ncvar_get(nc_dataDay, "lon")
# lat = ncvar_get(nc_dataDay, "lat")
# myTime = ncvar_get(nc_dataDay, "time")
# myDepth = ncvar_get(nc_dataDay, "depth")
# 
# #17912.5 days from 1950 =~ 1999-01 
# time2018=myTime>=43434
# 
# xmx=max(lon)
# xmn=min(lon)
# ymx=max(lat)
# ymn=min(lat)
# myExt=extent(c(xmn, xmx, ymn,ymx))

# so_array=ncvar_get(nc_dataDay, "so") #Sea_water_salinity
# sithick_array = ncvar_get(nc_dataDay, "sithick")
# btmT_array=ncvar_get(nc_dataDay, "bottomT") #Bottom_Temperature
# sob_array=ncvar_get(nc_dataDay, "sob") #Bottom_Temperature
# potTem_array=ncvar_get(nc_dataDay, "thetao") #Potential Temperature

# for(val in seq(1,length(myMtTime),1)){
# }
# 
# (sithick_array[,,1][is.na(sithick_array[,,1])]=0)
# sum(sithick_array[,,1])
# 
# so_slice= so_array[,,12,1] #Surface sea_water
# 
# salRst2=raster(t(so_slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),   
#               crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")) #Note that lon/lat variables were created above and is now used here
# plot(salRst)
# plot(salRst2)
# potTem_slice= potTem_array[,,1,]
# 
