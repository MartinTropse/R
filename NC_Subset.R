library(ncdf4)
library(raster)

setwd("D:/DataAnalys/OX2_SeasonPatternFish/ModelleringAvFisk/BALTICSEA_REANALYSIS_PHY_003_011_2nm1993_2019")

list.files()

nc_data = nc_open("dataset-reanalysis-nemo-monthlymeans_1601030281893.nc")

{
sink("C:/myGit/R/nc.txt")
  print(nc_data)
sink()  
}

parmList = c("sob","vo","thetao","uo","bottomT","so")
lon = ncvar_get(nc_data, "longitude")
lat = ncvar_get(nc_data, "latitude")
myTime = ncvar_get(nc_data, "time")
myDepth = ncvar_get(nc_data, "depth")

#17912.5 days from 1950 =~ 1999-01 
time1999=myTime>=17912.5

xmx=max(lon)
xmn=min(lon)
ymx=max(lat)
ymn=min(lat)
myExt=extent(c(xmn, xmx, ymn,ymx))


so_array=ncvar_get(nc_data, "so") #Sea_water_salinity
so_slice= so_array[,,1,] #Surface sea_water
btmT_array=ncvar_get(nc_data, "bottomT") #Bottom_Temperature
sob_array=ncvar_get(nc_data, "sob") #Bottom_Temperature
potTem_array=ncvar_get(nc_data, "thetao") #Potential Temperature
potTem_slice= potTem_array[,,1,]

so_slice1999 = so_slice[,,time1999]       
btmT_slice1999 = btmT_array[,,time1999]
sob_slice1999 = sob_array[,,time1999]
potTem_slice1999 = potTem_slice[,,time1999]

dimOut=dim(so_slice1999)
#dimOut=dim(btmT_slice1999)
#dimOut=dim(sob_slice1999)
#dimOut=dim(potTem_slice1999)

mapSeq=seq(from=1, to=dimOut[3], by=1) #Creates a series of times, if dimOut[3] is the time period in the array

setwd("D:/DataAnalys/OX2_GG_Bentos/EnvLayer/RasterStack")
#Loop separate each band into a raster layer, which in turn can be loaded as a rasterstack
#Rember to checkthat the correct "NC slice" is used and change the output location & name.

for(aTime in mapSeq) {
  timeMap=so_slice1999[,,aTime]
  soRst=raster(t(timeMap), crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
  soRst=flip(soRst, direction = "y")
  extent(soRst) = myExt
  myName = paste0("SalSurf", aTime,".tif")
  writeRaster(soRst, myName, "GTiff", overwrite=TRUE)  
}

soStack=raster::stack(list.files(file.path("D:/DataAnalys/OX2_GG_Bentos/EnvLayer/RasterStack")
                                    , pattern="*.tif$", full.names =TRUE))

mean <- calc(soStack, fun = mean, na.rm = T)
writeRaster(mean, "so_meanRaster.tiff", "GTiff", overwrite=TRUE)


###Get the max and min percentile per square over the timeperiod###
latSeq=seq(from=1, to=dimOut[1], by=1)
lonSeq=seq(from=1, to=dimOut[2], by=1)

myMax=matrix(nrow = dimOut[1], ncol=dimOut[2])
myMin=matrix(nrow = dimOut[1], ncol=dimOut[2])
  
#Goes through all array index, for every position a quantile of all time periods are created, 
#the 10% percentile value is then added to a new matrix at the same index
for(y in latSeq){
  for(x in lonSeq){
   myQuant=quantile(so_slice1999[y,x,], probs=seq(0,1,by=0.1),na.rm=TRUE)
    myMax[y,x]=myQuant[10] # 10% highest percentile
  }
}
  
for(y in latSeq){
  for(x in lonSeq){
    myQuant=quantile(so_slice1999[y,x,], probs=seq(0,1,by=0.1),na.rm=TRUE)
    myMin[y,x]=myQuant[2] # 10% lowest percentile
  }
}

minRst=raster(t(myMin), crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
minRst=flip(minRst, direction="y")
maxRst=raster(t(myMax), crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
maxRst=flip(maxRst, direction="y")

extent(maxRst) = myExt
extent(minRst) = myExt

plot(maxRst)
plot(minRst)


writeRaster(minRst, "So_Min.tif", "GTiff", overwrite=TRUE)
writeRaster(maxRst, "So_Max.tif", "GTiff", overwrite=TRUE)








#baseMax=matrix(nrow = dimOut[1], ncol=dimOut[2], )
#baseMatrix=matrix( rep( 0, len=16697), ncol = 283, nrow=59)
#naMap=is.na(myMin)

#baseMap = so_slice[,,1]
# {
# sink("LoopOutput.txt")
#   
#   
#   
# for(aTime in mapSeq) {
#   timeMap=so_slice[,,aTime]
#   naTime = is.na(timeMap)
#   for(x in lon){
#     for(y in lat){
#       NA1=naMap[x,y]
#       NA2=naTime[x,y]
#         if(NA1 == TRUE &&  NA2 == TRUE){
#           #Sys.sleep(0.1)
#           print("First")
#           print(timeMap[x,y])
#           next
#         }
#         else if(NA1 == FALSE && NA2 == FALSE){
#           meanVal = timeMap[x,y]
#           #Sys.sleep(0.1)
#           print("Second")
#           print(timeMap[x,y])
#         }
#         else if(NA1 == FALSE && NA2 == TRUE){
#           timeMap[x,y] = meanVal
#           #Sys.sleep(0.1)
#           print("Third")
#           print(timeMap[x,y])
#         }
#     }
#   }
#   baseMatrix=baseMatrix+timeMap
# }
# sink()
# }

#r <- raster()
#bb <- extent(-10, 10, -20, 20)
#extent(r) <- bb
#r <- setExtent(r, bb, keepres=TRUE)

# meanMatrix = baseMatrix/dimOut[3]
# meanRst=raster(t(meanMatrix), crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
# meanRst = flip(meanRst, direction = "y")
# extent(meanRst) = myExt
# writeRaster(meanRst, "doubt.tif", "GTiff", overwrite=TRUE)
# naMap=naMap*1
# naBase=is.na(baseMap)
# naBase=naBase*1
# newMap=naMap*naBase
# sum(naMap)
# sum(naBase)
# sum(newMap)
# naMap*naBase
# dim(naMap)
# dim(baseMap)
# sum(baseMap*naMap)

# for(aTime in mapSeq) {
#   timeMap=so_slice[,,aTime]
#   baseMap = baseMap+timeMap
# }

#+proj=longlat +datum=WGS84 +no_defs 
#"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"
#xmx=max(lon), xmn=min(lon), ymx=max(lat), ymn=min(lat))

# meanMap=baseMap/dimOut[3]
# meanRst=raster(t(meanMap), crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
# meanRst=flip(meanRst, direction="y")

#extent(meanRst)=myExt
#plot(meanRst)
#writeRaster(meanRst, "SO_meanRasterSo.tif", "GTiff", overwrite=TRUE)