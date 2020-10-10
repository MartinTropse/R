
"""
Collection of short GIS scritps that are useful and easy to forget!

"""

library(raster)

#Load and replace a certain raster value with NA, essentially clipping away that piece of the raster 
myRst=raster("E:/DataAnalys/OX2_GG_Bentos/EnvLayer/DepthEmodnet_KG_19m.tif")
myRst[myRst==0] = NA
writeRaster(myRst, "NewRst2.tif", crs=CRS("+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

#Move CSV coordinates in a desired direction 
DfQ2_4$top=DfQ2_4$top-1750

#Convert raster to matrix, becomes a long table with three columns. The two coordinates and the containing raster value.
#Making it easy to convert to data frame if necessary. 
seaFloat=rasterToPoints(seaFloat) 

#Check the data type of the raster
storage.mode(depthRst[])
#Change the data type of the raster
stamRst[]=as.numeric(stamRst[])

#Load a raster stack and creating a new raster containing the mean value of the raster stack layers 
potTemStack=raster::stack(list.files(file.path("E:/DataAnalys/OX2_SeasonPatternFish/ModelleringAvFisk/BALTICSEA_REANALYSIS_PHY_003_011_2nm1993_2019/RasterStack")
                                     , pattern="*.tif$", full.names =TRUE))
mean <- calc(potTemStack, fun = mean, na.rm = T)
writeRaster(mean, "potTem_meanRaster.tiff", "GTiff", overwrite=TRUE)


#A manual way to create a raster that contains the a quantile value per pixel, from multiple raster layer 
###Get the max and min percentile per square over the timeperiod###
latSeq=seq(from=1, to=dimOut[1], by=1)
lonSeq=seq(from=1, to=dimOut[2], by=1)
myMax=matrix(nrow = dimOut[1], ncol=dimOut[2])
myMin=matrix(nrow = dimOut[1], ncol=dimOut[2])
#Goes through all array index, for every position a quantile of all time periods are created, 
#the 10% percentile value is then added to a new matrix at the same index
for(y in latSeq){
  for(x in lonSeq){
    myQuant=quantile(potTem_slice1999[y,x,], probs=seq(0,1,by=0.1),na.rm=TRUE)
    myMax[y,x]=myQuant[10] # 10% highest percentile
  }
}
#This then create a raster file from above, assigns a CRS & extent, transpose&flips the data before exporting  
minRst=raster(t(myMin), crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
minRst=flip(minRst, direction="y")
extent(minRst) = myExt
plot(minRst)
writeRaster(minRst, "potTem_minRaster.tif", "GTiff", overwrite=TRUE)


#Select csv point data in a dataframe according to the quartile distrubution of a column  
df=read.csv("GadusMorhua_CPUE_Mean.csv", sep=',', encoding='UTF-8')
quantCPUE=quantile(df$CPUE_mean, probs = seq(0,1, 0.25), na.rm = TRUE)
DfQ2_4 = df[which(df$CPUE_mean>=quantCPUE[2]),]
DfQ2_4$Presence = rep(1, length(DfQ2_4$CPUE_mean))



