library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting

#Tutorial for NCDF4 data
#https://rpubs.com/boyerag/297592 
#wd = D:\Geodata\Raster
list.files()

nc_data=nc_open("dataset-reanalysis-nemo-monthlymeans_1601317108607.nc") #Copernicus dataset: BALTICSEA_REANALYSIS_PHY_003_011  
#nc_data=nc_open("gimms3g_ndvi_1982-2012.nc4")

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

so_slice = so_array[, , 1, 1] #Selects the data from the surface and first month  (since time is the third attribute and depth is fourth)
dim(so_slice)

#Ok, everything checks out, so we can go ahead and save this data in a raster. Note that we provide 
#the coordinate reference system "CRS" in the standard well-known text format. For this data set, it 
#is the common WGS84 system.  

salRst=raster(t(so_slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),   
  crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")) #Note that lon/lat variables were created above and is now used here

r <- flip(salRst, direction='y')

plot(salRst)
plot(r)

#We will need to transpose and flip to orient the data correctly. The best way to figure this out is 
#through trial and error, but remember that most netCDF files record spatial data from the bottom left corner.

writeRaster(r, "Sal2_SliteTime1Depth1.tif", "GTiff", overwrite=TRUE)

###How to pick out a specific point and look at the change over time 
# A RasterBrick is a multi-layer raster object. They are typically created from a multi-layer (band) file; 
# but they can also exist entirely in memory. They are similar to a RasterStack (that can be created with stack),
# but processing time should be shorter when using a RasterBrick. Yet they are less flexible as they can only point to a single file.
surface_so = so_array[,,1,]

myR_brick = brick(surface_so, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +no_defs+ towgs84=0,0,0"))
#Seems like brick can only work with 3 dimensional data, in this case: lon, lat & time  

pointLon = 18.70831
pointLat = 57.50822

KG_Point=extract(myR_brick, SpatialPoints(cbind(pointLon, pointLat)), method='simple') 
#For some reason it only extracts NA, same result as tutorial.

Sal=t(KG_Point)

KG_df = data.frame(timeMon=seq(from=1, to=1327, by=1), Sal)

ggplot(data=KG_df, aes(x=timeMon, y=Sal, group=1)) +
  geom_line() + # make this a line plot
  ggtitle("Growing season NDVI at Toolik Lake Station") +     # Set title
  theme_bw() # use the black and white theme



###CopyPaste from Tutorial###
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting

nc_data <- nc_open('gimms3g_ndvi_1982-2012.nc4')
# Save the print(nc) dump to a text file
{
  sink('gimms3g_ndvi_1982-2012_metadata.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")

head(lon) # look at the first few entries in the longitude vector

ndvi.array <- ncvar_get(nc_data, "NDVI") # store the data in a 3-dimensional array
dim(ndvi.array) 

fillvalue <- ncatt_get(nc_data, "NDVI", "_FillValue")
fillvalue

nc_close(nc_data) 
ndvi.array[ndvi.array == fillvalue$value] <- NA

ndvi.slice <- ndvi.array[, , 1] 
dim(ndvi.slice)

r <- raster(t(ndvi.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')
plot(r)

writeRaster(r, "GIMMS3g_1982.tif", "GTiff", overwrite=TRUE)

#Extract data at a study site
r_brick <- brick(ndvi.array, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- flip(t(r_brick), direction='y')

toolik_lon <- -149.5975
toolik_lat <- 68.6275
toolik_series <- extract(r_brick, SpatialPoints(cbind(toolik_lon,toolik_lat)), method='simple') #Something wrong with the code here, 
#clearly their should be 31 values of NDVI, one per year, but it produces only 31 NA. Hence the following code does not work as intended either. 

toolik_df <- data.frame(year= seq(from=1982, to=2012, by=1), NDVI=t(toolik_series)) 
ggplot(data=toolik_df, aes(x=year, y=NDVI, group=1)) +
  geom_line() + # make this a line plot
  ggtitle("Growing season NDVI at Toolik Lake Station") +     # Set title
  theme_bw() # use the black and white theme


#Difference in NDVI between two time periods
ndvi.slice.2012 <- ndvi.array[, , 31] 

ndvi.diff <- ndvi.slice.2012 - ndvi.slice # Note that ndvi.slice was made prior and contains data from 1982.



r_diff = raster(t(ndvi.diff), xmx=max(lon), xmn=min(lon), ymx=max(lat), ymn=min(lat), 
                crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_diff=flip(r_diff, direction='y')

plot(r_diff)



#Another way to open your raster with n bands, might only work with tif data. 
r<-stack('folderwithdata', pattern="$.tif" ) 
#Plot it just to see if everything is ok
plot(r)
#Check the number of bands
nlayers(r)
for(i in 1:nlayers(r)){
  band<-r[[i]]
  #save raster in a separate file
  writeRaster(band,paste('band',i,'.tif', sep=''))
}