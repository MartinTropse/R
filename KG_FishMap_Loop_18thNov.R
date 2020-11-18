library(ggplot2)
library(raster)
library(tidyr)
library(tmap)
library(tmaptools)
library(ggmap)
library(dplyr)
library(forcats)
library(sf)
library(foreach)

setwd("E:/DataAnalys/OX2_SeasonPatternFish/MAL_map")

#map extent
ext <- c(left = 10, bottom = 56, right = 13.2, top = 58)

#prepare the object for ggmap
bm<-ggmap::get_stamenmap(ext, zoom=8, maptype = "toner-lite") 

#the following function will be usef to fix ggmap bugs, see details https://github.com/dkahle/ggmap/issues/160 
# Define a function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(map) {
  
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

bm<-ggmap_bbox(bm)


# Load shapefile of part area
park_polys<-st_read("R_Karta/ParkArea.shp")

# Convert from WGS84 4326 to 3857
park_polys_3857 <- st_transform(park_polys, 3857)

##plot base map and park polygon 
p<-ggmap(bm) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = park_polys_3857, inherit.aes = FALSE,color="orange",lwd=1)#Add polygone  to map 

#fish capture file
dfSpec<-raster::shapefile("R_Karta/KG_FishCPUE.shp")
dfSpec_sf=st_as_sf(dfSpec)

#extract coordinates
x_list<-list()
y_list<-list()



#example of species Squalus acanthias
dfSpec_sf_2<-dfSpec_sf[which(dfSpec_sf$Species=="Squalus acanthias"),]

for (i in 1:length(dfSpec_sf_2$Year)) {
  x_list[i]<-dfSpec_sf_2$geometry[[i]][1]
  y_list[i]<-dfSpec_sf_2$geometry[[i]][2]
}

coord<-as.data.frame(cbind(unlist(x_list),unlist(y_list)))
df<-cbind(coord,dfSpec_sf_2$CPUE_numbe)

colnames(df)<-c("Longitude","Latitude","CPUE")
df$Log_CPUE = log(df$CPUE+1)

##add CPUE_number_per_hour to the map
p<-p+geom_point(data = df,aes(x =Longitude,
                              y = Latitude,
                              colour=Log_CPUE), size=2)+
  ##log normalization:CPUE_number_per_hour+1 to deal with zero
  scale_color_continuous(low = '#3182bd', high = '#bd0026')+
  labs(x="", y="")  

#quant=quantile(df$CPUE, probs=seq(0,1,0.05))



p<-p+geom_point(data = df,aes(x =Longitude,
                              y = Latitude,
                              colour=log(CPUE+1)),size=2)+
  ##log normalization:CPUE_number_per_hour+1 to deal with zero
  scale_color_continuous(low = '#3182bd', high = '#bd0026')+
  labs(x="", y="")  

####MapLoop####

specVec = c("Clupea harengus","Sprattus sprattus", "Merlangius merlangus", "Limanda limanda",
          "Hippoglossoides platessoides", "Trisopterus esmarkii", "Pleuronectes platessa", "Gadus morhua",
          "Trachinus draco","Trachurus trachurus", "Callionymidae","Platichthys flesus", "Engraulis encrasicolus",
          "Melanogrammus aeglefinus", "Eutrigla gurnardus")

sweList = c("Sill", "Skarpsill", "Vitling", "Sandskädda", "Lerskädda", "Vitlinglyran", "Rödspätta",
            "Torsk", "Fjärsing", "Taggmakrill", "Sjökocksfiskar", "Skrubbskädda", "Engraulis encrasicolus",
            "Kolja", "Knorrhane")

foreach(sw = sweList, lt = specVec) %do% {
  dfSpec_sf$Species=gsub(lt,sw, dfSpec_sf$Species)
  }


for(spec in sweList){
  p<-ggmap(bm) + 
    coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
    geom_sf(data = park_polys_3857, inherit.aes = FALSE,color="orange",lwd=1)#Add polygone  to map 
  
  dfSpec_sf_2<-dfSpec_sf[which(dfSpec_sf$Species==spec),]
  x_list<-list()
  y_list<-list()
  
  for (i in 1:length(dfSpec_sf_2$Year)) {
    x_list[i]<-dfSpec_sf_2$geometry[[i]][1]
    y_list[i]<-dfSpec_sf_2$geometry[[i]][2]
  }

  coord<-as.data.frame(cbind(unlist(x_list),unlist(y_list)))
  df<-cbind(coord,dfSpec_sf_2$CPUE_numbe)

  colnames(df)<-c("Longitude","Latitude","CPUE")
  df$Log_CPUE = log(df$CPUE+1)

##add CPUE_number_per_hour to the map
  p<-p+geom_point(data = df,aes(x =Longitude,
                                y = Latitude,
                                colour=Log_CPUE), size=2)+
    ##log normalization:CPUE_number_per_hour+1 to deal with zero
    scale_color_continuous(low = '#3182bd', high = '#bd0026')+
    labs(x="", y="", title = paste0(spec," logaritmiserad fångst data"),
         color="Log CPUE")+theme(legend.position = "bottom")
  
  #export
  pdf(paste0(spec," fångst data",".pdf",sep =""),width = 6,height = 8)
  print(p)
  dev.off()
  
}


