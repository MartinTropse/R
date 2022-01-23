library(ggplot2)
library(reshape2)
library(plyr)
#library(ggpubr)

setwd("D:/DataAnalys/OX2_SeasonPatternFish")
df=read.csv("Kattegatt_FishBits.csv", header = TRUE, sep=',')
df = na.omit(df)
levels(df)=c("Quarter 1", "Quarter 4")
o=gsub("1","Quarter 1",df$Quarter)
df$Quarter=gsub("4","Quarter 4",o)

#table(dfS$Year)

#Exclude 2011 & 2019, which lacks quarter sampling
idY = grep("2010|2012|2013|2014|2015|2016|2017|2018", df$Year)
dfY = df[idY,]

sDf=grep("Pleuronectes platessa", dfY$Species)
dfS = dfY[sDf,]

n = 10
dfS_top=dfS[dfS$CPUE_number_per_hour > quantile(dfS$CPUE_number_per_hour,prob=1-n/100),]
min(dfS_top$CPUE_number_per_hour)

dfS[dfS$CPUE_number_per_hour > quantile(dfS$CPUE_number_per_hour, prob=1-n/100),] #Selects the top 10% percentile
#quantile returns a signal digit value that repsents the minimum cutoff, which is then checked if it is higher then values in the selected column

plot(x=dfS$Year, y=dfS$CPUE_number_per_hour, main="Gadus morhua CPUE per year")


#CPUE cutoffs for top 10 percentile:
#Gadus Morhua: 18
#Clupea harengus: 800
#Scomber scombrus: 19
#


setwd("D:/DataAnalys/OX2_SeasonPatternFish/CPUE per length per haul per hour_2020-09-10 16_07_17")

#list.files()
df=read.csv("CPUE per length per haul per hour_2020-09-10 16_07_17.csv", header = TRUE, sep=',')

table(df$Year)

eastDf = df[which(df$ShootLong < 15), ]
idS=grep("Pleuronectes platessa", eastDf$Species)
dfS = eastDf[idS,]

plot(x=dfS$Year, y=dfS$CPUE_number_per_hour, main="Pleuronectes platessa CPUE per year")
plot(y=dfS$ShootLat, x=dfS$ShootLong)
