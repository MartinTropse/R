library(foreach)
library(ggpubr)
library(viridis)
library(reshape2)
library(plyr)

#Script that look if the quality of the sample is affected by having other samples in a 3 day proximity.
#Check for the difference and summarize in a graph. 

setwd("C:/Base/LittlePrjct/HaV_Bad")
df=read.csv("Badvatten(15-20).csv", encoding = "UTF-8")

df$Datum = as.Date(df$Datum, format = "%Y-%m-%d")

df$Coord=paste(df$Longitud, df$Latitud)

lowQldf=data.frame()

#Creates a dataframe where a "otjänligt" sample is followed by a second sample within 4 days, 
#per municipality & locality.    
for(mun in unique(df$Kommun)){
  aMun = df[df$Kommun == mun,]
  for(plac in unique(aMun$Coord)){
    aPlc = aMun[aMun$Coord == plac,]  
    for(xPos in seq(1,dim(aPlc)[1], 1)) {
      if(aPlc$Kvalitet[xPos] =="Otjänligt"){
         future  = aPlc$Datum[xPos] + as.difftime(4, unit="days")
         aRes = aPlc[aPlc$Datum >=aPlc$Datum[xPos] & aPlc$Datum <= future,]
         if(dim(aRes)[1]>1){
#          print(aRes)
#          print(dim(aRes[1]))
           lowQldf=rbind(lowQldf, aRes)
#          print(lowQldf)
#           Sys.sleep(15)
        }
      }
    }
  }  
}

upDf = data.frame()
head(lowQldf)

#
for(aCrd in unique(lowQldf$Coord)){
  tmpDf=lowQldf[lowQldf$Coord == aCrd,] #Creates a dataframe per unique locality
  rowSeq=seq(1, length(tmpDf),1)
  val = 0
  for(x in rowSeq){
    if(val == 0 & tmpDf$Kvalitet[x] == "Otjänligt"){
      val = val + 1
      subDf=tmpDf[-c(x),]
      upDf = rbind(upDf, subDf)      
    }
  }
}

table(upDf$Year)
table(upDf$Kvalitet)

table(lowQldf$Kvalitet)[1]-length(unique(lowQldf$Coord)) # 

#Summarize the water quality change between standard samples and "follow-up" samples. 
x1=melt(unlist(table(upDf$Kvalitet)/sum(table(upDf$Kvalitet))))
x2=melt(unlist(table(df$Kvalitet)/sum(table(df$Kvalitet))))

x2$Fact=rep("Standardprov", 3)
x1$Fact=rep("Uppföljningsprov", 3)

barDf=rbind(x1,x2)
names(barDf) <- c("Kvalite", "Ratio", "Typ")

barDf = barDf[!barDf$Kvalite == "Tjänligt",]
barGG = ggplot(data=barDf, aes(x=Kvalite, y=Ratio, fill = Kvalite))
barGG = barGG + geom_bar(stat="identity",colour = "black", width = 0.5) + facet_wrap(~Typ) + scale_fill_grey() + theme_light()
barGG = barGG + labs(x="", y="Andel")+theme(legend.title = element_blank(), legend.position = "bottom", legend.text = element_text(size=10), strip.text = element_text(size=10))


####Checks season patterns####
library(lubridate)
library(plyr)
library(ggplot2)
library(reshape2)

setwd("C:/Base/LittlePrjct/HaV_Bad")
df=read.csv("Badvatten(15-20).csv", encoding = "UTF-8")

df$Datum=strptime(df$Datum, format = "%Y-%m-%d")
df$Month=month(df$Datum)
df$Month = as.factor(df$Month)

levels(df$Month) = c("April","Maj", "Juni", "Juli","Augst", "Sept")

seanDf=plyr::ldply(tapply(df$Kvalitet, list(df$Month), table),rbind)
seanDf[is.na(seanDf)] <-0 

seanDf$Ratio=seanDf$Otjänligt/seanDf$Tjänligt
seanDf$AnmRatio = seanDf$`Tjänligt m. Anm.`/seanDf$Tjänligt
seanDf$.id <- factor(seanDf$.id, levels = seanDf$.id[order(seanDf$Ratio)])

ratDf = seanDf[,c(1,5:6)] 
meltRat = melt(ratDf)

levels(meltRat$variable) = c("Otjänligt", "Tjänligt m.a.")

#Barplot over samples with quality "otjänliga or tjänliga m.a." per month###
rat1 = ggplot(data=meltRat , aes(x=meltRat$.id, y=value, fill=variable)) 
rat1 = rat1 + geom_bar(stat='identity',position = "dodge", colour = "black")+scale_fill_manual(values = c("#21466c", "#ffc63a"))+theme_bw(12)
rat1 = rat1 + labs(x="", y="Andel per månad")+theme(legend.title = element_blank())


#Plot over the distrubution of sample effort per month across the years 2015-2020#
monthPerYr=ldply(tapply(df$Month, df$Year, table))
meltMonth=melt(monthPerYr)

levels(meltMonth$variable) = c(4,5,6,7,8,9)
meltMonth$variable
meltMonth=meltMonth[order(meltMonth$.id),]

#seanDf$.id <- factor(seanDf$.id, levels = seanDf$.id[order(seanDf$Ratio)])

x=melt(with(meltMonth, tapply(value, list(.id), sum)))
z=rep(x$value, 6)

meltMonth$Sum = z
meltMonth$Ratio = meltMonth$value/meltMonth$Sum

levels(meltMonth$variable) = c("April","Maj","Juni","Juli","Augst","Sept")
x[order(x$Var1,x$Var2)]

monthBar = ggplot(data=meltMonth, aes(x=variable, y=Ratio, fill=variable))
monthBar = monthBar + geom_bar(stat='identity')+facet_wrap(~meltMonth$.id)+labs(x="", y="Säsongsfördelning av prover")+theme_bw(11)
monthBar = monthBar + theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90))




###Script that summarize the change of pace in "follow-up" samples from 2015 to 2020. The graph summarizing municaplities by amount of failed samples 
###is include. Also includes ANOVA and other small pieces. Not so clean :/ 

setwd("C:/Base/LittlePrjct/HaV_Bad")
df=read.csv("Badvatten(15-20).csv", encoding = "UTF-8")
#muniDf[order(as.Date(muniDf$Datum, format="%Y-%m-%d")),]

y15=df[df$Year == "2015",]
y16=df[df$Year == "2016",]
y17=df[df$Year == "2017",]
y18=df[df$Year == "2018",]
y19=df[df$Year == "2019",]
y20=df[df$Year == "2020",]

y20$Coord=paste(y20$Longitud, y20$Latitud)
y19$Coord=paste(y19$Longitud, y19$Latitud)
y18$Coord=paste(y18$Longitud, y18$Latitud)
y17$Coord=paste(y17$Longitud, y17$Latitud)
y16$Coord=paste(y16$Longitud, y16$Latitud)
y15$Coord=paste(y15$Longitud, y15$Latitud)

table(y20$Kvalitet)

#Calculate the average time distance to next sample after, have to be run once for each year (Yeah this one is stupid :p)  
dateList = list()
for(x in unique(y20$Kommun)){
  muniDf = y20[y20$Kommun == x, ]
  muniDf=muniDf[order(as.Date(muniDf$Datum, format="%Y-%m-%d")),]
  for(pos in unique(muniDf$Coord)){
    coordDf = muniDf[muniDf$Coord == pos,]
    foreach(aTime = coordDf$Datum, aRow = seq(1, dim(coordDf)[1],1)) %do% {
      if(coordDf$Kvalitet[aRow] == "Otjänligt" & aRow < dim(coordDf[1])){
        dateDiff = as.integer(difftime(strptime(coordDf$Datum[aRow+1], format = "%Y-%m-%d"),
                                       strptime(coordDf$Datum[aRow], format = "%Y-%m-%d"),units="days"))
        dateList = append(dateList, dateDiff)
      }
    }
  }
}

#dateList15 = dateList
#dateList16 = dateList
#dateList17 = dateList
#dateList18 = dateList
#dateList19 = dateList
dateList20 = dateList

aFrame15=as.data.frame(do.call(rbind,dateList15))
aFrame15$Year = rep(2015, length(aFrame15$V1))
aFrame16=as.data.frame(do.call(rbind,dateList16))
aFrame16$Year = rep(2016, length(aFrame16$V1))
aFrame17=as.data.frame(do.call(rbind,dateList17))
aFrame17$Year = rep(2017, length(aFrame17$V1))
aFrame18=as.data.frame(do.call(rbind,dateList18))
aFrame18$Year = rep(2018, length(aFrame18$V1))
aFrame19=as.data.frame(do.call(rbind,dateList19))
aFrame19$Year = rep(2019, length(aFrame19$V1))
aFrame20=as.data.frame(do.call(rbind,dateList20))
aFrame20$Year = rep(2020, length(aFrame20$V1))

dfTimDist=rbind(aFrame15,aFrame16,aFrame17,aFrame18,aFrame19,aFrame20)
colnames(dfTimDist) = c("TimeDist", "Year")

#Anova test
res.aov <- aov(TimeDist ~ Year, data = dfTimDist)
print(summary(res.aov))

{
  sink("ANOVA_AvstandDagar_Resultat.txt")
  print(summary(res.aov))
  sink()
}

#Mean and SD summary
meanList=tapply(dfTimDist$TimeDist, dfTimDist$Year, mean)
meanList[1]/meanList[6]
meanList[1]-meanList[6]
tapply(dfTimDist$TimeDist, dfTimDist$Year, sd)
tapply(dfTimDist$TimeDist, dfTimDist$Year, sum)

write.table(dfTimDist, "TimDist_UnsuitableWater_2015_2020.csv", fileEncoding = "UTF-8", row.names = FALSE)
dfTimDist=read.table("TimDist_UnsuitableWater_2015_2020.csv", sep=',', header=T)


#Boxplot of  pace change among follow-up sample 2015-2020
boxDistTim=ggboxplot(dfTimDist, x = "Year", y = "TimeDist", 
                     color = "Year", palette = c("#00AFBB", "#E7B800", "#FC4E07", "#000000",#4b9484),
                                                 order = c("2015", "2016", "2017", "2018", "2019", "2020"),
                                                 ylab = "Tids avstånd i dagar", xlab = "År"))

boxGG=ggplot(dfTimDist, aes(x=as.factor(Year), y=TimeDist, fill=as.factor(Year))) + geom_boxplot() +
  scale_y_continuous(limits=c(0,16), breaks=seq(0,20,4))+theme_bw()
boxGG = boxGG + labs(x="År", y="Dagar till nästa provtagning från otjänligt prov") + theme(legend.title = element_blank())




#Summary of amount municaplities that have had a certain amount of "otjänliga prover" during 2015-2020
out15=tapply(y15$Kvalitet, y15$Kommun, table)
out16=tapply(y16$Kvalitet, y16$Kommun, table)
out17=tapply(y17$Kvalitet, y17$Kommun, table)
out18=tapply(y18$Kvalitet, y18$Kommun, table)
out19=tapply(y19$Kvalitet, y19$Kommun, table)
out20=tapply(y20$Kvalitet, y20$Kommun, table)

Kommun2015=plyr::ldply(out15, rbind)
Kommun2016=plyr::ldply(out16, rbind)
Kommun2017=plyr::ldply(out17, rbind)
Kommun2018=plyr::ldply(out18, rbind)
Kommun2019=plyr::ldply(out19, rbind)
Kommun2020=plyr::ldply(out20, rbind)

Kommun2015$Year=rep(2015,length(Kommun2015$.id))
Kommun2016$Year=rep(2016,length(Kommun2016$.id))
Kommun2017$Year=rep(2017,length(Kommun2017$.id))
Kommun2018$Year=rep(2018,length(Kommun2018$.id))
Kommun2019$Year=rep(2019,length(Kommun2019$.id))
Kommun2020$Year=rep(2020,length(Kommun2020$.id))

Kommun2015_2020=rbind(Kommun2015, Kommun2016, Kommun2017,Kommun2018,Kommun2019,Kommun2020)
Kommun2015_2020[is.na(Kommun2015_2020)] = 0

tapply(Kommun2015_2020$Otjänligt, Kommun2015_2020$Year, function(x) quantile(x, probs = seq(0,1,0.05)))

#Geom_Barplot section. 
tmp1=tapply(Kommun2015_2020$Otjänligt, Kommun2015_2020$Year, table)
KommunOtjanligt=melt(tmp1)

test=as.data.frame(plyr::ldply(tmp1, rbind))
test[is.na(test)] <-0
mlt_df=melt(test)
colnames(mlt_df) = c("Year", "variable","value" )

ggBar=ggplot(data=mlt_df, aes(x=as.factor(variable), y=value))
ggBar = ggBar + facet_wrap(~Year) + geom_bar(stat= "identity")+theme_bw()+labs(x="Antal otjänliga prover", y="Antal kommuner")+
  theme(axis.text.x = element_text(angle = 90))


###Top municipalities and other code remains###
x <- Kommun2015_2020[with(Kommun2015_2020,order(-Otjänligt)),]
x1 <- Kommun2015_2020[with(Kommun2015_2020,order(-Tjänligt)),]

options(max.print=100000)

{
  sink("KommunerByOtjanlig.txt")
  print(x)
  sink()
}

y1=tapply(Kommun2015_2020$Tjänligt, Kommun2015_2020$Year, sum)
y2=tapply(Kommun2015_2020$Otjänligt, Kommun2015_2020$Year, sum)

{
  sink("OtjanligTableSummary.txt")
  print(tapply(Kommun2015_2020$Otjänligt, Kommun2015_2020$Year, table))
  sink()
}

{
  sink("KommunerTjanligOtjanlig.txt")
  print(y1)
  print(y2)  
  sink()
}

kID=grep("Båstad", Kommun2015_2020$.id)
sID=grep("Stockholm", Kommun2015_2020$.id)
hID=grep("Helsingborg", Kommun2015_2020$.id)
vID=grep("Värnamo", Kommun2015_2020$.id)
lID=grep("Linköping", Kommun2015_2020$.id)
rID=grep("Ronneby", Kommun2015_2020$.id)

Bastd=Kommun2015_2020[kID,]
Stkhl=Kommun2015_2020[sID,]
Helsng=Kommun2015_2020[hID,]
Varnmo=Kommun2015_2020[vID,]
Linkpg=Kommun2015_2020[lID,]
Ronby=Kommun2015_2020[rID,]

{
  sink("ToppKommuner.txt")
  print(Bastd)
  print(Stkhl) 
  print(Helsng) 
  print(Varnmo) 
  print(Linkpg) 
  print(Ronby) 
  sink()
}

Kommun2015 <- Kommun2015[c(".id","Tjänligt","Tjänligt m. Anm.","Otjänligt")]

{
  sink("Y20_List.txt")
  print(chk20$value)
  sink()  
}  

bollDf=y15[y15$Kommun == "Bollnäs",] 
chk1=data.frame(do.call(rbind, out15),fill=T)
lapply(chk$value, function(x) write.table( data.frame(x), 'test.csv'  , append= T, sep=',' , fileEncoding = ))

chk$Test=as.vector(chk$value)
capture.output(summary(mylist), file = "My New File.txt")