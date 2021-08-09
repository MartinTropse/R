###Skapar ett sammanslaget dataset med provvärden och metodik###
library(plyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(reshape2)
library(fivethirtyeight)
library(ggthemes)
library(stringr)
library(viridis)


setwd("C:/Base/LittlePrjct/HaV_Bad/IE_Method")
df=read.csv("Metoddatabadvatten.csv", sep=',', encoding = 'UTF-8')

#df20=subset(df, df$År == 2020)
df15=subset(df, df$År > 2014)

dfMt=plyr::ldply(tapply(df20$IE.metod, df20$Kommun, table),rbind)

{
sink("EntrolrtE_Kommun.txt")
print(dfMt[order(dfMt$`Enterolert-E®/Quanti-Tray®`,decreasing = TRUE),])
sink()  
}
  
dfMt[order(dfMt$`Enterolert-E®/Quanti-Tray®`,decreasing = TRUE),]

####
bsDf = read.csv("Badvatten_15-20.csv", encoding = 'UTF-8')
#bsDf20 = subset(bsDf, bsDf$Year == 2020)
bsDf15 = subset(bsDf, bsDf$Year > 2014)

x=stringr::str_extract(bsDf15$Badplats, "(?<=>).*[<]")
x=substr(x, 1, nchar(x)-1)
bsDf15$Bad_ID = x

colnames(df15)[2] = "Bad_ID"
colnames(df15)[1] = "Datum"

bsDf15$Datum=as.Date(bsDf15$Datum, format = "%Y-%m-%d")
df15$Datum = as.Date(df15$Datum, format = "%Y-%m-%d")

mrgDf=merge(df15, bsDf15, by=c('Bad_ID', 'Datum'))

write.csv(mrgDf, "Bad_MetodMrg_1520.csv", sep=',', row.names = FALSE, fileEncoding = "UTF-8")

###Gör korrelations graf mellan andel Entorlert och otjänliga prover. 
tapply(mrgDf$Län, mrgDf$Kvalitet, table)
tapply(mrgDf$Län, mrgDf$IE.metod, table)


###Diverging bar plot of the top 30 municipalities###   
setwd("C:/Base/LittlePrjct/HaV_Bad/IE_Method")

df = read.csv("Provresultat.csv", encoding = 'UTF-8')
df[is.na(df)] = 0

#Få fram 15 kommuner med mest otjänliga prover
df1=subset(df, df$Kvalitet == "Otjänligt")
ta1=as.data.frame(tapply(df1$Kvalitet, df1$Kommun, table))

names(ta1) = c("Otjanliga")
ta1$Kommun = row.names(ta1)
ta1=ta1[order(ta1$Otjanliga), ]
top15=ta1[(nrow(ta1)-14):nrow(ta1),]
top20=ta1[(nrow(ta1)-19):nrow(ta1),]

df$Year = year(as.Date(df$Datum, format = "%Y-%m-%d"))
df$Kvalitet=gsub("Otjänligt", 1, df$Kvalitet) 
df$Kvalitet=gsub("Tjänligt", 0, df$Kvalitet) 
df$Kvalitet=gsub("0 m. Anm.", 0, df$Kvalitet) 
df$Kvalitet = as.integer(df$Kvalitet)

df06=subset(df, df$Year < 2017)

mean1=as.data.frame(tapply(df06$Kvalitet, df06$Kommun, mean))
sd1=as.data.frame(tapply(df06$Kvalitet, df06$Kommun, sd))
mean1$Kommun = row.names(mean1)
sd1$Kommun = row.names(sd1)
sd15=sd1[grep(paste(top20$Kommun, collapse = "|"), sd1$Kommun),]
mn15=mean1[grep(paste(top20$Kommun, collapse = "|"), mean1$Kommun),]

names(sd15) = c("Otjanliga","Kommun")
names(mn15) = c("Otjanliga", "Kommun")

df2015=subset(df, df$Year > 2016 & df$Year < 2021)
df_1515=df2015[grep(paste(top20$Kommun, collapse = "|"), df2015$Kommun),]

meanYear = as.data.frame(tapply(df_1515$Kvalitet, list(df_1515$Kommun, df_1515$Year), mean))
meanYear$Kommun = row.names(meanYear)

z2015=round((meanYear$'2015' - mn15$Otjanliga)/sd15$Otjanliga, 2)
z2016=round((meanYear$'2016' - mn15$Otjanliga)/sd15$Otjanliga, 2)
z2017=round((meanYear$'2017' - mn15$Otjanliga)/sd15$Otjanliga, 2)
z2018=round((meanYear$'2018' - mn15$Otjanliga)/sd15$Otjanliga, 2)
z2019=round((meanYear$'2019' - mn15$Otjanliga)/sd15$Otjanliga, 2)  
z2020=round((meanYear$'2020' - mn15$Otjanliga)/sd15$Otjanliga, 2)


#x=cbind(z2015, z2016, z2017, z2018, z2019,z2020)
#x1$year = rep(c("2015","2016", "2017", "2018", "2019", "2020"),  each = 15)
x=cbind(z2017, z2018, z2019,z2020)

x1=melt(x)
y1=rep(meanYear$Kommun,4)
x1$Kommun = y1
x1=x1[,3:4]
x1$year = rep(c("2017", "2018", "2019", "2020"),  each = 20)
x1$type=ifelse(x1$value < 0, "Under", "Över")

ggplot(x1, aes(x=Kommun, y=value, label=value)) + 
  geom_bar(stat='identity', aes(fill=type), width=.5)  +
  scale_fill_manual(labels = c("Above Period Average", "Below Period Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(title= "Vattenkvalite utveckling",x="", y="Relativ förändring") + 
  coord_flip() + theme_bw() + theme(legend.title = element_blank())


x1$aCol = rep(1, length(x1$year))

gg=ggplot(x1, aes(x=Kommun, y=value, label=value)) + 
  geom_bar(stat='identity', aes(fill=type), width=.5)  + facet_grid(aCol~year) +
  scale_fill_manual(labels = c("Över kommunsnittet", "Under kommunsnittet"), 
                    values = c("Under"="#00ba38", "Över"="#f8766d")) +
  labs(title= "Vattenkvalite utveckling",x="", y="Relativ förändring inom kommunen") + theme_bw() 
gg = gg + theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5))+coord_flip()
 

gg=ggplot(x1, aes(x=Kommun, y=value, label=value)) + 
  geom_bar(stat='identity', aes(fill=type), width=.3)  + facet_grid(~year) +
  scale_fill_manual(labels = c("Sänkning otjänliga prov", "Ökning otjänliga prov"), 
                    values = c("Under"="#00ba38", "Över"="#f8766d")) +
  labs(x="", y="") + theme_bw() 
gg = gg + theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5, size = 12), strip.background.y = element_blank(), strip.text.y = element_blank(),legend.position = "bottom", strip.background.x = element_blank(), strip.text.x = element_text(size=12), axis.text.x = element_text(angle=90))

gg=ggplot(x1, aes(x=Kommun, y=value, label=value)) + 
  geom_bar(stat='identity', aes(fill=type), width=.3)  + facet_grid(aCol~year) +
  scale_fill_manual(labels = c("Sänkning otjänliga prov", "Ökning otjänliga prov"), 
                    values = c("Under"="#00ba38", "Över"="#f8766d")) +
  labs(x="", y="") + theme_bw() 
gg = gg + theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5, size = 12), strip.background.y = element_blank(), strip.text.y = element_blank(),legend.position = "bottom", strip.background.x = element_blank(), strip.text.x = element_text(size=12))+coord_flip()


###Sammanställning av ration av otjänliga prove och prov med anm., i relation Entrolert-e mätningar###
setwd("C:/Base/LittlePrjct/HaV_Bad/IE_Method")
df=read.csv("Bad_MetodMrg_1520.csv", encoding = "UTF-8")

df$Kvalitet=gsub("Tjänligt m. Anm.", "Tjänligt_MedAnm", df$Kvalitet)

table(df$IE.metod)
table(df$Ecoli.metod)

tapply(df$Kvalitet, df$IE.metod, table)
x=unlist(tapply(df$Kvalitet, df$IE.metod, function(x) table(x)/sum(table(x)))) #Lista ut vad som pågår ^^

IE_Rat=read.csv("IE_Values.csv", encoding="UTF-8")
IE_Sub=IE_Rat[IE_Rat$Kvalite == "Tjänligt m. Anm."|IE_Rat$Kvalite == "Otjänligt",]
IE_Sub$Metod=gsub("Enterolert-E®/Quanti-Tray", "Entrolert E", IE_Sub$Metod)

IE_RG=ggplot(data=IE_Sub, aes(x=Kvalite, y=Ratio, fill=Kvalite))
IE_RG = IE_RG + geom_bar(stat = "identity", colour = "grey") +facet_grid(~Metod)
IE_RG = IE_RG + theme(axis.text.x = element_blank(), axis.title.x = element_blank(), legend.title = element_blank())+scale_fill_manual(values = c("#313447","#ebeefd"))
IE_RG = IE_RG + theme(strip.text = element_text(size=12), axis.text = element_text(size=11))

###Check E_Coli Method### 
subDf = df[!df$Ecoli.metod == "ISO 9308-3",]
subDf$Ecoli.metod=gsub("Colilert®-18/Quanti-Tray®", "Colilert_18", subDf$Ecoli.metod)
subDf$Ecoli.metod=gsub("SS-EN ISO 9308-2:2014", "ISO 9308-2:2014", subDf$Ecoli.metod)

CL_RG = unlist(tapply(subDf$Kvalitet, subDf$Ecoli.metod, function(x) table(x)/sum(table(x))))
CL_RG = melt(CL_RG)
CL_RG$Fact=row.names(CL_RG)
CL_RG = tidyr::separate(CL_RG, Fact, c("Metod", "Kvalite"), "\\.")
CL_RG = CL_RG[CL_RG$Kvalite == "Otjänligt"|CL_RG$Kvalite == "Tjänligt_MedAnm",]

GG_CL = ggplot(data=CL_RG, aes(x=Kvalite, y=value, fill=Kvalite))
GG_CL = GG_CL + geom_bar(stat = "identity", colour = "grey") +facet_grid(~Metod)
GG_CL = GG_CL + theme(axis.title.x = element_blank(), legend.title = element_blank())+scale_fill_manual(values = c("#313447","#ebeefd"))+labs(y="Ratio")
GG_CL = GG_CL + theme(strip.text = element_text(), axis.text = element_text(size=12), axis.text.x = element_blank(), legend.position = "bottom")

###E_Coli_Method_ProportionalChange###
df$Ecoli.metod=gsub("Colilert®-18/Quanti-Tray®", "Colilert_18", df$Ecoli.metod)
df$Ecoli.metod=gsub("SS-EN ISO 9308-2:2014", "ISO 9308-2:2014", df$Ecoli.metod)
smlDf = df[!df$Ecoli.metod == "ISO 9308-3",]

coliDta=melt(unlist(tapply(smlDf$Ecoli.metod, smlDf$Year, table)))
coliDta$Ratio=coliDta$value/rep(table(df$Year), each=4)

coliDta$Fact = row.names(coliDta)
coliDta=tidyr::separate(coliDta, Fact, c("År", "Metod"), "\\.")

gg3 = ggplot(coliDta, aes(y=Ratio,x=År,fill = Metod))
gg3 = gg3 + geom_bar(stat="identity", color ="black") + scale_fill_brewer(palette = 3)
gg3 = gg3 + labs(y="Andel av vattenprover", x="", title = "Fördelning av mätmetoder för E.Coli")+theme_bw()+theme(legend.title = element_blank())

###IE_Method_ProportionalChange###
df$IE.metod = gsub("Enterolert-E®/Quanti-Tray®", "Entrolert E",df$IE.metod)

coliDta=melt(unlist(tapply(df$IE.metod, df$Year, table)))
coliDta$Ratio=coliDta$value/rep(table(df$Year), each=3)
coliDta$Fact = row.names(coliDta)
coliDta=tidyr::separate(coliDta, Fact, c("År", "Metod"), "\\.")

gg4 = ggplot(coliDta, aes(y = Ratio,x=År, fill = Metod))
gg4 = gg4 + geom_bar(stat="identity", color ="black") + scale_fill_brewer(palette = 3)
gg4 = gg4 + labs(y="Andel av vattenprover", x="", title = "Fördelning av mätmetoder för IE")+theme_bw()+theme(legend.title = element_blank())

###Sortera fram kommuner som har högst frekvens av ISO 9308-2:204 & Entrolert E###
#Heatmap, län och kommun
df17=df[df$Year > 2016,]
df17$IE.metod = as.factor(df17$IE.metod)
df17$Ecoli.metod = as.factor(df17$Ecoli.metod)
df17$IE.metod=gsub("Enterolert-E®/Quanti-Tray", "Enterolert_E", df17$IE.metod)
df17$IE.metod=gsub("Enterolert_E®", "Enterolert_E", df17$IE.metod) # god knows why ^^ 

df17$IE.metod = droplevels(df17$IE.metod)

IE_Kom=melt(unlist(tapply(df17$IE.metod, df17$Kommun.x, table)))
IE_Kom$temp = row.names(IE_Kom)
IE_Kom=tidyr::separate(IE_Kom, temp, c("Kommun","Metod"), "\\.")
x3=melt(unlist(tapply(IE_Kom$value, IE_Kom$Kommun, function(x) x/sum(x))))
IE_Kom$Ratio=x3$value

#df17 = df17[df17$Ecoli.metod == "ISO 9308-2:2014",]
#df17$Ecoli.metod=droplevels(df17$Ecoli.metod)

CL_Kom=melt(unlist(tapply(df17$Ecoli.metod, df17$Kommun.x, table)))
CL_Kom$temp = row.names(CL_Kom)
CL_Kom=tidyr::separate(CL_Kom, temp, c("Kommun","Metod"), "\\.")

x3=melt(unlist(tapply(CL_Kom$value, CL_Kom$Kommun, function(x) x/sum(x))))

CL_Kom$Ratio=x3$value
CL_Kom = CL_Kom[CL_Kom$Metod == "SS-EN ISO 9308-2:2014",]
x = IE_Kom[IE_Kom$Metod == "Entrolert E",]

IE_Kom$Split=rep(c("A","B","C"), each = 259)
IE_A=IE_Kom[IE_Kom$Split == "A",]
IE_B=IE_Kom[IE_Kom$Split == "B",]
IE_C=IE_Kom[IE_Kom$Split == "C",]

#IE_A$H = rep(20, length(IE_A$Ratio))
#IE_A$Kommun = as.factor(IE_A$Kommun)

A_Heat=ggplot(data=IE_A, aes(y=Kommun, x=Metod, fill=Ratio))
A_Heat = A_Heat + geom_tile()+ scale_y_discrete(limits = rev)
A_Heat = A_Heat + theme(axis.text.x = element_text(angle=90, size =6), axis.text.y = element_text(size=6), axis.title = element_blank(), legend.title = element_blank())+coord_equal(ratio = 1)