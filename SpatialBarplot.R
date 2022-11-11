library(reshape2)
library(ggplot2)
library(viridis)


setwd("C:/Bas/AquaBiota/Projekt/Life DNAquatics/Spatial_study")

df=read.csv("SeqData_Moalven.csv", fileEncoding = "UTF-8-BOM")
env=read.csv("EnvData_Moalven.csv", fileEncoding = "UTF-8-BOM")

df=df[1:114,]#Cuts away none-data rows 
env=env[1:114,]#Cuts away none-data rows 

env=env[which(env$Type != "Marine"),]

df=df[df$Sample.ID %in% env$Sample.ID,] #De-selects marine samples from speceis data. 

spc = df[,4:37]
row.names(spc) = df$Sample.ID
row.names(df) = df$Sample.ID

spc[is.na(spc)] = 0
spc=spc[,colSums(spc)>0]#Removes columns from species that lack presence after subsetting 
rmvSpc=c("Gadus.morhua","Myoxocephalus.sp...Rötsimpa.","Gobio.gobio.sandkrypare", "Clupea.harengus")
spc=spc[,!(names(spc) %in% rmvSpc)]
spcList = names(spc)

for(idx in seq(dim(spc)[1])){
  x=(spc[idx,]/rowSums(spc)[idx])*100
  spc[idx,] = x
}
 
myName=as.data.frame(env$NAMN, row.names = row.names(env))

latList=sort(names(spc)) 
sweList=c("Braxen","Löja","Ål","Ruda","Siklöja","Sik","Stensimpa","Karp","Gädda","Storspigg","Gärs","Nejonöga","Leuciscus.sp.","Leusiscus.spp.","Lake","Regnbåge","Nors","Abborre","Elritsa","Småspigg","Mört","Lax","Öring","Fjällröding","Bäckröding","Gös","Harr")
spc=spc[,sort(names(spc))]
names(spc) = sweList
row.names(env) = env$Sample.ID

spc=merge(spc, myName,by = "row.names")
spc$rowName=row.names(spc)
names(spc)[29] = "Namn"


spcMlt=melt(spc, value.name = "SeqData")
names(spcMlt) = c("Sample","Namn","Species","SeqData")
spcMlt$Species = as.character(spcMlt$Species)

spcMlt=spcMlt[order(spcMlt[,"Species"], decreasing =TRUE),]

gg=ggplot(data=spcMlt, aes(x=Namn, y=factor(Species, 
                                              levels = rev(levels(factor(Species)))), fill=SeqData))
gg=gg+geom_tile(color ="#000000")+  scale_fill_gradient2(
  low = "#FFFFFF", 
  mid = "#eeefec", 
  high = "brown", 
  midpoint = .06
)
gg=gg+labs(y="", x="", title = "Sekvensfördelning (%) av fiskarter i Moälven, per prov")
gg=gg+theme(axis.text.x = element_text(angle = 90), axis.text.y = element_text(size = 14), plot.title = element_text(size = 16), legend.title = element_blank())


