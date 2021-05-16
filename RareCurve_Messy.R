library(vegan)

setwd("C:/Users/MartinAndersson/AquaBiota/Gruppwebbplats för AquaBiota - Documents/PROJEKT/2020040 eDNA_Vombsjön/2_Arbetsdokument/NM_Rapport")
list.files()

df = read.csv("RareData_Vombsjon.csv")
df = df[,2:14]
df = df[c(1:6,8:10),]

mean(rowSums(df))
sd(rowSums(df))

#df = read.csv("RareDataAmnt_Vombsjon.csv")
#df = df[c(1:6, 8:10),]

mySeq=seq(2,10,1)
colSeq=seq(1,13,1)
x=df[sample(nrow(df), 3), ]


countList = list()

for(z in 2:9){
  for(x in 1:10){
    print(z)
    y<-df[sample(nrow(df), z), ]
  countList <- c(countList, sum(0<colSums(y)))
  }
}

newDf=do.call(rbind, countList)
sampleAmt=rep(2:9, times=1, each=10)
newDf = as.data.frame(newDf)

newDf$SmplAmt = sampleAmt

newMean=list(tapply(newDf$V1, newDf$SmplAmt, mean))
newSd=list(tapply(newDf$V1, newDf$SmplAmt, sd))

newMeanX=as.data.frame(do.call(cbind, newMean))
newSdX=as.data.frame(do.call(cbind, newSd))

newMeanX$V2 = newSdX$V1

write.table(newDf, "MyRarefactionCal.csv", sep = ',', row.names = FALSE)
write.table(newMeanX, "MyRarefactionCal1.csv", sep = ',', row.names = FALSE)
