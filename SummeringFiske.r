setwd("C:/Bas/AquaBiota/Projekt/OX2/FiskeBilaga/FiskeData")

df=read.csv("KG_FiskeData_UTF8.csv", sep=',',encoding="UTF-8", header=TRUE)


names(df) = c("Utförare", "Landningsår", "Redskap", "Kvantitet_kg", "Fisk_MAF", "Fisk_LatSwe", "Fisk_Eng", 
              "Lat", "Long", "Layer", "Path")

quantile(df$Kvantitet_kg, probs = seq(0,1,0.001))

df$Cor=paste(df$Lat, df$Long)
length(unique(df$Lat))
length(unique(df$Long))
length(unique(df$Cor))

df = df[which(df$Fisk_MAF == "SPR"),]

out=list(tapply(df$Kvantitet_kg, df$Cor, sum))
dfRst = do.call(cbind, out)

dim(dfRst)

# asDf=as.data.frame(dfRst)
# max(asDf$V1)
# quantile(asDf$V1, probs=seq(0,1,0.1))

write.table(dfRst, "FiskIntensitet_PLE.csv", sep=',', fileEncoding = "UTF-8")