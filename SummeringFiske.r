setwd("C:/Bas/AquaBiota/Projekt/Visning/Fiske") #Sätt till mappen som har import filen. 

list.files() #Skriver ut alla filer i mappen, går att kopiera till read.csv nedan om man så vill.
df=read.csv("fishData_polen.csv", sep=',',encoding="UTF-8", header=TRUE) #Läser datafilen med fiskedata

names(df) = c("Utförare", "Landningsår", "Redskap", "Kvantitet_kg", "Fisk_MAF", "Fisk_LatSwe", "Fisk_Eng", 
              "Lat", "Long", "Layer", "Path") #Sätter nya kolumnnamn till datasetet.

df$Cor=paste(df$x, df$y) #Kombinerar lat & long till ett värde som blir unikt för varje inrapporteringspunkt. 

unique(df$Fisk_MAF) #Använd för att se över vilka unika arter/MAF förkortningar som finns i datasetet
df = df[which(df$Fisk_MAF == "HER"),] #Ändra gruppen som ska selekteras för kartan, t.ex. "HER" eller "COD"
out=list(tapply(df$Kvantitet_kg, df$Cor, sum)) #Beräknar aggregerade värde för arten vid varje unik inrappoteringspunkt. 
dfExp = do.call(cbind, out) #Binder ihop listorna från ovan till en dataframe för export. 

write.table(dfExp, "FiskIntensitetHER.csv", sep=',', fileEncoding = "UTF-8") #Exportera data


# tempDf=as.data.frame(dfRst)
# max(tempDf$V1)
# quantile(tempDf$V1, probs=seq(0,1,0.1))

#quantile(df$Kvantitet_kg, probs = seq(0,1,0.001))#Ger en överblick 