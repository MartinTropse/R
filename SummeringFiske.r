setwd("C:/Bas/AquaBiota/Projekt/Visning/Fiske") #S�tt till mappen som har import filen. 

list.files() #Skriver ut alla filer i mappen, g�r att kopiera till read.csv nedan om man s� vill.
df=read.csv("fishData_polen.csv", sep=',',encoding="UTF-8", header=TRUE) #L�ser datafilen med fiskedata

names(df) = c("Utf�rare", "Landnings�r", "Redskap", "Kvantitet_kg", "Fisk_MAF", "Fisk_LatSwe", "Fisk_Eng", 
              "Lat", "Long", "Layer", "Path") #S�tter nya kolumnnamn till datasetet.

df$Cor=paste(df$x, df$y) #Kombinerar lat & long till ett v�rde som blir unikt f�r varje inrapporteringspunkt. 

unique(df$Fisk_MAF) #Anv�nd f�r att se �ver vilka unika arter/MAF f�rkortningar som finns i datasetet
df = df[which(df$Fisk_MAF == "HER"),] #�ndra gruppen som ska selekteras f�r kartan, t.ex. "HER" eller "COD"
out=list(tapply(df$Kvantitet_kg, df$Cor, sum)) #Ber�knar aggregerade v�rde f�r arten vid varje unik inrappoteringspunkt. 
dfExp = do.call(cbind, out) #Binder ihop listorna fr�n ovan till en dataframe f�r export. 

write.table(dfExp, "FiskIntensitetHER.csv", sep=',', fileEncoding = "UTF-8") #Exportera data


# tempDf=as.data.frame(dfRst)
# max(tempDf$V1)
# quantile(tempDf$V1, probs=seq(0,1,0.1))

#quantile(df$Kvantitet_kg, probs = seq(0,1,0.001))#Ger en �verblick 