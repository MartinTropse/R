setwd("C:/Bas/AquaBiota/Projekt/Visning/Fiske") #Sätt till mappen som har import filen. 

list.files() #Skriver ut alla filer i mappen, går att kopiera till read.csv nedan om man så vill.
df=read.csv("FiskedataHav_Pol.csv", sep=',',encoding="UTF-8", header=TRUE) #Läser datafilen med fiskedata

names(df)
#names(df) = c("ID", "Landningsår", "DecLat", "DecLon", "Redskap", "Fisk_MAF", "Fisk_Sve", "Kvantitet_kg",
#              "Fisk_Lat","Fisk_Eng","Swe99_x","Swe99_y")   #Sätter nya kolumnnamn till datasetet.
            
df$Cor=paste(df$x, df$y) #Kombinerar lat & long till ett värde som blir unikt för varje inrapporteringspunkt. 
unique(df$Fiskslag.MAF)
unique(df$Fisk_MAF) #Använd för att se över vilka unika arter/MAF förkortningar som finns i datasetet
df = df[which(df$Fiskslag.MAF == "HER",] #Ändra gruppen som ska selekteras för kartan, t.ex. "HER" eller "COD"
out=list(tapply(df$Kvantitet..kg., df$Cor, sum)) #Beräknar aggregerade värde för arten vid varje unik inrappoteringspunkt. 
dfExp = as.data.frame(do.call(cbind, out)) #Binder ihop listorna från ovan till en dataframe för export. 

dfExp$Swe99X=df$x
dfExp$Swe99Y=df$y

names(dfExp)[1] = "Kvantitet_KG"

write.table(dfExp, "FiskIntensitet_PolandZ_HER.csv", sep=',', fileEncoding = "UTF-8", row.names = F) #Exportera data


# tempDf=as.data.frame(dfRst)
# max(tempDf$V1)
# quantile(tempDf$V1, probs=seq(0,1,0.1))

#quantile(df$Kvantitet_kg, probs = seq(0,1,0.001))#Ger en överblick 