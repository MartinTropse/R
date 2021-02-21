require(ggplot2)
require(GGally)
require(CCA)
library(cowplot)
library(corrplot)
library(PerformanceAnalytics)
library(Hmisc)

setwd("C:/Bas/AquaBiota/Projekt/OX2/Bentos punktkartor/Tumlare")
df<-read.csv("TumlarAnalysLongFormat_Upd.csv")

df<-df[,4:19]

#Plot correlations of each pair of variables:ggplot generalized pairs plot
pdf("pair_corr.pdf",width = 14,height = 14)
ggpairs(df)+theme(strip.text = element_text(size=6))
dev.off()

str(df)
numDf=df[,c(1:3,6:16)]

res2 <- rcorr(as.matrix(numDf))

rVal=res2$r #Correlation cof
pVal=res2$P #P-values

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

CorrMtrx=flattenCorrMatrix(rVal, pVal)
CorrTum = CorrMtrx[which(CorrMtrx$row == "TumlarMin"), ]
#cor(x=numDf, y=numDf$TumMinut, method="spearman")
cor(x=numDf, y=numDf$TumMinut, method="pearson")
plot(density(numDf$YrkesfiskeTorskmodell))
CorrTum$pAdjust=p.adjust(CorrTum$p, "fdr")

write.table(CorrTum, "PAdjustedCorr_Tumlare.csv", row.names = FALSE, col.names = TRUE)


###Nominal assocation###
facDf=df[,c(4,7:8)]
facDf=na.omit(facDf)
facDf=facDf[which(facDf$HelcomHabitat != "Oklassad"),]



str(facDf)
table(facDf$Substrat)
facDf$Substrat=droplevels(facDf$Substrat)
table(facDf$HelcomHabitat)
facDf$HelcomHabitat=droplevels(facDf$HelcomHabitat)


###ANOVA###
interaction.plot(facDf$Substrat, facDf$HelcomHabitat, facDf$TumlarMin)
results<- lm(data=facDf, TumlarMin ~ Substrat * HelcomHabitat)
anova(results)

TumAOV=aov(data=facDf, TumlarMin ~ Substrat * HelcomHabitat)
summary(TumAOV)



###Graphs###
ggplot(data=df, aes(y=df$TumlarMin, x = df$AvstdTorsklek)) +
  geom_point() +
  stat_smooth(method='lm')+
  theme_bw(14)+labs(y="Minuter av tumlardetektion", x="Avstånd från lekområden torsk")

head(df)

ggplot(data=df, aes(y=df$TumlarMin, x = df$FiskeSill)) +
  geom_point() +
  stat_smooth(method="lm")+
  theme_bw(14)+labs(y="Minuter av tumlardetektion", x="Rapporterad fångster av sill")

ggplot(data=df, aes(y=df$TumlarMin, x = df$FiskeTorsk)) +
  geom_point() +
  stat_smooth(method="lm")+
  theme_bw(14)+labs(y="Minuter av tumlardetektion", x="Rapporterad fångster av torsk")

dfSub = na.omit(df)

ggplot(data=dfSub, aes(y=dfSub$TumlarMin, x = dfSub$Substrat, fill = dfSub$Substrat)) +
  geom_boxplot() +
  theme_bw(14)+labs(y="Minuter av tumlardetektion", x="Substrat")+
  theme(legend.title = element_blank())

dfSub = df[which(df$HelcomHabitat != "Oklassad"),]
dfSub$HelcomHabitat = gsub("Sandbotten blandad fauna", "Sandbottenfauna", dfSub$HelcomHabitat)
dfSub$HelcomHabitat = gsub("Mjukbotten med sjopennor", "Mjukbotten med sjöpennor", dfSub$HelcomHabitat)
dfSub$HelcomHabitat = gsub("Rev/Sandbotten", "Rev och sand", dfSub$HelcomHabitat)

ggplot(data=dfSub, aes(y=TumlarMin, x = HelcomHabitat, fill = HelcomHabitat)) +
  geom_boxplot() +
  theme_bw(14)+labs(y="Minuter av tumlardetektion", x="Modellerade habitat")+
  theme(legend.title = element_blank())