setwd("D:/Users/martin/Desktop/PhD Hörnan/Experiment/Sterilisering experiment 2014-08/R")

library(vegan)
#library(Hmisc)
#library(Rcmdr)
library(Rcpp)
library(ggplot2)
#library(RSvgDevice)
library(gridExtra)
library(indicspecies)
library(edgeR)
library(mvabund)
#library(lmPerm)
library(spaa)
#library(outliers) (can't be loaded if running nmds analysis)
library(car)
library(labdsv)
library(reshape2)
library(stringr)
library(grid)
library(phyloseq)
library(limma)

#TOC concentration
setwd("C:/Users/martin/Dropbox/R/Sterility/Test/R")
list.files()
concentration<- read.table("Concentration.txt", header = TRUE, sep="\t")

## Convert variables to factors
concen1 <- within(concentration, {
  treat <- factor(Treatment)
  lake <- factor(Lake)
  })
list.files()
aol1<-aov(concen1$TOC.mg~concen1$treat*concen1$lake) 
summary(aol1)

treat2 <- factor(Treatment)
aol2<-aov(TOC.mg~treat*lake)


#Bacterial Yield Calculation
Yield <- read.table("BacYield.txt", header = TRUE,sep="\t")

boxplot(Yield$SampleYield~Yield$Lake)
boxplot(Yield$SampleYield~Yield$Treatment)
hist(Yield$SampleYield)
LogYield<-log(Yield$SampleYield)
hist(LogYield)

shapiro.test(Yield$SampleYield) #Not normally distrubuted
shapiro.test(LogYield) #Normally distrbuted

YieldANO <- aov(LogYield~Yield$Lake*Yield$Treatment)
YiedlANO1way <- aov(LogYield~Yield$Treatment)

summary(YieldANO)
?TukeyHSD

Phoc<-TukeyHSD(YieldANO)
Phoc1way <- TukeyHSD(YiedlANO1way)
summary(Phoc)


#FDOM##################################################
library(vegan)
FDOM<- read.table("FDOMAll1.txt", header = TRUE,sep="\t")

attach(FDOM)
class(FDOM$Treatment)
boxplot(Value~Treatment)

FDOMANOVA <- aov(Value ~ Treatment)
summary(FDOMANOVA)

shapiro.test(residuals(FDOMANOVA))
hist(residuals(FDOMANOVA))
fit <- lm(abund~time,data=demo1)
hist(residuals(fit))
shapiro.test(residuals(fit))
qqnorm(residuals(fit))

tapply(Value, Treatment, mean) #Gets the mean from col "Value" for each "Treatment"
tapply(Value, Treatment, sd) #Same but Sd


TukeyHSD(FDOMANOVA, "Treatment") 
########Other
write.table(concen1, ("C:/Users/martin/Desktop/PhD Hörnan/R/Monica Scripts/Results 2nd sequencing +old/concen1.txt"), sep="\t")


#### Test of PARAFAC components####
setwd("D:/PhD Hörnan/Experiment/Sterilisering experiment 2014-08/R")
components<-read.table("correctedcomponents.txt", header=TRUE, sep="\t")

list.files()
water<-as.factor(components$Water)
sterilization<- as.factor(components$Treatment)

O<-(components$C1)
K<-(1/sqrt(components$C1))
J<-(0.33/sqrt(components$C1))
I<-(0.5/sqrt(components$C1))
L<-(log(components$C1))

hist(O)
hist(L)
hist(K)
hist(J)
hist(I)

shapiro.test(O)
shapiro.test(L)
shapiro.test(K)
shapiro.test(J)
shapiro.test(I)

qqnorm(O)
qqline(O)
qqnorm(L)
qqline(L)
qqnorm(K)
qqline(K)
qqnorm(J)
qqline(J)
qqnorm(I)
qqline(I)

J1<-aov(J~water*sterilization)
summary(J1)

boxplot(components$C1R~components$Treatment+components$Water, ylab="test")
boxplot(components$C1R~sterilization)

AOVC1<-aov(components$C1~water*sterilization)
AOVC1log<-aov(c1log~water*sterilization)

summary(AOVC1)
summary(AOVC1LOG)

##Testing the distrubution of residuals
TukeyHSD(AOVC1, "sterilization")
shapiro.test(residuals(AOVC1)) #Not so normal....
hist(residuals(AOVC1))
shapiro.test(residuals(AOVC1log))
hist(residuals(AOVC1log))



shapiro.test(residuals(AOVC2))
hist(residuals(AOVC2))

boxplot(components$C2~water)
boxplot(components$C2~sterilization)
AOVC2<-aov(components$C2~water*sterilization)
summary(AOVC2)

TukeyHSD(AOVC2, "sterilization")

boxplot(components$C3~water)
boxplot(components$C3~sterilization)
AOVC3<-aov((components$C3~water*sterilization))
summary(AOVC3)

TukeyHSD(AOVC3, "sterilization")

boxplot(components$C4~water)
boxplot(components$C4~sterilization)
AOVC4<-aov(components$C4~water*sterilization)
summary(AOVC4)

TukeyHSD(AOVC4, "sterilization")

boxplot(components$C5~water)
boxplot(components$C5~sterilization)
AOVC5<-aov(components$C5~water*sterilization)
summary(AOVC5)

TukeyHSD(AOVC5, "sterilization")


###FDOM normalized by TOC amount###
list.files()
fdom_norm<-read.table("fdom_normalized.txt", header=TRUE, sep="\t")
boxplot(fdom_norm$Norm_FDOM~fdom_norm$Lake)
boxplot(fdom_norm$Norm_FDOM~fdom_norm$Sterilization)

a1<-aov(fdom_norm$Norm_FDOM~fdom_norm$Lake*fdom_norm$Sterilization)
summary(a1)
TukeyHSD(a1, "fdom_norm$Lake")
TukeyHSD(a1, "fdom_norm$Sterilization")

###BIX and HIX###
list.files()
meta<- read.table("metadata1.txt", header=TRUE, sep="\t")   #No blank values included

hist(meta$Yield)
resb<-residuals(bixaov)
shapiro.test(resb) ## normally distrubuted
boxplot(meta$Yield~meta$Treatment)
boxplot(meta$Yield~meta$Water)

bixaov<-aov(meta$Yield~meta$Water*meta$Treatment)
bixaov2<-aov(meta$Yield~meta$Water*meta$BIX)
bixaov3<-aov(meta$Yield~meta$Water*meta$BIX)
bixaov4<-aov(meta$BIX~meta$Water)
bixaov5<-aov(meta$BIX~meta$Treatment)

summary(bixaov)
summary(bixaov2)
summary(bixaov3)
summary(bixaov4)
summary(bixaov5)

####Rank two-way anova of components###
meta2<-read.table("metadata2.txt", header=TRUE, sep="\t")

rankC1<-rank(meta2$C1)
rankC2<-rank(meta2$C2)
rankC3<-rank(meta2$C3)
rankC4<-rank(meta2$C4)
rankC5<-rank(meta2$C5)

rankc1aov<-aov(rankC1~meta2$Treatment*meta2$Water)
summary(rankc1aov)
boxplot(rankC1~meta2$Treatment)
TukeyHSD(rankc1aov,'meta2$Treatment') 

rankc2aov<-aov(rankC2~meta2$Treatment*meta2$Water)
summary(rankc2aov)
boxplot(rankC2~meta2$Treatment)
TukeyHSD(rankc2aov,'meta2$Treatment')

rankc3aov<-aov(rankC3~meta2$Treatment*meta2$Water)
summary(rankc3aov)
boxplot(rankC3~meta2$Treatment)
TukeyHSD(rankc3aov,'meta2$Treatment')

rankc4aov<-aov(rankC4~meta2$Treatment*meta2$Water)
summary(rankc4aov)
boxplot(rankC4~meta2$Treatment)
TukeyHSD(rankc4aov,'meta2$Treatment')

rankc5aov<-aov(rankC5~meta2$Treatment*meta2$Water)
summary(rankc5aov)
boxplot(rankC5~meta2$Treatment)
TukeyHSD(rankc5aov,'meta2$Treatment')
