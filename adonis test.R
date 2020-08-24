setwd("C:/PhD Hörnan/Experiment/Sterilisering experiment 2014-08/R")

library(vegan)
list.files()

meta<-read.table("metadata2.txt", header=TRUE,sep="\t")
meta<-meta[grep("Ori", meta$Treatment, invert=TRUE),]
adoncomp<-(meta[,3:7])
env<-(meta[,1:2])
dim(adoncomp)
dim(env)
meta$Treatment


meanco1<-tapply(adoncomp$C1, list(env$Treatment, env$Water),mean)
meanco2<-tapply(adoncomp$C2, list(env$Treatment, env$Water),mean)
meanco3<-tapply(adoncomp$C3, list(env$Treatment, env$Water),mean)
meanco4<-tapply(adoncomp$C4, list(env$Treatment, env$Water),mean)
meanco5<-tapply(adoncomp$C5, list(env$Treatment, env$Water),mean)

k<-aov(adoncomp$C4~env$Treatment*env$Water)
summary(k)
TukeyHSD(k, "env$Treatment")

r<-aov(adoncomp$C3~env$Treatment*env$Water)
summary(r)
TukeyHSD(r, "env$Treatment")

k2<-aov(adoncomp$C2~env$Treatment*env$Water)
summary(k2)
TukeyHSD(k2, "env$Treatment")

k1<-aov(adoncomp$C1~env$Treatment*env$Water)
summary(k1)
TukeyHSD(k1, "env$Treatment")

k5<-aov(adoncomp$C5~env$Treatment*env$Water)
summary(k5)
TukeyHSD(k5, "env$Treatment")

#Generarting Z-Score
zadoncomp=scale(adoncomp, center = TRUE, scale = TRUE)

###PERMANOVA RAW COMPONENT###
euclid_matx<-vegdist(adoncomp, method="euclidean")
zeuclid_matx<-vegdist(zadoncomp, method="euclidean")
bray_matx<-vegdist(adoncomp, method="bray")

adoniscomtest<-adonis(bray_matx~env$Treatment*env$Water, permutations = 999)
adoniserawcomtest<-adonis(euclid_matx~meta$Treatment*meta$Water, permutations = 999)#used in m
zadoniserawcomtest<-adonis(zeuclid_matx~meta$Treatment*meta$Water, permutations = 999)
#scale(A, center = TRUE, scale = TRUE)


###PERMANOVA CORRECTED COMPONENT###
components1<-read.table("correctedcomponents_fixed.txt", header=TRUE, sep="\t")
components1<-components1[grep("Ori", components1$Treatment, invert=TRUE),]

dim(components1)
dim(adoncomp)

ncomp<-components1[,c(3:7)]

neuclid_matx<-vegdist(ncomp, method="euclidean")
nbray_matx<-vegdist(ncomp, method="bray")

nbadoniscomtest<-adonis(nbray_matx~components1$Treatment*components1$Water, permutations = 999)
neadoniscomtest<-adonis(neuclid_matx~components1$Treatment*components1$Water, permutations = 999) #used in m

