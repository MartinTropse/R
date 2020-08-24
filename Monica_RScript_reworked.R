setwd("D:/PhD Hörnan/R/Monica Scripts/Results 2nd sequencing +old")

list.files()
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
library(lmPerm)
library(spaa)
#library(outliers) (can't be loaded if running nmds analysis)
library(car)
library(labdsv)
library(reshape2)
library(stringr)
library(grid)
library(phyloseq)
library(limma)

#INDEX
#M-1: Loading and sorting the data
#M-2: ALPHA BETA DIVERSITY
#M-3: NMDS
#M-4: Diversity Measurements
#M-5: Testing residuals for Diversity test
#M-6: Indval indicator test
#M-7: EdgeR test (indicator test)
#M-7.1 Relative abundance of Edge result
#M-8: Stacked bargraph (Summary of taxonomic groups in samples)
#M-9: Niche width
#M-10: Detecting outliers
#M-11: Testing cell abundance between treatments WITH REP-MEAS
#M-12: Testing abundance SIZE difference between treatments with FSC median WITH REP-MEAS
#M-13: Most commmon phyla

#The R script Jungle! 
#MANOVA, ANOVA, SIMPER, separating time and treatment, checking normalization, kinetics test and so on.

M-1:
  #otu<-t(read.csv(file="otu_table.csv", row.names=1, sep="\t"))   ### Otu table
otu<-t(read.csv(file="otu_table_inoc_switch.csv", row.names=1, sep=";")) ###there is strong inidcation that pilar switched our incolum samples with two other samples, so we ran it again
#mdata<-read.csv(file="metadata.csv", row.names=1, sep=";")   ### sample data
mdata<-read.csv(file="metadata_test_inoc_switch.csv", row.names=1, sep=";") 
#mdata1<-mdata[3:90,] # excluding inoculum samples

tax<-as.data.frame(read.csv(file="assignments.csv", header=F, row.names=1, sep=";"))   ### taxonomy file
tax[1:3,]
otu[1:3,1:3]
# remove bad samplesinoculum2


# check different files and their correspondence 

d1<-merge(otu,tax, by.x = "row.names", by.y = "row.names")  # otu + taxonomy (total)

d2<-merge(t(otu),mdata, by.x = "row.names", by.y = "row.names") # otu + metadata (90 samples)
#d2<-merge(t(otu),mdata1, by.x = "row.names", by.y = "row.names") # otu + metadata (88 samples)

n<-nrow(otu)
m<-nrow(d2)
d4<-d2[,2:n] # otu table

m10<-grep("m10_*", colnames(otu))#Get the start OTU
m11<-grep("m11_*", colnames(otu))#get end OTU
inoc<-grep("ino*", colnames(otu))


start_otu<-otu[,m10]
end_otu<-otu[,m11]
ino_otu<-otu[,inoc]



t_ino_otu<-t(ino_otu)
t_end_otu<-t(end_otu)

t_start_otu<-as.data.frame(t_end_otu)##Be careful here, renamed it to t_start to match
#t_start_otu<-as.data.frame(t_ino_otu)##Be careful here, renamed it to t_start to match
#following script

#t_start_otu<-t(start_otu)
#t_start_otu<-as.data.frame(t_start_otu)

check1<-rrarefy(t_start_otu, min(rowSums(t_start_otu)))
check2<-check1[ ,colSums(check1)!=0]
check2<-t(check2)
check2<-as.data.frame(check2)
dim(check2)
sum(check2$m11_10_1)


rd4<-rarefy(d4,min(rowSums(d4))) # rarefy to smallest integer
crd4 = rd4[ ,colSums(rd4)!=0]   # remove OTUs with 0 ins
#attach(check_df)
#detach(check_df)
check_df<-as.data.frame(check2)
check_df$otu_sum<-rowSums(check_df)
check_df$otu_sum

#check3<-check_df[order(otu_sum),]# 
check3<-check_df[order(-check_df$otu_sum),]
new_t<-read.table("new_taxa.txt", sep ="\t")


row.names(new_t)<-new_t$V1

check4<-merge(check3, new_t, by.x = "row.names", by.y = "row.names")
class(check5)
check5<-check4[order(-check4$otu_sum),]
check5$otu_sum
write.table(check5, "end_top_otu.txt", sep = "\t")

M-1.1
comp_otu<-read.csv("comparing_top_otu.csv", header = T)



M-2
#order(-check4$###ALPHA BETA DIVERSITY
# samples with more than 5000 reads
d5<-d4[rowSums(d4)>5000,] 
list5<-rowSums(d4)>5000
rd5<-rrarefy(d5,min(rowSums(d5))) # rarefy to smallest integer
crd5 = rd5[ ,colSums(rd5)!=0] # X samples with more than 5000 reads

md5<-cbind(crd5,d2[,428:431]) # otu + metadata (all samples had more than 5000 reads)
###IF I WANT TO TEST RICHNESS CHAO in only the end samples I have to change d2 here to d2_end

#create lists for subsets


M-3
####  NMDS  ####
nmds<-metaMDS(crd4,distance="bray", trymax=200)
#nmds<-metaMDS(crd5,distance="bray", trymax=200)
#nmds2.fit<-envfit(nmds2, e2,perm=1000,na.rm=TRUE)

nmds$stress

n99<-scores(nmds)

res<-as.data.frame(merge(n99,d2, by.x = "row.names", by.y = "row.names"))
ggplot(res, aes(NMDS2, NMDS1, shape=as.factor(Treatment))) + geom_point(aes(size=5,colour=Time)) + theme_bw(20)+ xlab("Dimension 1") + ylab("Dimension 2") + scale_shape(name="Pulse or Continuous")

M-4
##### diversity estimates#################
eR<-as.data.frame(t(estimateR(crd5)))  #chao1 and ACE
H <- diversity(crd5,index="shannon")
S <- specnumber(crd5)
J <- H/log(S)  # Pielou's


eR.box<-boxplot(eR$S.chao1~md5$Time*md5$Treatment,ylab="Chao1",tck=0.03,las=2,cex.axis=0.9,cex.lab=0.9, col=c("gray80","white"))#test if the richness in dark is significantly higher than light
par(bty="l",tck=0.03,mar=c(9,4.1,4.1,2.1))

boxplot(J~md5$Treatment*md5$Time,ylab="Pielou's",las=2,cex.axis=0.8,cex.lab=0.8)
dev.off()

boxplot(H~md5$Treatment*md5$Time,ylab="diversity",las=2,cex.axis=0.8)


##testing diversity significance

aol1<-aov(eR$S.chao1~md5$Treatment*md5$Time) 
summary(aol1) # Only time significant

write.table(eR, ("C:/Users/martin/Desktop/PhD Hörnan/R/Monica Scripts/Results 2nd sequencing +old/eR.txt"), sep="\t")
write.table(md5, ("C:/Users/martin/Desktop/PhD Hörnan/R/Monica Scripts/Results 2nd sequencing +old/md5.txt"), sep="\t")

posthoc1<- TukeyHSD(aol1)

aol2<-aovp(J~md5$Treatment*md5$Time)  # nothing significant
summary(aol2)

aol3<-aovp(H~md5$Treatment*md5$Time)  # nothing significant
summary(aol3)

aol4<-aov(eR$S.chao1~md5$Treatment) 
summary(aol4) # Testing only end samples with d2_end! A one way Anova

aol5<-aov(eR$S.chao1~md5$Time) 
summary(aol5)

M-5
##testing residuals
fit <- lm(eR$S.chao1~md5$Treatment*md5$Time,data=eR)
hist(residuals(fit))
shapiro.test(residuals(fit))####The test of normality of the residuals was met

## Bartlett Test of Homogeneity of Variances
bartlett.test(eR$S.chao1~md5$Time, data = eR) 
leveneTest(changeCH4 ~ lake, data = anova)#homogeniety of variance met

M-6
#######################################################
###INDVAL##############################

#ind1<-(read.csv(file="otu_table_indval_treat_and_time.csv", row.names=1, sep=";"))###Un-rarefied otu table with 4 groups (1: Con start, 2:pulse_start, 3:Cont-end, 4:pulse:end) 
#ind1<-(read.csv(file="otu_table_indval_time.csv", row.names=1, sep=";"))###Un-rarefied otu table with only 2 groups, reprersenting time (start and end)
ind1<-(read.csv(file="otu_table_indval_treatment.csv", row.names=1, sep=";"))###Un-rarefied otu table with only 2 groups, reprersenting Treatment (cont. and pulse)
g<-ncol(ind1)
sprow<-ind1[,1]
ind<-ind1[,2:g]
ind<-rrarefy(ind,min(rowSums(ind)))
ind2<-cbind(sprow,ind)


ind3<-ind2[ ,colSums(ind2)!=0]
iva <- indval(ind3[,-1], ind3[,1],permutations=how(nperm=999))
?indval
gr <- iva$maxcls[iva$pval<=0.01]
iv <- iva$indcls[iva$pval<=0.01]
pv <- iva$pval[iva$pval<=0.01]
fr <- apply(ind3[,-1]>0, 2, sum)[iva$pval<=0.01]

class(iva)
dim()

indvalsummary <- data.frame(group=gr, indval=iv, pvalue=pv, freq=fr)
indvalsummary <- indvalsummary[order(indvalsummary$group, -indvalsummary$indval),]
indvalsummary

prob.corrected = p.adjust(indvalsummary$pval, "holm")

##re-do with other groups


M-7
######################################edgeR analysis
Group <- factor(paste(d2$Treatment,d2$Time,sep=";"))
Group<-Group[13:24]#ONLY END SAMPLES
group2=as.factor(sapply(strsplit(as.vector(Group),";"), head,1))
d<- DGEList(counts = data_subset, group=group2)
d <- calcNormFactors(d)
d <- estimateCommonDisp(d,verbose=T) ####This gives Biological coefficient of dispersal which is the coefficent of variation with which the (unknown) true abundance of the genes varies between replicate samples
###representing the CV that would remain between biological replicates if seq depth was infinite
d <- estimateTagwiseDisp(d)
de.tgw <- exactTest(d,pair=c("c","p"))
summary(decideTestsDGE(de.tgw, p.value=0.01))
topTags(de.tgw)

###edgeR defining each treatment combination as a group
##in Dark treat
data_subset <- t(r.dark[,colSums(r.dark)>10])
Group <- factor(paste(z2$Treatment,z2$Aminoacid))
group2=as.factor(sapply(strsplit(as.vector(Group),";"), head,1))
d<- DGEList(counts = data_subset, group=group2)
d <- calcNormFactors(d)
d <- estimateCommonDisp(d,verbose=T) 
d <- estimateTagwiseDisp(d)
cbind(z2,Group=Group)
design<-model.matrix(~0+Group)
colnames(design)<-levels(Group)
fit<-glmFit(d,design)
head(fit$design)
colnames(fit)

lrt<-glmLRT(fit,contrast=c(-1,1,0,0,0,0,0,0,0,0,0)) #finds OTUS between asp acid and arginine
topTags(lrt)

M-7.1
####FINDING THE RELATIVE ABUNDANCE OF THE EDGE OTUs
taxonredux<-read.csv(file="tax_all_end_test1.csv", row.names=1, sep=";")
taxonredux_end<-cbind(taxonredux[,1],taxonredux[,8:20])

pulse1<-colSums(taxonredux[taxonredux$phylum=="pulse1",2:13])/colSums(taxonredux[,2:13])#relative abund of OTU 268 that was found to be representative in the pulse treatment at the END, however the order of pulse anc continous in the otu tbale is mixed! so cant trust that its the first 6 that are cont and last 6 that are pulse! look at the d2$Treatment table
pulse2<-colSums(taxonredux[taxonredux$phylum=="pulse2",2:13])/colSums(taxonredux[,2:13])#rel abun OTU1
pulse3<-colSums(taxonredux[taxonredux$phylum=="pulse3",2:13])/colSums(taxonredux[,2:13])#rel abun OTU 243
cont1<-colSums(taxonredux[taxonredux$phylum=="cont1",2:13])/colSums(taxonredux[,2:13])#rel abun OTU 361
cont2<-colSums(taxonredux[taxonredux$phylum=="cont2",2:13])/colSums(taxonredux[,2:13])#rel abun OTU 3
cont3<-colSums(taxonredux[taxonredux$phylum=="cont3",2:13])/colSums(taxonredux[,2:13])#rel abun OTU 23
pulse4<-colSums(taxonredux[taxonredux$phylum=="pulse4",2:13])/colSums(taxonredux[,2:13])#rel abun OTU 18
cont4<-colSums(taxonredux[taxonredux$phylum=="cont4",2:13])/colSums(taxonredux[,2:13])#rel abun OTU 19




M-8
########### stacked bargraph#############
dat.m<-melt(crd4,id.vars="")#The crd4 depends on if i want to look at start or end samples
str(dat.m)
h1<-t(crd4)
dim(h1)
dim(dat.m)
dim(crd4)
head(dat.m)
head(d1$V2)
d1$V2[1]

head(d1$V2)[1]
as.vector(head(d1$V2)[1])
strsplit(as.vector(d1$V2), ";")
head(assignments)
sapply(strsplit(as.vector(d1$V2), ";"), length)
assignments = strsplit(as.vector(d1$V2), ";")
assignments[sapply(assignments, length) >2]
assignments[sapply(assignments, length) <3 ] = "Unassigned"
?ddply
?tapply
assignments[sapply(assignments, length) >2] = sapply(assignments[sapply(assignments, length) >2], function(x) x[3])##it was >2
##Not sure about this one, let see if we can find other solution. 
assignments

unlist(assignments)
?unlist
assignments = unlist(assignments)

head(crd4)
d1$Row.namesz
names(assignments) = d1$Row.names
head(dat.m)
head(dat.m$Var2)
assignments[as.vector(dat.m$Var2)]
str(dat.m)
assignments
dat.m$assign = assignments[as.vector(dat.m$Var2)]
str(dat.m)
dat.m$assign
library(plyr)
dat.m$Var1
dat.m$assign
str(dat.m)
head(dat.m)
ddply(dat.m, c("Var1", "assign"), summarize, counts = sum(value))
data.pergropup=ddply(dat.m, c("Var1", "assign"), summarize, counts = sum(value))
write.table(data.pergropup,"D:/PhD Hörnan/R/Monica Scripts/Results 2nd sequencing +old/data.pergropup1.txt", sep="\t")
#ggplot(data.pergropup,aes(x=Var1,y=value,fill=counts))+geom_bar(stat="identity")
#head(data.pergropup)
ggplot(data.pergropup,aes(x=Var1,y=counts,fill=assign))+geom_bar(stat="identity")
sapply(assignments[sapply(assignments, length) >2], function(x) x[3])

#####################ONLY END TIME SEPARATELY################
d2_end<-d2[13:24,]###Using the otu table assuming inoculum was switched with two start samples otherwise it would be (11:22)
n<-nrow(otu)
m<-nrow(d2_end)

d4<-d2_end[,2:n] # because we saw an effect of treatment but also a strong effect of time, to analyze treament with influence of time we look at only the end difference between treat.


rd4<-rrarefy(d4,min(rowSums(d4))) # rarefy to smallest integer
crd4 = rd4[ ,colSums(rd4)!=0]   # remove OTUs with 0 instances
ncol(crd4)#Result:112
nmds<-metaMDS(crd4,distance="bray", trymax=200)
#nmds<-metaMDS(crd5,distance="bray", trymax=200)
#nmds2.fit<-envfit(nmds2, e2,perm=1000,na.rm=TRUE)
nmds$stress

n99<-scores(nmds)

res<-as.data.frame(merge(n99,d2, by.x = "row.names", by.y = "row.names"))
ggplot(res, aes(NMDS2, NMDS1, shape=as.factor(Treatment))) + geom_point(aes(size=6,colour=Treatment)) + theme_bw(20)+ xlab("Dimension 1") + ylab("Dimension 2") + scale_shape(name="Pulse & Continuous")

adonis(crd4 ~ Treatment, data=d2_end, permutations=1000, method="bray")

dim(crd4)
d2_end

####ONLY START TIME SEPARATELY###
d2_start<-d2[1:12,]###Using the otu table assuming inoculum was switched with two start samples otherwise it would be (11:22)
d4<-d2_start[,2:n] # because we saw an effect of treatment but also a strong effect of time, to analyze treament with influence of time we look at only the end difference between treat.


rd4<-rrarefy(d4,min(rowSums(d4))) # rarefy to smallest integer
crd4 = rd4[ ,colSums(rd4)!=0]   # remove OTUs with 0 instances
ncol(crd4)#Result:217


#########SIMPER##########
#Group
Group2 <- factor(paste(d2_end$Treatment,sep=";"))
Group2
#simper1<-simper(crd4,Group)
#head(simper1,ordered=T)
simper2<-simper(crd4,Group2)
head(simper2,ordered=T)

###INDVAL with only END samples ##############################

ind1<-(read.csv(file="otu_table_indval_end.csv", row.names=1, sep=";"))###Un-rarefied otu table with end samples to compare treatments

g<-ncol(ind1)
sprow<-ind1[,1]
ind<-ind1[,2:g]
ind<-rrarefy(ind,min(rowSums(ind)))
ind2<-cbind(sprow,ind)


ind3<-ind2[ ,colSums(ind2)!=0]
iva <- indval(ind3[,-1], ind3[,1],permutations=how(nperm=999))

gr <- iva$maxcls[iva$pval<=0.01]
iv <- iva$indcls[iva$pval<=0.01]
pv <- iva$pval[iva$pval<=0.01]
fr <- apply(ind3[,-1]>0, 2, sum)[iva$pval<=0.01]
indvalsummary <- data.frame(group=gr, indval=iv, pvalue=pv, freq=fr)
indvalsummary <- indvalsummary[order(indvalsummary$group, -indvalsummary$indval),]
indvalsummary

prob.corrected = p.adjust(indvalsummary$pval, "hochberg")
prob.corrected

M-9
##############niche width###########################
d2_end<-d2[13:24,]#all samples from end of incubation (with inoc switch otu tbale its 13:24 with other otu table its 11:22)
d2_start<-d2[1:12,]#with otu table with 2 missing samples at start it 1:10, with inoc switch tbale its 1:12
d2_cont<-rbind(d2[1,],d2[5:9,],d2[13,],d2[17:21,])#Only continous samples
d2_pulse<-rbind(d2[2:4,],d2[10:12,],d2[14:16,],d2[22:24,])#Only pulse samples

d4<-d2_end[,2:n] # because we saw an effect of treatment but also a strong effect of time, to analyze treament with influence of time we look at only the end difference between treat.
d4<-d2_start[,2:n]
d4<-d2_cont[,2:n]
d4<-d2_pulse[,2:n]

rd4<-rrarefy(d4,min(rowSums(d4))) # rarefy to smallest integer
crd4 = rd4[ ,colSums(rd4)!=0]   # remove OTUs with 0 instances
crd4.rel.abun<-prop.table(crd4,margin=1)#calculates the relative abundance

breadth<-niche.width(crd4.rel.abun,method="levins")
breadth<-as.vector(breadth)
mean.rel.abun<-colMeans(crd4.rel.abun)
plot(log10(mean.rel.abun),breadth,pch=16,col="dimgrey")


M-10
#detecting outliers
crd4.rel.abun<-prop.table(crd4,margin=1)
rownames(crd4.rel.abun)<-rownames(crd4)
breadth<-niche.width(crd4.rel.abun,method="levins")
names(breadth)<-colnames(crd4.rel.abun)
mean.rel.abun<-colMeans(crd4.rel.abun)
plot(log10(mean.rel.abun),breadth,pch=16,col="dimgrey",bty="l")

breadth1<-breadth[log10(mean.rel.abun)>-5]#defining the cutoff of rare below -3.5
cut.abun<-log10(mean.rel.abun)
cut.abun1<-cut.abun[cut.abun>-5]
#good_otus = c("OTU.1","OTU.243","OTU.268","OTU.148","OTU.361","OTU.296")
sizes = rep(0.7, length(cut.abun1))
#sizes[names(cut.abun1) %in% good_otus] = 3
plot(cut.abun1,breadth1,pch=16,bty="o",col=ifelse(breadth1>6,"red3",ifelse(breadth1<2,"royalblue4","dimgrey")),xlab="mean relative abundance (log)",ylab="niche breadth", cex=1,tck=-0.02, cex.axis=0.9,cex.lab=0.9)
#text(cut.abun1,breadth1,labels=row.names(t(breadth1)),cex=sizes,offset=10)

#boxplot
boxplot(t(breadth1),main=("niche breadth"))


M-11
##########testing cell abundance difference between treatments WITH REP-MEAS

abund<-(read.csv(file="abund_maov.csv", row.names=1, sep=";",header=T))

## Convert variables to factor
abun1 <- within(abund, {
  treat <- factor(treat)
  time <- factor(time)
  id <- factor(sample)
})

par(cex = .6)

with(abun1, interaction.plot(time, treat, abund,
                             
                             ylab = "mean of abund", xlab = "time", trace.label = "treat"))

abun2 <- aov(abund ~ treat * time + Error(id), data = abun1)
summary(abun2)

posthoc <- TukeyHSD(x=demo2, 'treat', conf.level=0.95)#Testing when the abundnace becomes significantly different between treatments

posthoc 

fit <- lm(abund~treat*time,data=abun1)
hist(residuals(fit))
shapiro.test(residuals(fit))#testing residuals?
?shapiro.test
list.files()
norm<-read.table("Normal.txt", header=TRUE, sep="\t")
class(norm$Normaldata)
class(norm$Treatment)
norm$Treatment
normA<-aov(norm$Normaldata~norm$Treatment)
hist(normAR)
normAR<- residuals(normA)
shapiro.test(normAR)
qqnorm(normAR)
require(MASS) ## for oats data set
require(nlme) ## for lme()
require(multcomp) ## for multiple comparison stuff


#testing residuals
Lme.mod <- lme(abund ~ time * treat, random = ~1 | id, data = abun1)
the_residuals <- residuals(Lme.mod)
anova(Lme.mod)
summary(Lme.mod)
oats <- residuals(Lme.mod)
qqnorm(oats, main="Normal Q-Q") # #not a normal distribution because the dots deviate from the line, so we apply a permutational test!A quantile normal plot - good for checking normality
qqline(oats)

abun2 <- aov(abund ~ treat * time + Error(id), data = abun1)###it will still be as signifcant pvalue<0.001
summary(abun2)


###ANOTHER WAY OF DOING 2-WAY ANOVA with REP MEAS
#DV ~ IV1 * IV2 + Error(subject/(IV1*IV2)) THIS IS A Two factor design with repeated measures on both factors
abund<-(read.csv(file="abund_maov.csv", row.names=1, sep=";",header=T))

abun2 <- aov(abund ~ treat*time + Error(sample/treat*time),data=abund)##didnt work!
summary(abun2)###THIS IS THE ANOVA I USED IN THE PAPER (Treat:Time) were significant, so there was a difference over time

M-12
##########testing abundance SIZE difference between treatments with FSC median WITH REP-MEAS

FSC<-(read.csv(file="FSC_maov2.csv", row.names=1, sep=";",header=T))#I included time in the analysis but what i want is just to compare the end samples between cont and pulse
FSC<-FSC[25:48,]

write.table(FSC,("C:/Users/martin/Desktop/PhD Hörnan/R/Monica Scripts/FSC.txt"), sep="\t")

aov1<-aov(ymedian ~ treat+sample, data = FSC,strata=sample)
summary(aov1)     

Convert variables to factor
demo1 <- within(FSC, {
group <- factor(treat)
time <- factor(time)
id <- factor(sample)
})

par(cex = .6)

with(demo1, interaction.plot(time, group, ymedian,

ylab = "mean of ymedian", xlab = "time", trace.label = "group"))

demo1.aov <- aov(ymedian ~ group  + Error(id), data = demo1)#This way i get the within subjects analysis that takes into consideration that we used the same sample twice
summary(demo1.aov)#The F value is very low, i dont know what that means...

####ANOTHER WAY OF TESTING REP MEAS ANOVA for cell size!
FSC<-(read.csv(file="FSC_maov2.csv", row.names=1, sep=";",header=T))#I included time in the analysis but what i want is just to compare the end samples between cont and pulse
FSC<-FSC[25:48,]

aov.out = aov(ymedian ~ treat + Error(sample/treat), data=FSC) #THIS WAS THE ANOVA USED IN THE PAPER
summary(aov.out)


require(MASS) ## for oats data set
require(nlme) ## for lme()
require(multcomp) ## for multiple comparison stuff


#testing residuals
Lme.mod <- lme(ymedian ~ treat, random = ~1 | sample/treat, data = FSC)
the_residuals <- residuals(Lme.mod)
anova(Lme.mod)
summary(Lme.mod)
oats <- residuals(Lme.mod)
qqnorm(oats, main="Normal Q-Q") # #not a normal distribution because the dots deviate from the line, so we apply a permutational test!A quantile normal plot - good for checking normality
qqline(oats)##close to normal, otherwise can run a permutational anova but still is not significant


M-13
###MOST common PHYLA
###############ONLY START SAMPLES
d2_start<-d2[1:12,]
n<-nrow(otu)
m<-nrow(d2_start)
d4<-d2_start[,2:n] # because we saw an effect of treatment but also a strong effect of time, to analyze treament with influence of time we look at only the end difference between treat.


rd4<-rrarefy(d4,min(rowSums(d4))) # rarefy to smallest integer
crd4_start = rd4[ ,colSums(rd4)!=0]   # remove OTUs with 0 instances
ncol(crd4_start)


#START
taxonredux<-merge(t(crd4_start),tax, by.x = "row.names", by.y = "row.names")#used all start samples here 
#write.csv(taxonredux,"tax_all__start.csv",row.names=T)#took out to add the phylogeny names to taxa
taxonredux<-read.csv(file="tax_all_start.csv", row.names=1, sep=";")

#END
taxonredux<-merge(t(crd4_end),tax, by.x = "row.names", by.y = "row.names")#used all end samples here 
#write.csv(taxonredux,"tax_all_end.csv",row.names=T)#took out to add the phylogeny
taxonredux<-read.csv(file="tax_all_end.csv", row.names=1, sep=";")


#Phylum subsets either START or END
cyano<-colSums(taxonredux[taxonredux$phylum=="Cyanobacteria",2:13])/colSums(taxonredux[,2:13])
proteo<-colSums(taxonredux[taxonredux$phylum=="Proteobacteria",2:13])/colSums(taxonredux[,2:13])#MOST COMMON PHYLA for both start and end samples
bact<-colSums(taxonredux[taxonredux$phylum=="Bacteroidetes",2:13])/colSums(taxonredux[,2:13])
actino<-colSums(taxonredux[taxonredux$phylum=="Actinobacteria",2:13])/colSums(taxonredux[,2:13])

#Phylum subsets EITHER START CONT/PULSE or END CONT/PULSE
proteo<-colSums(taxonredux[taxonredux$phylum=="Proteobacteria",2:7])/colSums(taxonredux[,2:7])#MOST COMMON PHYLA for both start and end samples
proteo<-colSums(taxonredux[taxonredux$phylum=="Proteobacteria",8:13])/colSums(taxonredux[,8:13])
bact<-colSums(taxonredux[taxonredux$phylum=="Bacteroidetes",2:13])/colSums(taxonredux[,2:13])
actino<-colSums(taxonredux[taxonredux$phylum=="Actinobacteria",2:13])/colSums(taxonredux[,2:13])





The R script Jungle! 
  
#######A dumb script#######  
dum<-read.table("Dum.txt", sep="\t")

print(dum[4,1])
  
  
  ##############################testing slopes of kinetic measures between treat and time
  
  #abund<-(read.csv(file="kinetic_slopes.csv", row.names=1, sep=";",header=T))
  #abund<-abund[,1:3]
  
  ## Convert variables to factor
  demo1 <- within(abund, {
    group <- factor(treat)
    time <- factor(time)
    id <- factor(slope)
  })


demo1.aov <- aov(slope ~ treat * time + Error(id), data = demo1)
summary(demo1.aov)


abund1<-abund[1:12,]
abund1

mod1<-aov(slope~treat,data=abund1)
summary(mod1)#not significnat!

abund2<-abund[13:24,]
abund2
mod1<-aov(slope~treat,data=abund2)
summary(mod1)

##########testing abund of days of kinetic measures between treat and time

#abund<-(read.csv(file="kinetic_abund.csv", row.names=1, sep=";",header=T))
#abund<-abund[,1:3]

## Convert variables to factor
demo1 <- within(abund, {
  group <- factor(treat)
  time <- factor(time)
  id <- factor(abund)
})


demo1.aov <- aov(abund ~ treat * time + Error(id), data = demo1)
summary(demo1.aov)

##another way of testing residuals
fit <- lm(abund~time,data=demo1)
hist(residuals(fit))
shapiro.test(residuals(fit))####The test of nomrlaity of the residuals was met

## Bartlett Test of Homogeneity of Variances
bartlett.test(abund ~ time, data = demo1) 
leveneTest(abund ~ time, data = demo1)#homogeniety of varaince met



###############ONLY START SAMPLES
d2_start<-d2[1:12,]
d4<-d2_start[,2:n] # because we saw an effect of treatment but also a strong effect of time, to analyze treament with influence of time we look at only the end difference between treat.


rd4<-rrarefy(d4,min(rowSums(d4))) # rarefy to smallest integer
crd4 = rd4[ ,colSums(rd4)!=0]   # remove OTUs with 0 instances

nmds<-metaMDS(crd4,distance="bray", trymax=200)
#nmds<-metaMDS(crd5,distance="bray", trymax=200)
#nmds2.fit<-envfit(nmds2, e2,perm=1000,na.rm=TRUE)
nmds$stress

n99<-scores(nmds)

res<-as.data.frame(merge(n99,d2, by.x = "row.names", by.y = "row.names"))
ggplot(res, aes(NMDS2, NMDS1, shape=as.factor(Treatment))) + geom_point(aes(size=6,colour=Treatment)) + theme_bw(20)+ xlab("Dimension 1") + ylab("Dimension 2") + scale_shape(name="Pulse & Continuous")

####Indicspeceis analysis ###ONLY USING END SAMPLES FOR THIS ANALYSIS
data_subset <- t(crd4[,colSums(crd4)>10])
dim(data_subset)
Group <- factor(paste(d2$Treatment,d2$Time,sep=";"))
indval = multipatt(data_subset, Group,control = how(nperm=1)) ####Alex script using crd4

data_subset <- t(rd4[,colSums(rd4)>10])
dim(data_subset)
Group <- factor(paste(d2$Treatment,d2$Time,sep=";"))

###VENNDIAGRAM###

###Diference Between turnover times
Tt<-(read.csv(file="turnover_time_raw.csv", row.names=1, sep=";"))
aol1<-aov(Tt_raw~treat,data=Tt)  # nothing significant
summary(aol1)


###### testing difference between Treatments and Time
adonis(crd4 ~ factor(Treatment)*factor(Time), data=d2, permutations=1000, method="bray")#adonis is analogous to MANOVA
dim(crd4)
length(d2$Treatment)
d2$Time
adonis(crd4 ~ Time*abund, data=d2, permutations=1000, method="bray")#EVA had concerns that we cant have a two-way anova with both start and end samples! So we used only end with d2_end otu table
adonis(crd4 ~ Time, data=d2, permutations=1000, method="bray")
adonis(crd4 ~ Treatment, data=d2, permutations=1000, method="bray")

write.table (crd4,("C:/Users/martin/Desktop/Test/crd4.txt"), sep="\t")

#adonis(crd4 ~ Treatment * Time + vessel, data = d2,strata=d2$vessel)#eva had concnerns about 2-way anova with start and end samples, this was Alex suggestion to fix it
