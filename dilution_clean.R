#Dilution to extinction experiment cleaner#
setwd("C:/PhD Hörnan/Experiment/ThePositiveExperiment/R")

library(vegan)
library(ggplot2)
library(gplots)
library(reshape2)
library(MASS)
library(moments)
library(plyr)
library(viridis)
library(indicspecies)
library(labdsv)
library(Hmisc)



scq<-read.table("raw_doc_quality1.1.txt", sep = "\t", header=T)
scq2<-read.table("doc_data_improved.txt", sep = "\t", header = T, row.names = 1)
otu_b<-read.table("otu_bord.txt", header=T, sep = "\t")
meta<-read.table("meta_da.txt", header=T, row.names = 1, sep = "\t")
rd5<-read.table("rarefy_otu.txt", sep ="\t", header=T, row.names = 1)
motu<-read.table("new_motu.txt", sep = "\t", header=T, row.names = 1)
motu3<-read.table("diversity_function1.1.txt", header = T, sep = "\t")

head(otu_b)


my.color<-colorRampPalette(c('Red','Blue'))(6)
my.color3<-colorRampPalette(c('Grey','Black'))(4)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbbPalettew1 <- c("#EEEEEE","#BBBBBB", "#000000" )
cbbPalettew2 <- c("#CCCCCC", "#999999", "#666666", "#333333", "#000000")
my.color2<-c("#CCCCCC", "#666666","#000000")

themeset<-theme_bw() + theme(plot.title=element_text(size=16),
                             panel.grid.major = element_blank(), 
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                             legend.text=element_text(size=13.5), legend.title=element_text(size=14.5), 
                             axis.title=element_text(size=14), legend.key = element_blank())

themeset2<-theme_bw() + theme(plot.title=element_text(size=17),
                              panel.grid.major = element_blank(),
                              panel.border = element_blank(),
                              legend.text=element_text(size=17), legend.title=element_text(size=17), 
                              axis.title=element_text(size=17), legend.key = element_blank())


###BCC PERMANOVA###
#Care with misleading column names.
rd5<-as.data.frame(t(rd5))
row.names(rd5)<-gsub("X", "S", row.names(rd5))
# row.names(rd5)=gsub("X","", row.names(rd5))
row.names(meta)

nmeta<-merge(rd5, meta, by.x = "row.names", by.y = "row.names")
nmeta<-as.data.frame(nmeta)

nmeta$inoculum<-nmeta$div
nmeta$diversity<-nmeta$bact
id=grep("D_1", nmeta$diversity, invert=T)
nmeta1=nmeta[id,]

bac_no1=vegdist(nmeta1[,2:2692], method = "bray", permutation =999)

#Used in manuscript for correalation of BCC and treatment 
adon_module1<-adonis(bac~nmeta$inoculum*nmeta$medium*nmeta$diversity, permutations = 1007)
adon_no1_modle=adonis(bac_no1~nmeta1$inoculum*nmeta1$medium*nmeta1$diversity, permutations = 1007)
#test of bcc against treatments without dilution 1 

# Test of bcc and factors without dilution 1. Not important. 
# nmeta=as.data.frame(t(nmeta))
# rd5=as.data.frame(t(rd5))
# 
# id1=grep("D_1", nmeta$diversity, invert =T)
# d=nmeta[id1,]
# d2=d[,1:2691]
# 
# 
# d3<-as.data.frame(apply(d2[1:217,], 2, function(x) as.numeric(as.character(x))))
# bac_ny<-vegdist(d3, method ="bray")
# bac_no1=adonis(bac_ny~d$medium*d$inoculum*d$diversity, permutations = 1007, method = "bray")
# d[,2692:2698]


###Mantel tests###
rd5<-as.data.frame(t(rd5))
row.names(meta)
row.names(rd5)

# a=gsub("X", "S",row.names(rd5))
# 
# row.names(rd5)=a
row.names(scq2)

nmeta<-merge(rd5, meta, by.x = "row.names", by.y = "row.names")
nmeta<-as.data.frame(nmeta)
row.names(rd5)=gsub("S","", row.names(rd5))

temp<-merge(rd5, scq2, by.x = "row.names", by.y = "row.names")
doc<-temp[,2697:2704]
row.names(doc)=temp$Row.names


sort(as.numeric(row.names(doc)))
sort(as.numeric(row.names(p_comp)))
b=p_comp$Sample_number
row.names(p_comp)=b
p_comp=p_comp[,-4]

d=merge(doc, p_comp, by.x = "row.names", by.y = "row.names")
comp_dstmx=d[,13:16]

bac2=temp2[,c(16:length(temp[1,]))] ##OTUs
bac<-vegdist(rd5, method ="bray")

dist_mx_comp2<-vegdist(doc, method="euclidean")#DOC parameters distmx
dist_mx_realcmp<-vegdist(comp_dstmx, method="euclidean")#DOC the actual compo


dist_mx_bacd<-vegdist(bac1, method="bray")#OTU parameters matching DOC
dist_env_f<-vegdist(env2, method="euclidean")#function parameters
dist_mx_bacf<-vegdist(bac2, method="bray")#OTU parameters matching function

envdoc_model<-mantel(dist_mx_bacd, dist_mx_comp2, method = "pearson", permutations = 1007)
envdoc_remodel<-mantel(dist_mx_bacd, dist_mx_realcmp, method = "pearson", permutations = 1007)
#used in manuscript
envf_model<-mantel(dist_env_f, dist_mx_bacf, method = "pearson", permutations = 1007)
#used in manuscript

#mantel tests of function correlations to individual medium

M1=(temp[temp$Medium == "M1",])
M2=(temp[temp$Medium == "M2",])
M3=(temp[temp$Medium == "M3",])
M4=(temp[temp$Medium == "M4",])       
Mix=(temp[temp$Medium == "Mix",])
no1=(temp[!temp$Dilution == "1",])

bacM1<-M1[,2:2692]
bacM2<-M2[,2:2692]
bacM3<-M3[,2:2692]
bacM4<-M4[,2:2692]
bacMix<-Mix[,2:2692]
bacno1<-no1[,2:2692]
row.names(bacM3)

bac1<-temp[,2:2692]

row.names(motu3)=motu3$Row.names
row.names(rd5)
row.names(motu3)

temp2=merge(motu3, rd5, by.x = "row.names", by.y = "row.names")
env2=temp2[,c(4,14,15)] ##functions
env3=temp2[,c(1,4,5,14,15)] 
row.names(env3)=env3$Row.names
env3=env3[,-1]

m1x=merge(env3, M1, by.x = "row.names", by.y = "row.names")
m2x=merge(env3, M2, by.x = "row.names", by.y = "row.names")
m3x=merge(env3, M3, by.x = "row.names", by.y = "row.names")
m4x=merge(env3, M3, by.x = "row.names", by.y = "row.names")
mixx=merge(env3, Mix, by.x = "row.names", by.y = "row.names")
no1d=merge(env3, no1, by.x = "row.names", by.y = "row.names")

head(m1x)
m1xfn=m1x[,c(3, 5:6)]
m2xfn=m2x[,c(3, 5:6)]
m3xfn=m3x[,c(3, 5:6)]
m4xfn=m4x[,c(3, 5:6)]
mixxfn=mixx[,c(3, 5:6)]
no1fn=no1d[,c(3, 5:6)]

m1xotu=m1x[,c(8:2697)]
m2xotu=m2x[,c(8:2697)]
m3xotu=m3x[,c(8:2697)]
m4xotu=m4x[,c(8:2697)]
mixxotu=mixx[,c(9:2697)]
no1otu=no1d[,c(8:2697)]

m1bac=vegdist(m1xotu, method = "bray")
m2bac=vegdist(m2xotu, method = "bray")
m3bac=vegdist(m3xotu, method = "bray")
m4bac=vegdist(m4xotu, method = "bray")
mixbac=vegdist(mixxotu, method = "bray")
no1bac=vegdist(no1otu, method = "bray")

m1fn=vegdist(m1xfn, method = "euclidean")
m2fn=vegdist(m2xfn, method = "euclidean")
m3fn=vegdist(m3xfn, method = "euclidean")
m4fn=vegdist(m4xfn, method = "euclidean")
mixfn=vegdist(mixxfn, method = "euclidean")
no1fun=vegdist(no1fn, method = "euclidean")

modelM1=mantel(m1bac, m1fn, method ="pearson", permutations = 1004)
modelM2=mantel(m2bac, m2fn, method ="pearson", permutations = 1004)
modelM3=mantel(m3bac, m3fn, method ="pearson", permutations = 1004)
modelM4=mantel(m4bac, m4fn, method ="pearson", permutations = 1004)
modelMix=mantel(mixbac, mixfn, method ="pearson", permutations = 1004)
modelno1=mantel(no1bac, no1fun, method ="pearson", permutations = 1004)


###Alpha_diversity###
alpha_div<-read.table("melt_alpha.txt", sep = "\t", header=T, row.names=1)
alpha_c<-read.table("alpha_clean1.1.txt", sep = "\t", header = T, row.names = 1)
mean<-read.table("ggplot_m_error.txt", sep = "\t", header=T)
sd<-read.table("ggplot_sd_error.txt", sep = "\t", header=T)

chkm<-with(alpha_c,tapply(Alpha_div, list(Medium), mean))
chkm<-with(alpha_c,tapply(Alpha_div, list(Medium, Inoculum), mean))
chksm<-with(alpha_c,tapply(Alpha_div, list(Medium), sd))
chki<-with(alpha_c,tapply(Alpha_div, list(Inoculum), mean))
chksi<-with(alpha_c,tapply(Alpha_div, list(Inoculum), sd))
chkd<-with(alpha_c,tapply(Alpha_div, list(Diversity), mean))
chksd<-with(alpha_c,tapply(Alpha_div, list(Diversity), sd))

mean(alpha_c$Alpha_div)


###Alpha_anova
alpha_c$Diversity<-as.factor(alpha_c$Diversity)

alpha_anov<-aov(alpha_c$Alpha_div~alpha_c$Medium*alpha_c$Inoculum*alpha_c$Diversity)
summary(alpha_anov)

TukeyHSD(alpha_anov, "alpha_c$Diversity")
TukeyHSD(alpha_anov, "alpha_c$Medium")
TukeyHSD(alpha_anov, "alpha_c$Inoculum")
mean(alpha_c$Alpha_div)

#Alpha_diversity_graph#
limits2<-aes(ymax=mean+sd,ymin=mean-sd)
alpha_div$Diversity=gsub("Diversity", "", alpha_div$Diversity)
bw3=colorRampPalette(c('grey','white'))(3)
write.table(alpha_div, "dagens_alpha.txt", sep = "\t")

gp2<-ggplot(alpha_div, aes(x=Diversity, y=mean, ymax=mean+sd, ymin=mean-sd, fill=Medium))
gp2<-gp2 + geom_bar(stat="identity", position = "dodge", color = "white")
gp2<-gp2 + facet_grid(~Inoculum)
gp2<-gp2 + themeset2
gp2<-gp2 + theme(axis.title = element_text(size=18), legend.text = element_text(size = 16), axis.text = element_text(size =16, color ="black"), legend.title = element_text(size = 18))              
# gp2<-gp2 + scale_fill_manual(values=bw3)
gp2<-gp2 + scale_fill_viridis(discrete=T, name=NULL)
gp2<-gp2 + labs(y="OTU count", x="Dilution")
gp2<-gp2 + geom_errorbar(position = position_dodge(width=0.9), width=0.55)
gp2<-gp2 + theme(strip.text = element_text(size =16), legend.position = "bottom", legend.title = element_blank())
gp2<-gp2 + theme(axis.line = element_line(), strip.background = element_rect("white"))

###Indicator species###

start=Sys.time()

test<-motu[,4:2693]
id<-colSums(test)!=0
h1<-test[,id]


motu$new_div=as.integer(gsub("[2-6]", "2",motu$Diversity))
length(motu$new_div)

div<-cbind(as.integer(motu$Diversity), h1) 
inoc<-cbind(as.integer(motu$Inoculum), h1) 
med<-cbind(as.integer(motu$Medium), h1)
div2<-cbind(as.integer(motu$new_div), h1)

end=Sys.time()
end-start




motu$Diversity
motu$Inoculum
as.integer(motu$Medium)

as<-as.integer(motu$Inoculum)

nind<-med# change here to set factor. 

nind[is.na(nind)]=0
str(nind)


nind$OTU_000001[!grepl("[1-9]", nind$OTU_000001)]       
data$ID[!grepl("xyx", data$ID)       
       
iva <- indval(nind[,-1], nind[,1],permutations=how(nperm=999))
gr <- iva$maxcls[iva$pval<=0.05]
iv <- iva$indcls[iva$pval<=0.05]
pv <- iva$pval[iva$pval<=0.05]
fr <- apply(nind[,-1136]>0, 2, sum)[iva$pval<=0.05]
id <- iva$pval<=0.05
rfr<- iva$relfrq[id,]

indvalsummary <- data.frame(group=gr, indval=iv, pvalue=pv, freq=fr)
max(indvalsummary$indval)
indvalsummary<-cbind(rfr,indvalsummary)
# indvalsummary<-cbind(rfr,indvalsummary)
indvalsummary <- indvalsummary[indvalsummary$indval>0.5,]

ind_med<-indvalsummary
ind_med$group<-gsub("5", "Mix", ind_med$group)
ind_med$group<-gsub("1", "M1", ind_med$group)
ind_med$group<-gsub("2", "M2", ind_med$group)
ind_med$group<-gsub("3", "M3", ind_med$group)
ind_med$group<-gsub("4", "M4", ind_med$group)

write.table(ind_div, "ind_div.txt", sep="\t")
write.table(ind_ino, "ind_ino.txt", sep="\t")
write.table(ind_med, "ind_med.txt", sep="\t")

###NMDS Plots###
nmds_plot_d<-read.table("clean_nmds_plotdat.txt", sep="\t", header=T, row.names = 1)
nmds_plot_d$Medium<-as.factor(nmds_plot_d$Medium)
colnames(nmds_plot_d)[5]<-"Diversity"
nmds_plot_d$Diversity<-as.factor(nmds_plot_d$Diversity)

class(nmds_plot_d$Diversity)

gm<-ggplot(nmds_plot_d, aes(NMDS1, NMDS2, color=Diversity)) 
gm<-gm + geom_point(aes(colour=Diversity, ), size = 7) 
gm<-gm + labs(x="Dimension 1", y="Dimension 2")
gm<-gm + scale_color_viridis(discrete = TRUE, name=NULL)
gm<-gm + themeset
gm<-gm + theme(axis.text = element_text(size = 28, color = "black"), axis.title = element_text(size=26), legend.title = element_text(size=30), legend.text = element_text(size=30))
gm=gm + theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 1))

# gm<-gm + guides(color = guide_legend(keywidth = 1, keyheight = 1))

im<-ggplot(nmds_plot_d, aes(NMDS1, NMDS2)) 
im<-im + geom_point(aes(colour=Inoculum), size = 7) 
im<-im + labs(x="Dimension 1", y="Dimension 2")
im<-im + scale_color_viridis(discrete = TRUE, name=NULL)
im<-im + themeset
im<-im + theme(axis.text = element_text(size = 28, color = "black"), axis.title = element_text(size=26), legend.title = element_text(size=30), legend.text = element_text(size=30))
im=im + theme(legend.position = "bottom")


mm<-ggplot(nmds_plot_d, aes(NMDS1, NMDS2)) 
mm<-mm + geom_point(aes(colour=Medium), size = 7) 
mm<-mm + labs(x="Dimension 1", y="Dimension 2")
mm<-mm + scale_color_viridis(discrete = TRUE,  name="",labels =c("M1", "M2", "M3", "M4", "Mix"))
mm<-mm + themeset
mm<-mm + theme(axis.text = element_text(size = 28, color = "black"), axis.title = element_text(size=26), legend.title = element_text(size=30), legend.text = element_text(size=30))
mm=mm + theme(legend.position = "bottom")

###TOC stats and plots###
mdata<-read.table("toc_clean.txt", header=T, sep="\t") #for plot
dtc<-read.table("delta_carbon1.7.txt", header=T, sep="\t") #for aov 
colnames(dtc)<-c("TOC_delta","Inoculum", "Medium", "Diversity")
dtc$Diversity<-as.factor(dtc$Diversity)

# id<-grepl("1", dtc$Diversity)
# id2<-!id
# dtc<-dtc[id2,] #remove firts diversity
dtc$Diversity<-droplevels(dtc$Diversity)
levels(dtc$Diversity)<-c("1", "2", "3", "4", "5")

dtm<-tapply(dtc$TOC_delta*-1, list(dtc$Inoculum, dtc$Diversity),mean)
dts<-tapply(dtc$TOC_delta*-1, list(dtc$Inoculum, dtc$Diversity),sd)

exptm<-tapply(dtc$TOC_delta, dtc$Inoculum,mean)
exptmm<-tapply(dtc$TOC_delta, dtc$Medium, mean)
exps<-tapply(dtc$TOC_delta, list(dtc$Inoculum, dtc$Diversity),sd)

dtm1<-tapply(dtc$TOC_delta, list(dtc$Inoculum, dtc$Diversity,dtc$Medium),mean)
dts1<-tapply(dtc$TOC_delta, list(dtc$Inoculum, dtc$Diversity, dtc$Medium),sd)


mdtm<-melt(dtm)
mdts<-melt(dts)
mdtm$sd<-mdts$value

mdtm1<-melt(dtm1)
mdts1<-melt(dts1)
mdtm1$sd<-mdts1$value
mdtm$value=mdtm$value*-1

# mdtm<-read.table("mdtm.txt", sep = "\t", header=T) #made from melted delta_carbon data
bgtoc<-ggplot(mdtm, aes(x=as.factor(mdtm$Var2), y=mdtm$value, fill=mdtm$Var1, ymax=mdtm$value+mdtm$sd, ymin=mdtm$value-mdtm$sd))
bgtoc<-bgtoc + geom_bar(position="dodge", stat="identity")
bgtoc<-bgtoc + labs(y=expression(paste(Delta,"TOC mg L" ^"-1")),x="Dilution")
bgtoc<-bgtoc + scale_fill_manual(values=my.color2, name="Inoculum")
bgtoc<-bgtoc + themeset2 + theme(axis.line = element_line())
bgtoc<-bgtoc + geom_errorbar(position=position_dodge(width = 0.9), width = 0.3, color = "gray")                         
bgtoc<-bgtoc + theme(axis.text = element_text(size=16, color = "black"), legend.text = element_text(size=18), legend.title = element_text(size=18), axis.title = element_text(size=22)) 

mdtm1$value<-mdtm1$value*-1

a=mdtm1[mdtm1$Var1 == "Lake",]
b=mdtm1[!mdtm1$Var1 == "Lake",]
a$value=a$value+0.1
d=rbind(a,b)
sort(d$Var1)

da=ggplot(ot, aes(x=ot$Var2, y=ot$value, fill=ot$Var1,ymax=ot$value+ot$sd, ymin=ot$value-ot$sd))
da=da + geom_bar(stat="identity", position ="dodge")
da=da + facet_wrap(~Var3)
da<-da + themeset2 + theme(axis.line = element_line())
da<-gtoc + geom_errorbar(position=position_dodge(width = 0.9), width = 0.4, color = "black")

ot=d[order(d$Var1),]
count(ot$Var3)
ot$Var2=as.factor(ot$Var2)
ot=read.table("data_weird.txt", sep = "\t", header=T)
ot$Var2=as.factor(ot$Var2)
ot$numb=row.names(ot)
ot=ot[order(as.numeric(ot$numb)),]
t=ot$sd<1
ot=ot[t,]
write.table(ot, "data_weird.txt2", sep="\t")
write.table(alpha_div, "data_weird.txt3", sep="\t")

ot=read.table("data_weird.txt2", sep ="\t")

identical(alpha_div$Inoculum,ot$Var1)
identical(alpha_div$Diversity,ot$Var2)
identical(alpha_div$Medium,ot$Var3)
levels(alpha_div$Diversity)
levels(ot$Var2)=c(" 1", " 2", " 3", " 4", " 5", " 6")

all.equal(alpha_div$Diversity,ot$Var2)

class(ot)
class(alpha_div)
str(ot)
str(alpha_div)

gtoc<-ggplot(ot, aes(x=ot$Var2, y=ot$value, fill=ot$Var3, ymax=ot$value+ot$sd, ymin=ot$value-ot$sd))
gtoc<-gtoc + geom_bar(position="dodge", stat="identity", color ="white")
gtoc<-gtoc + facet_grid(~Var1)
gtoc<-gtoc + themeset2 + theme(axis.line = element_line())
gtoc<-gtoc + geom_errorbar(position=position_dodge(width = 0.9), width = 0.4, color = "black")
gtoc<-gtoc + scale_fill_viridis(name="Inoculum", discrete = T)
gtoc<-gtoc + theme(axis.line = element_line())
gtoc<-gtoc + geom_errorbar(position=position_dodge(width = 0.9), width = 0.4, color = "black")                         
gtoc<-gtoc + theme(axis.text = element_text(size=16, color = "black"), legend.text = element_text(size=16), legend.title = element_text(size=18), axis.title = element_text(size=18)) 
gtoc<-gtoc + theme(legend.position = "bottom", legend.title = element_blank())
gtoc<-gtoc + labs(x="Dilution", y="OTU count")
gtoc<-gtoc + theme(strip.background = element_rect("white"))

mdtm1$value=mdtm1$value*-1
ot=ot[order(row.names(ot)),]

cgtoc<-ggplot(mdtm1, aes(x=as.factor(mdtm1$Var2), y=mdtm1$value, fill=mdtm1$Var1, ymax=mdtm1$value+mdtm1$sd, ymin=mdtm1$value-mdtm1$sd))
cgtoc<-cgtoc + geom_bar(position="dodge", stat="identity", color ="black")
cgtoc<-cgtoc + labs(y=expression(paste(Delta,"TOC mg L" ^"-1")),x="Biodiversity gradient")
cgtoc<-cgtoc + facet_grid(~Var3)
cgtoc<-cgtoc + scale_fill_viridis(name="Inoculum", discrete = T)
# cgtoc<-cgtoc + scale_fill_manual(values=my.color2, name="Inoculum")
cgtoc<-cgtoc + themeset2 + theme(axis.line = element_line())
cgtoc<-cgtoc + geom_errorbar(position=position_dodge(width = 0.9), width = 0.4, color = "black")                         
cgtoc<-cgtoc + theme(axis.text = element_text(size=16, color = "black"), legend.text = element_text(size=18), legend.title = element_text(size=18), axis.title = element_text(size=22)) 

###TOC ANOVA used in draft###
anova_toc<-aov(dtc$TOC_delta~dtc$Inoculum*dtc$Medium*as.factor(dtc$Diversity))
summary(anova_toc)

tukeymed<-TukeyHSD(anova_toc, "dtc$Medium")
tukeydiv<-TukeyHSD(anova_toc, "dtc$Diversity")
tukeyino<-TukeyHSD(anova_toc, "dtc$Inoculum")

id<-grep("1", dtc$Diversity, invert = T)#To remove time point 1
dtc_no1<-dtc[id,]

anova_tocn01<-aov(dtc_no1$TOC_delta~dtc_no1$Inoculum*dtc_no1$Medium*as.factor(dtc_no1$Diversity))
summary(anova_tocn01)

#TOC one way anova showed in graph
anova_t1<-aov(dtc_no1$ delta_toc~dtc_no1$Diversity) # One way ANOVA
summary(anova_t1)

###Heatmaps###
big_data<-read.table("big_data_update2.txt", sep ="\t")
im_otu<-read.table("rarefied_otu_5000.txt", sep="\t", header = T, row.names = 1)

sort(big_data$variable)
count(big_data$variable)
levels(big_data$variable)

# im_otu<-as.data.frame(t(im_otu))#Make sure im_otu is flipped right
d13<-im_otu$Medium #We use this to get the pattern of the medium. 
count(d13)

d42<-rep(d13, each =56) #makes a vector that repeat the position of each medium/sample 56 times to match the 56 phyla 
big_data$medium<-d42

write.table(d42,"media_sequence.txt", sep="\t")

test<-ddply(big_data, c('tax'), summarize, sum(counts))
quantile(test$..1)
low_taxa<-test$..1<80.5 
high_taxa<-test$..1>100 #divide the data into a high and low. Ending up using log instead.

high_seq<-test[high_taxa,]
low_seq<-test[low_taxa,]
h_s<-high_seq$tax
l_s<-low_seq$tax

search.stringl <- paste0(l_s, collapse = "|") #Paste0 identifies what is separating, so you dont need to write it otherwise same as paste
search.stringh <- paste0(h_s, collapse = "|")
idh<-grep(search.stringh, big_data$tax)
idl<-grep(search.stringl, big_data$tax)
high_seq_df<-big_data[idh,]  
low_seq_df<-big_data[idl,]

levels(low_seq_df$Inoculum)<-c("Lake", "Stream", "Wetland")
levels(high_seq_df$Inoculum)<-c("Lake", "Stream", "Wetland")
high_seq_df$logcounts<-log(high_seq_df$counts+1)
low_seq_df$logcounts<-log(low_seq_df$counts+1)
high_seq_df$tax<-gsub("Bacteroidetes", "Bacteroid.", high_seq_df$tax)

tapply(high_seq_df$counts, list(high_seq_df$tax, high_seq_df$medium), sum)
tapply(high_seq_df$counts, list(high_seq_df$tax, high_seq_df$medium, high_seq_df$Inoculum), sum)
tapply(high_seq_df$counts, list(high_seq_df$medium), sum)
tapply(high_seq_df$logcounts, list(high_seq_df$tax, high_seq_df$medium, high_seq_df$Inoculum), sum)

ng<- ggplot(high_seq_df, aes(x = medium, y = tax, fill = logcounts))
ng <- ng + geom_tile(color="white", size=0.1)
ng <- ng + theme(axis.text.x = element_text(size = 14))
ng <- ng + scale_fill_viridis(name = "Seq. log")
ng <- ng + facet_grid(~Inoculum)
ng <- ng + labs(y=NULL, x =NULL,title="High frequency phyla")
ng <- ng + theme(plot.title = element_text(hjust=0.5,size =18), axis.text.x = element_text(size=16, color="black"), axis.text.y = element_text(size = 13, color="black"), legend.text = element_text(size=13), legend.title = element_text(size=13))
ng <- ng + theme(strip.text = element_text(size = 16), strip.background = element_blank())

low_seq_df$tax<-gsub("Verrucomicrobia Incertae Sedis", "Verruc.Inc.Sedis", low_seq_df$tax)
lg<- ggplot(low_seq_df, aes(x = medium, y = tax, fill = logcounts))
lg <- lg + geom_tile(color="white", size=0.1)
lg <- lg + theme(axis.text.x = element_text(size = 14))
lg <- lg + scale_fill_viridis(name = "Seq. count log")
lg <- lg + facet_grid(~Inoculum)
lg <- lg + labs(y=NULL, x = NULL, title="Low frequency phyla")  
lg <- lg + theme(plot.title = element_text(hjust=0.5,size =18), axis.text.x = element_text(size=16, color="black"), axis.text.y = element_text(size = 13, color="black"), legend.text = element_text(size=13), legend.title = element_text(size=14))
lg <- lg + theme(strip.text = element_text(size = 16), strip.background = element_blank())


###Abundance###
abun<-read.table("abundance_data1.1.txt", sep="\t", header=T)

abun26=abun[!abun$Diversity=="1",]
model_abmax26<-aov(abun26$MAX~abun26$Medium*abun26$Inoculum*abun26$Diversity)
summary(model_abmax26)

abun$Diversity<-as.factor(abun$Diversity)
model_abmax<-aov(abun$MAX~abun$Medium*abun$Inoculum*abun$Diversity)
summary(model_abmax)

TukeyHSD(model_abmax, "abun$Diversity")
TukeyHSD(model_abmax, "abun$Inoculum")
TukeyHSD(model_abmax, "abun$Medium")

#PERMANOVA of growth curve data#
curve<-abun[,3:8]
maxab<-abun[,9]
curve1<-na.omit(curve)
colnames(curve1)<-c("T1","T2", "T3","T4","T5","T6")
abun1<-na.omit(abun)

curve1$M1<-as.integer(curve1$M1)
curvdis<-vegdist(curve1, method = "euclidean")
adon_curve<-adonis(curvdis~abun1$Inoculum*abun1$Medium*abun1$Diversity, method = "euclidean", permutations = 999)


###Max abundance ggplot###
max_table<-read.table("max_table.txt", sep="\t", header=T)

mabg<-ggplot(max_table, aes(y=mL, x=Diversity, color=Inoculum))
mabg<-mabg + geom_point(size=2)
mabg<-mabg + geom_smooth(method="loess", se=FALSE)
mabg<-mabg + facet_grid(~Medium)
mabg<-mabg + themeset
mabg<-mabg + scale_color_manual(values=my.color2, labels=c("Wetland", "Stream", "Lake"))
mabg<-mabg + labs(y=expression(paste("Max cells mL" ^-1)), x="Biodiversity gradient")#, title=expression(paste("Cells mL" ^-1, " by diversity"))) 
mabg<-mabg + theme(strip.text = element_text(size=15))
mabg<-mabg + theme(plot.title = element_text(hjust=0.5))


###4D growth curve###
ok<-read.table("Corr_Growth_Curve_data1.1.txt", sep ="\t", header=T)


ok$Diversity<-gsub("Diversity", "Dilution", ok$Diversity)

ok$variable=gsub("6", "10", ok$variable)
ok$variable=gsub("5", "8", ok$variable)
ok$variable=gsub("4", "6", ok$variable)
ok$variable=gsub("3", "4", ok$variable)


ok$variable=as.integer(ok$variable)

count(ok$variable)

ag1<-ggplot(ok, aes(y=mL, x=variable, color=Medium))
# ag1<-ag1 + geom_point(size=2)
ag1<-ag1 + geom_smooth(method="loess", se=FALSE)
ag1<-ag1 + facet_wrap(~Inoculum+Diversity)
ag1<-ag1 + themeset
# ag1<-ag1 + scale_color_manual(values=my.color)
ag1<-ag1 + labs(y=expression(paste("Cells mL" ^-1)),x="Growth days")
ag1<-ag1 + theme(strip.text = element_text(size=14), axis.text = element_text(size = 11, color = "black"))
ag1<-ag1 + theme(strip.background = element_rect(fill = alpha('white', 0.3)))
ag1<-ag1 + theme(legend.position = "bottom", legend.title = element_blank())
ag1<-ag1 + scale_x_continuous(breaks=seq(0, 10, by=2))

###DOC Quality analysis/overview###
scq<-read.table("raw_doc_quality1.1.txt", sep = "\t", header=T)
scqid<-read.table("raw_doc_quality_id.txt", sep = "\t", header=T)
scq$Diversity<-as.factor(scq$Diversity)

a<-grepl("1", scq$Diversity)
b<-!a
scq<-scq[b,]

#PERMANOVA DOC QUALITY/PEAKS#
dist_mx_comp<-vegdist(scq[,c(4, 6:8,10)], method="euclidean")#DOC Peaks
rl_comp_mx=vegdist(p_comp[,5:8], method="euclidean") #parafac components

comp_adon=adonis(rl_comp_mx~p_comp$Inoculum*p_comp$Medium*p_comp$Dilution, permutation = 1007, method = "bray")
#used in manuscript

dist_mx_comp2<-vegdist(scq[,c(4, 6:11)], method="euclidean")#with bix&hix
adon_raw_comp<-adonis(dist_mx_comp~scq$Inoculum*scq$Medium*scq$Diversity, permutations = 1007)
row.names(alpha_c)=alpha_c$Row.names
alfa=data.frame(alfa=alpha_c$Alpha_div)
row.names(alfa)<-row.names(alpha_c)
row.names(alfa)<-gsub("S", "", row.names(alfa))

sort(as.numeric(row.names(alpha_c)))

dim(scq)
dim(rd5)

###plot DOC quality###
p_comp=read.csv("positive_components.csv", header = T, sep =";")

comp_temp<-scq[,c(1:4, 6:8,10)]
row.names(comp_temp)<-row.names(scq)

alpa<-as.data.frame(motu3$Alpha.div)
row.names(alpa)<-row.names(motu3)

temp_df<-merge(alpa, comp_temp,by.x = "row.names", by.y = "row.names")
write.table(temp_df, "pls_component_n.txt", sep ="\t")

melt_comp<-melt(comp_temp, id.vars = c('Inoculum', 'Medium', 'Diversity'))

d<-melt_comp$value<=0
e<-!d
melt_comp<-melt_comp[e,]
levels(melt_comp$variable)=c("Peak C", "Peak A", "Peak T", "Peak B", "Peak M")

p_comp=p_comp[,-4]
p_compmlt=melt(p_comp, id.vars = c('Inoculum', 'Medium', 'Dilution'))


cgg<-ggplot(melt_comp, aes(y=value, x=as.factor(Diversity), color=Medium))
# cgg<-cgg + geom_smooth(se=F)
cgg<-cgg + geom_point(size=3)
cgg<-cgg + facet_grid(~variable~Inoculum)
cgg<-cgg + theme_bw()
cgg<-cgg + scale_color_viridis(discrete = T, name=NULL)
cgg<-cgg + theme(strip.background = element_blank(), strip.text = element_text(size = 14), axis.text = element_text(color="black", size =13), axis.title = element_text(size=14), legend.text = element_text(size=13), legend.title = element_text(size=14)) 
cgg<-cgg + labs(y="R.U.",x="Dilution")
cgg<-cgg + theme(legend.position = "bottom")
# cgg<-cgg + scale_shape_manual(values = c(1,16,5))



a=p_compmlt[p_compmlt$value>0.05,]
levels(a$variable)=c("Component 1", "Component 2", "Component 3", "Component 4")
cg2<-ggplot(a, aes(y=value, x=as.factor(Dilution), color=Medium))
# cgg<-cgg + geom_smooth(se=F)
cg2<-cg2 + geom_point(size=3)
cg2<-cg2 + facet_grid(~variable~Inoculum)
cg2<-cg2 + theme_bw()
cg2<-cg2 + scale_color_viridis(discrete = T, name=NULL)
cg2<-cg2 + theme(strip.background = element_blank(), strip.text = element_text(size = 14), axis.text = element_text(color="black", size =13), axis.title = element_text(size=14), legend.text = element_text(size=13), legend.title = element_text(size=14)) 
cg2<-cg2 + labs(y="R.U.",x="Dilution")
cg2<-cg2 + theme(legend.position = "bottom")

###Yield calculation###
abun<-read.table("abundance_data1.1.txt", sep="\t", header=T)
dtc<-read.table("delta_carbon1.7.txt", sep="\t", header=T)

dtc$TOC_delta<-dtc$TOC_delta*-1

row.names(dtc)
d=merge(dtc, motu3, by.x = "row.names", by.y = "row.names")

tf=rcorr(d$tf,d$Alpha.div, type =c("spearman"))
tf=rcorr(d$TOC_delta,d$Alpha.div, type =c("spearman"))

merg_dt_ab<-merge(dtc, abun, by.x = "row.names", by.y = "row.names")
id<-grepl("Diversity.y|Inoculum.y|MixediuMix|Number", colnames(merg_dt_ab))
id2<-!id
merg_dt_ab<-merg_dt_ab[,id2]
merg_dt_ab$yield<-(merg_dt_ab$TOC_delta/merg_dt_ab$MAX)

write.table(merg_dt_ab, "checking_data.txt", sep = "\t")
merg_dt_ab$Diversity.x<-as.factor(merg_dt_ab$Diversity.x)

anova_yield<-aov(merg_dt_ab$yield~merg_dt_ab$Inoculum.x*merg_dt_ab$Medium*merg_dt_ab$Diversity.x)
summary(anova_yield)

row.names(merg_dt_ab)
row.names(motu3)=motu3$Row.names
sort(row.names(motu3))

yield_alpha=merge(motu3, merg_dt_ab, by.x = "row.names", by.y = "row.names")
yield_corr=rcorr(yield_alpha$Alpha.div, yield_alpha$yield, type = c("spearman"))
cort=rcorr(motu3$TOC,motu3$Alpha.div, type = c("spearman"))


###doc quality tapplies_anovas### 
tapply(scq$Peak_C, list(scq$Inoculum), mean)
tapply(scq$Peak_C, list(scq$Medium), mean)
tapply(scq$Peak_C, list(scq$Diversity), mean)

hist(scq$Peak_C)
model_C<-aov(scq$Peak_C~scq$Inoculum*scq$Medium*scq$Diversity)
summary(model_C)

tapply(scq$Peak_T, list(scq$Inoculum), mean)
tapply(scq$Peak_T, list(scq$Medium), mean)
tapply(scq$Peak_T, list(scq$Diversity), mean)

hist(scq$Peak_T)
model_T<-aov(scq$Peak_T~scq$Inoculum*scq$Medium*scq$Diversity)
summary(model_T)

# tapply(scq_nou$Peak_B2, list(scq_nou$Source), mean)#without outliers
# tapply(scq_nou$Peak_B2, list(scq_nou$OriginC), mean)#without outliers
# tapply(scq_nou$Peak_B2, list(scq_nou$BDlevel), mean)#without outliers

tapply(scq$Peak_B, list(scq$Inoculum), mean)
tapply(scq$Peak_B, list(scq$Medium), mean)
tapply(scq$Peak_B, list(scq$Diversity), mean)

hist(scq$Peak_B)
model_B<-aov(scq$Peak_B~scq$Inoculum*scq$Medium*scq$Diversity)
summary(model_B)

tapply(scq$HIX, list(scq$Inoculum), mean)
tapply(scq$HIX, list(scq$Medium), mean)
tapply(scq$HIX, list(scq$Diversity), mean)

hist(scq$HIX)
model_hix<-aov(scq$HIX~scq$Inoculum*scq$Medium*scq$Diversity)
summary(model_hix)

tapply(scq$BIX, list(scq$Inoculum), mean)
tapply(scq$BIX, list(scq$Medium), mean)
tapply(scq$BIX, list(scq$Diversity), mean)

hist(log(scq$BIX))
model_bix<-aov(log(scq$BIX)~scq$Inoculum*scq$Medium*scq$Diversity)
summary(model_bix)

qqnorm(model_bix$residuals)
qqline(model_bix$residuals)
hist(model_bix$residuals)

tapply(scq$Peak_A, list(scq$Source), mean)
tapply(scq$Peak_A, list(scq$OriginC), mean)
tapply(scq$Peak_A, list(scq$BDlevel), mean)
tapply(ncq$Peak_A, list(ncq$Source), mean) #with control 

hist(scq$Peak_A)
model_A<-aov(scq$Peak_A~scq$Source*scq$OriginC*scq$BDlevel)
summary(model_A)

###Alpha_diversity_function_graph###
motu3<-read.table("diversity_function1.1.txt", header = T, sep = "\t")

m1d<-grep("M1", motu3$Medium)
m2d<-grep("M2", motu3$Medium)
m3d<-grep("M3", motu3$Medium)
m4d<-grep("M4", motu3$Medium)
mixd<-grep("Mix", motu3$Medium)

dfM1<-motu3[m1d,]
dfM2<-motu3[m2d,]
dfM3<-motu3[m3d,]
dfM4<-motu3[m4d,]
dfmix<-motu3[mixd,]

mean(dfmix$Alpha.div)

write.table(dfM1, "dfM1.txt", sep = "\t")
write.table(dfM2, "dfM2.txt", sep = "\t")
write.table(dfM3, "dfM3.txt", sep = "\t")
write.table(dfM4, "dfM4.txt", sep = "\t")
write.table(dfmix, "dfmix.txt", sep = "\t")

motu4=motu3[,c(2:5,13:14)]
out<-melt(motu4, id.vars = c('Inoculum.y', 'Medium', 'Alpha_div'))

idm=grep("Mix",motu3$Medium)
motu_mix=motu3[idm,]

mx=ggplot(motu_mix,aes(y=Max.Abundance, x=motu_mix$Alpha.div, color=Medium))
mx=mx + geom_point(size=4, color ="yellow")
mx=mx + geom_smooth(se=FALSE,color ="yellow")   
mx=mx + theme_dark()
mx=mx + theme(scale_color_viridis())
mx=mx + labs(y=expression(paste("Cells mL" ^-1)),x="OTU Count")
mx=mx + theme(axis.title = element_text(size=30), axis.text = element_text(size=28, color ="black"), legend.title = element_blank(), legend.text = element_text(size=30))
mx=mx + theme(legend.position = "bottom")
mx=mx + theme(panel.border = element_blank())
mx=mx + theme(axis.line = element_line(colour = "black"))
mx=mx + scale_color_viridis(discrete = TRUE)

motu3$Alpha.div
motu3$Yield
motu3$Medium

motu3$tf

yp=ggplot(motu_mix,aes(y=Yield, x=Alpha.div, color=Medium))
yp=yp + geom_point(size=4, color ="yellow")
yp=yp + geom_smooth(se=FALSE, color = "yellow")   
yp=yp + theme_dark()
yp=yp + ylab(expression(paste("Cells mL" ^"-1", "/ ", Delta, "TOC mg L" ^"-1")))
yp=yp + labs(x="OTU Count")
yp=yp + theme(axis.title = element_text(size=30), axis.text = element_text(size=28, color ="black"), legend.title = element_blank(), legend.text = element_text(size=30))
yp=yp + theme(legend.position = "bottom")
yp=yp + scale_y_continuous(labels = function(x) format(x, scientific = TRUE))
yp=yp + theme(panel.border = element_blank())
yp=yp + theme(axis.line = element_line(colour = "black"))
yp=yp + scale_color_viridis(discrete = TRUE)
# yp=yp + facet_grid(~Inoculum.y)

motu_mix$tf=(motu_mix$TOC*-1)

tp=ggplot(motu_mix,aes(y=(tf), x=motu_mix$Alpha.div, color=Medium))
tp=tp + geom_point(size=4, color = "yellow")
tp=tp + geom_smooth(se=FALSE, color = "yellow")   
tp=tp + theme_dark()
tp=tp + ylab(expression(paste(Delta,"TOC mg L" ^"-1")))
tp=tp + labs(x="OTU Count")
tp=tp + theme(axis.title = element_text(size=30), axis.text = element_text(size=28, color ="black"), legend.title = element_blank(), legend.text = element_text(size=30))
tp=tp + theme(legend.position = "bottom")
tp=tp + theme(panel.border = element_blank())
tp=tp + theme(axis.line = element_line(colour = "black"))
tp=tp + scale_color_viridis(discrete = TRUE)

# tp=tp + facet_grid(~Inoculum.y)
id<-grep("1", motu3$Diversity.y, invert = T)
motu_sub<-motu3[id,]

motu3$Diversity.y<-as.factor(motu3$Diversity.y)
motu_sub$Diversity.y=as.factor(motu_sub$Diversity.y)

yield_aov= with(motu3,aov(log(yield)~Inoculum.y*Medium*Diversity.y))
yield_saov=with(motu_sub,aov(log(yield)~Inoculum.y*Medium*Diversity.y))
summary(yield_aov)
summary(yield_saov)


###Functional correlations against alpha diversity###
cort=rcorr(motu3$TOC,motu3$Alpha.div, type = c("spearman"))
cort=rcorr(motu3$TOC,motu3$Alpha.div, type = c("spearman"))
cort=rcorr(motu3$tod,motu3$Alpha.div, type = c("spearman"))

motu3$TOC=motu3$TOC*-1

rm(list=ls())
plot(motu3$TOC~motu3$Alpha.div)

cora=rcorr(motu3$Alpha.div,motu3$MAX, type = c("pearson"))
cory=rcorr(motu3$Alpha.div,motu3$yield, type = c("pearson"))

m1t=rcorr(dfM1$Alpha.div,dfM1$TOC, type = c("spearman"))
m1a=rcorr(dfM1$Alpha.div,dfM1$Max.Abundance, type = c("spearman"))
m1y=rcorr(dfM1$Alpha.div,dfM1$Yield, type = c("spearman"))

plot(dfM1$Max.Abundance~dfM1$Alpha.div)
plot(dfM2$Max.Abundance~dfM2$Alpha.div)
plot(dfM3$Max.Abundance~dfM3$Alpha.div)
plot(dfM4$Max.Abundance~dfM4$Alpha.div)
plot(dfmix$Max.Abundance~dfmix$Alpha.div)

m2t=rcorr(dfM2$Alpha.div,dfM2$TOC, type = c("spearman"))
m2a=rcorr(dfM2$Alpha.div,dfM2$Max.Abundance, type = c("spearman"))
m2y=rcorr(dfM2$Alpha.div,dfM2$Yield, type = c("spearman"))

m3t=rcorr(dfM3$Alpha.div,dfM3$TOC, type = c("spearman"))
m3a=rcorr(dfM3$Alpha.div,dfM3$Max.Abundance, type = c("spearman"))
m3y=rcorr(dfM3$Alpha.div,dfM3$Yield, type = c("spearman"))

m4t=rcorr(dfM4$Alpha.div,dfM4$TOC, type = c("spearman"))
m4a=rcorr(dfM4$Alpha.div,dfM4$Max.Abundance, type = c("spearman"))
m4y=rcorr(dfM4$Alpha.div,dfM4$Yield, type = c("spearman"))

mmt=rcorr(dfmix$Alpha.div,dfmix$TOC, type = c("spearman"))
mma=rcorr(dfmix$Alpha.div,dfmix$Max.Abundance, type = c("spearman"))
mmy=rcorr(dfmix$Alpha.div,dfmix$Yield, type = c("spearman"))

p_vec=as.numeric(c("0.527","0.000011", "0.0105", "0.89","0.0025","0.287","0.0014","0.00362", "0.347", "0.00005487583","0.111","0.4675","0.0247","0.3065488", "0.0006864601"))
p.adjust(p_vec, method = c("holm"))
length(p_vec)

m1t$P
m2t$P
m3t$P
m4t$P
mmt$P
m1y$P
m2y$P
m3y$P
m4y$P
mmy$P
m1a$P
m2a$P
m3a$P
m4a$P
mma$P


plot(dfmix$Yield~dfmix$Alpha.div)
plot(dfmix$TOC~dfmix$Alpha.div)
plot(dfmix$Max.Abundance~dfmix$Alpha.div)

peak_acorr<-rcorr(temp_df$`motu3$Alpha.div`,temp_df$Peak_A, type = c("spearman"))
peak_ccorr<-rcorr(temp_df$`motu3$Alpha.div`,temp_df$Peak_C, type = c("spearman"))
peak_tcorr<-rcorr(temp_df$`motu3$Alpha.div`,temp_df$Peak_T, type = c("spearman"))
peak_bcorr<-rcorr(temp_df$`motu3$Alpha.div`,temp_df$Peak_B, type = c("spearman"))
peak_mcorr<-rcorr(temp_df$`motu3$Alpha.div`,temp_df$Peak_M, type = c("spearman"))
