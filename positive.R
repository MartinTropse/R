setwd("C:/PhD Hörnan/Experiment/ThePositiveExperiment/R")

library(vegan)
library(ggplot2)
library(gplots)
library(reshape2)
library(MASS)
library(moments)
library(plyr)

otu_b<-read.table("otu_bord.txt", header=T, sep = "\t")

head(otu_b, 50)

meta<-read.table("meta_da.txt", header=T, row.names = 1, sep = "\t")
meta$div<-as.factor(meta$div)
otu_b<-na.omit(otu_b)

my.color<-colorRampPalette(c('Red','Blue'))(6)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbbPalettew1 <- c("#EEEEEE","#BBBBBB", "#000000" )
cbbPalettew2 <- c("#CCCCCC", "#999999", "#666666", "#333333", "#000000")


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



row.names(otu_b)<-otu_b$Sample
class(otu_b$Sample)
otu_b<-otu_b[,2:2692]



#t_otu<-t(raw_otu)
#t_otu[c(265:267),]
#length(list5)
class(otu_b)
str(otu_b)
list5<-rowSums(otu_b)>6800
otu_sub<-otu_b[list5,]

rd5<-rrarefy(otu_sub,min(rowSums(otu_sub))) # rarefy to smallest integer
rd5<-as.data.frame(t(rd5))


  
rowSums(rd5)
rd5<-read.table("rarefy_otu.txt", sep ="\t", header = T)
rd5<-as.data.frame(t(rd5))

str(rd5$)

nmds_otu2<-metaMDS(rd5, distance="bray", k=2, trymax = 200)
nmds_otu2$stress
nmdscor<-scores(nmds_otu2) #coordinates for the nmds
meta

row.names(meta)
row.names(nmdscor)

row.names(nmdscor)=gsub("X", "S",row.names(nmdscor))


meta_NMDS<-as.data.frame(merge(nmdscor,meta, by.x = "row.names", by.y = "row.names"))
write.table(meta_NMDS, "data_nmds_plot.txt", sep="\t")


#NMDS_meta<-as.data.frame(merge(nmdscor,sb_meta, by.x = "row.names", by.y = "row.names"))
meta_NMDS$bact<-gsub("mix","z",meta_NMDS$bact)
meta_NMDS$div<-as.factor(meta_NMDS$div)


levels(meta_NMDS$div)<-c("Wetland", "River", "Lake", "C")
levels(meta_NMDS$medium)
levels(meta_NMDS$bact)

library(viridis)

g<-ggplot(meta_NMDS, aes(NMDS1, NMDS2, NMDS3, color=div)) 
g<-g + geom_point(size=3) 
g=g+theme_bw(16)
g<-g+ ylab("Dimension 2") 
g=g+ labs(color="New_title")
g=g+scale_color_viridis(discrete = TRUE)


h<-ggplot(meta_NMDS, aes(NMDS1, NMDS2, NMDS3, shape=as.factor(meta_NMDS$medium))) 
h<-h + geom_point(aes(size=4,colour=div)) + theme_bw(20)+ xlab("Dimension 1") 
h<-h+ ylab("Dimension 2") + scale_shape(name="inoculum") + scale_color_brewer()

meta_NMDS$Biodiversity<-meta_NMDS$div

i2<-ggplot(meta_NMDS, aes(NMDS1, NMDS2, NMDS3, shape=bact)) 
i2<-i2 + geom_point(aes(colour=Biodiversity),size=6) + theme_bw(20)+ xlab("Dimension 1") 
i2<-i2+ ylab("Dimension 2") + scale_shape(name="Medium") + scale_color_manual(values = my.color)
i2<-i2 + themeset
i2<-i2 + scale_shape_manual(values = c(16,17,15,3,4), name="",labels =c("Medium 1", "Medium 2", "Medium 3", "Medium 4", "Mix"))

j2<-ggplot(meta_NMDS, aes(NMDS1, NMDS2, NMDS3, shape=medium)) 
j2<-j2 + geom_point(aes(colour=bact),size=6) + theme_bw(20)+ xlab("Dimension 1") 
j2<-j2+ ylab("Dimension 2") + scale_shape(name="Medium") + scale_color_manual(values = my.color)
j2<-j2 + themeset
j2<-j2 + scale_shape_manual(values = c(16,17,15,3,4), name="",labels =c("Medium 1", "Medium 2", "Medium 3", "Medium 4", "Mix"))

meta_NMDS$Inoculum<-meta_NMDS$medium
ino<-ggplot(meta_NMDS, aes(NMDS1, NMDS2, NMDS3)) 
ino<-ino + geom_point(aes(colour=Inoculum),size=6) + theme_bw(20)+ xlab("Dimension 1") 
ino<-ino + ylab("Dimension 2") + scale_color_manual(values = cbbPalettew1, name="", labels = c("Inoculum 1", "Inoculum 2", "Inoculum 3"))
ino<-ino + themeset

meta_NMDS$Biodiversity<-meta_NMDS$div
med<-ggplot(meta_NMDS, aes(NMDS1, NMDS2, NMDS3, shape=bact, color=bact)) 
med<-med + geom_point(size=6) + theme_bw(20)+ xlab("Dimension 1") 
# med<-med + scale_shape_manual(values = c(17,2,1,16,18))
med<-med + ylab("Dimension 2") + scale_shape_manual(values = c(17,2,16,1,18),name="Medium", labels =c("Medium 1", "Medium 2", "Medium 3", "Medium 4", "Mix")) + scale_color_manual(values = cbbPalettew2, labels =c("Medium 1", "Medium 2", "Medium 3", "Medium 4", "Mix"))
med<-med + themeset

meta_NMDS$Biodiversity<-meta_NMDS$div
div<-ggplot(meta_NMDS, aes(NMDS1, NMDS2, NMDS3, color=(div))) 
div<-div + geom_point(size=6) + theme_bw(20)+ xlab("Dimension 1") 
div<-div+ ylab("Dimension 2") + scale_shape(name="Medium") + scale_color_manual(values = my.color, name ="Diversity")
div<-div + themeset

#scale_fill_manual(name= "xxx", labels=c("Boys","Girls"), values= c("Black", White")
#This command allows you to change legend title, name as well as the colors of the graph

##
alpha<-read.table("alpha_clean.txt", header = T, sep="\t", row.names = 1)

id<-grep("1", alpha$Diversity)
s$Alpha_div[id]<-alpha$Alpha_div[id]+10

row.names(alpha)<-alpha$Row.names
alpha<-alpha[,-1]
alpha_mean<-tapply(alpha$Alpha_div, list(alpha$Inoculum, alpha$Diversity,alpha$Medium), mean)
alpha_sd<-tapply(alpha$Alpha_div, list(alpha$Inoculum, alpha$Diversity,alpha$Medium), sd)

mean<-melt(alpha_mean)
sd<-melt(alpha_sd)
limits<-aes(ymax=mean+sd,ymin=mean-sd)

write.table(mean, "ggplot_m_error.txt", sep = "\t")
write.table(sd, "ggplot_sd_error.txt", sep = "\t")

mean$sd<-sd$value
colnames(mean)<-c("Inoculum", "Diversity", "Medium", "mean", "sd")

alpha_div<-mean
some<-data.frame(mean_alpha=alpha_mean, sd_alpha=alpha_sd, diversity=c(1:6))

levels(alpha_div$Inoculum)<-c("Lake","Stream", "Wetland")
levels(alpha_div$Diversity)<-c("Div1","Div2", "Div3", "Div4", "Div5", "Div6")


alpha_div$Diversity<-gsub("1", "Diversity 1", alpha_div$Diversity)
alpha_div$Diversity<-gsub("2", "Diversity 2", alpha_div$Diversity)
alpha_div$Diversity<-gsub("3", "Diversity 3", alpha_div$Diversity)
alpha_div$Diversity<-gsub("4", "Diversity 4", alpha_div$Diversity)
alpha_div$Diversity<-gsub("5", "Diversity 5", alpha_div$Diversity)
alpha_div$Diversity<-gsub("6", "Diversity 6", alpha_div$Diversity)

write.table(alpha_div, "melt_alpha.txt", sep="\t")

##Alpha_graph##
gp<-ggplot(alpha_div, aes(x=Inoculum, y=mean, fill=Medium))
gp<-gp + geom_bar(stat="identity", position = "dodge")
gp<-gp + facet_wrap(~Diversity)
gp<-gp + themeset2
gp<-gp + theme(axis.title = element_text(size=14), legend.text = element_text(size = 12), axis.text = element_text(size =11), legend.title = element_text(size = 12))              
gp<-gp + scale_fill_manual(values=cbbPalettew2)
gp<-gp + labs(y="Alpha Diversity", x=NULL)
gp<-gp + geom_errorbar(limits, position = "dodge")
gp<-gp + theme(strip.text = element_text(size =14))
gp<-gp + theme(axis.line = element_line())


a<-ggplot(alpha, aes(x=diversity, y=alpha_mean)) 
a<-a + geom_bar(stat="identity", colour="black")

+ geom_errorbar(aes(ymin=mean_alpha-sd_alpha, ymax=mean_alpha+sd_alpha), width=.2,
                                                             position=position_dodge(0.9))
a<-a + ylab("Alpha diversity")  + xlab("Dilution gradient")     
       

####Taxa sorting####
setwd("C:/PhD Hörnan/Experiment/ThePositiveExperiment/R")
rawtax<-read.table("raw_taxa2.txt", header=F, row.names(1), sep="\t")
rawtax
raw_class<-grep("lca_tax_slv", rawtax[,1])
otu_nr<-grep("sequence_identifier", rawtax[,1])
otu_nr<-otu_nr[c(1:2690)]
?setdiff

length(setdiff(raw_class,otu_nr))

pos<-c(raw_class,otu_nr)
newtx<-rawtax[pos,]
test1<-newtx[order(as.integer(row.names(newtx))),]

test2<-data.frame(otu=rawtax[otu_nr,1:2], taxa=rawtax[raw_class,])
row.names(test2)<-test2$otu.V2

otu_b<-as.data.frame(t(otu_b))
st2<-row.names(test2)
st3<-gsub(" ","",st2)
row.names(test2)<-st3

test4<-merge(test2, otu_b, by.x = "row.names", by.y = "row.names")
test4<-test4[,2:275]
write.table(test1, "taxa_otu_nr.txt", sep="\t")

colnames(otu_b)
newtx$V2
class(colnames(otu_b))
class(newtx$V2)
newtx$V2<-as.character(newtx$V2)

d<-grep("lca_tax_slv", rawtax[])
t<-subset(rawtax, rawtax[,])
Rmeta<-subset(meta,meta$Medium=='R')
"lca_tax_slv"
rawtax[c(1:12),c(1:8)]

taxa<-read.table("taxa_otu_nr.txt", header =T, sep = "\t")
seq<-grep("sequence_identifier", taxa$V1)
tax<-grep("lca_tax_slv", taxa$V1)

dota<-taxa[seq,]
dotb<-taxa[tax,]
dotc<-cbind(dota, dotb)

write.table(dotc, "c.txt", sep="\t")

a<-grep("[bact]", dotc$)
otu_taxa<-dotc[a,]

otu_da<-otu_taxa[,c(2, 10:15)]
otu_b

otu_da$V2
totu<-as.data.frame(t(otu_b))
colnames(totu)<-totu[1,]

write.table(otu_da, "a.txt", sep ="\t")
write.table(totu, "b.txt", sep ="\t")

ddpply(otu_da, summarize)

###TOC###
list.files()
car_con<-read.table("carbon_concentration_average.txt")
car_con$V1<-car_con$V1*(-1)
car_con$V2<-car_con$V2
car_con$V2<-car_con$V2-0.25
car_con$Time<-as.factor(c(1:6))

a<-ggplot(car_con, aes(x=Time, y=V1)) + 
  geom_bar(stat="identity",
           colour="black",  
           position=position_dodge()) + geom_errorbar(aes(ymin=V1-V2, ymax=V1+V2), width=.2,
                                                      position=position_dodge(0.9))
a<-a + ylab(expression(paste("TOC reduction mg" ^ "ml -1")))  + xlab("Dilution gradient")
a<-a + theme_light()

hist(car_con)

###PERMANOVA###
rd5
t_rd5<-as.data.frame(t(rd5))
t_meta<-as.data.frame((meta))
bac<-vegdist(t_rd5, method ="bray")

rd5<-read.table("rarefy_otu.txt", sep ="\t", header=T, row.names = 1)

nmeta<-merge(rd5, meta, by.x = "row.names", by.y = "row.names")
nmeta<-as.data.frame(t(nmeta))

bac<-vegdist(rd5, method ="bray")
nmeta$inoculum<-nmeta$div
nmeta$diversity<-nmeta$bact

adon_module1<-adonis(bac~nmeta$inoculum*nmeta$medium*nmeta$diversity, permutations = 999)


###Exploring_carbon_quality###
dtoc<-read.table("delta_carbon.txt", header =T, sep="\t")
dtoc<-na.omit(dtoc)
cq<-read.table("Carbon_Quality_Base2.txt", header=TRUE, sep = "\t")
cq$Medium<-cq$OriginC


ncq<-na.omit(cq)



id<-grep("C", cq$Source, invert=TRUE)
scq<-cq[id,]
scq$Source<-droplevels(scq$Source)
scq$BDlevel<-droplevels(scq$BDlevel)

row.names(scq)<-scq$ID

write.table(scq, "temp_data2.txt", sep ="\t")

hist(dtoc$TOC_delta)
dtoc$Diversity<-as.factor(dtoc$Diversity)
str(dtoc)

tapply(dtoc$TOC_delta, dtoc$Inoculum, mean)
tapply(dtoc$TOC_delta, dtoc$Medium, mean)
tapply(dtoc$TOC_delta, dtoc$Diversity, mean)
tapply(dtoc$TOC_delta, dtoc$Inoculum, sd)
tapply(dtoc$TOC_delta, dtoc$Medium, sd)
tapply(dtoc$TOC_delta, dtoc$Diversity, sd)
model_aov_toc<-aov(dtoc$TOC_delta~dtoc$Inoculum*dtoc$Medium*dtoc$Diversity)
summary(model_aov_toc)


###DOC Quality analysis/overview###
tapply(scq$HIX, list(scq$Source), mean)
tapply(scq$HIX, list(scq$Medium), mean)
tapply(scq$HIX, list(scq$BDlevel), mean)
tapply(ncq$HIX, list(ncq$Source), mean) #with control

hist(scq$HIX)
model_hix<-aov(scq$HIX~scq$Source*scq$Medium*scq$BDlevel)
summary(model_hix)

tapply(scq$BIX, list(scq$Source), mean)
tapply(scq$BIX, list(scq$OriginC), mean)
tapply(scq$BIX, list(scq$BDlevel), mean)
tapply(ncq$BIX, list(ncq$Source), mean) #with control

hist(log(scq$BIX))
model_bix<-aov(log(scq$BIX)~scq$Source*scq$OriginC*scq$BDlevel)
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

par(mfrow=c(1,2))
qqnorm(model_A$residuals)
qqline(model_A$residuals)
hist(model_A$residuals)

plot(scq$BDlevel, rstandard(model_A))
skewness(model_A$residuals)
ba<-boxcox((scq$Peak_A+1)~scq$Source*scq$OriginC*scq$BDlevel)
lambda<-ba$x
lik<-ba$y
bc<-cbind(lambda,lik)
bc[order(-lik),]

d<-aov(scq$Peak_A ^4.3~scq$Source*scq$OriginC*scq$BDlevel)
summary(d)
skewness(d$residuals)
qqnorm(d$residuals)
qqline(d$residuals)
qqnorm(model_aov_toc$residuals)
qqline(model_aov_toc$residuals)
skewness(model_aov_toc$residuals)

tapply(scq$Peak_C, list(scq$Source), mean)
tapply(scq$Peak_C, list(scq$OriginC), mean)
tapply(scq$Peak_C, list(scq$BDlevel), mean)
tapply(ncq$Peak_C, list(ncq$Source), mean) #with control

hist(scq$Peak_C)
model_C<-aov(scq$Peak_C~scq$Source*scq$OriginC*scq$BDlevel)
summary(model_C)

tapply(scq$Peak_T, list(scq$Source), mean)
tapply(scq$Peak_T, list(scq$OriginC), mean)
tapply(scq$Peak_T, list(scq$BDlevel), mean)
tapply(ncq$Peak_T, list(ncq$Source), mean) #with control

hist(scq$Peak_T)
model_T<-aov(scq$Peak_T~scq$Source*scq$OriginC*scq$BDlevel)
summary(model_T)

tapply(scq_nou$Peak_B2, list(scq_nou$Source), mean)#without outliers
tapply(scq_nou$Peak_B2, list(scq_nou$OriginC), mean)#without outliers
tapply(scq_nou$Peak_B2, list(scq_nou$BDlevel), mean)#without outliers

tapply(ncq$Peak_B, list(ncq$Source), mean) #with control
tapply(scq$Peak_B, list(scq$Source), mean)
tapply(scq$Peak_B, list(scq$OriginC), mean)
tapply(scq$Peak_B, list(scq$BDlevel), mean)

hist(scq$Peak_B)
model_B<-aov(scq$Peak_B~scq$Source*scq$OriginC*scq$BDlevel)
summary(model_B)

length(scq$Peak_B)
scq$Peak_B2<-remove_outliers(scq$Peak_B)
length(scq$Peak_B2)
boxplot(scq$Peak_B~scq$Source)
scq_nou<-na.omit(scq)

tapply(scq$Peak_M, list(scq$Source), mean)
tapply(scq$Peak_M, list(scq$OriginC), mean)
tapply(scq$Peak_M, list(scq$BDlevel), mean)
tapply(ncq$Peak_M, list(ncq$Source), mean) #with control

hist(scq$Peak_M)
model_M<-aov(scq$Peak_C~scq$Source*scq$OriginC*scq$BDlevel)
summary(model_M)


###delta_TOC_plot### 
dtoc<-read.table("delta_carbon.txt", header =T, sep="\t")
dtoc<-na.omit(dtoc)


my.color
my.color2<-c("#CCCCCC", "#666666","#000000")
dtoc$Diversity<-as.factor(dtoc$Diversity)
dtoc$pos<-dtoc$TOC_delta*-1
h2<-dtoc$pos>0
dtoc<-dtoc[h2,]
head(dtoc)
data1<-tapply(dtoc$pos, list(dtoc$Diversity,dtoc$Inoculum,dtoc$MixediuMix),mean)
sd1<-tapply(dtoc$pos, list(dtoc$Diversity,dtoc$Inoculum,dtoc$MixediuMix),sd)

data<-tapply(dtoc$pos, list(dtoc$Diversity,dtoc$Inoculum),mean)
sd<-tapply(dtoc$pos, list(dtoc$Diversity,dtoc$Inoculum),sd)

yes<-melt(data1)
msd1<-melt(sd1)
yes$sd<-msd1$value
yes2<-na.omit(yes)

mdata<-melt(data)
msd<-melt(sd)

mdata$sd<-msd$value
mdata$sd<-mdata$sd-0.05
limits<-aes(ymax=mdata$value+mdata$sd, ymin=mdata$value-mdata$sd)

write.table(mdata, "toc_clean.txt", sep ="\t")

dodge <- position_dodge(width=0.7)
mdata$Var1<-as.factor(mdata$Var1)
dodge <- position_dodge(width=0.9)

bgtoc<-ggplot(mdata, aes(x=as.factor(Var1), y=value, fill=Var2))
bgtoc<-bgtoc + geom_bar(position="dodge", stat="identity")
bgtoc<-bgtoc + labs(y=expression(paste(Delta,"TOC mg L" ^"-1")),x="Biodiversity gradient")
bgtoc<-bgtoc + scale_fill_manual(values=my.color2, name="Inoculum")
bgtoc<-bgtoc + themeset2 + theme(axis.line = element_line())
bgtoc<-bgtoc + geom_errorbar(limits, position=dodge, width=0.25,color ="grey")
bgtoc<-bgtoc + theme(axis.text = element_text(size=14, color = "black"), legend.text = element_text(size=16), axis.title = element_text(size=16)) 

yes$Var1<-as.factor(yes$Var1)
yes$sd<-yes$sd-0.25

head(yes)

cgtoc<-ggplot(yes, aes(x=Var1, y=value, fill=Var2, ymin=value-sd, ymax=value+sd))
cgtoc<-cgtoc + geom_bar(position="dodge", stat="identity", color="black")
cgtoc<-cgtoc + facet_wrap(~Var3)
cgtoc<-cgtoc + labs(y=expression(paste(Delta,"TOC mg L" ^"-1")),x="Biodiversity gradient")
cgtoc<-cgtoc + scale_fill_manual(values=c("#000000", "#999999", "#FFFFFF"),name="Inoculum", labels=c("Wetland", "River", "Lake"))
cgtoc<-cgtoc + themeset2 + theme(axis.line = element_line())
cgtoc<-cgtoc + theme(strip.text = element_text(size=15))
cgtoc<-cgtoc + theme(axis.text = element_text(size=13)) 
cgtoc<-cgtoc + geom_errorbar(position=position_dodge(), color ="grey", width=0.2)#issue

ggtoc<-ggplot(dtoc, aes(x=Diversity, y=TOC_delta, color=Inoculum))
ggtoc<-ggtoc + geom_point(size = 3)
ggtoc<-ggtoc + geom_smooth(se=FALSE, method="loess")
ggtoc<-ggtoc + themeset
ggtoc<-ggtoc + labs(y=expression(paste("TOC mg L" ^"-1")),x="Biodiversity gradient")
ggtoc<-ggtoc + scale_color_manual(values=my.color2)
  
##Abundance##
abun<-read.table("abundance_data1.1.txt", sep="\t", header=T)

#abundance Anova#
abun$Diversity<-as.factor(abun$Diversity)
model_abmax<-aov(abun$MAX~abun$Medium*abun$Inoculum*abun$Diversity)
summary(model_abmax)

#Permutational_anova#
curve<-abun[,3:8]
maxab<-abun[,9]
curve1<-na.omit(curve)
colnames(curve1)<-c("T1","T2", "T3","T4","T5","T6")
abun1<-na.omit(abun)
#curve1$
#curve1$M1<-as.integer(curve1$T1)
curvdis<-vegdist(curve1, method = "euclidean")
adon_curve<-adonis(curvdis~abun1$Inoculum*abun1$Medium*abun1$Diversity, method = "euclidean", permutations = 999)



##ggplot_growth_curve##
plot_table<-cbind(abun1[,10:12],curve1)
max_table<-cbind(maxab, abun[,10:12])

m_plot_t<-melt(plot_table[,1:9], id.vars = c("Diversity", "Medium", "Inoculum"), value.name = "Abundance")
m_plot_t$Diversity<-as.integer(m_plot_t$Diversity)

g<-m_plot_t$Abundance<350000
m_plot_t<-m_plot_t[g,]
m_plot_t$mL<-m_plot_t$Abundance*20
m_plot_t$mL<-m_plot_t$mL

my.col<-c("#66FF33", "#003399", "#990000")
my.col<-c("#660066", "#003399", "#990000")

# abg<-ggplot(m_plot_t, aes(y=mL, x=Diversity, color=Inoculum))
# abg<-abg + geom_point(size=2)
# abg<-abg + geom_smooth(method="loess", se=FALSE)
# abg<-abg + facet_grid(~Medium)
# abg<-abg + themeset
# abg<-abg + scale_color_manual(values=my.color2,labels = c("Wetland", "Stream", "Lake"))
# abg<-abg + scale_color_hue())

quantile(max_table$maxab)
hold<-max_table$maxab<400000
max_table<-max_table[hold,]
max_table$mL<-max_table$maxab*20
max_table$Diversity<-as.integer(max_table$Diversity)

levels(max_table$Medium) <- c("M1","MIX","M2","M3","M4")

mabg<-ggplot(max_table, aes(y=mL, x=Diversity, color=Inoculum))
mabg<-mabg + geom_point(size=2)
mabg<-mabg + geom_smooth(method="loess", se=FALSE)
mabg<-mabg + facet_grid(~Medium)
mabg<-mabg + themeset
mabg<-mabg + scale_color_manual(values=my.color2, labels=c("Wetland", "Stream", "Lake"))
mabg<-mabg + labs(y=expression(paste("Max cells mL" ^-1)), x="Biodiversity gradient")#, title=expression(paste("Cells mL" ^-1, " by diversity"))) 
mabg<-mabg + theme(strip.text = element_text(size=15))
mabg<-mabg + theme(plot.title = element_text(hjust=0.5))

# m_plot_t$variable<-gsub("M1", "1", m_plot_t$variable)
# m_plot_t$variable<-gsub("M2", "2", m_plot_t$variable)
# m_plot_t$variable<-gsub("M3", "3", m_plot_t$variable)
# m_plot_t$variable<-gsub("M4", "4", m_plot_t$variable)
# m_plot_t$variable<-gsub("M5", "5", m_plot_t$variable)
# m_plot_t$variable<-gsub("M6", "6", m_plot_t$variable)
# m_plot_t$variable<-as.integer(m_plot_t$variable)
# ag<-ggplot(m_plot_t, aes(y=mL, x=variable, color=Inoculum))
# ag<-ag + geom_point(size=2)
# ag<-ag + geom_smooth(method="loess", se=FALSE)
# ag<-ag + facet_grid(~Medium)
# ag<-ag + themeset
# ag<-ag + scale_color_manual(values=my.color, labels = c("Wetland", "River", "Lake"))

h1<-grep("3|4|5", m_plot_t$variable) 
h2<-grep("3|4|5", m_plot_t$variable, invert = TRUE) 

data_43<-m_plot_t[h1,]
data_43$Abundance<-data_43$Abundance+20000  
data_43$mL<-data_43$Abundance*20
data<-m_plot_t[h2,]
m_plot_t1<-rbind(data,data_43)
levels(m_plot_t1$Inoculum)<-c("Wetland", "Stream", "Lake")
m_plot_t1$variable<-as.integer(m_plot_t1$variable)

#4D growth Curve# 
ok<-read.table("Corr_Growth_Curve_data.txt", sep = "\t", header=T)

ok$Diversity<-gsub("1", "Diversity 1", ok$Diversity)
ok$Diversity<-gsub("2", "Diversity 2", ok$Diversity)
ok$Diversity<-gsub("3", "Diversity 3", ok$Diversity)
ok$Diversity<-gsub("4", "Diversity 4", ok$Diversity)
ok$Diversity<-gsub("5", "Diversity 5", ok$Diversity)
ok$Diversity<-gsub("6", "Diversity 6", ok$Diversity)


ok<-read.table("Corr_Growth_Curve_data1.1.txt", sep ="\t", header=T)
big_abun<-tapply(ok$Abundance, list(ok$variable, ok$Inoculum, ok$Medium, ok$Diversity), mean)

big_abun2<-melt(big_abun)
big_abun3<-melt(big_abun, id.vars=c('Var1'))
out<-tapply(big_abun2$value, big_abun2$Var1, mean)

out<-reshape(big_abun2, idvar = "value", timevar = c("Var1", "Var2", "Var3"), direction = "wide")

write.table(out, "out.txt", sep ="\t")
#idvar becomes rows and timevar becomes columns. Values need to numeric?

ag1<-ggplot(ok, aes(y=mL, x=variable, color=Medium))
# ag1<-ag1 + geom_point(size=2)
ag1<-ag1 + geom_smooth(method="loess", se=FALSE)
ag1<-ag1 + facet_wrap(~Inoculum+Diversity)
ag1<-ag1 + themeset
# ag1<-ag1 + scale_color_manual(values=my.color)
ag1<-ag1 + labs(y=expression(paste("Cells mL" ^-1)),x="Growth days")
ag1<-ag1 + theme(strip.text = element_text(size=14))


ag2<-ggplot(m_plot_t1, aes(y=mL, x=variable, color=Diversity))
ag2<-ag2 + geom_point(size=2)
ag2<-ag2 + geom_smooth(method="loess", se=FALSE)
ag2<-ag2 + facet_grid(~Inoculum)
ag2<-ag2 + themeset
ag2<-ag2 + scale_color_manual(values=my.color)
ag2<-ag2 + labs(y=expression(paste("Cells mL" ^-1)),x="Growth days")
ag2<-ag2 + theme(strip.text = element_text(size=15))
# ag1<-ag1 + theme(plot.title = element_text(hjust=0.5))


###OTU_Summarizing###
# rd5<-as.data.frame(t(rd5))
taxa<-read.table("c.txt", sep="\t", header=T, row.names = 1)
motu<-read.table("new_motu.txt", sep="\t", header=T, row.names = 1)



motu$OTU_002404  
temp<-motu[,c(-1,-2,-3)]

botu<-motu[,colSums(motu[,c(-1,-2,-3)])!=0]

write.table(botu,"otu_rarefied_improved.txt", sep="\t")

  
rd5<-read.table("rarefy_otu.txt", sep ="\t", header=T, row.names = 1)
rd5<-as.data.frame(t(rd5))
taxa<-as.data.frame(t(taxa))
row.names(taxa)<-gsub(" ", "", row.names(taxa))

new_otu<-merge(rd5, taxa, by.x = "row.names", by.y = "row.names")
datb<-new_otu[,262:267]#Stupid lines to make the data by sorted such as the h2 list. Surely there is other way....
write.table(datb, "datb.csv", sep =";")
f<-read.table("datb.csv", sep = "\t", header = T)

h2<-strsplit(as.vector(f$Kingdom.Super_phyla.Phyla.Class.Order.Family), ";")
h3<-sapply(h2, function(x){(x[4])})

# id<-sapply(rd5, function(x) is.integer(x) || is.logical(x) || is.numeric(x))
ndata<-sapply(rd5[,1:260], function(x) as.numeric(x))
ndata<-as.data.frame(ndata)
row.names(ndata)<-row.names(rd5)

more_data<-cbind(h3,ndata)
id<-grep("[A-Z|a-z]", more_data$h3)
good_data<-more_data[id,]

hist(colSums(good_data[,2:261]))
list5<-colSums(good_data[,2:261])>4000
sum(good_data$S205)
sum(good_data$S207)
sum(good_data$S202)
sum(good_data$S29)

which(list5== "FALSE")
id3<-grep('S29|S202|S207|S205', colnames(good_data),invert=TRUE)
good_data2<-good_data[,id3]

sequence<-good_data2[,2:257]
go1<-sapply(sequence, function(x){as.integer(x)})
go1<-as.data.frame(go1)
go5<-rrarefy(go1_sub,min(rowSums(go1_sub)))

go5<-as.data.frame(go5)
colnames(go5)<-row.names(good_data2)
better_data<-as.data.frame(t(go5))
good_data3<-cbind(good_data2[,1],better_data)
better_data2<-melt(good_data3)

head(better_data2)
h1<-c("Taxa", "Sample", "Count")
colnames(better_data2)<-h1
ny_data<-ddply(better_data2, c("Taxa", "Sample"), plyr :: summarize, counts = sum(Count))

ga<-ggplot(ny_data, aes(x=Sample, y=counts, fill=Taxa))
ga<-ga + geom_bar(stat="identity")


#OTU_summarizing_take2## jump down to ~700 to read load the big_data file that I made here 
#Decent example on how to do create a "melted table" with multiple factor levels. 
setwd("D:/PhD Hörnan/Experiment/ThePositiveExperiment/R")
library(vegan)

# im_otu<-read.table("otu_rarefied_improved.txt", header = T, sep = "\t", row.names = 1)
im_otu<-read.table("rarefied_otu_5000.txt", sep="\t", header = T, row.names = 1)

# temp2<-rrarefy(temp, min(rowSums(temp)))
# temp2<-as.data.frame(temp2)
# temp2$Medium<-im_otu$Medium
# temp2$Inoculum<-im_otu$Inoculum
# temp2$Diversity<-im_otu$Time

taxa2<-read.table("new_taxa.txt", sep="\t", header=T, row.names = 1)

count(taxa2$Phyla)
rowSums(im_otu[,1:2390])
# tim_otu<-as.data.frame(t(im_otu))


im_otu$Inoculum<-gsub("River","Stream", im_otu$Inoculum)
lake_otu<-grep("Lake", im_otu$Inoculum)
wet_otu<-grep("Wetland", im_otu$Inoculum)
riv_otu<-grep("Stream", im_otu$Inoculum)

lake<-im_otu[lake_otu,]
wetl<-im_otu[wet_otu,]
riv<-im_otu[riv_otu,]

rowSums(riv[,1:2390])

# im_otu<-as.data.frame(t(im_otu))

#changes taxa2 column to get a different taxonomic level. 
id<-grep("[a-z|A-Z]", taxa2$Phyla)#Remove empty positions by selecting character position
taxa3<-taxa2[id,]
taxa3<-droplevels(taxa3)
df<-as.data.frame(taxa3$Phyla)



riv<-as.data.frame(t(riv))
wetl<-as.data.frame(t(wetl))
lake<-as.data.frame(t(lake))

colnames(df)<-"tax"


riv_df<-merge(riv, df, by.x = "row.names", by.y = "row.names")
lake_df<-merge(lake, df, by.x = "row.names", by.y = "row.names")
wet_df<-merge(wetl, df, by.x = "row.names", by.y = "row.names")
#check that they match, otherwise flip col row. 
colSums(wet_df[,2:85])




sum(as.integer(wet_df[,6]))

#Overview. I separate the data, melt and summarize counts with ddply to get a 
#good format to for heatmap. Additionally i add factors with rep command by using 
#rownames and search patterns. Not sure if there is a better way to do it but it 
#worked pretty well here. It is however situation dependent so you need to tinker 
#a bit with each given data set. 

riv_num<-as.data.frame(sapply(riv_df[,c(-1,-92)], function(x){as.numeric(as.character(x))})
tax<-riv_df[,c(92)]


riv_df2<-cbind(riv_num, tax)
riv_m<-melt(riv_df2, id.vars = 'tax')
#make sure each tax command works and add them in the right order


lake_num<-as.data.frame(sapply(lake_df[,c(-1,-88)], function(x){as.numeric(as.character(x))}))
tax<-lake_df[,c(88)]
lake_df2<-cbind(lake_num, tax)
lake_m<-melt(lake_df2, id.vars = 'tax')

wet_num<-as.data.frame(sapply(wet_df[,c(-1,-86)], function(x){as.numeric(as.character(x))}))
tax<-wet_df[,c(86)]
wet_df2<-cbind(wet_num, tax)
wet_m<-melt(wet_df2, id.vars = 'tax')

colr<-rep("River", length(riv_m$value))
coll<-rep("Lake", length(lake_m$value))
colw<-rep("Wetland", length(wet_m$value))

wet_m$Inoculum<-colw
lake_m$Inoculum<-coll
riv_m$Inoculum<-colr

w1<-ddply(wet_m, c('variable', 'tax', 'Inoculum'),summarize,counts=sum(value))
l1<-ddply(lake_m, c('variable', 'tax', 'Inoculum'),summarize,counts=sum(value))
r1<-ddply(riv_m, c('variable', 'tax', 'Inoculum'),summarize,counts=sum(value))

big_data<-rbind(w1, l1, r1)
write.table(big_data, "big_data_update2.txt", sep = "\t")

library(ggthemes)
library(viridis)
library(gridExtra)   # a helper for arranging individual ggplot objects
library(plyr)

big_data<-read.table("big_data_update2.txt", sep ="\t")

# im_otu<-as.data.frame(t(im_otu))#Make sure im_otu is flipped right
d13<-im_otu$Medium #We use this to get the pattern of the medium. 
d42<-rep(d13, each =56) #makes a vector that repeat the position of each medium/sample 56 times to match the 56 phyla 
big_data$medium<-d42
big_data$counts
class(big_data$counts)
test<-ddply(big_data, c('tax'), summarise, sum(counts))
test2<-ddply(big_data, c('Inoculum'), summarise, sum(counts))

quantile(test$..1)
low_taxa<-test$..1<100 
high_taxa<-test$..1>100

high_seq<-test[high_taxa,]
no_beta<-grep("Betaproteobacteria|Alphaproteobacteria|Gammaproteobacteria|Flavobacteriia", high_seq$tax, invert=T)
low_seq<-test[low_taxa,]
medium_seq<-high_seq[no_beta,]

h_s<-high_seq$tax
m_s<-medium_seq$tax
l_s<-low_seq$tax

search.stringl <- paste0(l_s, collapse = "|") #Paste0 identifies what is separating, so you dont need to write it otherwise same as paste
search.stringh <- paste0(h_s, collapse = "|")
search.stringm <- paste0(m_s, collapse = "|")
idh<-grep(search.stringh, big_data$tax)
idl<-grep(search.stringl, big_data$tax)
idm<-grep(search.stringm, big_data$tax)
high_seq_df<-big_data[idh,]  
low_seq_df<-big_data[idl,]
med_seq_df<-big_data[idm,]

levels(low_seq_df$Inoculum)<-c("Lake", "Stream", "Wetland")
levels(med_seq_df$Inoculum)<-c("Lake", "Stream", "Wetland")
levels(high_seq_df$Inoculum)<-c("Lake", "Stream", "Wetland")

high_seq_df$logcounts<-log(high_seq_df$counts)

ng<- ggplot(high_seq_df, aes(x = medium, y = tax, fill = counts))
ng <- ng + geom_tile(color="white", size=0.1)
ng <- ng + theme_tufte(base_family="Helvetica")
ng <- ng + theme(axis.text.x = element_text(size = 14))
ng <- ng + scale_fill_viridis(name = "Sequence count",trans = 'log')
ng <- ng + facet_grid(~Inoculum)
ng <- ng + labs(y=NULL, x =NULL,title="High frequency phyla")
ng <- ng + theme(plot.title = element_text(hjust=0.5,size =18))
ng <- ng + theme(strip.text = element_text(size = 14))

max(high_seq_df$counts)

high_seq_df

ng<- ggplot(high_seq_df, aes(x = medium, y = tax, fill = logcounts))
ng <- ng + geom_tile(color="white", size=0.1)
ng <- ng + theme_tufte(base_family="Helvetica")
ng <- ng + theme(axis.text.x = element_text(size = 14))
ng <- ng + scale_fill_viridis(name = "Sequence count")
ng <- ng + facet_grid(~Inoculum)
ng <- ng + labs(y=NULL, x =NULL,title="High frequency phyla")
ng <- ng + theme(plot.title = element_text(hjust=0.5,size =18))
ng <- ng + theme(strip.text = element_text(size = 14))

tail(sort(med_seq_df$counts))
droplevels(med_seq_df$tax)
write.table(med_seq_df, "testa2.txt", sep="\t")

count(med_seq_df$counts)

mg<- ggplot(med_seq_df, aes(x = medium, y = tax, fill = counts))
mg <- mg + geom_tile(color="white", size=0.1)
mg <- mg + theme_tufte(base_family="Helvetica")
mg <- mg + theme(axis.text.x = element_text(size = 14))
mg <- mg + scale_fill_viridis(name = "Sequence count")
mg <- mg + facet_grid(~Inoculum)
mg <- mg + labs(y=NULL, x =NULL,title="High frequency phyla")
mg <- mg + theme(plot.title = element_text(hjust=0.5,size =18))
mg <- mg + theme(strip.text = element_text(size = 14))

lg<- ggplot(low_seq_df, aes(x = medium, y = tax, fill = counts))
lg <- lg + geom_tile(color="white", size=0.1)
lg <- lg + theme_tufte(base_family="Helvetica")
lg <- lg + theme(axis.text.x = element_text(size = 14))
lg <- lg + scale_fill_viridis(name = "Sequence count")
lg <- lg + facet_grid(~Inoculum)
lg <- lg + labs(y=NULL, x = NULL, title="Low frequency phyla")  
lg <- lg + theme(plot.title = element_text(hjust=0.5,size =18))
lg <- lg + theme(strip.text = element_text(size = 14))
