setwd("C:/PhD Hörnan/Experiment/Adaptation 2016_05/Analysis/Clean")
meta<-read.table(file="adaptation_meta.txt", header=TRUE, row.names=1, sep="\t")

library(vegan)
library(Rcpp)
library(ggplot2)
library(Hmisc)
library(reshape2)

meta<-na.omit(meta)
meta$Time<-as.factor(meta$Time)

themeset<-theme_bw() + theme(plot.title=element_text(size=16),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                          legend.text=element_text(size=13.5), legend.title=element_text(size=14.5), 
                          axis.title=element_text(size=14), legend.key = element_blank())
#panel.border = element_blank(),

dodge <- position_dodge(width=0.9)

cm_1<-tapply(meta$C1, list(meta$Time, meta$Medium), mean)
cs_1<-tapply(meta$C1, list(meta$Time, meta$Medium), sd)

data<-data.frame(cm=c(cm_1[,1],cm_1[,2]), Medium=rep(c("Clear", "Humic"), len =12, each=6),  
cs= c(cs_1[,1], cs_1[,2]), time=as.factor(rep(c(1:6), len=12)))
str(data)
limits <- aes(ymax = cm + cs, ymin=cm - cs)

p <- ggplot(data,aes(fill=Medium, y=cm, x=time))
p <- p + geom_bar(stat= "identity", position=dodge)
p <- p + geom_errorbar(limits, position=dodge, width=0.2)
p<-p + themeset + ylab("Fluroscent signal") + xlab("Time")+ggtitle("C1")
p<-p + scale_fill_brewer()

cm_2<-tapply(meta$C2, list(meta$Time, meta$Medium), mean)
cs_2<-tapply(meta$C2, list(meta$Time, meta$Medium), sd)
data2<-data.frame(cm=c(cm_2[,1],cm_2[,2]), Medium=rep(c("Clear", "Humic"), len =12, each=6),  
                 cs= c(cs_2[,1], cs_2[,2]), time=as.factor(rep(c(1:6), len=12)))
limits2 <- aes(ymax = data2$cm + data2$cs, ymin=data2$cm - data2$cs)

p2 <- ggplot(data2,aes(fill=Medium, y=cm, x=time))
p2 <- p2 + geom_bar(stat= "identity", position=dodge)
p2 <- p2 + geom_errorbar(limits2, position=dodge, width=0.2)
p2 <- p2 + themeset + ylab("Fluroscent signal") + xlab("Time")+ggtitle("C2")
p2 <- p2 + scale_fill_brewer()

cm_3<-tapply(meta$C3, list(meta$Time, meta$Medium), mean)
cs_3<-tapply(meta$C3, list(meta$Time, meta$Medium), sd)
data3<-data.frame(cm=c(cm_3[,1],cm_3[,2]), Medium=rep(c("Clear", "Humic"), len =12, each=6),  
                  cs= c(cs_3[,1], cs_3[,2]), time=as.factor(rep(c(1:6), len=12)))
limits3 <- aes(ymax = data3$cm + data3$cs, ymin=data3$cm - data3$cs)

p3 <- ggplot(data3,aes(fill=Medium, y=cm, x=time))
p3 <- p3 + geom_bar(stat= "identity", position=dodge)
p3 <- p3 + geom_errorbar(limits, position=dodge, width=0.2)
p3 <- p3 + themeset + ylab("Fluroscent signal") + xlab("Time")+ggtitle("C3")
p3 <- p3 + scale_fill_brewer()

cm_4<-tapply(meta$C4, list(meta$Time, meta$Medium), mean)
cs_4<-tapply(meta$C4, list(meta$Time, meta$Medium), sd)
data4<-data.frame(cm=c(cm_4[,1],cm_4[,2]), Medium=rep(c("Clear", "Humic"), len =12, each=6),  
                  cs= c(cs_4[,1], cs_4[,2]), time=as.factor(rep(c(1:6), len=12)))
limits4 <- aes(ymax = data4$cm + data4$cs, ymin=data4$cm - data4$cs)

p4 <- ggplot(data4,aes(fill=Medium, y=cm, x=time))
p4 <- p4 + geom_bar(stat= "identity", position=dodge)
p4 <- p4 + geom_errorbar(limits, position=dodge, width=0.2)
p4 <- p4 + themeset + ylab("Fluroscent signal") + xlab("Time")+ggtitle("C4")
p4 <- p4 + scale_fill_brewer()



#name="Medium", labels=c("what", "Humic"),element_text(size=20)
#p<-ggplot(data=meta, aes(HIX, fill=Medium)) + geom_histogram(binwidth = 0.5) 
#p<-p + scale_fill_manual(name= "Medium", labels=c("Clear", "Humic"), values= cbbPalette) 
#p<-p + scale_fill_brewer(name= "Medium", labels=c("Clear", "Humic")) 
#p<-p + ggtitle("HIX by medium") + theme_light() + ylab("nm ratio")
#p + position_dodge(width = 0.9)

comp_fac<-read.table("Normalized_component.txt", header=T, sep="\t")
colnames(comp_fac)<-c("C1%","C2%","C3%", "C4%", "C1","C2","C3","C4","Time","Origin","Medium")
spl<-comp_fac[,c(1:4,9:11)]
dpl<-comp_fac[,c(5:8,9:11)]
str(comp_fac)
b<-"Nvariable"

test<-melt(spl, id=c("Time", "Medium", "Origin"), value.name = "Components")
test2<-melt(dpl, id=c("Time", "Medium", "Origin"), value.name = "NComponents")
#test2<-test2[,4:5]
#colnames(test2)<-c("Component","Fluroscent signal", "Time","Origin","Medium")
#str(test)
#gr <- c(test, test2) 
#gr<-as.data.frame(gr)


comp_fac$Time<-as.factor(comp_fac$Time)
gr$Time<-as.factor(gr$Time)
test$Time<-as.factor(test$Time)
test2$Time<-as.factor(test2$Time)

spl$Time<-as.factor(spl$Time)
dpl$Time<-as.factor(dpl$Time)

norm_comp_dist<-vegdist(spl[,c(1:4)], method="euclidean")
raw_comp_dist<-vegdist(dpl[,c(1:4)], method="euclidean")

#repeated permutational anova used in manuscript#
adon_norm_comp<-adonis(norm_comp_dist~spl$Origin*spl$Medium*spl$Time, strata = spl$Time, data=spl, permutations = 999)
raw_norm_comp<-adonis(raw_comp_dist~dpl$Origin*dpl$Medium*dpl$Time, data=dpl, permutations = 999)

colnames(test)<-gsub("History", "Origin", colnames(test))
str(test)

#Graphs if relative and raw components in manuscript
dp<-ggplot(data=test, aes(y=Components,x=Time, color=Medium, shape=Origin))
dp<-dp + geom_point(size=5)
dp<-dp + facet_grid(.~variable)
dp<-dp + themeset
dp<-dp + ylab("Relative abundance components (%)")
# dp<-dp + scale_shape_manual(name=c("History"),values=c(1,16)) #Change name within legend without gsub
dp<-dp + scale_shape_manual(values=c(1,16), name=NULL)
dp<-dp + theme(axis.text = element_text(size = 22))
dp<-dp + theme(axis.title = element_text(size = 24))
dp<-dp + theme(axis.title.x = element_blank())
dp<-dp + theme(strip.text = element_text(size = 24))
dp<-dp + theme(legend.text = element_text(size = 22))
dp<-dp + theme(legend.title = element_blank())
dp<-dp + theme(legend.position = "bottom")

#dp<-dp + scale_color_manual(values = c("#000000","#CCCCCC")) 

#dp + geom_smooth(method=lm, se=FALSE)


ep<-ggplot(data=test2, aes(y=NComponents,x=Time, color=Medium, shape=Origin))
ep<-ep + geom_point(size=5)
ep<-ep + facet_grid(.~variable)
ep<-ep + themeset
ep<-ep + ylab("Fluorescence intensity (R.U.)")
ep<-ep + scale_shape_manual(values=c(1,16))
ep<-ep + theme(axis.text = element_text(size = 22,color ="black"))
ep<-ep + theme(axis.title = element_text(size = 24))
ep<-ep + theme(axis.title.x = element_blank())
ep<-ep + theme(strip.text = element_text(size = 22))
ep<-ep + theme(legend.text = element_text(size = 22))
ep<-ep + theme(legend.title = element_blank())
ep<-ep + theme(legend.position = "bottom")



#ep<-ep + scale_color_manual(values = c("#000000","#CCCCCC")) 

meta$Origin

ga <- ggplot(meta, aes(y = MaxAbun, x = Time, shape = Origin, color = Medium))
ga <- ga + geom_point(size=8)
ga <- ga + scale_shape_manual(values=c(4,24))
ga <- ga + themeset
ga <- ga + theme(text=element_text(size=18, family="Helvetica"))
ga <- ga + ylab("Max abundance / mL")
ga <- ga + ylim(0, 3000000)
ga <- ga + theme(axis.title = element_text(size=21), legend.text = element_text(size=19), legend.title = element_text(size=19))
ga <- ga + theme(axis.text = element_text(size=21))



##You can write a factor within a column and ggplot will subset and plot that data only. Cool!

setwd("C:/PhD Hörnan/Experiment/Adaptation 2016_05/Analysis")

taxa<-(read.csv(file="taxonomy_only.csv", row.names=1, sep="\t"))
otu<-(read.csv(file="OTU_Taxa_stripped.txt", header=TRUE, row.names=1, sep="\t")) 
meta<-read.table(file="adaptation_meta.txt", header=TRUE, row.names=1, sep="\t")
motu<-read.table(file="meta_otu.txt", header=TRUE, row.names = 1, sep="\t")
tax<-read.table(file="tax_data.csv", header=TRUE, row.names =1, sep="\t")
#tax<-t(tax)

otu<-t(otu)
otu<-as.data.frame(otu)

#tax<-as.data.frame(tax)
h2<-strsplit(as.vector(tax$hierarchy), ";")
h3<-h2!=c("Unclassified") 
new_tax<-tax[h3,]
rnam<-row.names(new_tax)
new_tax1<-cbind(rnam, new_tax)
str(otu)
otu[,] = apply(otu[,], 2, function(x) as.integer(as.character(x)))
class(otu)
str(otu)

#c<-row.names(otu)
#length
#otu<-cbind(otu,c)

h1<-merge(otu,new_tax1, by.x="row.names", by="row.names")
dim(h1)
h1<-h1[,1:89]
row.names(h1)<-h1$Row.names
hd<-h1[,2:89]
hd<-t(hd)
hd<-as.data.frame(hd)
otu<-hd
str(hd)

h1$
row.names(h1)
colnames(h1)<-
dim(h1)
str(h1)
h1<-t(h1)
row.names(h1)
colnames(h1)<-h1[1,]

otu<-h1 ##

class(otu)
str(otu)
otu[,] = apply(otu[,], 1, function(x) as.integer(as.character(x)))
#for stats[,i] <- as.numeric(as.character(stats[,i]))  
dim(otu)
str(otu)
str(tax)
taxa<-taxa[,2:7]


otu_rr<-rrarefy(otu,min(rowSums(otu))) # rarefy to smallest integer
otud = otu_rr[ ,colSums(otu_rr)!=0]   # remove OTUs with 0 instances #Removed ~90 OTU:s

divotu<-otud[rowSums(otud)>5000,] 
list5<-rowSums(otud)>5000
rd5<-rrarefy(divotu,min(rowSums(divotu))) # rarefy to smallest integer
crd5 = rd5[ ,colSums(rd5)!=0] # X samples with more than 5000 reads

dim(crd5)

phyla<-taxa[,3]
length(phyla)
phyla<-na.omit(phyla)

str(crd5)
class(crd5)
head(crd5)
dat.m<-melt(crd5,id.vars="")#The crd4 depends on if i want to look at start or end samples
row.names

str(dat.m)
head(dat.m)
head(taxa)
taxa[1]


head(taxa)[1] 
as.vector(head(taxa)[1])
strsplit(as.vector(tax$hierarchy), ";")
head(assignments)
#sapply(strsplit(as.vector(d1$V2), ";"), length)
assignments = strsplit(as.vector(new_tax$hierarchy), ";")
b<-assignments[sapply(assignments, length) >2]
assignments[sapply(assignments, length) <3 ] = "Unassigned"

assignments[sapply(assignments, length) >2] = sapply(assignments[sapply(assignments, length) >2], function(x) x[3])##it was >2

##Not sure about this one, let see if we can find other solution. 
assignments

unlist(assignments)
d<-rownames(tax)

assignments = unlist(assignments)
head(crd5)
names(assignments) = rownames(new_tax)

head(dat.m)
head(dat.m$Var2)
assignments[as.vector(dat.m$Var2)]
dat.m$assign = assignments[as.vector(dat.m$Var2)]


dat.m$assign2<-unlist(dat.m$assign)
str(dat.m)
dat.m$assign
library(plyr)
dat.m$Var1
dat.m$assign
str(dat.m)

head(dat.m)
ddply(dat.m, c("Var1", "assign2"), summarize, counts = sum(value))
data1=ddply(dat.m, c("Var1", "assign2"), plyr :: summarize, counts = sum(value))

str(data1)
#write.table(data.pergropup,"data.pergropup.txt", sep = "\t")
#mydata<-read.table("data.pergropup.txt", header=TRUE, sep = "\t")
#oridata<-read.table("data.pergropup1.txt", header = TRUE, sep = "\t")


#mydata<-subset(mydata, mydata$counts!=c("Unassigned", "Chloroplast"))
#rd5<-rrarefy(mydata$X,min(rowSums(mydata$X))) # rarefy to smallest integer
#crd5 = rd5[ ,colSums(rd5)!=0] # X samples with more than 5000 reads

str(mydata)
str(oridata)

data.pergropup$counts
#ggplot(data.pergropup,aes(x=Var1,y=value,fill=counts))+geom_bar(stat="identity")
head(data.pergropup)
dim(data.pergropup)
g<-ggplot(data1,aes(x=Var1,y=counts,fill=assign2))+geom_bar(stat="identity")
g<-g + xlab("Samples") + ylab("Count")
g+scale_fill_hue(name="Phyla")

ggplot(oridata,aes(x=counts,y=X,fill=assign))+geom_bar(stat="identity")
sapply(assignments[sapply(assignments, length) >2], function(x) x[3])
