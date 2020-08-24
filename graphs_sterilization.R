setwd("C:/PhD Hörnan/Experiment/Sterilisering experiment 2014-08/R")

#Parts of the script doesnt work cause I renamed things. Cause im an idiot x)

library(Hmisc)
library(reshape2)
library(ggplot2)





TOC<- read.table("TOC_2016_05.csv", header = TRUE, sep="\t")
concentration<- read.table("Concentration2.txt", header = TRUE, sep="\t")
df_yield<-read.table("Yield.txt", header=TRUE, sep = "\t")
meta<-read.table("metadata2.txt", header = T, sep = "\t")
row.names(meta)<-meta$Sample


blacktowhite <- c("#333333","#666666","#999999","#CCCCCC","#FFFFFF")
fdom<-read.table("fdom_normalized.txt", header=TRUE, sep="\t")

citation(package = "indicspecies")

fmean<-tapply(fdom$Norm_FDOM, list(fdom$Lake,fdom$Sterilization), mean)
fsd<-tapply(fdom$Norm_FDOM, list( fdom$Lake,fdom$Sterilization), sd)
fmean<-t(fmean)
fsd<-t(fsd)
##Lili made same information but in 3 columns rather then as a matrix. 
matrix<-as.matrix(fmean)
matrix2<-as.matrix(fsd)
B<-t(matrix)
sd<-t(matrix2)

Lake<-c(rep("B",4),rep("L",4),rep("R",4),rep("S",4))
Bmean<-(B[1:4,1])
Lmean<-(B[1:4,2])
Rmean<-(B[1:4,3])
Smean<-(B[1:4,4])

treat_mean<-c(Bmean,Lmean,Rmean,Smean)
treat<-rep(c("0.1","0.22","AC","AC2"),4)
newd<-data.frame(Lake,treat,treat_mean)


sdB<-(sd[1:4,1])
sdL<-(sd[1:4,2])
sdR<-(sd[1:4,3])
sdS<-(sd[1:4,4])

newd$sd<-(c(sdB,sdL,sdR,sdS))
newd$name<-c(paste(newd$Lake, newd$treat))


fdombar1 <- barplot(height = newd$treat_mean,
                    beside = true, las = 2,
                    names.arg = newd$name,
                    ylim = c(0, 250),
                    cex.names = 0.75, xaxt = "n",
                    main = "FDOM",
                    ylab = "FDOM/TOC",
                    border = "black", axes = TRUE)

text(x = fdombar1, y = par("usr")[3] - 1, srt = 45,
     adj = 1, labels = newd$name, cex=0.6, xpd = TRUE)

segments(fdombar1, newd$treat_mean - newd$sd, fdombar1,
         newd$treat_mean + newd$sd, lwd = 1)

arrows(fdombar1, newd$treat_mean - newd$sd, fdombar1,
       newd$treat_mean + newd$sd, lwd = 1, angle = 90, code =3, length = 0.05)

####Lets make another FDOM graph!####
fmean<-as.data.frame(fmean)

rownames(fmean)<-gsub("Siggefora", "Siggeforasjön", rownames(fmean))
rownames(fmean)<-gsub("Bjorklinge", "Långsjön", rownames(fmean))
rownames(fmean)<-gsub("Lotsjon", "Lötsjön", rownames(fmean))
rownames(fmean)<-gsub("Ramsjon", "Ramsjön", rownames(fmean))

fdombar <- barplot(height = fmean,
                   beside = TRUE, las = 1,
                   ylim = c(0,250),
                   cex.names = 0.75,
                   main = "FDOM",
                   ylab = "Fluroscent signal",
                   xlab = "Lakes",
                   border = "black", axes = TRUE,
                   legend.text = TRUE,
                   args.legend = list(title = "Sterilization", bty = "n", 
                                      x = "topright",
                                      cex = 1.1))



segments(fdombar, fmean - fsd, fdombar, fmean + fsd,  
         lwd = 1.5)
arrows(fdombar, fmean - fsd, fdombar, fmean + fsd, angle = 90,  
       lwd = 1.5, code = 3, length = 0.05)

###TOC graph###
concentration$steril<-gsub("0.22","0.2",concentration$steril)
concentration$steril<-gsub("Ori","Original",concentration$steril)

concentration$lake<-gsub("Siggerfora", "Siggeforasjön", concentration$lake)
concentration$lake<-gsub("Bjorklinge", "Långsjön", concentration$lake)
concentration$lake<-gsub("Lotsjon", "Lötsjön", concentration$lake)
concentration$lake<-gsub("Ramsjon", "Ramsjön", concentration$lake)

con_filter<-subset(concentration, concentration$steril == ("0.22|0.1"))
id<-grep("0.22|0.1", concentration$steril)


indct<-grep("[Ori]", concentration$steril, invert = T)
sub_concen<-concentration[indct,]
meanmx<-tapply(concentration$mean, list(concentration$steril, concentration$lake), mean)
sdmx<-tapply(concentration$sd, list(concentration$steril, concentration$lake), mean)




#meanmx<-tapply(concentration$mean, list(concentration$steril, concentration$lake)
 #      ,function(x)  c(x = x))
#sdmx<-tapply(concentration$sd, list(concentration$steril, concentration$lake)
 # ,function(x) c(x = x))

#meanmx<-t(meanmx)
#sdmx<-t(sdmx)


tocbar<-barplot(height = meanmx,
                ylim= c(0, 30),
                main= "DOC",
                # ylab= "TOC mg/L",
                # xlab= "Lake",
                cex.names = 1.2,
                col=blacktowhite,
                beside = T, las = 1,
                legend.text = T, 
                args.legend = list(title = "Sterilization",
                x = "topleft",
                cex = 1.1, bty ="n"))


mtext(expression(paste(plain("DOC mg C"), " L" ^ "-1")),  side=2, line=2.0, cex=1.2)
mtext("Lake", side=1, line=2.2, cex=1.2)


segments(tocbar, meanmx - sdmx, 
         tocbar, meanmx + sdmx, lwd = 1.5)
arrows(tocbar, meanmx - sdmx, tocbar, 
       meanmx + sdmx, angle = 90, lwd = 1.5, code = 3, length = 0.05)

###BIX and Yield Graph###
df_yield$Lake<-gsub("Siggefora", "Siggeforasjön", df_yield$Lake)
df_yield$Lake<-gsub("Bjorklinge", "Långsjön", df_yield$Lake)
df_yield$Lake<-gsub("Lotsjon", "Lötsjön", df_yield$Lake)
df_yield$Lake<-gsub("Ramsjon", "Ramsjön", df_yield$Lake)
df_yield$Treatment<-gsub("0.22", "0.2", df_yield$Treatment)

df_yield$SampleYield<-df_yield$SampleYield*20

df_yield1<-df_yield[c(1:15,19:27,31:48), ]
model_dyield<-aov(df_yield1$SampleYield~df_yield1$Treatment*df_yield1$Lake)
summary(model_dyield)
TukeyHSD(model_dyield, "df_yield1$Treatment")

model_ori_yield<-aov(df_yield$SampleYield~df_yield$Treatment*df_yield$Lake)
summary(model_ori_yield)
TukeyHSD(model_ori_yield, "df_yield$Treatment")

  
ymmx<-tapply(df_yield$SampleYield, list(df_yield$Treatment, df_yield$Lake), mean)
ysmx<-tapply(df_yield$SampleYield, list(df_yield$Treatment, df_yield$Lake), sd)

B<-(max(ymmx)+max(ysmx))

baryield <- barplot(height = ymmx,
               beside = TRUE, las = 1,
               ylim = c(0, B),
               cex.names = 1.2,
               main = "Bacterial yield",
               # ylab = "Abundance mL/??TOC mg/L",
               # xlab = "Lake",
               border = "black", axes = TRUE,
               legend.text = TRUE,
               args.legend = list(title = "Sterilization", bty = "n",
                                  x = "topright",
                                  cex = 1.1))

mtext(expression(paste( plain("cell mL") ^ plain("-1"), 
                        plain("/   TOC mg/L") ^ plain("-1"))), side=1.5, line=3, cex=1.2)
mtext("??", side=1.5, line=2, cex=1.2)
mtext("Lake", side=1.5, line=1, cex=1.2)

segments(baryield, ymmx - ysmx, 
         baryield, ymmx + ysmx, lwd = 1.5)
         
arrows(baryield, ymmx - ysmx, 
                baryield, ymmx + ysmx, 
                angle = 90, lwd = 1.5, code = 3, length = 0.05)

###BIX###
meta$Treatment<-gsub("Ori", "Original",meta$Treatment)
meta$Treatment<-gsub("0.22", "0.2", meta$Treatment)
meta$Water<-gsub("Siggefora", "Siggeforasjön", meta$Water)
meta$Water<-gsub("Bjorklinge", "Långsjön", meta$Water)
meta$Water<-gsub("Lotsjon", "Lötsjön", meta$Water)
meta$Water<-gsub("Ramsjon", "Ramsjön", meta$Water)

bmmx<-tapply(meta$BIX, list(meta$Treatment, meta$Water), mean)
bsmx<-tapply(meta$BIX, list(meta$Treatment, meta$Water), sd) 

barbix <- barplot(height = bmmx,
                    beside = TRUE, las = 1,
                    ylim = c(0.4, 1),
                    cex.names = 1.2,
                    col=blacktowhite,
                    lwd=1, #Stand for line width
                    main = "BIX",
                    xpd=FALSE, #Cutes the values based on the scale  
                    border = "black", axes = TRUE,
                    legend.text = TRUE,
                    args.legend = list(title = "Sterilization", 
                                       x = "topright",
                                       cex = 1.1,bty = "n"))#Removes legend box, needs to be in the args legend bracket

mtext("BIX", side=2, line=2.2, cex=1.2)
mtext("Lake", side=1, line=2.2, cex=1.2)


segments(barbix, bmmx - bsmx, 
         barbix, bmmx + bsmx, lwd = 1.5)

arrows(barbix, bmmx - bsmx, 
       barbix, bmmx + bsmx, 
       angle = 90, lwd = 1.5, code = 3, length = 0.05)


barplot(mat, beside=TRUE, ylim=ylim, col=1, lwd=1:2, angle=45, density=seq(5,35,5))
barplot(mat, add=TRUE, beside=TRUE, ylim=ylim, col=1, lwd=1:2, angle=c(45, 135), density=seq(5,35,5))
legend("top", legend=1:7, ncol=7, fill=TRUE, cex=1.5, col=1, angle=45, density=seq(5,35,5))
legend("top", legend=1:7, ncol=7, fill=TRUE, cex=1.5, col=1, angle=c(45, 135), density=seq(5,35,5))


blacktowhite <- c("#333333","#666666","#999999","#CCCCCC","#FFFFFF")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



#Other BIX things#
ind1<-grep("[Orignal]", meta$Treatment, invert = T)
sub_bixm<-meta[ind1,]

rank_bix<-rank(sub_bixm$BIX)
B<-(max(bmmx)+max(bsmx))
BIX_aov_NoOri<-aov(rank(sub_bixm$BIX)~sub_bixm$Treatment*sub_bixm$Water)
summary(BIX_aov_NoOri)

yldind<-grep("[Ori]",meta$Treatment, invert = T)
sub_yield<-meta[yldind,]
yield_mean_no_ori<-tapply(sub_yield$, list(sub_yield$Treatment, sub_yield$Lake), mean)
bix_mean_no_ori<-tapply(sub_bixm$BIX, list(sub_bixm$Treatment, sub_bixm$Water), mean)

h1<-melt(yield_mean_no_ori, id=c(row.names(yield_mean_no_ori)),value.name = "Yield")
h2<-melt(bix_mean_no_ori, id=c(row.names(bix_mean_no_ori)), value.name = "BIX")
h1$BIX<-h2$BX 
bray_dist_y_bix<-vegdist(h1[,3:4], method = "bray")
model_bix_y<-adonis(bray_dist_y_bix~h1$Var1*h1$Var2, permutations = 999)

id<-grep("Original", invert =T, meta$Treatment)
new_df<-as.data.frame(meta[id,])
row.names(new_df)
bix_yield_data<-merge(new_df$BIX, yield, by.x = "row.names", by.y = "row.names")

plot(h1$Yield~h1$BIX)
rcorr(bix_yield_data$x,bix_yield_data$`df_yield$SampleYield`, type = "spearman")

g<-ggplot(bix_yield_data, aes(bix_yield_data$x,bix_yield_data$`df_yield$SampleYield`))
g<-g + geom_point(color = "steelblue")
g <- g + ylab("Bacterial Yield") + xlab("BIX") 
g<-g+theme(axis.title=element_text(size=14))

#g<-g + geom_smooth(method = "lm")  


ok<-sub_yield$SampleYield[c(1:43)]
sub_bixm$yield<-ok
try<-cbind(sub_bixm$yield,sub_bixm$BIX)
mtry<-as.matrix(try)
df_y_bix<-as.data.frame(mtry)
#out<-aov(ok~sub_bixm$BIX)
#plot(ok~sub_bixm$BIX)
#summary(out)


###HIX###
attach(bixm)
hmmx<-tapply(HIX, list(Treatment, Water), mean)
hsmx<-tapply(HIX, list(Treatment, Water), sd) 

B<-(max(hmmx)+max(hsmx))


barhix <- barplot(height = hmmx,
                  beside = TRUE, las = 1,
                  ylim = c(0, 20),
                  cex.names = 0.9,
                  main = "HIX",
                  ylab = "HIX",
                  xlab = "Lake",
                  border = "black", axes = TRUE,
                  legend.text = TRUE,
                  args.legend = list(title = "Sterilization", 
                                     x = "topleft",
                                     cex = .9))

segments(barhix, hmmx - hsmx, 
         barhix, hmmx + hsmx, lwd = 1.5)

arrows(barhix, hmmx - hsmx, 
       barhix, hmmx + hsmx, 
       angle = 90, lwd = 1.5, code = 3, length = 0.05)


##Abundance
ab<-read.table("ab.txt", header = TRUE, sep ="\t")
ab$Medium<-gsub("Björklinge", "Långsjön", ab$Medium)

a=aov(ab$max_abund~ab$Medium*ab$Sterilization)
c=lm(ab$max_abund~ab$Medium*ab$Sterilization)
b=Anova(c, type =3)

lsmeans(c, specs='ab$Medium')


TukeyHSD(a, "ab$Medium")




ammx<-tapply(ab$max_abund, list(ab$Sterilization, ab$Medium), mean)
asmx<-tapply(ab$max_abund, list(ab$Sterilization, ab$Medium), sd) 

blacktowhite4 <- c("#333333","#777777","#CCCCCC","#FFFFFF")
blacktogrey4 <- c("#111111","#444444","#AAAAAA","#DDDDDD")

me<-melt(ammx)
se<-melt(asmx)
me$sd<-se$value
colnames(me)<-c("Sterility", "Lake", "Mean", "sd")



###ggplot graph TOC&Abundance###
ggab<-ggplot(me, aes(x=me$Sterility, y=me$Mean, ymin=me$Mean-me$sd, ymax=me$Mean+me$sd, fill=me$Lake))
ggab<-ggab + geom_bar(stat="identity", position = "dodge", color="black")
ggab<-ggab + theme_bw(20)
ggab<-ggab + scale_fill_manual(values=blacktogrey4, name=NULL)
ggab<-ggab + labs(x=NULL, y=expression(paste("BA (cells mL" ^-1,")")))
ggab<-ggab + geom_errorbar(position =position_dodge(width=0.9), width=0.28, colour="#666666")
ggab<-ggab + theme(panel.border = element_blank(), axis.line = element_line(),legend.title = element_blank()) 
ggab<-ggab + theme(axis.text = element_text(size = 24, color ="black"), axis.title = element_text(size = 27), legend.text = element_text(size = 24))
ggab<-ggab + theme(legend.direction = "horizontal", legend.position = "bottom")
ggab<-ggab + scale_x_discrete(label=c("0.1", "0.2","AC","AC2"))


meanmx<-tapply(meta$TOC, list(meta$Treatment, meta$Water), mean)
sdmx<-tapply(meta$TOC, list(meta$Treatment, meta$Water), sd)

meltmean<-melt(meanmx)
sdmlt<-melt(sdmx)                    
meltmean$sd<-sdmlt$value
colnames(meltmean)<-c("Sterility", "Lake", "Mean", "sd")

str(meltmean)
meltmean$Lake<-gsub("Bjorklinge", "Långsjön", meltmean$Lake)
meltmean$Lake<-gsub("Lotsjon", "Lötsjön", meltmean$Lake)
meltmean$Lake<-gsub("Ramsjon", "Ramsjön", meltmean$Lake)
meltmean$Lake<-gsub("Siggefora", "Siggeforasjön", meltmean$Lake)

ggt<-ggplot(meltmean, aes(x=meltmean$Sterility, y=meltmean$Mean, ymin=meltmean$Mean-meltmean$sd, ymax=meltmean$Mean+meltmean$sd, fill=meltmean$Lake))
ggt<-ggt + geom_bar(stat="identity", position = "dodge", color = "black")
ggt<-ggt + theme_bw(20)
ggt<-ggt + scale_fill_manual(values=gr_bl4, name="")
ggt<-ggt + labs(x=NULL, y=expression(paste("DOC mg C mL" ^-1)))
ggt<-ggt + scale_x_discrete(labels=c("0.1", "0.2", "AC", "AC2", "Original"), name=NULL)
ggt<-ggt + geom_errorbar(position =position_dodge(width=0.9), width=0.28, colour="#666666")
ggt<-ggt + theme(panel.border = element_blank(), axis.line = element_line(),legend.title = element_blank()) 
ggt<-ggt + theme(axis.text = element_text(size = 24, color = "black"), axis.title = element_text(size = 27), legend.text = element_text(size = 24))
ggt<-ggt + theme(legend.direction = "horizontal", legend.position = "bottom")

gr_bl4<-colorRampPalette(c("Black", "Gray"))(4)
bw5_color<-c("#000000", "#444444", "#999999", "#CCCCCC", "#FFFFFF")
bw4_color<-c("#000000", "#666666", "#BBBBBB", "#FFFFFF")

B<-(max(ammx)+max(asmx))
barabn <- barplot(height = ammx,
                  beside = TRUE, las = 1,
                  ylim = c(0, B),
                  cex.names = 1.2,
                  col=blacktowhite4,
                  main = "Bacterial abundance",
                  # ylab = "Abundance / mL",
                  # xlab = "Lake",
                  border = "black", axes = TRUE,
                  legend.text = TRUE,
                  args.legend = list(title = "Sterilization", 
                                     x = "topleft",
                                     cex = 1.1, bty = "n"))

mtext(expression(paste(plain("Cell mL") ^ plain("-1"))), side=1.5, line=3, cex=1.2)
mtext("Lake", side=1, line=2.2, cex=1.2)

segments(barabn, ammx - asmx, 
         barabn, ammx + asmx, lwd = 1.5)

arrows(barabn, ammx - asmx, 
       barabn, ammx + asmx, 
       angle = 90, lwd = 1.5, code = 3, length = 0.05)


###Percentage of carbon utilized calculation###
ab$fgC<-ab$max_abund*20

write.table(ab, "fgC_calc_start.txt", sep="\t")

test<-ab$fgC*(1*10^-12)
concentration
meta$  


