setwd("D:/PhD Hörnan/R/Useful_scripts")

#Scripts useful for data sorting


#Expanded sorting
# sorting examples using the mtcars dataset
attach(mtcars)
# sort by mpg
newdata <- mtcars[order(mpg),] 
# sort by mpg and cyl
newdata <- mtcars[order(mpg, cyl),]
#sort by mpg (ascending) and cyl (descending)
newdata <- mtcars[order(mpg, -cyl),] 
detach(mtcars)

#reorder, looks similiar... 
nba$Name<-with(reorder(Name, PTS)))


###See all objects and remove all objects
objects()
rm(list = ls()) 

##Collets data based on class
#Ex1 takes out columns
out1<-sapply(newdata, is.numeric)
t_data<-newdata[,out1] 


##Levels function##
#Get you a list of all categorize within a factor# 
taxa_lvl<-levels(df$variable)

#One line to change a range of columns to numeric class#
ndata=sapply(data[,1:90], function(x) as.numeric(x))

#This line is sometimes necessary when transforing "values" that are interpreted as factor
#back to a numeric vector.This is because factors are stored internally as integers with a table to give the factor level labels. 
# Just using as.numeric will only give the internal integer codes.
d<-apply(ino_temp[1:359,], 2, function(x) as.numeric(as.character(x)))

#Include all things that are "computable" from a data frame and exclude factors etc. 
id<-sapply(ndata, function(x) is.integer(x) || is.logical(x) || is.numeric(x)) 

##Test all positions inside data frame is lower, higher or equal to a value. 
df[df == 0]<-NA

#This one keeps only values above 0. "melt" is the name of the vector here :p
hold<-melt_h4$bray_distance[melt_h4$bray_distance >0]


#Merge command of two df that joins by row.names# 
h1<-merge(otu,new_tax1, by.x="row.names", by="row.names")

#Ex2 takes out columns or rows depending on the "," . 
new_df <- newdata[,sapply(newdata,is.numeric)]
new_dfa <- newdata[sapply(newdata,is.numeric),]

setwd("D:/PhD Hörnan/Dokument/R/Assignment 2")
newdata<-read.table("GastricCancerData_Lab2.txt", sep="\t", header=T)

###Change the class for a number of columns or rows  
#Greps columns that matches name "x" 
val<-grep("[Vv]alue", colnames(newdata))
#Loops value positions and turns them into class something
for(i in val)
{
  newdata[,i]<-as.numeric(newdata[,i])
}

##Grep combined with a loop. The grep becomes the column the loop iterates the function within.
A <- grep("AffyID|ABS", colnames(newdata))

for(i in A){ 
  print(paste("class:",class(newdata[,i])))
  newdata[,i] <- as.factor(newdata[,i])
  print(paste("class:",class(newdata[,i])))	
  print('---------------')
}

#Print the results, paste also puts the results from "class:", with the results class(data)
#output. 

# grep to values. Then use the not function to switch the false to true, thus hitting
# everything that is not hit by that grepl.
a<-grepl("[Vv]alue|[Aa]ffyID",colnames(newdata))
b<-!a

c<-grepl("[Vv]alue|[Aa]ffyID",colnames(newdata))
dd<-grep("[Gg]en[bB]ank*", colnames(ddata))
ny_data<-ddata[,dd]
d=0

ny_data[2]

vmv<-c(1:6, NA, NA, 9:12)
ifelse(is.na(vmv),0, vmv)
ifelse(is.na(ny_data),0, ny_data)

out<-grepl("[0-9]",ny_data)
num_data<-ny_data[out]


##For loop, to test a value for a condition. The "1:length(data)" makes the loop go from 1
#to whatever length the data has. 
num_data2<-sapply(ny_data, is.na)
num_data3<-!num_data2
number<-ny_data[num_data3]

G1<-na.omit(ny_data)
G2<-ny_data[G1]
length(G2)
length(number)

num_data2<-sapply(ny_data, is.na)
num_data3<-!num_data2
number<-ny_data[num_data3]

##Create matrix, then checks how great a percent of a column/row is NA. 
m1<-c(23, 12, 18, NA)
m2<-c(29, NA, 34, NA)
m3<-c(46, 11, NA, NA)
m4<-c(NA, NA, NA, 12)

matr<-matrix(c(m1,m2,m3,m4), byrow = T, nrow = 4)
matr<-as.data.frame(matr)
matr1<-sapply(matr, is.na)
matr1<-!matr1
B <- 1*matr1
B<-as.data.frame(B)
C<-apply(B, 2,mean)
D<-C>0.6
F1<-matr[,D]
mean(F1,na.rm=T)
####

for(i in 1:length(num_data))
{
  if(num_data[i]<1000000) print(paste(num_data[i],"is low!"))
  if(num_data[i]>1000000) print(paste(num_data[i],"is high!"))
  }


##Loop that checks if a value is higher or lower than set value and pushes it into a 
#pre define vector. Pop takes away the value from the first position in vector, in this case
#1. 

M=1
N=1
P<-vector(mode="numeric",length = 0)

for(i in 1:length(num_data))
{
  if(num_data[i]<1000000) push(N,num_data[i])
  if(num_data[i]>1000000) push(M,num_data[i])
}
MM<-pop(M)
MM[1]

#if(num_data[i]>100000) print(i)


###Subset function. Takes a data frame a creates a subset data frame that correlates
#with a value in two column. In this case the value 1 in column time and L in medium.  
L_1<-subset(meta, meta$Medium == "L" & meta$Time == "1")
#Same this one ask for a value above certain thereshold, useful!
d<-subset(diamonds, diamonds$carat>1.1 & diamonds$cut=='Good')

#To checks if vectors are identical/same where are the differences
#all tests if all components are identical
#which looks at which positions that are deviating. 
which(check != check2)
all(check == check2)


###Subsample 
d5<-c(1:100)
DD5<-sample(d5,size=50,replace=T)
newdata[sample(nrow(newdata),3),]
E5<-sample(nrow(newdata),3,replace=T)



print(sample(1:3))
print(sample(1:3, size=3, replace=FALSE))  # same as previous line
print(sample(c(2,5,3), size=4, replace=TRUE)
      print(sample(1:2, size=10, prob=c(1,3), replace=TRUE))


#Loop and tapply combination that allows to test many separate columns at the same time.
#also use an empty list to store several matrices from the output. 
      
setwd("D:/PhD Hörnan/Experiment/Sterilisering experiment 2014-08/Sterilisering experiment 2014-08/PARAFAC_2016_04/R")
list.files()
komponent<-read.table("FDOM_NC_SOCS_2016_11.txt", sep ="\t", header = TRUE)
gr1<-grep("C.", colnames(komponent))
data<- list()
      
for(i in gr1) 
{
b<-tapply(komponent[,i],list(komponent$X,komponent$X.1),mean)
data[[i]]<-b
}      

try3<-unlist(data)
C1<-as.data.frame(data)
str(C1)

#quick command for greping the opposite as the search line
#grep("[Ori]", TOC$Treatment, invert=T) matching everything that is not Ori

##Stack function##
#Stacks columns into one column and creates a secondary column that indicate the origin of
#the data
setwd("D:/PhD Hörnan/Experiment/Sterilisering experiment 2014-08/R")
temp<-(cbind(meta$HIX,meta$BIX))
str(temp)
dim(temp)
dtemp<-as.data.frame(temp)
stack(dtemp)

###How to create a useful random data set### 
myData <- data.frame(PID = rep(seq(from = 1,
                                   to = 50, by = 1), 20),
                     stress = sample(x = 1:100,
                                     size = 1000,
                                     replace = TRUE),
                     image = sample(c("Happy", "Angry"),
                                    size = 1000,
                                    replace = TRUE),
                     music = sample(c("Disney", "Horror"),
                                    size = 1000,
                                    replace = TRUE)
)


###Loop to convert all of a set number of columnns to a new class
myData <- within(myData, {
  PID   <- factor(PID)
  image <- factor(image)
  music <- factor(music)
})

###Works similar as tapply but gives the output as a data frame###
myData.mean <- aggregate(myData$stress,
                         by = list(myData$PID, myData$music,
                                   myData$image),
                         FUN = 'mean')

##A function that can help check for unbalanced data## 
ezDesign(data = amotu_sub, x = Time,  y = ID) #y is here samples and x is a factor.

##Table function creates a summary of the frequency of values, useful when looking if the
##the data is balanced. The second line select by those frequency in a data frame. 
temp = as.data.frame(table(alpha$ID))
alpha[temp$Freq<2,]

##Creating a data frame from a group of vectors##
indvalsummary <- data.frame(group=gr, indval=iv, pvalue=pv, freq=fr)
#Apply function, good when applying a fucntion on all rows or columns, easy! 
#1 is row and 2 is column
tester <- apply(abund_num, FUN=max, 1)

##Reshape function, from long to wide format## 

set.seed(45)

dat1 <- data.frame(
  name = rep(c("firstName", "secondName"), each=4),
  numbers = rep(1:4, 2),
  value = rnorm(8)
)

##Strsplit, to alter data##
strsplit(as.vector(Group),";"), head,1)


str(dat1)
out<-reshape(dat1, idvar = "name", timevar = "numbers", direction = "wide")
out2=as.data.frame(t(dat1))
#idvar becomes rows and timevar becomes columns. Values need to numeric?


##Connect colors to a factor column##
?gl
library(scales)
dat <- data.frame(Phylum = gl(7,2)) ##gl 
#gl=generate factor levels, first value give the amount of levels and second repitions of those
#can also take length and labels as commands . 

n <- nlevels(dat$Phylum) #Show how many levels a factor has, 7 here
dat.col <- data.frame(Phylum =unique(dat$Phylum),#shows unique levels
                      BactPhyColors =brewer_pal()(n))  ## you can also use rainbow(n)
##Creates a data.frame with one color per level
out<-merge(dat,dat.col)
#Merges via the phylum column i presume

##What the transform function does##
# dataFrame <- transform(dataFrame, newColumnName = some equation)
# So, to get the sum of two columns and store that into a new column with transform(), you would use code such as:

dataFrame <- transform(dataFrame, newColumn = oldColumn1 + oldColumn2)

##command to count amount within all combinations of two factors 
count(attacks, wkday, hour)
table(data)#does something similiar.

#function that drops unused factor levels from a dataset.
x <- droplevels(x)


#Decent example on how to do melt table with multiple factor levels. 
setwd("C:/PhD Hörnan/Experiment/ThePositiveExperiment/R")
library(vegan)

im_otu<-read.table("otu_rarefied_improved.txt", header = T, sep = "\t", row.names = 1)
taxa2<-read.table("new_taxa.txt", sep="\t", header=T, row.names = 1)

# tim_otu<-as.data.frame(t(im_otu))

lake_otu<-grep("Lake", im_otu$Inoculum)
wet_otu<-grep("Wetland", im_otu$Inoculum)
riv_otu<-grep("River", im_otu$Inoculum)

lake<-im_otu[lake_otu,]
wetl<-im_otu[wet_otu,]
riv<-im_otu[riv_otu,]

# im_otu<-as.data.frame(t(im_otu))

#changes taxa2 column to get a different taxonomic level. 
id<-grep("[a-z|A-Z]", taxa2$Phyla)#Remove empty positions by selecting character position
taxa3<-taxa2[id,]
taxa3<-droplevels(taxa3)
df<-as.data.frame(taxa3$Phyla)

row.names(df)<-row.names(taxa3)

riv<-as.data.frame(t(riv))
wetl<-as.data.frame(t(wetl))
lake<-as.data.frame(t(lake))

colnames(df)<-"tax"
riv_df<-merge(riv, df, by.x = "row.names", by.y = "row.names")
lake_df<-merge(lake, df, by.x = "row.names", by.y = "row.names")
wet_df<-merge(wetl, df, by.x = "row.names", by.y = "row.names")
#check that they match, otherwise flip col row. 

#Overview. I separate the data, melt and summarize counts with ddply to get a 
#good format to for heatmap. Additionally i add factors with rep command by using 
#rownames and search patterns. Not sure if there is a better way to do it but it 
#worked pretty well here. It is however situation dependent so you need to tinker 
#a bit with each given data set. 

riv_num<-as.data.frame(sapply(riv_df[,c(-1,-92)], function(x){as.numeric(x)}))
tax<-riv_df[,c(92)]
riv_df2<-cbind(riv_num, tax)
riv_m<-melt(riv_df2, id.vars = 'tax')
#make sure each tax command works and add them in the right order

lake_num<-as.data.frame(sapply(lake_df[,c(-1,-88)], function(x){as.numeric(x)}))
tax<-lake_df[,c(88)]
lake_df2<-cbind(lake_num, tax)
lake_m<-melt(lake_df2, id.vars = 'tax')

wet_num<-as.data.frame(sapply(wet_df[,c(-1,-86)], function(x){as.numeric(x)}))
tax<-wet_df[,c(86)]
wet_df2<-cbind(wet_num, tax)
wet_m<-melt(wet_df2, id.vars = 'tax')

colr<-rep("River", length(riv_m$value))
coll<-rep("Lake", length(lake_m$value))
colw<-rep("Wetland", length(wet_m$value))

wet_m$Inoculum<-colw
lake_m$Inoculum<-coll
riv_m$Inoculum<-colr

b=Sys.time()

w1<-ddply(wet_m, c('variable', 'tax', 'Inoculum'),summarize,counts=sum(value))#counts is the name of the column. 
l1<-ddply(lake_m, c('variable', 'tax', 'Inoculum'),summarize,counts=sum(value))#
r1<-ddply(riv_m, c('variable', 'tax', 'Inoculum'),summarize,counts=sum(value))

big_data<-rbind(w1, l1, r1)
a=Sys.time()
print(a-b)

im_otu<-as.data.frame(t(im_otu))#Make sure im_otu is flipped right
d13<-im_otu$Medium #We use this to get the pattern of the medium. 
d42<-rep(d13, each =56) #makes a vector that repeat the position of each medium/sample 56 times to match the 56 phyla 
big_data$medium<-d42

write.table(big_data, "big_data.txt", sep = "\t")

##Example on how you can take a column, e.g, and make into multiple searhcommand
setwd("C:/PhD Hörnan/Experiment/ThePositiveExperiment/R")
test<-read.table("example.txt", sep = "\t", header=T, row.names=1)

low_taxa<-test$..1<40000 
high_taxa<-test$..1>40000

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


###Easy way to split data according to multiple conditions and allow seperate functions
setwd("C:/PhD Hörnan/Experiment/ThePositiveExperiment/R")
m_plot_t1<-read.table("Growth_curve_positive_data.txt", header = T, sep = "\t")
head(m_plot_t1)
#2 and 3 conditions example
aas<-c(m_plot_t1$Inoculum=="Wetland" & m_plot_t1$variable>3 & m_plot_t1$Diversity>3)
aas<-c(m_plot_t1$Inoculum=="Wetland" & m_plot_t1$Diversity=="1")
bas<-!aas
out1<-m_plot_t1[aas,]
out2<-m_plot_t1[bas,]

#Example of a more complicated condition
t5=c(m_plot_t1$Diversity>3 & m_plot_t1$Abundance >5000 & (m_plot_t1$Medium == 'Mix'|m_plot_t1$Medium == "M4"))
df_temp=m_plot_t1[t5,]

id=grep('Mix|M4', m_plot_t1$Medium) 
m_plot_t2=m_plot_t1[id,]
t3=c(m_plot_t2$Diversity>3 & m_plot_t2$Abundance >5000 & m_plot_t2) 
t4=m_plot_t2[t3,]



dim(m_plot_t1)
dim(out2)
dim(out1)

###Comparing/finding positions in vectors###
identical(x,y)
#will return "true" if two vectors are identical, else returns false. 


###Remove Outliers function 
a=c(1,3,5,6,5,3,4,6,7,1,2,1220)

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

d=remove_outliers(a, na.rm = TRUE)
e=as.numeric(na.omit(d))



#How to categorize values into a factor
v2=rnorm(1000)
v2=as.data.frame(v2)
v2$v3 <- cut(v2$v2, breaks = c(min(v2$v2), -0.7,  0, 0.6, max(v2$v2)),  
             labels = c('low', 'med','medplus', 'high'), right = FALSE)

#Cumulative sum 
data[, 2] <- cumsum(data[, 2])

#Basic for loop syntax
for(x in 1:10){
  print("Testing")}  

#Superscript/elevated text in ggplot
ggplot(mtcars, aes(hp, mpg)) + 
  geom_point() +
  labs(x = bquote('x axis'~(Å^2)), y = "y axis") 

dfan$Normliz= round((dfan$Count - mean(dfan$Count))/sd(dfan$Count), 2)
dfan$logc = ifelse(dfan$Normliz < 0, "below", "above")
dfan$Height= as.factor(dfan$Height)

#A bar chart showing normalized rate of change. 
g2 = ggplot(dfan, aes(x=Height, y=Normliz, label=Normliz))
g2 = g2 + geom_bar(stat='identity', aes(fill=logc), width=.5)
g2 = g2 + scale_fill_manual(name="Height", 
                            labels=c("Above Average", "Below Average"),
                            values = c("above"="#00ba38", "below"="#f8766d"))
g2 = g2 + theme_bw()
g2 = g2 + coord_flip()