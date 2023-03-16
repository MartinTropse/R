"""
# logic regression parameters:
# type=3, using logistic regression model
# ntrees=2, number of logic trees to be fit
# select=1, type of model selection to be carried out: (1) fit a single model, (2) fit multiple models, (3) cross-validation, (4) null-model permutation test, (5) conditional permutation test, (6) a greedy stepwise algorithm, or (7) Monte Carlo Logic Regression (using MCMC). 
# nleaves =10, maximum number of leaves to be fit in all trees combined

Detailed description about the logistic regression model and logreg library: 
https://rdrr.io/cran/LogicReg/    #Library documentation 
https://rdrr.io/cran/LogicReg/man/logreg.html #Description of the specific model function 

#Documention by the authors describing  the theory (with examples) and implementation 
https://research.fredhutch.org/content/dam/stripe/kooperberg/ingophd-logic.pdf  #
"""

rm(list=ls())

library(LogicReg)
library(ggplot2)
library(RVAideMemoire)
library(lubridate)
library(tibble)
library(reshape2)
library(stringr)

hostName = c("Beaver","Human", "Reindeer")  
hostAbrv = c("B", "H", "R")
hostDf = data.frame(hostAbrv, hostName)
hostBinaryCutoff = 0.5
ntree = 1
nleav = 2

#Set the randoms seed, the second num value while decided how many of each host model that is created
randSeeds = sample.int(100, 10, replace = FALSE)

#Change folder to export location (sub directory will be created within it with the results)
setwd("C:/Base/Project/ECWA-NOR/PCR_Sekvenser/ECWANOR_202212/alignment/input_ML")

#Slightly complicated way to create timestamp and modify it twice in the same line
foldName=gsub(" ","_",gsub("(-|:)","",paste0("HostModels_",substr(Sys.time(), 1, nchar(as.character(Sys.time()))-3))))
dir.create(foldName)
#Set the current work directory to where
setwd(file.path(getwd(),foldName))

for(aSeed in randSeeds){
  set.seed(aSeed)
  for(host in hostAbrv){
    animal = host #Decides the host model to be trained                
    #Change to location where you have input data
    infile = "C:/Base/Project/ECWA-NOR/PCR_Sekvenser/ECWANOR_202212/alignment/input_ML/202301/AllSamples_ompF_csgD_202301_Input.csv"
    raw<-read.table(infile,sep=",",header=TRUE, stringsAsFactors = FALSE) ## makes everything tab-separated?
    
    #Data cleaning steps 
    raw=as.data.frame(apply(raw, 2, function(x) as.factor(x)))
    raw[raw=="TRUE"]<-"T"
    raw$Sample=gsub(" ","",raw$Sample)
    row.names(raw)=raw$Sample
    
    #List of sample categories, for instance B=Beaver U=unknown. 
    #Also assumes that samples of respective category starts with this letter. 
    sources =c('B', 'H', 'R','U') 
    host_partition = NULL
    
    comparison=sources
    labels=substr(raw[,1],1,1) ## so does this take the first letter of each isolate (the host) and create a vector indicating the host label
    check=which(labels==animal) ## Returns the rows that matches the host letter
    
    #creates a vector of a length equal to the number of samples
    dataindex= vector(length=length(labels)) 
    
    #This fills the dataindex with TRUE values if the samples belong to any category (H, R, B)
    for (i in 1:length(labels)){
      print(i)
      if  (length(which(c(animal, comparison)==labels[i]))>0) {
        dataindex[i]=TRUE
      }
    }
    
    # eliminate rows not fitting with any host category (if any)
    raw=raw[dataindex,]
    
    #Creates a bool vector that tells if the sample belongs to modelhost  
    outcome = labels == animal
    
    #Section for identifying SNP nucleotides, by counting how many times each nucleotide occurs in a row
    numbers=vector(length=dim(raw)[2]) ## this will create a vector of length equal to the number of columns (i.e., the length of the sequence to be analyzed) 
    
    #This checks in each column, how many times the values are same as the first row in that value.
    #With the purpose to see if all rows are same in respective column.
    for (i in 1:length(numbers)){
      numbers[i]= sum(raw[,i]== raw[1,i])  
    }
    
    #Convert the numeric list to boolean, depending if it is same length as total rows. 
    #This is used to find SNPs and then deselect columns with only one nucleotide (in next row).  
    same=(numbers==dim(raw)[1])
    
    ## This selects SNP columns, and add a start column which indicate if modelhost or not
    data=cbind(outcome,raw[!same]) 
    data$outcome=as.factor(data$outcome)
    
    ### analysis procedure, construct holders for the data for logic regression 
    binA=NULL
    binC=NULL
    binG=NULL 
    binT=NULL
    binM=NULL
    first=TRUE
    
    #Create boolean tables based on the presence abscense of nucleotides and gaps. 
    #binA will for instance go through 
    for (i in 1:length(colnames(data)) ){  #if fail, check if first column is sample name and move to second
      if (first) {
        binA=(data[i]=='A')
        binC=(data[i]=='C')
        binG=(data[i]=='G')
        binT=(data[i]=='T')
        binM=(data[i]=='-')
        first=FALSE
      }
      else  {
        binA <- cbind(binA, data[i]=='A')
        binC <- cbind(binC, data[i]=='C')
        binG <- cbind(binG, data[i]=='G')
        binT <- cbind(binT, data[i]=='T')
        binM <- cbind(binM, data[i]=='-')
      }
    }
    
    # assign names to the binary columns.
    colnames(binA) <- paste(colnames(binA),'_A',sep='')
    colnames(binC) <- paste(colnames(binC), '_C', sep='')
    colnames(binG) <- paste(colnames(binG), '_G', sep='')                
    colnames(binT) <- paste(colnames(binT), '_T', sep='')
    colnames(binM) <- paste(colnames(binM), '_-', sep='')
    
    # combine all the binary data by columns  
    tmpbin=cbind(binA, binC, binG, binT, binM)
    
    ix=0
    bin=rep(NULL, dim(data)[1])
    
    #Eliminate zeros columns, i.e. columns that never had a specificed nucleotid in any sample.  
    #For instance column csgD_100 in binA, if csgD_100 never had an "A" in any of the samples.
    for (i in 2:dim(tmpbin)[2]){
      if (sum(tmpbin[,i])>0 ){
        ix=ix+1
        bin=cbind(bin, tmpbin[,i]) 
        colnames(bin)[ix]=colnames(tmpbin)[i]
        print(paste("Stored column position:",  i))
      }  
    }
    #Fit a logistic regression model.   
    SearchIterations <- 100000
    #Decides the prediction strength threshold for when a sample is classified as a host. 
    
    
    #Settings for the algorithm searching for the local/global maximum on the likelihood curve
    Annealing <- logreg.anneal.control(start=3.5, end=-4, iter=SearchIterations, update= 10000) 
    TreeControl <- logreg.tree.control(treesize=10, opers=1, minmass=0)    ## default setting
    
    wbin = bin[grepl("^U.+",row.names(bin)),] #Store water samples bin
    bin = bin[!(grepl("^U.+",row.names(bin))),] #Removes water sample prior to training data randomization 
    
    subID=sort(sample(1:dim(bin)[1],0.8*dim(bin)[1], replace=FALSE)) #Randomly select 80% of samples/rows for training set
    allID=seq(1, dim(bin)[1],1) #Takes out all "row values"
    oppID=allID[!(allID %in% subID)] #Takes out the remaining 20% of the samples for testing
    sbin=bin[subID,] #Selects train data
    tbin=bin[oppID,] #Select test data
    soutcome=outcome[subID]
    
    #Train/test the models
    logregfit0<-logreg(soutcome, bin=sbin, type=3, select=1, ntrees= ntree, nleaves=nleav, anneal.control=Annealing, tree.control=TreeControl)
    logregfit1<-logreg(soutcome, bin=sbin, type=3, select=2, ntrees=c(1,ntree), nleaves=c(1,nleav), oldfit = logregfit0,anneal.control=Annealing, tree.control=TreeControl)
    logregfit2<-logreg(select = 3, oldfit = logregfit1)
    
    plot(logregfit2) # Plot with test score from k-fold validations of all models 
    #summary(logregfit2)
    
    #Summarize mean cv-test score for respective tree and leaf 
    meanCVTest = tapply(logregfit2$cvscores$test,list(logregfit2$cvscores$ntree,logregfit2$cvscores$nleaf),mean) #Mean of CV-score values per leave and tree
    #Find the lowest CV-avg test value, ~i.e. typically the best performing model
    minCvTestAvg = round(min(meanCVTest, na.rm = TRUE),3)  
    
    #SDCVTest = tapply(logregfit2$cvscores$test,list(logregfit2$cvscores$ntree,logregfit2$cvscores$nleaf),sd) #SD of CV-score values per leave and tree
    #maxCvTestAvg = meanCVTest == max(meanCVTest, na.rm = TRUE) #Find the highest CV-value, normally not necessary
    
    logregfit2$cvscores$test.ave = round(logregfit2$cvscores$test.ave,3)
    lowestCVMetaData = logregfit2$cvscores[(logregfit2$cvscores$k == 10) & (logregfit2$cvscores$test.ave == minCvTestAvg),]
    lowestCVModelPos = as.numeric(row.names(lowestCVMetaData))/10
    
    #Meta data for lowest CV-model
    print(lowestCVMetaData)
    
    #Choose the model based on the cv-test evaluation plot above 
    cvBest_model<-logregfit1$alltrees[[lowestCVModelPos]] #Lowest cv-test score k-fold validation test.  
    cvBestPredict=predict(logregfit1,newbin=tbin)
    
    #Creates tree and leaf count variable from lowest CV-model
    lowCVTree = lowestCVMetaData$ntree
    lowCVLeaf = lowestCVMetaData$nleaf
    colNameBestModel = paste0("tr",lowCVTree,".lf",lowCVLeaf)
    
    #Prediction / validation steps
    bestModel=cvBestPredict[colNameBestModel] #Change to the used model
    bestModel=cbind(as.data.frame(substr(labels[oppID], 1, 1)),bestModel)
    names(bestModel) <-c("HostCatg","Prediction")
    
    #Shows the mean prediction value per host category 
    tapply(bestModel$Prediction, bestModel$HostCatg, mean)
    
    bestModel$BinaryPred=ifelse(bestModel$Prediction>hostBinaryCutoff, 1,0)
    bestModel$BinaryHost=ifelse(bestModel$HostCatg == animal, animal, "N")    
    
    #Creates confusion matrix 
    conMx=as.data.frame(matrix(nrow = 2, ncol = 2), row.names = c(animal, "N"))
    names(conMx) = c("0","1")
    
    #Summarize the predictions of the model 
    truePos = length(bestModel$BinaryPred[which(bestModel$BinaryHost == animal & bestModel$BinaryPred==1)])
    falseNeg = length(bestModel$BinaryPred[which(bestModel$BinaryHost == animal & bestModel$BinaryPred==0)])
    trueNeg = length(bestModel$BinaryPred[which(bestModel$BinaryHost == "N" & bestModel$BinaryPred==0)])
    falsePos = length(bestModel$BinaryPred[bestModel$BinaryHost == "N" & bestModel$BinaryPred==1])
    
    conMx[1,1] = falseNeg
    conMx[1,2] = truePos  
    conMx[2,1] = trueNeg
    conMx[2,2] = falsePos
    
    #Accuracy
    Accurarcy=(conMx[1,2]+conMx[2,1])/sum(conMx)
    
    #Sensitivity TP/(TP/FN). 
    sensRes=conMx[1,2]/(conMx[1,2]+conMx[1,1])
    #Specificy TN/(TN+FP)
    specRes=conMx[2,1]/(conMx[2,1]+conMx[2,2])
    
    #Variables (tree/leaf/host) to create export string 
    txtTree = paste0("T", lowCVTree)
    txtLeaf = paste0("L", lowCVLeaf)
    hostOrg = hostDf$hostName[hostDf$hostAbrv == animal]
    
    #Name for the results summary file  
    resSumry=paste0("ResultSumry_",hostOrg,txtTree,txtLeaf,"_",lubridate::as_date(Sys.time()),"_predCT_",hostBinaryCutoff,"_seed_",aSeed,".txt")
    #Determines where the export file goes
    
    permRes=perm.anova(Prediction ~ HostCatg, data=bestModel, nperm = 999,
               progress = TRUE)
    
    write.csv(bestModel, paste0(substr(resSumry,1,nchar(resSumry)-4),".csv"), row.names = FALSE)
    
    #Creates a summary file by capturing print-messages and export them as a txt-file
    sink(resSumry)
    print(resSumry)
    print(cvBest_model)
    print(paste("Model accuracy:", Accurarcy))
    print(paste("Model sensitivity:", sensRes))
    print(paste("Model specificty:", specRes))
    print("Confusion matrix prediction:")
    print(conMx)
    print("Mean logistic-regression prediction by host")
    print(tapply(bestModel$Prediction, bestModel$HostCatg, mean))
    print("Resul of permutation ANOVA:")
    print("Prediction ~ HostCatg, data=bestModel, nperm = 999, progress = TRUE") #Changes to the set perm anova arguments
    print(permRes)
    sink()
  }
}

"""
###Water sample classification###
Similiary to the predict steps above, but using wbin (water samples)
Also does not validate the outcome, since 'true' true/false is unknown

Repeat this step for each host model trained (and change the output name!)
"""

#Determines the prediction strength needed to classify the sample.  
waterBinaryCutoff = 0.5

cvWestPredict=predict(logregfit1,newbin=wbin)

westModel=as.data.frame(cvWestPredict$tr3.lf14)  #Change to the used model 
names(westModel) = "Prediction"
westModel$BinaryPred=ifelse(westModel$Prediction>waterBinaryCutoff, 1,0)
row.names(westModel)=row.names(wbin)

#Creates sample column at first position
westModel=add_column(westModel, row.names(westModel),.before = "Prediction")
waterSumry=paste0("WaterClassified_",resSumry)
names(westModel) = c("Sample", "Prediction", "BinaryPrediction")
write.table(westModel, waterSumry,fileEncoding = "UTF-8", sep = "\t", row.names = F)


"""
#Evaluates water samples based on the predicitons of previous models. 

The input of this section is summary of prediction values and binary classification
from previous models. This is put togther manually from the different model exports 
in the previous section. 
"""

setwd("C:/Base/Project/ECWA-NOR/PCR_Sekvenser/ECWANOR_202212/alignment/input_ML/202301")
list.files()
df=read.csv("WaterSample_AllModels_Input.csv", fileEncoding = "UTF-8")

df[,2:4]=sapply(df[,2:4], function(x) as.numeric(gsub(",",".", x)))

bvCount=sum(df$BinaryPred_B)
hmCount=sum(df$BinaryPred_H)
rnCount=sum(df$BinaryPred_R)

allPredict=melt(df[,1:4], id.vars = c("Sample"), value.name = "Prediction")
allBinary=melt(df[,c(1,5:7)], id.vars = c("Sample"), value.name = "Binary")
allComb=cbind(allPredict, allBinary[,3])

allComb$variable=gsub("ModelPred_","",allComb$variable)
names(allComb)[4] = "BinaryPred"

#Loop that compares and classify the binary outcome of the other models.
hostCol = list()
newSampleCol = list()

#Add the specificity of respective model
for(smp in unique(allComb$Sample)){
  smpPos = grep(smp, allComb$Sample)
  tempDf = allComb[smpPos,] 
  if(sum(tempDf$BinaryPred) == 0){
    hostCol = append(hostCol, "Undetermined")
    newSampleCol = append(unlist(newSampleCol), smp)
    print(paste(smp, "was classified as undetermined"))
  }
}

for(smp in unique(allComb$Sample)){
  smpPos = grep(smp, allComb$Sample)
  tempDf = allComb[smpPos,] 
  if(sum(tempDf$BinaryPred) == 1){
    tempDf = tempDf[grep(1, tempDf$BinaryPred),]
    hostCol = append(hostCol, unlist(tempDf$variable))
    newSampleCol = append(unlist(newSampleCol), smp)
    print(paste(smp, "was classified as", tempDf$variable))
    #Sys.sleep(0.5)
  }
}

"""
This loop compares water samples where mulitple models have given positive predictions.
It takes the prediction strength of the sample, and the models specifity, and combine
it into an evulation value. 

This is compared to a 'difference value' which is set below (0.2 by default). 

The highest evulation value of the sample is then substracted from all evulation values 
(i.e all positive model predictions for that sample). 

All evaulation values that remains above -0.2 after substraction, are the included 
as a joint positive prediction. If only one value remains, the sample get the prediction
from that model. 
"""
#Add the specificity of respective model 
Host = c("B", "H","R")
Specificity = c(1,0.8461, 0.9705)

hostFrame=data.frame(Host, Specificity)
hostFrame=hostFrame[order(hostFrame$Host),]

diffVal = 0.2

for(smp in unique(allComb$Sample)){
  smpPos = grep(smp, allComb$Sample)
  tempDf = allComb[smpPos,]
  tempDf = tempDf[order(tempDf$variable),]
  if(sum(tempDf$BinaryPred) > 1){
    print(paste("Comparing positive predictions in sample:", smp))
    tempDf=tempDf[grep(1, tempDf$BinaryPred),]
    posHosts=unique(stringr::str_sub(tempDf$variable, -1))
    regxPtn=paste0("(",unlist(posHosts),"$", ")",collapse = "|")
    tempDf$Evaluation=tempDf$Prediction[grep(regxPtn, tempDf$variable)]+hostFrame$Specificity[grep(regxPtn, hostFrame$Host)]
    print(tempDf)
    tempDf$Evaluation=tempDf$Evaluation-max(tempDf$Evaluation)
    tempDf=tempDf[tempDf$Evaluation>-diffVal,]
    prediction=paste0(tempDf$variable, collapse ="|")
    hostCol = append(hostCol,unlist(paste0(tempDf$variable, collapse ="|")))
    newSampleCol = append(unlist(newSampleCol), smp)
    print(paste(smp,"was labeled as", paste0(tempDf$variable, collapse ="|")))
  }
}  

classFrame=data.frame(newSampleCol,unlist(hostCol))
names(classFrame)  = c("Sample","Classification")
classFrame=classFrame[order(classFrame$Sample),]

#Cleaning steps prior to export
classFrame$Classification = gsub("_R","ReinDeer", classFrame$Classification)
classFrame$Classification = gsub("_H","Human", classFrame$Classification)
classFrame$Classification = gsub("_B","Beaver", classFrame$Classification)
classFrame$Classification = gsub("Prediction","", classFrame$Classification)

#Checks that sample names/order is identical in original- and 
#classification dataframe, then proceed to merge them and export.
if(sum(df$Sample == classFrame$Sample)==dim(df)[1]){
  exprtFrame=cbind(classFrame, df[,grep("^Prediction.+", names(df))])
  print(table(exprtFrame$Classification))
  write.csv(exprtFrame, "classified_WaterSamples.csv", row.names = FALSE)
}