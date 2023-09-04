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
library(foreach)
library(vegan)
library(dplyr)

"""
Defines the function that goes into the result file, finds the model string, and replace it with a biomarker string. 
From readability viewpoint it would be better if the function was loaded from a library outside of the main script, but that is a detail :o 
"""

addBiomarker <- function(file_path) {
  # Initialize an empty list to store the extracted integers
  integer_list <- list()
  
  # Open the file for reading
  file_conn <- file(file_path, "r")
  
  # Read the lines from the file
  lines <- readLines(file_conn)
  
  # Close the file connection
  close(file_conn)
  
  # Open the file for writing (to overwrite the original file)
  file_conn <- file(file_path, "w")
  
  for (line in lines) {
    # Check if the line contains capital X followed by integers
    if (grepl("X\\d+", line)) {
      biomarkLine <- line
      # Extract integer values following capital X
      strLine <- str_extract_all(line, "X\\d+")
      strLine <- str_extract_all(strLine, "\\d+")
      bioIntList <- as.numeric(unlist(strLine))
      
      for (bioInt in bioIntList) {
        biomarkLine <- gsub(paste0("X", bioInt), colnames(bin)[bioInt], biomarkLine)
      }
      
      # Replace the original line with the modified biomarkLine
      cat(biomarkLine, file = file_conn, sep = "\n")
    } else {
      # If the line doesn't contain the pattern, write it as is to the file
      cat(line, file = file_conn, sep = "\n")
    }
  }
  
  # Close the file connection
  close(file_conn)
}


#These needs to be in matching order
hostName = c("Beaver","Human", "Reindeer")  
hostAbrv = c("B", "H", "R")

hostDf = data.frame(hostAbrv, hostName)

#Set the cutoff value for when prediction becomes positive/negative
hostBinaryCutoff = 0.5
waterBinaryCutoff = 0.5

#Number of trees and leafs used in the logic-reg models. 
ntree = 1
nleav = 4

#Value used to separate between host-models, when mulitiple positive prediction occurs in unknown water samples
diffVal = -0.2

#Set the randoms seed, the second num value while decided how many of each host model that is created
randSeeds = sample.int(100, 2, replace = FALSE)

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
    cvBestPredict = predict(logregfit1,newbin=tbin)
    
    #Creates tree and leaf count variable from lowest CV-model
    lowCVTree = lowestCVMetaData$ntree
    lowCVLeaf = lowestCVMetaData$nleaf
    colNameBestModel = paste0("tr",lowCVTree,".lf",lowCVLeaf)
    
    #Prediction / validation steps
    bestModel = cvBestPredict[colNameBestModel] 
    bestModel = cbind(as.data.frame(substr(labels[oppID], 1, 1)),bestModel)
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
    resSumry=paste0("Seed_",aSeed,"Sumry_",hostOrg,txtTree,txtLeaf,"_",lubridate::as_date(Sys.time()),"_predCT_",hostBinaryCutoff,".txt")
    #Determines where the export file goes
    
    permRes=perm.anova(Prediction ~ HostCatg, data=bestModel, nperm = 999,
               progress = TRUE)
    #Add sample name to the 
    bestModel$Sample = row.names(tbin)
    bestModel <- bestModel %>% select("Sample", everything())
      
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
    print("Result of permutation ANOVA:")
    print("Prediction ~ HostCatg, data=bestModel, nperm = 999, progress = TRUE") #Changes to the set perm anova arguments
    print(permRes)
    sink()
    pdf(file = paste0(substr(resSumry,1,nchar(resSumry)-4),".pdf"), width = 8, height = 6)
    plot(logregfit2)
    dev.off()
    
    addBiomarker(resSumry)
    
    #Classifying water samples using best model from host-test, and exports the results 
    cvWatrPredict = predict(logregfit1,newbin=wbin)
    watrModel = cvWatrPredict[colNameBestModel] 
    names(watrModel) = "Prediction"
    watrModel$BinaryPred=ifelse(watrModel$Prediction>waterBinaryCutoff, 1,0)
    row.names(watrModel)=row.names(wbin)
    watrModel=add_column(watrModel, row.names(watrModel),.before = "Prediction")
    waterFile = paste0(substr(resSumry,1, nchar(resSumry)-4),"WaterClsf", ".csv")
    names(watrModel) = c("Sample", paste0(animal, "_Prediction"), paste0(animal,"_BinaryPrediction"))
    write.table(watrModel, waterFile,fileEncoding = "UTF-8", sep = ",", row.names = F)
    #
  }
}

"""
Automize the water classified evaluation based on file results exported in host-loop above

Goes through each seed and host-model. 
Combine the prediction values for each host, identify respective specificity value
Cleans and re-writes summary-textfiles 
"""
#setwd("C:/Base/Project/ECWA-NOR/PCR_Sekvenser/ECWANOR_202212/alignment/input_ML/HostModels_20230402_0939")

fileList = list.files()

rplcList = (gsub("^Seed_","", fileList, ignore.case = TRUE))
rplcList = rplcList[grepl("Sumry", rplcList)]
seedList = sort(unique((gsub("Sumry..+", "", rplcList))))
  
anmRgx = paste0(hostName[1],".+Water")

for(secSeed in seedList){
  csvSeed = fileList[grepl(paste0("Seed_",secSeed,".+\\.csv"),fileList)] 
  txtSeed = fileList[grepl(paste0("Seed_",secSeed,".+\\.txt"),fileList)]
  #Creates an empty dataframe, loads temporarily a water classifaction file to decided row lenght of empty df. 
  anmFile = csvSeed[grepl(anmRgx, csvSeed)]
  watrDf = as.data.frame(matrix(nrow = dim(read.csv(anmFile, sep = ","))[1]))
  tempDf = read.csv(csvSeed[grepl(paste0(hostName[1],".+Water"),csvSeed)], sep = ",")
  row.names(watrDf) = tempDf$Sample
  rm(tempDf)
  
  foreach(anmNme = hostName, anmAbr = hostAbrv) %do% {  #hostName is created at the start of script
    hostDf = read.csv(csvSeed[grepl(paste0(anmNme,".+Water"), csvSeed)], sep = ",")
    hostDf = hostDf[,c(paste0(anmAbr, "_Prediction"), paste0(anmAbr,"_BinaryPrediction"))]
    watrDf = cbind(watrDf, hostDf)
  }  
  watrDf = watrDf[,which(names(watrDf) != "V1")]
  
  #Section that cleans txt-data and locate specifity value from respective host-run
  colPtrn = c("(Confusion matrix prediction:|Model accuracy:.+|(Resul of permutation ANOVA|Result of permutation ANOVA)|Permutation.+| Mean logistic-regression.+)")
  spfcDf = as.data.frame(matrix(nrow = 1), ncol = 0)
  
  foreach(anmNme = hostName, anmAbr = hostAbrv) %do% {  #hostName is created at the start of script
    txtFile = txtSeed[grepl(paste0(anmNme,".+\\.txt"),txtSeed)]
    hostTxt = read.table(txtFile, sep = "\n")
    txtDf = as.data.frame(apply(hostTxt,1, function(x) gsub("\\[1\\]","",x)))
    names(txtDf)[1] = "SumryData"
    boolRowlst = grepl(colPtrn, txtDf$SumryData)
    #Add new lines a set positions within the sumry txt-file
    foreach(rwBool = boolRowlst, rwPos = seq(length(boolRowlst))) %do%{
      if(rwBool){
        txtDf[rwPos,] = paste("\n",txtDf[rwPos,])
      }
    }
    txtDf = as.data.frame(apply(txtDf,1, function(x)gsub("^ +","",x)))
    txtDf = as.data.frame(apply(txtDf,1, function(x)gsub("\n +","\n",x)))
    #Exports and overwrites previous sumry txt-files
    write.csv(txtDf, txtFile, col.names = FALSE,row.names = FALSE, quote = FALSE)
    
    #Locates the specificty value for respective host-model
    spfcVal = stringr::str_extract(txtDf[apply(txtDf,1,function(x) grepl("specificty", x)),], "([0-9]\\.[0-9]{1,20}|[0-9])")
    spfcVal = round(as.numeric(spfcVal),4)
    watrDf[paste0(anmAbr,"_Specificty")] = rep(spfcVal, 1, dim(watrDf)[1])
  }
  watrDf = watrDf[,which(names(watrDf) != "V1")]
  watrDf = watrDf[,order(names(watrDf))]
  write.csv(watrDf, paste0("WaterClassified_AllHosts", "_Seed",secSeed,".csv"))
}


###Evaluate the exported waterClassified data from all host-models
for(thrSeed in seedList){
  classFile = paste0("WaterClassified_AllHosts_Seed",thrSeed,".csv")
  print(paste0("Classifying samples within: ",classFile))
  Sys.sleep(2)
  df = read.csv(classFile, fileEncoding = "UTF-8")
  names(df)[1] = "Sample"
  
  colCnt = dim(df)[2]
  rowCnt = dim(df)[1]
  
  df[,2:colCnt]=sapply(df[,2:colCnt], function(x) as.numeric(gsub(",",".", x)))
  
  predCol = names(df)[grepl("^[A-Z]_Pred.+", names(df), ignore.case = TRUE)]
  biprCol = names(df)[grepl("^[A-Z]_Binary.+", names(df), ignore.case = TRUE)]
  specCol = names(df)[grepl("^[A-Z]_Spec.+", names(df), ignore.case = TRUE)]
  
  #Melts the dataset into long format 
  mltPredict = melt(df[,c("Sample",predCol)], id.vars = c("Sample"), value.name = "Prediction")
  mltBinary = melt(df[,c("Sample", biprCol)], id.vars = c("Sample"), value.name = "Binary")
  mltSpec = melt(df[,c("Sample", specCol)], id.vars = c("Sample"), value.name = "Specificity")
  mltClass = cbind(mltPredict, mltBinary[,3], mltSpec[,3])
  names(mltClass) = c("Sample","Host","Prediction","BinaryPred","Specificity")
  
  foreach(hstNm = hostName, hstAbr = hostAbrv) %do% {
    mltClass$Host = gsub(paste0(hstAbr, "_Prediction"),hstNm,mltClass$Host)
  }
  
  classDf = data.frame(Sample = df$Sample, Classified = NA)
  
  for(smpl in unique(mltClass$Sample)){
    smpPos = grep(smpl, mltClass$Sample)
    tempDf = mltClass[smpPos,]
    tempDf = tempDf[order(tempDf$Host),]
    if(sum(tempDf$BinaryPred) > 1){
      tempDf=tempDf[grep(1, tempDf$BinaryPred),]
      tempDf$Evaluation = tempDf$Prediction+tempDf$Specificity
      tempDf$Evaluation = tempDf$Evaluation-max(tempDf$Evaluation)
      tempDf = tempDf[tempDf$Evaluation>diffVal,-dim(tempDf)[2]]
      classDf[classDf$Sample == smpl, "Classified"] = paste0(tempDf$Host, collapse = "|")
      print(paste0("Sample ",smpl, " was classified as ", classDf[classDf$Sample == smpl, "Classified"]))
    }
    if(sum(tempDf$BinaryPred) == 1){ 
      tempDf= tempDf[grep(1, tempDf$BinaryPred),]
      classDf[classDf$Sample == smpl, "Classified"] = paste0(tempDf$Host, collapse = "|")
      print(paste0("Sample ",smpl, " was classified as ", classDf[classDf$Sample == smpl, "Classified"]))
      }
    if(sum(tempDf$BinaryPred) == 0){
      classDf[classDf$Sample == smpl, "Classified"] = "Unclassified"
      print(paste0("Sample ",smpl, " was classified as ", classDf[classDf$Sample == smpl, "Classified"]))
    }
  }
  
  df = merge(df, classDf, by.x = "Sample")
  names(df) = gsub("Prediction", "Pred", names(df))
  names(df) = gsub("Binary", "Bin", names(df))
  write.csv(df,classFile, row.names = FALSE)
}