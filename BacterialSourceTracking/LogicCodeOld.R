 # This is the main program for analyzing the E.coli genetics data
rm(list = ls()) ## This appears to be delete all objects from memory, possibly to clear the workspace prior to working with the logic regression analysis

# include functions
library(LogicReg) ## loads the LogicReg package;
SearchIterations <- 1000000    # for fitting the best model
Annealing <- logreg.anneal.control(start=3, end=-4, iter=SearchIterations, update= -1) ## sets the annealing temperature for the annealing algorithm (dictates how the models are built iteratively,
                                                                                       ## and the probability that a move is accepted the farther along the annealing algorithm we are)
                                                                                       ## also does not print any output
TreeControl <- logreg.tree.control(treesize=8, opers=1, minmass=0)    # default setting -> max 8 leaves per tree constructed; both OR and AND operators allowed; min of 0 for trees taking values of 1/0

# Library for logistic regression
# library(MASS)
# ##test ##
# probs=c(0.2, 0.5, 0.7, 0.1, 0.3, 0.54, 0.77,0,54, 0.76, 0.18, 0.39, 0.549, 0.772) ## test library for logistic regression
# outcome=c(1, 0, 1, 0, 1, 0, 1,0, 1, 0, 1, 0, 1)
# specificity=.80
# findthres(probs,outcome,specificity)

probs=c(0.2, 0.5, 0.7, 0.1, 0.3, 0.54, 0.77,0,54, 0.76, 0.18, 0.39, 0.549, 0.772)
outcome <- c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1)
probs[as.numeric(outcome)==2]
probs[as.numeric(outcome)==1]

findthres <- function(probs,outcome,specificity)
{
  # function for choosing the threshold to set sensitivity at a specific value
  
  group1=probs[as.numeric(outcome)==2] ## why does this line of code specify as.numeric(outcome)==2 ? --> the values of outcome should be 0 and 1, no? (unless we specify that ourselves later?)
  group2=probs[as.numeric(outcome)==1]
  sortprobs=sort(probs) ## what does the probs do for us here?
  n1=length(group1)
  n2=length(group2)
  sens=NULL
  spec=NULL
  tmpthres=NULL
  record=TRUE
  thres=NULL
  for (i in 1:(length(probs)-1) ){
    tmpthres[i] = (sortprobs[i] + sortprobs[i+1])/2
    mis1 = sum(group1<tmpthres[i])
    mis2 = sum(group2>tmpthres[i])
    sens[i] = 1-mis1/n1
    spec[i] = 1-mis2/n2
    if ((spec[i]>=specificity) & record) {
      thres=tmpthres[i]
      record=FALSE
    }
  }
  return(thres)
}

logic <- function (raw, labels, outfile, animal, comparison, ind, specificity, host_partition)
  # sources of interest
  # sources for comparison
  # labels: the first column of the datafram "raw" --> raw data? --> so like my "designations column?"
  # ind: a switch to indicate whether to keep specificity at a specific value, 1=Yes, 0=NO
  # specificity: if ind=TRUE, keep specificity at this value
{
  check=which(labels==animal) ## need to make sure (i think) that labels and animal are in the same order, right? otherwise i dont think they will match -- doesnt matter
  if (length(check)==0) {
    sink(file = outfile, append = TRUE, type = c("output","message"), split = FALSE)
    print(infile)
    print(animal)
    print( paste("No observation for animal",animal));
    sink()
    return
  }
  
  # construct the data for analysis
  # indicate all the samples which belong to the animal or comparison group, that is, to exclude samples like "Wild Dog" --> Im assuming I must do this for each ITGR and animal OMGGGGG
  dataindex= vector(length=length(labels)) ## -> creating a vector length equal to the number of samples/strains in the file analyzed
  for (i in 1:length(labels)){
    if  (length(which(c(animal, comparison)==labels[i]))>0) {
      dataindex[i]=TRUE
    }
  }
  # eliminate unuseful rows
  raw=raw[dataindex,]
  # record labels
  labels=substr(raw[,1],1,1) ## so does this take the first letter of each isolate (the host) and create a vector indicating the host labels for each isolate?
  outcome= apply(as.matrix(labels),1, function(x) sum(x==animal)>0 )   #define outcome vector -> summing the isolates that come from the hsot of interest? (where it would equal 1?) or what??
  # exclude labels (different length)
  samples=raw[1]   # vector contains names of each sample
  raw=raw[-1]      # exclude the sample names, the rest columns are for analysis
  # eliminate columns with same values
  numbers=vector(length=dim(raw)[2]) ## this will create a vector of length equal to the number of columns (i.e., the length of the sequence to be analyzed, i believe) 
  for (i in 1:length(numbers)){
    numbers[i]= sum(raw[,i]== raw[1,i]) 
  }
  same=(numbers==dim(raw)[1])
  data=cbind(outcome,raw[!same]) ## so this function appears to eliminate any duplicate columns? (or removes columns with the same data -- maybe to identify the SNPs?)
  
  # data=data[-1]
  # numbers2=vector(length=dim(data)[2])
  # for (i in 1:length(numbers2)){
  #   numbers2[i]= sum(data[,i]== data[1,i])
  # }
  # same=(numbers2==dim(data)[1])
  # sum(same)
  
  # Make factors of categorical variables and add labels
  l = c("NO", "YES")
  data$outcome <- factor(data[,1], levels=c(FALSE,TRUE), label=l)
  l=c("A", "C", "G", "T", "-")                                         ## This will label all the data: either as yes/no for if they belong to the host of interest (outcome), and to label each
  for (i in 2:dim(data)[2]) {                                          ## nucleotide (coded numerically) with their corresponding nucleotide letter
    data[,i] <- factor(data[,i], levels=1:5,labels=l)
  }
  
  ### analysis procedure
  
  # construct the data for logic regression
  binA=NULL
  binC=NULL
  binG=NULL                                                            ## So after we code our data with the appropriate labels, we bin each according to their appropriate nucleotide
  binT=NULL
  binM=NULL
  first=TRUE
  for (i in 2:length(colnames(data)) ){
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
  colnames(binG) <- paste(colnames(binG), '_G', sep='')                ## then we rename these new columns to delineate the SNP [gene, position]_[SNP]
  colnames(binT) <- paste(colnames(binT), '_T', sep='')
  colnames(binM) <- paste(colnames(binM), '_-', sep='')
  
  # combine all the binary data
  tmpbin=cbind(binA, binC, binG, binT, binM)
  
  # eliminate the all zeros columns
  ix=0
  bin=rep(NULL, dim(data)[1])
  for (i in 1:dim(tmpbin)[2]){
    if (sum(tmpbin[,i])>0 ){
      ix=ix+1
      bin=cbind(bin, tmpbin[,i]) 
      colnames(bin)[ix]=colnames(tmpbin)[i]
    }  
  }
  
  # logic regression with the following parameters
  # type=3, using logistic regression model
  # ntrees=2, number of logic trees to be fit
  # select=1, type of model selection to be carried out: (1) fit a single model, (2) fit multiple models, (3) cross-validation, (4) null-model permutation test, (5) conditional permutation test, (6) a greedy stepwise algorithm, or (7) Monte Carlo Logic Regression (using MCMC). 
  # nleaves =10, maximum number of leaves to be fit in all trees combined
  
  ### fit a logistic regression model with 2 trees, no cross-validation for model selection --> why no cross validation? is that separate?
  SearchIterations <- 1000000    # for fitting the best model 
  Annealing <- logreg.anneal.control(start=3, end=-4, iter=SearchIterations, update= -1) ## do we need to modify these parameters for my analysis or do we assume its fine
  TreeControl <- logreg.tree.control(treesize=10, opers=1, minmass=0)    ## default setting
  set.seed(2012)
  logregfit<-logreg(outcome, bin=bin, type=3, select=1, ntrees=2, nleaves=10, anneal.control=Annealing, tree.control=TreeControl)
  probs <- predict(logregfit, newbin=bin, type="response")
  
  if (ind==1){ 
    # keep specificity at a specific value 
    thres=findthres(probs,data$outcome,specificity)
  } else
    if (ind==0){
      thres=0.5
    }
  
  pred <- ifelse(probs<=thres,"NO","YES")
  mc <- table(data$outcome, pred)    # confusion matrix           
  if (length(mc)==2 && colnames(mc)[1]=="NO") {
    sens <- 0
    spec <- 1
  }else 
    if (length(mc)==2 && colnames(mc)[1]=="YES" ) {
      sens <- 1
      spec <- 0
    }else
      if (length(mc)==4 && colnames(mc)[1]=="NO" ) {
        sens <- mc[2,2]/(mc[2,1]+ mc[2,2])   
        spec<- mc[1,1]/(mc[1,1]+ mc[1,2])    
      }
  sens
  spec
  
  # output
  # generate a temple file to store the model in string format
  sink(file = "/Users/danyu/Desktop/tmp.txt",append = FALSE)
  print(logregfit$model)
  sink()
  
  sink(file = outfile, append = TRUE, type = c("output","message"), split = FALSE)
  print(infile)
  print(animal)
  print(samples[data$outcome!= pred,1])
  print(mc)
  print(c("senci",round(sens,2),"speci",round(spec,2)),quote=FALSE)
  sink()
  # read the model in string from the temp file
  con <- file("/Users/danyu/Desktop/tmp.txt", "rt")
  strmodel<-readLines(con, 1) # Read one line 
  for (j in 1:logregfit$model$ntrees[1] ){ 
    temp= which(logregfit$model$trees[[j]]$trees[3]>0)
    for (k in 1:length(temp)){
      varnum=logregfit$model$trees[[j]]$trees[temp[k],3]
      replace = colnames(bin)[logregfit$model$trees[[j]]$trees[temp[k],3]]
      strmodel=sub(paste('X',toString(varnum),sep=''),replace,strmodel)
    }
  }
  sink(file = "/Users/danyu/Desktop/results_logic.txt", append=TRUE)
  print(strmodel)
  close(con)
  sink()
  # record the partition of samples for each host
  if (length(host_partition)==0) {host_partition=rep(0,length(outcome)) }
  for (i in 1:length(outcome)) {
    if (data$outcome[i]=="YES" & pred[i]=="YES") {host_partition[i]=1}
  }
  
  return(host_partition)
}


# set and read in the raw data
infile= "/Users/danyu/Desktop/ThreeOldGenes.txt"   # input
raw<-read.table(infile,sep="\t",header=TRUE) ## makes everything tab-separated?
labels=substr(raw[,1],1,1)      #labels: vector contains the animal source of each sample --> do i need to rearrange my files? my first column also contains the designation; maybe its the first letter?

# set the file path for output
outfilepath= "/Users/danyu/Desktop/"        #  output path

# sources of interest
sources =c('B', 'C', 'D', 'E', 'G', 'H', 'K', 'M', 'L', 'O', 'P', 'Q', 'S', 'V', 'Y')    # no water and wild dog in the data      # animal sources -- need to update for my host categories

# target outcomes
list_animals = list(c('B'))           # outcome ??? for bovine????

host_partition=NULL

# analysis
for (i in 1:length(list_animals)){
  animal <- list_animals[[i]]
  comparison=sources
  # logic regression
  logic(raw,labels,paste(outfilepath,"results_logic.txt",sep=""), animal,comparison, 1, 0.95, host_partition)  #output file name  "results_logic.txt"
}

## how about the cross-validation and permutation tests though????
