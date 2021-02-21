library(randomForest)
library(raster)
library(ggplot2)
library(dismo)
library(maptools)
library(pROC)
library(sf)

###Adding a new line###

#https://uc-r.github.io/random_forests

###Model Randomforest epibenot 6m. Sjopennor och grävande megafauna OSPAR### 
setwd("D:/Project/Bas/CurrentWD/Epibentos_Mod")
aResp = "Natura2000_Sandbank_1100.tif"

list.files("MDA_Response")

<<<<<<< HEAD
predictors=raster::stack(list.files("MDA_Predict", pattern ="*.tif$", full.names=TRUE))#change to predictors_b for infauna_models 
paR=raster("MDA_Response/SG_MEGA.tif", crs="+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
=======
#substr(aResp, 1, nchar(aResp)-4)
>>>>>>> 85f77f44efbb380e8c7d6443540f9f4765666df6

predictors=raster::stack(list.files("MDA_Predict", pattern ="*.tif$", full.names=TRUE))
paR=raster(paste0("MDA_Response/",aResp), crs="+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

plot(predictors)
extent(paR)
SMDA_extent=extent(predictors)
paR=rasterToPoints(paR)

paEnv=extract(predictors, paR[,1:2]) #Takes out the EnvLayers that matches the response variable locations
paEnv=cbind(paEnv, paR[,1:3])  #binds the dataframe of the env with actual data from the response
head(paEnv)

myTry = seq(1,5,by=1)
myK=kfold(paEnv, k=5)
paEnv = as.data.frame(paEnv)
paEnv$K = myK

test=paEnv[which(paEnv$K == 1),] 
train=paEnv[which(paEnv$K > 1),]

<<<<<<< HEAD
head(train)

for(x in myTry){
  rTrn_rf=randomForest(x=train[,c(1:5)],y=train[,8], ntree=2000, mtry=x)#Test number of random variables at each tree split 
  print(rTrn_rf)
=======
for(x in myTry){
  rTrn_cf=randomForest(x=train[,c(1,3:5)],y=as.factor(train[,8]), ntree=2000, mtry=x)#Test number of random variables at each tree split 
  print(rTrn_cf)
>>>>>>> 85f77f44efbb380e8c7d6443540f9f4765666df6
}

#rTrn_cf=randomForest(x=train[,c(1,3:5)],y=as.factor(train[,8]), ntree=2000, mtry=2, importance = TRUE)

#Cutoff arguments: 0.6, c 
rTrn_cf=randomForest(x=train[,c(1,3:5)],y=as.factor(train[,8]), ntree=2000, mtry=2, importance = TRUE, cutoff = c(0.6,1-0.6))

#Creates an AUC graph
#!!!!Remember to change to the correct column for the response layer!!! 

<<<<<<< HEAD
rTrn_rf=randomForest(x=train[,c(1:5)],y=train[,8], ntree=2000, mtry=4)#classification model 
#eva <- evaluate(paEnv[paEnv[,8]==1, ], paEnv[paEnv[,8]==0, ], rTrn_cf)
evaR <- evaluate(paEnv[paEnv[,8]==1, ], paEnv[paEnv[,8]==0, ], rTrn_rf)
#evaRT <- evaluate(paEnv[test[,8]==1, ], paEnv[test[,8]==0, ], rTrn_rf)
=======
par(pty = "s") ## pty sets the aspect ratio of the plot region. s" - creates a square plotting region
png(filename=paste0(substr(aResp,1,nchar(aResp)-4),".png"), width=700, height = 600)
aucPlt=roc(train$Natura2000_Sandbank_1100, rTrn_cf$votes[,1], plot=TRUE, legacy.axes=TRUE, percent = TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#4daf4a", lwd=4, print.auc=TRUE)
dev.off()
par(pty = "m") #                "m" - (the default) creates a maximal plotting region
>>>>>>> 85f77f44efbb380e8c7d6443540f9f4765666df6

write.csv(test, paste0(substr(aResp,1, nchar(aResp)-4), "_testdata.csv"))

pred_Train_rf <- predict(predictors, rTrn_rf, ext=SMDA_extent)
#pred_Train_cf <- predict(predictors, rTrn_cf, ext=fullExt)


{
<<<<<<< HEAD
  sink("OSPAR_MEGA_SG.txt")
  print(rTrn_rf)
  print(evaR)
  print(importance(rTrn_rf))
  print(pred_Train_rf)
  print(evaRT)
=======
  sink(paste0(substr(aResp,1,nchar(aResp)-4), ".txt"))
  print(rTrn_cf)
  print(importance(rTrn_cf))
>>>>>>> 85f77f44efbb380e8c7d6443540f9f4765666df6
  sink()
}

rstMask=raster("D:/Project/Bas/QGIS/mask2.tif")
smlExt=extent(rstMask)

<<<<<<< HEAD
trh=threshold(evaR, "spec_sens")

plot(pred_Train_rf)
plot(pred_Train_rf>trh) #Use the Treshold and create a map in QGIS?

write.csv(test, "test_RF_P5_OSPAR_SG_MEGA.csv")
writeRaster(pred_Train_rf, "pred_RF_P5_OSPAR_SG_MEGA.tif", crs="+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

=======
pred_Train_cf <- predict(predictors, rTrn_cf, ext=smlExt)
>>>>>>> 85f77f44efbb380e8c7d6443540f9f4765666df6


plot(pred_Train_cf)

#pred_Train_cf <- predict(predictors, rTrn_cf, ext=fullExt)
#trh=threshold(evaR, "spec_sens") #Only used with regression
#plot(pred_Train_rf>trh) #Use the Treshold and create a map in QGIS

writeRaster(pred_Train_cf, paste0(substr(aResp, 1, nchar(aResp)-4),"_cf_P4.tif"), crs="+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")


####Infauna-modelling####
setwd("D:/Project/Bas/CurrentWD/Infauna_Mod")

<<<<<<< HEAD
predictors=raster::stack(list.files("C:/Bas/CurrentWD/Infauna_Mod/MDA_Predict", pattern ="*.tif$", full.names=TRUE))
paR=raster("MDA_Response/HELCOM_AB_H3O2.tif", crs="+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
###Prediction Area###
#areaMask=raster("C:/Bas/CurrentWD/BTM/data_6m/mask2.tif", crs="+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#fullExt=extent(predictors)
=======
predictors=raster::stack(list.files("D:/Project/Bas/CurrentWD/Infauna_Mod/MDA_Predict", pattern ="*.tif$", full.names=TRUE))
myResp="HELCOM_AB_H3M.tif"

paR=raster(paste0("MDA_Response/", myResp), crs="+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
fullExt=extent(predictors)
>>>>>>> 85f77f44efbb380e8c7d6443540f9f4765666df6

plot(predictors)
extent(paR)
#extent(predictors)

paR=rasterToPoints(paR)

paEnv=extract(predictors, paR[,1:2]) #Takes out the EnvLayers that matches the response variable locations
paEnv=cbind(paEnv, paR[,1:3])  #binds the dataframe of the env with actual data from the response

paEnv=as.data.frame(na.omit(paEnv))

<<<<<<< HEAD

=======
>>>>>>> 85f77f44efbb380e8c7d6443540f9f4765666df6
myTry = seq(1,6,by=1)
myK=dismo::kfold(paEnv, k=5)

paEnv$K = myK

test=paEnv[which(paEnv$K == 1),] 
train=paEnv[which(paEnv$K > 1),]

write.csv(test, "Test_H3O2.csv")

for(x in myTry){
  rTrn_cf=randomForest(x=train[,c(1:5)],y=as.factor(train[,9]), ntree=2000, mtry=x)#Test number of random variables at each tree split 
  print(rTrn_cf)
}

rTrn_cf=randomForest(x=train[,c(1:5)],y=as.factor(train[,9]), ntree=2000, mtry=5) 


#Creates an AUC graph, remember to change to the correct column for the response layer 
par(pty = "s") ## pty sets the aspect ratio of the plot region. s" - creates a square plotting region
png(filename=paste0(substr(myResp,1,nchar(myResp)-4),".png"), width=700, height = 600)
aucPlt=roc(train$HELCOM_AB_H3O2, rTrn_cf$votes[,1], plot=TRUE, legacy.axes=TRUE, percent = TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#4daf4a", lwd=4, print.auc=TRUE)
dev.off()
par(pty = "m") #                "m" - (the default) creates a maximal plotting region

<<<<<<< HEAD
rTrn_rf=randomForest(x=train[,c(1:6)],y=train[,9], ntree=2000, mtry=2)#classification model 
#eva <- evaluate(paEnv[paEnv[,8]==1, ], paEnv[paEnv[,8]==0, ], rTrn_cf)
evaR <- evaluate(paEnv[paEnv[,9]==1, ], paEnv[paEnv[,9]==0, ], rTrn_rf)
evaRT <- evaluate(paEnv[test[,9]==1, ], paEnv[test[,9]==0, ], rTrn_rf)

=======
>>>>>>> 85f77f44efbb380e8c7d6443540f9f4765666df6


smlExt=extent(predictors_b)

pred_Train_rf <- predict(predictors, rTrn_rf, ext=smlExt)
#pred_Train_cf <- predict(predictors, rTrn_cf, ext=fullExt)


setwd("C:/Bas/CurrentWD/Infauna_Mod")

{
<<<<<<< HEAD
  sink("HELCOM_AB_H3O2.txt")
  print(rTrn_rf)
  print(evaR)
  print(importance(rTrn_rf))
  #varImpPlot(rTrn_rf)
  print(pred_Train_rf)
  print(evaRT)  
  sink()
}


trh=threshold(evaR, "spec_sens")

plot(pred_Train_rf)
plot(pred_Train_rf>trh) #Use the Treshold and create a map in QGIS?



writeRaster(pred_Train_rf, "RF_P6_HELCOM_AB_H3O2.tif", crs="+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")































#writeRaster(predN_crf2, "pred_sjoGrovN.tif", crs="+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")




#####TrialRun####3
predictors=raster::stack(list.files("E:/DataAnalys/OX2_GG_Bentos/EnvLayer/IDW/3500_IDW_5D", pattern ="*.tif$", full.names=TRUE))
paR=raster("E:/DataAnalys/OX2_GG_Bentos/EnvLayer/Response/pa_SjoGrov.tif", crs="+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
#Notice that raster::stack only takes one single argument here, the return from list.files(). 

plot(predictors)
plot(paR)

extent(paR)
=======
  sink(paste0(substr(myResp,1,nchar(myResp)-4), ".txt"))
  print(rTrn_cf)
  print(importance(rTrn_cf))
  sink()
}

rstMask=raster("D:/Project/Bas/QGIS/mask2.tif")
smlExt=extent(rstMask)
>>>>>>> 85f77f44efbb380e8c7d6443540f9f4765666df6
extent(predictors)

pred_Train_cf <- predict(predictors, rTrn_cf, ext=smlExt)

#trh=threshold(evaR, "spec_sens") #Use in regression 
plot(pred_Train_cf)

maskPoly=sf::st_read("D:/Project/Bas/CurrentWD/BTM/data_6m/MaskPoly.shp", stringsAsFactors = FALSE)
#sf::st_as_sf("", crs="+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

plot(maskPoly)
r2 <- crop(pred_Train_cf, extent(maskPoly))
smlPred  <- raster::mask(r2, maskPoly) #Looks wrong in windows but works correct once loaded in QGIS


#plot(pred_Train_rf>trh) #Use the Treshold and create a map in QGIS
writeRaster(pred_Train_cf, paste0(substr(myResp,1, nchar(myResp)-4) , "_cf_P6.tif"), crs="+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
writeRaster(smlPred, paste0(substr(myResp,1, nchar(myResp)-4) , "_SMDA_cf_P6.tif"), crs="+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")




smlPred_cf2 <- setMinMax(smlPred_cf)


###Tutorial####
library(pROC) # install with install.packages("pROC")
library(randomForest) # install with install.packages("randomForest")

#######################################
##
## Generate weight and obesity datasets.
##
#######################################
set.seed(420) # this will make my results match yours

num.samples <- 100

## genereate 100 values from a normal distribution with
## mean 172 and standard deviation 29, then sort them
weight <- sort(rnorm(n=num.samples, mean=172, sd=29))

## Now we will decide if a sample is obese or not. 
## NOTE: This method for classifying a sample as obese or not
## was made up just for this example.
## rank(weight) returns 1 for the lightest, 2 for the second lightest, ...
##              ... and it returns 100 for the heaviest.
## So what we do is generate a random number between 0 and 1. Then we see if
## that number is less than rank/100. So, for the lightest sample, rank = 1.
## This sample will be classified "obese" if we get a random number less than
## 1/100. For the second lightest sample, rank = 2, we get another random
## number between 0 and 1 and classify this sample "obese" if that random
## number is < 2/100. We repeat that process for all 100 samples
obese <- ifelse(test=(runif(n=num.samples) < (rank(weight)/num.samples)), 
                yes=1, no=0)
obese ## print out the contents of "obese" to show us which samples were
## classified "obese" with 1, and which samples were classified
## "not obese" with 0.

## plot the data
plot(x=weight, y=obese)

## fit a logistic regression to the data...
glm.fit=glm(obese ~ weight, family=binomial)
lines(weight, glm.fit$fitted.values)


#######################################
##
## draw ROC and AUC using pROC
##
#######################################

## NOTE: By default, the graphs come out looking terrible
## The problem is that ROC graphs should be square, since the x and y axes
## both go from 0 to 1. However, the window in which I draw them isn't square
## so extra whitespace is added to pad the sides.
roc(obese, glm.fit$fitted.values, plot=TRUE)

## Now let's configure R so that it prints the graph as a square.
##
par(pty = "s") ## pty sets the aspect ratio of the plot region. Two options:
##                "s" - creates a square plotting region
##                "m" - (the default) creates a maximal plotting region
roc(obese, glm.fit$fitted.values, plot=TRUE)

## NOTE: By default, roc() uses specificity on the x-axis and the values range
## from 1 to 0. This makes the graph look like what we would expect, but the
## x-axis itself might induce a headache. To use 1-specificity (i.e. the 
## False Positive Rate) on the x-axis, set "legacy.axes" to TRUE.
roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE)

## If you want to rename the x and y axes...
roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage")

## We can also change the color of the ROC line, and make it wider...
roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4)

## If we want to find out the optimal threshold we can store the 
## data used to make the ROC graph in a variable...
roc.info <- roc(obese, glm.fit$fitted.values, legacy.axes=TRUE)
str(roc.info)

## and then extract just the information that we want from that variable.
roc.df <- data.frame(
  tpp=roc.info$sensitivities*100, ## tpp = true positive percentage
  fpp=(1 - roc.info$specificities)*100, ## fpp = false positive precentage
  thresholds=roc.info$thresholds)

head(roc.df) ## head() will show us the values for the upper right-hand corner
## of the ROC graph, when the threshold is so low 
## (negative infinity) that every single sample is called "obese".
## Thus TPP = 100% and FPP = 100%

tail(roc.df) ## tail() will show us the values for the lower left-hand corner
## of the ROC graph, when the threshold is so high (infinity) 
## that every single sample is called "not obese". 
## Thus, TPP = 0% and FPP = 0%

## now let's look at the thresholds between TPP 60% and 80%...
roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]

## We can calculate the area under the curve...
roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)

## ...and the partial area under the curve.
roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE, print.auc.x=45, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822")


#######################################
##
## Now let's fit the data with a random forest...
##
#######################################
rf.model <- randomForest(factor(obese) ~ weight)
rf.model$votes

rTrn_cf=randomForest(x=train[,c(1:5)],y=as.factor(train[,8]), ntree=2000, mtry=2)

class(rTrn_rf$predicted)
class(rTrn_cf$votes)

?randomForest
## ROC for random forest
roc(obese, rf.model$votes[,1], plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#4daf4a", lwd=4, print.auc=TRUE)




#######################################
##
## Now layer logistic regression and random forest ROC graphs..
##
#######################################
roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)

plot.roc(obese, rf.model$votes[,1], percent=TRUE, col="#4daf4a", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)
legend("bottomright", legend=c("Logisitic Regression", "Random Forest"), col=c("#377eb8", "#4daf4a"), lwd=4)


#######################################
##
## Now that we're done with our ROC fun, let's reset the par() variables.
## There are two ways to do it...
##
#######################################
par(pty = "m")