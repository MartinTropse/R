library(randomForest)
library(raster)
library(ggplot2)
library(dismo)
library(maptools)

###Model Randomforest epibenot 6m. Sjopennor och grävande megafauna OSPAR### 
setwd("C:/Bas/CurrentWD/Epibentos_Mod")

predictors=raster::stack(list.files("MDA_Predict", pattern ="*.tif$", full.names=TRUE))#change to predictors_b for infauna_models 
paR=raster("MDA_Response/SG_MEGA.tif", crs="+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")


#plot(predictors)
extent(paR)
SMDA_extent=extent(predictors)
paR=rasterToPoints(paR)

paEnv=extract(predictors, paR[,1:2]) #Takes out the EnvLayers that matches the response variable locations
paEnv=cbind(paEnv, paR[,1:3])  #binds the dataframe of the env with actual data from the response


set.seed(1)

myTry = seq(1,5,by=1)
myK=kfold(paEnv, k=5)
paEnv = as.data.frame(paEnv)
paEnv$K = myK

test=paEnv[which(paEnv$K == 1),] 
train=paEnv[which(paEnv$K > 1),]

head(train)

for(x in myTry){
  rTrn_rf=randomForest(x=train[,c(1:5)],y=train[,8], ntree=2000, mtry=x)#Test number of random variables at each tree split 
  print(rTrn_rf)
}



rTrn_rf=randomForest(x=train[,c(1:5)],y=train[,8], ntree=2000, mtry=4)#classification model 
#eva <- evaluate(paEnv[paEnv[,8]==1, ], paEnv[paEnv[,8]==0, ], rTrn_cf)
evaR <- evaluate(paEnv[paEnv[,8]==1, ], paEnv[paEnv[,8]==0, ], rTrn_rf)
#evaRT <- evaluate(paEnv[test[,8]==1, ], paEnv[test[,8]==0, ], rTrn_rf)

plot(rTrn_rf)

pred_Train_rf <- predict(predictors, rTrn_rf, ext=SMDA_extent)
#pred_Train_cf <- predict(predictors, rTrn_cf, ext=fullExt)


{
  sink("OSPAR_MEGA_SG.txt")
  print(rTrn_rf)
  print(evaR)
  print(importance(rTrn_rf))
  print(pred_Train_rf)
  print(evaRT)
  sink()
}


trh=threshold(evaR, "spec_sens")

plot(pred_Train_rf)
plot(pred_Train_rf>trh) #Use the Treshold and create a map in QGIS?

write.csv(test, "test_RF_P5_OSPAR_SG_MEGA.csv")
writeRaster(pred_Train_rf, "pred_RF_P5_OSPAR_SG_MEGA.tif", crs="+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")







####Infauna-modelling####
setwd("C:/Bas/CurrentWD/Infauna_Mod")

predictors=raster::stack(list.files("C:/Bas/CurrentWD/Infauna_Mod/MDA_Predict", pattern ="*.tif$", full.names=TRUE))
paR=raster("MDA_Response/HELCOM_AB_H3O2.tif", crs="+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
###Prediction Area###
#areaMask=raster("C:/Bas/CurrentWD/BTM/data_6m/mask2.tif", crs="+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#fullExt=extent(predictors)

plot(predictors)
extent(paR)
#extent(predictors)

paR=rasterToPoints(paR)

paEnv=extract(predictors, paR[,1:2]) #Takes out the EnvLayers that matches the response variable locations
paEnv=cbind(paEnv, paR[,1:3])  #binds the dataframe of the env with actual data from the response

paEnv=as.data.frame(na.omit(paEnv))


myTry = seq(1,6,by=1)
myK=dismo::kfold(paEnv, k=5)

paEnv$K = myK

test=paEnv[which(paEnv$K == 1),] 
train=paEnv[which(paEnv$K > 1),]

write.csv(test, "Test_H3O2.csv")

for(x in myTry){
  rTrn_rf=randomForest(x=train[,c(1:6)],y=train[,9], ntree=2000, mtry=x)#Test number of random variables at each tree split 
  print(rTrn_rf)
}



rTrn_rf=randomForest(x=train[,c(1:6)],y=train[,9], ntree=2000, mtry=2)#classification model 
#eva <- evaluate(paEnv[paEnv[,8]==1, ], paEnv[paEnv[,8]==0, ], rTrn_cf)
evaR <- evaluate(paEnv[paEnv[,9]==1, ], paEnv[paEnv[,9]==0, ], rTrn_rf)
evaRT <- evaluate(paEnv[test[,9]==1, ], paEnv[test[,9]==0, ], rTrn_rf)



smlExt=extent(predictors_b)

pred_Train_rf <- predict(predictors, rTrn_rf, ext=smlExt)
#pred_Train_cf <- predict(predictors, rTrn_cf, ext=fullExt)


setwd("C:/Bas/CurrentWD/Infauna_Mod")

{
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
extent(predictors)
paR=rasterToPoints(paR)

paEnv=extract(predictors, paR[,1:2]) #Takes out the EnvLayers that matches the response variable locations
paEnv=cbind(paEnv, paR[,1:3])  #binds the dataframe of the env with actual data from the response
i <- which(is.na(paEnv[,1])) 
paEnv = paEnv[-i,]

#paN_Env = paEnv[which(paEnv[,8] >= 6302000),]
#paS_Env = paEnv[which(paEnv[,8] < 6302000),]

#plot(x=paN_Env[,7], y=paN_Env[,8])
#plot(x=paS_Env[,7], y=paS_Env[,8])


cs_rf1=randomForest(x=paEnv[,1:9], y=as.factor(paEnv[,12]), ntree = 4000) #Classification because response is a factor 
#cs_rf2=randomForest(x=paS_Env[,1:6], y=as.factor(paS_Env[,9]), ntree = 1000)

myTry = seq(0,6,by=1)

#rs_rf2S=randomForest(x=paS_Env[,1:6], y=paS_Env[,9], ntree = 1000)
#rs_rf2N=randomForest(x=paN_Env[,1:6], y=paN_Env[,9], ntree = 1000)
#varImpPlot(rs_rf2N)
#plot(cs_rf1)

#varImpPlot(cs_rf1)
#plot(cs_rf1)

myK=kfold(paEnv, k=5)
paEnv = as.data.frame(paEnv)
paEnv$K = myK

test=paEnv[which(paEnv$K == 1),] 
train=paEnv[which(paEnv$K > 1),]

#rTrn_rf=randomForest(x=train[,1:9],y=train[,12], ntree=3000)

plot(rTrn_rf)
varImpPlot(rTrn_rf)
TestExt=extent(c(min(test$x), max(test$x), min(test$y), max(test$y)))
TrainExt=extent(c(min(train$x), max(train$x), min(train$y), max(train$y)))
#flExt=extent(c(min(paEnv[,8]),max(paEnv[,8]),min(paEnv[,7]), max(paEnv[,7])))

predN_crf2 <- predict(predictors, cs_rf1, ext=myN_Ext)
eva <- evaluate(paS_Env[paEnv[,9]==1, ], paEnv[paEnv[,9]==0, ], rs_rf2N)





ymi=min(paN_Env[,8])
ymx=max(paN_Env[,8])
xmx=max(paN_Env[,7])
xmi=min(paN_Env[,7])
s_ymi=min(paS_Env[,8])
s_ymx=max(paS_Env[,8])
s_xmx=max(paS_Env[,7])
s_xmi=min(paS_Env[,7])

fl_ymi=min(paEnv[,8])
fl_ymx=max(paEnv[,8])
fl_xmx=max(paEnv[,7])
fl_xmi=min(paEnv[,7])

myN_Ext=extent(c(xmi, xmx, ymi,ymx))
myS_Ext=extent(c(s_xmi, s_xmx, s_ymi, s_ymx))

predN_crf2 <- predict(predictors, rs_rf2S, ext=myN_Ext)
predS_crf2 <- predict(predictors, rs_rf2N, ext=myS_Ext)

plot(predN_crf2)
plot(predS_crf2)

#sjoGrov_xy.csv

evaN <- evaluate(paN_Env[paN_Env[,9]==1, ], paN_Env[paN_Env[,9]==0, ], rs_rf2S)
evaS <- evaluate(paS_Env[paS_Env[,9]==1, ], paS_Env[paS_Env[,9]==0, ], rs_rf2N)

writeRaster(predN_crf2, "pred_sjoGrovN.tif", crs="+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
writeRaster(predS_crf2, "pred_sjoGrovS.tif", crs="+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

myK=kfold(paEnv, k=5)
paEnv = as.data.frame(paEnv)
paEnv$K = myK

test=paEnv[which(paEnv$K == 1),] 
train=paEnv[which(paEnv$K > 1),]



rTrn_rf=randomForest(x=train[,1:6],y=train[,9], ntree=3000)
plot(rTrn_rf)
varImpPlot(rTrn_rf)
TestExt=extent(c(min(test$x), max(test$x), min(test$y), max(test$y)))
TrainExt=extent(c(min(train$x), max(train$x), min(train$y), max(train$y)))
flExt=extent(c(min(paEnv[,8]),max(paEnv[,8]),min(paEnv[,7]), max(paEnv[,7])))

pred_rrf=predict(predictors, rTrn_rf, ext=TestExt)
pred_rrf_fl=predict(predictors, rTrn_rf, ext=extent(predictors))

plot(pred_rrf)
plot(pred_rrf_fl)

eva2 <- evaluate(test[test[,9]==1, ], test[test[,9]==0, ], rTrn_rf)
writeRaster(pred_rrf, "pred_sjoGrov.tif", crs="+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

writeRaster(pred_rrf_fl, "pred_sjoGrovFl.tif", crs="+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
#265374.2238999999826774,6233232.1377999996766448 : 361134.2238999999826774,6418872.1377999996766448
extent(predictors)

myExt
TestExt
TrainExt

#table(paEnv[,7])
#eva <- evaluate(paEnv[paEnv[,7]==1, ], paEnv[paEnv[,7]==0, ], cr_rf2)
#paEnv = as.data.frame(paEnv)
#ew <- extent(SpatialPoints(bf[bf[,1] <= -102, 1:2]))
#colnames(paEnv)[7] = "pres_abs"
#paEnv$pres_abs = as.factor(paEnv$pres_abs)
#out=kfold(paEnv, 5)
#train = paEnv[out >= 2,]
#test = paEnv[out == 1, ]
#test = extent(SpatialPoints((paEnv[,1:6])))