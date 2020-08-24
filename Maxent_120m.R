library(dismo)
library(raster)
library(tictoc)
library(ggmap)

#setwd("E:/MMV/2_Arbetsdokument/Prediktioner/Vektor/Projekteringsomraden")
setwd("E:/MMV/2_Arbetsdokument/Prediktioner/Raster/Predict_120m")
setwd("E:/MMV/2_Arbetsdokument/Prediktioner/Raster")

#stamRst=raster("Grid120_SmlStamInt.tif")
#storage.mode(stamRst[])
#stamRst[]=as.numeric(stamRst[])

#writeRaster(stamRst, "Grid120_StamSmint.tif")




#fltRst=raster("FlytVindpark120m.tif") 
#fltMx = rasterToPoints(fltRst)
atcRst=raster("FastVindpark120m.tif")
atcMx = rasterToPoints(atcRst)

###Maxent Attached Windparks###
coord = atcMx[,1:2]
predictors=raster::stack(list.files(file.path("E:/MMV/2_Arbetsdokument/Prediktioner/Raster/Predict_120m"), pattern="_*.tif$", full.names =TRUE))

djupRst=raster("E:/MMV/2_Arbetsdokument/Prediktioner/Raster/Predict_120m/Grid120_CoastDist.tif")
elRst=raster("E:/MMV/2_Arbetsdokument/Prediktioner/Raster/Grid120_Elomrade.tif")
storage.mode(elRst[]) = "double" #Change integer into double
storage.mode(elRst[])



#stamRst=raster("E:/MMV/2_Arbetsdokument/Prediktioner/Raster/Predict_120m/Grid120_StamnatDist.tif")
#djupMx=rasterToPoints(djupRst)
#djupMx[,3]=as.integer(djupMx[,3])

#stamRst=raster("E:/MMV/2_Arbetsdokument/Prediktioner/Raster/Predict_120m/Grid120_StamnatDist.tif")
#stamMx=rasterToPoints(stamRst)
#stamMx[,3]=as.integer(stamMx[,3])

####load raster file
# coastRst=raster("E:/MMV/2_Arbetsdokument/Prediktioner/Raster/Predict_120m/Grid120_CoastDist.tif")
# stamRst=raster("E:/MMV/2_Arbetsdokument/Prediktioner/Raster/Predict_120m/Grid120_StamnatDist.tif")
# depthRst=raster("E:/MMV/2_Arbetsdokument/Prediktioner/Raster/Predict_120m/Grid120_Djup.tif") 
# 
# ###check data type of raster file 
# storage.mode(stamRst[])
# storage.mode(depthRst[])
# storage.mode(coastRst[])
# ###change values into integer
# coastRst[]<-as.integer(coastRst[])
# stamRst[]=as.integer(stamRst[])
# depthRst[]=as.integer(depthRst[])
# 
# writeRaster(depthRst, "Grid120_DjupInt.tif")
# writeRaster(stamRst, "Grid120_StamInt.tif")
# writeRaster(coastRst, "Grid120_CoastInt.tif")

###plotr raster file
#plot(coastRst)

#coastMx<-rasterToPoints(coastRst)
#coastMx_t<-t(coastMx)
#dim(coastMx_t)
#rf<-raster(coastMx_t[nrow(coastMx_t):1,])
#writeRaster(coastDf, "Coast_Int120.tif", datatype='INT2S')

set.seed(1)

plot(predictors)
backGrd=randomPoints(predictors, 1000000) #Johan mentioned that ab/pres data should be balanced, but not sure if it applies here :o
#presVals = extract(predictors, coord)

presGrp=kfold(coord, 5)
presTrain=coord[presGrp == 1, ] #change train/test prop
presTest=coord[presGrp > 1, ] 

backGrp=kfold(backGrd, 5)
backTrain=backGrd[backGrp == 1, ]
backTest=backGrd[backGrp > 1, ]

M_Atc120=maxent(predictors,presTrain[,1:2])

plot(M_Atc120)
response(M_Atc120)
eval_Atc120 = evaluate(presTest[,1:2], backTest[,1:2], M_Atc120, predictors)
px_atc120 = predict(predictors, M_Atc120, progress='')

pdf("PrognosFastsittandeVind3_120m.pdf", width=14)
par(mfrow=c(1,2))
cutoff = threshold(eval_Atc120, "spec_sens")
plot(px_atc120, main="Prognos fastsittande vindkraft", colNA="black")
par(mar=c(5,4,4,4))
plot(px_atc120 > cutoff, main="Presence/Absence", colNA="black")
dev.off()


###Maxent Floating###
foord = fltMx[,1:2]
predictors=raster::stack(list.files(file.path("E:/MMV/2_Arbetsdokument/Prediktioner/Raster/Predict_120m"), pattern="_*.tif$", full.names =TRUE))

set.seed(1)
plot(predictors)
backGrd=randomPoints(predictors, 500000)

presGrp=kfold(foord, 5)
presTrain=foord[presGrp > 1, ] #change train/test prop
presTest=foord[presGrp == 1, ] 

backGrp=kfold(backGrd, 5)
backTrain=backGrd[backGrp > 1, ]
backTest=backGrd[backGrp == 1, ]

M_Flt120=maxent(predictors,presTrain[,1:2])

plot(M_Flt120)
response(M_Flt120)
eval_Flt120 = evaluate(presTest[,1:2], backTest[,1:2], M_Flt120, predictors)
px_flt120 = predict(predictors, M_Flt120, progress='')

writeRaster(px_flt120, "float120Predict.tif")
writeRaster(px_atc120, "attach120Predict.tif")


pdf("PrognosFlytandeVindNy_120m.pdf", width=14)
par(mfrow=c(1,2))
cutoff = threshold(eval_Flt120, "spec_sens")
plot(px_flt120, main="Prognos flytande vindkraft", colNA="black")
par(mar=c(5,4,4,4)+0.5)
plot(px_flt120 > cutoff, main="Presence/Absence", colNA="black")
dev.off()