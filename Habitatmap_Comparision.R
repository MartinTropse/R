library(raster)
library(dismo)
library(reshape2)

setwd("C:/Bas/CurrentWD/Epibentos_Mod/RF_ModelReslt/Regression")

list.dirs("HELCOM_AB_H1V")
list.files("HELCOM_AB_H1V")
list.files("HELCOM_AB_H2T1")

J1Vr=raster("HELCOM_AB_J1V/pred_RF_P4_HELCOM_AB_J1V.tif")
H1Vr=raster("HELCOM_AB_H1V/pred_RF_P5_HELCOM_AB_H1V.tif")
H2T1=raster("HELCOM_AB_H2T1/pred_RF_P4_HELCOM_AB_H2T1.tif")

#J1V % Var explained: 31.23- max TPR+TNR at : 0.5283333
#H1V % Var explained: 14.06- max TPR+TNR at : 0.2930833
#H2T1% Var explained: 31.85- max TPR+TNR at : 0.43605

halSpec[halSpec>0] = 1 
pSGU[pSGU$SGU_100_MDA > 0,3] = 1


H1Vd=as.data.frame(rasterToPoints(H1Vr))
J1Vd=as.data.frame(rasterToPoints(J1Vr))
H2T1d=as.data.frame(rasterToPoints(H2T1))


hist(H1Vd$pred_RF_P5_HELCOM_AB_H1V)
hist(J1Vd$pred_RF_P4_HELCOM_AB_J1V)
hist(H2T1d$pred_RF_P4_HELCOM_AB_H2T1)

J1Vd$pred_RF_P4_HELCOM_AB_J1V

max(H2T1d$pred_RF_P4_HELCOM_AB_H2T1)
max(J1Vd$pred_RF_P4_HELCOM_AB_J1V)
max(H1Vd$pred_RF_P5_HELCOM_AB_H1V)

H1Vd$HELCOM = rep("H1V", length(H1Vd$pred_RF_P5_HELCOM_AB_H1V))
J1Vd$HELCOM = rep("J1V", length(J1Vd$pred_RF_P4_HELCOM_AB_J1V))
H2T1d$HELCOM = rep("H2T1", length(H2T1d$pred_RF_P4_HELCOM_AB_H2T1))

head(H1Vd)
head(J1Vd)
head(H2T1d)

#xDim=seq(1, length(H1Vd$pred_RF_P5_HELCOM_AB_H1V), by=1)
xDim=seq(1, 10, by=1)

x=max(c(H1Vd$pred_RF_P5_HELCOM_AB_H1V[1], J1Vd$pred_RF_P4_HELCOM_AB_J1V[1], H2T1d$pred_RF_P4_HELCOM_AB_H2T1[1]))

xMaxList = list()
modList = list()

for(x in xDim){
  xMax=max(c(H1Vd$pred_RF_P5_HELCOM_AB_H1V[x], J1Vd$pred_RF_P4_HELCOM_AB_J1V[x], H2T1d$pred_RF_P4_HELCOM_AB_H2T1[x]))
  xMaxList[[length(xMaxList)+ 1]] <- xMax
  if(H1Vd$pred_RF_P5_HELCOM_AB_H1V[x] == xMax){
    modList[[length(modList)]] <- H1Vd$HELCOM[x]    
  }
  if(J1Vd$pred_RF_P4_HELCOM_AB_J1V[x] == xMax){
    modList[[length(modList)+1]] <- J1Vd$HELCOM[x]
  }
  if(H2T1d$pred_RF_P4_HELCOM_AB_H2T1[x] == xMax){
    modList[[length(modList)+1]] <- H2T1d$HELCOM[x]
  }
}

head(H2T1d, 10)
head(J1Vd, 10)

# (H2T1d$pred_RF_P4_HELCOM_AB_H2T1 == xMax){
#   xMaxList[[length(xMaxList)]] <- H2T1d$HELCOM[x]

xMaxList
my_list <- list("XXX",                             # Create example list
                letters[1:4],
                5:3)

for(i in 1:3) {                                    # Head of for-loop
  new_element <- rep(i, 3)                         # Create new list element
  my_list[[length(my_list) + 1]] <- new_element    # Append new list element
}

# Apply the max TRP-TRN value after comparision to respective layer
# H1Vd[H1Vd$pred_RF_P5_HELCOM_AB_H1V < 0.43605,] = 0 
# J1Vd[J1Vd$pred_RF_P4_HELCOM_AB_J1V < 0.5283,] = 0 
# H2T1d[H2T1d$pred_RF_P4_HELCOM_AB_H2T1 < 0.43605,] = 0 