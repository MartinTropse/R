###Tutorial OpenXLSX####
#https://www.youtube.com/watch?v=nI-IlHvf1-c&ab_channel=MichaelMahoney
rm(list = ls())

library(openxlsx)
library(tidyverse)
library(palmerpenguins)
library(readxl)

setwd("C:/Calluna/Projekt/CementaSlite/Sandbox")



myTrial=openxlsx::openXL("C:/Calluna/Projekt/CementaSlite/Sandbox/trial.xlsx")
myTrial=openxlsx::loadWorkbook("trial.xlsx")

myData=readxl::read_excel("trial.xlsx")





myTrial$conditionalFormatting
myTrial$createFillNode

mtcars %>% 
  filter(mpg>20) -> mpg

penguins -> penguins 
createWorkbook() -> wb

addWorksheet(wb, "mpg")
addWorksheet(wb, "penguins")

writeData(wb, "mpg", mpg)
saveWorkbook(wb, file = "trial.xlsx", overwrite = TRUE)

hstyle <- createStyle(halign = "center", fontColour = "blue", fontSize = 14)
cstyle <- createStyle(halign = "center")

writeData(wb, "penguins", penguins, headerStyle = hstyle)

saveWorkbook(wb, file = "trial.xlsx", overwrite = TRUE)

addStyle(wb, "mpg",cstyle, rows = 2:2000, cols = 1:2, gridExpand = TRUE)
addStyle(wb, "penguins",cstyle, rows = 2:2000, cols = 1:100, gridExpand = TRUE)

setColWidths(wb, "penguins", cols = 1:ncol(penguins), widths = 20)

freezePane(wb, "penguins", firstRow=TRUE)
