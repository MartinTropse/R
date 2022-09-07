###Tutorial OpenXLSX####
#https://www.youtube.com/watch?v=nI-IlHvf1-c&ab_channel=MichaelMahoney
library(openxlsx)
library(tidyverse)
library(palmerpenguins)

mtcars %>% 
  filter(mpg>20) -> mpg



penguins -> penguins 
createWorkbook() -> wb

addWorksheet(wb, "mpg")
addWorksheet(wb, "penguins")

hstyle <- createStyle(halign = "center", fontColour = "blue", fontSize = 14)
cstyle <- createStyle(halign = "center")

writeData(wb, "mpg", mpg)
writeData(wb, "penguins", penguins, headerStyle = hstyle)

addStyle(wb, "mpg",cstyle, rows = 2:2000, cols = 1:100, gridExpand = TRUE)
addStyle(wb, "penguins",cstyle, rows = 2:2000, cols = 1:100, gridExpand = TRUE)

setColWidths(wb, "penguins", cols = 1:ncol(penguins), widths = 20)

freezePane(wb, "penguins", firstRow=TRUE)
saveWorkbook(wb, file = "trial.xlsx", overwrite = TRUE)