#workflow for updating a file from an excel sheet

#0. Load the relevant libraries
library(lipdR)
library(readxl)
library(writexl)
library(tidyverse)
library(glue)
library(geoChronR)
library(vegan)
library(ape)
library(Lakes380DataTools)


#1. Load in the LiPD file you want to update
L <- readLipd("~/Dropbox/lipdverse/Lakes380National/LakeOkareka_15325.Lakes380.lpd")

#2. After changing the values in an excel table, update the Lipd based on that excel file
L <- updateLipdFromSpreadsheet(L,path = "~/Download/LakeOkareka_15325.Lakes380-v1.0.0.xlsx")

#3. "Map" the age model to all the paleoData sets, making sure they're all using the potentially updated age model
L <- mapAgeSummaryToAllPaleoData(L)

#create outputs
outfile <- paste0(L$dataSetName,"-v",getVersion(L))

#4. Create a new dashboard plot
dash <- plotDashboard(L)

#save it
ggsave(filename = paste0(outfile,".pdf"), plot = dash)

#5 write the updated excel spreadsheet
lipd2excel(L,outname = file.path("~/Download/",paste0(outfile,".xlsx")))

#6 save the LiPD file, overwriting the old one
writeLipd(L,path = getwd())
