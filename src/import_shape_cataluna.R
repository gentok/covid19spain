#' ---
#' title: "Import and Save Shapefile for Calalonia"
#' author: "Gento Kato"
#' date: "April 17, 2020"
#' ---

## Set Working Directory to the current directory 
## (If using RStudio, can be set automatically)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Clear Workspace
rm(list=ls())

##
require(ggmap)
require(sf)
require(maptools)
require(dplyr)
require(readxl)

####################################
## Prepare and Modify Shape Files ##
####################################

## Original Files Downloaded from https://www.ine.es/censos2011_datos/cen11_datos_resultados_seccen.htm

# Read the neighborhood shapefile data and plot
shpfile <- "../data/shapefile/bm5mv21sh0tc1_20200101_0/bm5mv21sh0tpm1_20200101_0.shp"
sh <- st_read(shpfile, stringsAsFactors = FALSE)

saveRDS(sh, "../data/shapefile/shape_cataluna.rds")

plot(sh$geometry)

###########################################
## Revise Municipality Names for sh ##
###########################################

# New Variable for Matching
sh$mundesc <- sh$NOMMUNI

## Rename Municipality Names in sh to match with coviddt
# sh$mundesc[grep(", La$",sh$NOMMUNI)] <- 
#   paste("La", gsub(", La$","",sh$NOMMUNI[grep(", La$",sh$NOMMUNI)]))
# sh$mundesc[grep(", L$",sh$NOMMUNI)] <- 
#   paste("L", gsub(", L$","",sh$NOMMUNI[grep(", L$",sh$NOMMUNI)]))
# sh$mundesc[grep(", L'$",sh$NOMMUNI)] <- 
#   paste0("L'", gsub(", L'$","",sh$NOMMUNI[grep(", L'$",sh$NOMMUNI)]))
# sh$mundesc[grep(", Les$",sh$NOMMUNI)] <- 
#   paste("Les", gsub(", Les$","",sh$NOMMUNI[grep(", Les$",sh$NOMMUNI)]))
# sh$mundesc[grep(", El$",sh$NOMMUNI)] <- 
#   paste("El", gsub(", El$","",sh$NOMMUNI[grep(", El$",sh$NOMMUNI)]))
# sh$mundesc[grep(", Els$",sh$NOMMUNI)] <- 
#   paste("Els", gsub(", Els$","",sh$NOMMUNI[grep(", Els$",sh$NOMMUNI)]))
# sh$mundesc[grep(", Es$",sh$NOMMUNI)] <- 
#   paste("Es", gsub(", Es$","",sh$NOMMUNI[grep(", Es$",sh$NOMMUNI)]))
# sh$mundesc[which(sh$NOMMUNI=="Santa Maria de Corcó")] <-
#   "L'Esquirol"
# sh$mundesc[which(sh$NOMMUNI=="Saus,  Camallera i Llampaies")] <-
#   "Saus Camallera i Llampaies"
# sh$mundesc[which(sh$NOMMUNI=="Cruïlles,  Monells i Sant Sadurní de l'Heura")] <-
#   "Cruïlles Monells i Sant Sadurní de l'Heura"
# sh$mundesc[which(sh$NOMMUNI=="Roda de Barà")] <-
#   "Roda de Berà"
# sh$mundesc[which(sh$NOMMUNI=="Calonge")] <-
#   "Calonge i Sant Antoni"
sh$mundesc[which(sh$NOMMUNI=="Brunyola i Sant Martí Sapresa")] <-
  "Brunyola"
sh$mundesc[which(sh$NOMMUNI=="Saus, Camallera i Llampaies")] <-
  "Saus Camallera i Llampaies"
sh$mundesc[which(sh$NOMMUNI=="Cruïlles, Monells i Sant Sadurní de l'Heura")] <-
  "Cruïlles Monells i Sant Sadurní de l'Heura"

## All lower case
sh$mundesc <- tolower(sh$mundesc)

## Add one row with (altres municipis)
sh[nrow(sh)+1,] <- NA
sh$mundesc[nrow(sh)] <- "(altres municipis)"

saveRDS(sh, "../data/shapefile/shape_cataluna_rev.rds")