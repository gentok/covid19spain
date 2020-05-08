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
shpfile <- "../data/shapefile/municipios_y_distritos_madrid/municipios_y_distritos_madrid.shp"
sh <- st_read(shpfile, stringsAsFactors = FALSE)

## Municipality ID
sh$munid <- as.numeric(sh$codigo_geo)

saveRDS(sh, "../data/shapefile/shape_madrid.rds")