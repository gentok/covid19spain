#' ---
#' title: "Revise Demographic Data (Cataluna)"
#' author: "Gento Kato"
#' date: "May 4, 2020"
#' ---

#################
## PREPARATION ##
#################

## Set Working Directory to the current directory 
## (If using RStudio, can be set automatically)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Clear Workspace
rm(list=ls())

## Import Relevant Data
coviddt <- readRDS("../data/covid_madrid.rds")
demodt <- readRDS("../data/demo_madrid.rds")
shapedt <- readRDS("../data/shapefile/shape_madrid.rds")
censusdt <- readRDS("../data/census2011_madrid.rds")

##########################################
## Revise Municipality Names for demodt ##
##########################################

## Rename Municipality Names in demodt to match with coviddt
demodt$mundesc[grep(" \\(la\\)$",demodt$mundesc)] <- 
  paste("la", gsub(" \\(la\\)$","",demodt$mundesc[grep(" \\(la\\)$",demodt$mundesc)]))
demodt$mundesc[grep(" \\(el\\)$",demodt$mundesc)] <- 
  paste("el", gsub(" \\(el\\)$","",demodt$mundesc[grep(" \\(el\\)$",demodt$mundesc)]))
demodt$mundesc[grep(" \\(los\\)$",demodt$mundesc)] <- 
  paste("los", gsub(" \\(los\\)$","",demodt$mundesc[grep(" \\(los\\)$",demodt$mundesc)]))
demodt$mundesc[grep(" \\(las\\)$",demodt$mundesc)] <- 
  paste("las", gsub(" \\(las\\)$","",demodt$mundesc[grep(" \\(las\\)$",demodt$mundesc)]))
demodt$mundesc[demodt$mundesc=="el alamo"] <- "el álamo"

###############################################
## Revise Municipality Names for census data ##
###############################################

## Rename Municipality Names in censusdt to match with coviddt
censusdt$mundesc[grep(", la$",censusdt$mundesc)] <- 
  paste("la", gsub(", la$","",censusdt$mundesc[grep(", la$",censusdt$mundesc)]))
censusdt$mundesc[grep(", el$",censusdt$mundesc)] <- 
  paste("el", gsub(", el$","",censusdt$mundesc[grep(", el$",censusdt$mundesc)]))
censusdt$mundesc[grep(", los$",censusdt$mundesc)] <- 
  paste("los", gsub(", los$","",censusdt$mundesc[grep(", los$",censusdt$mundesc)]))
censusdt$mundesc[grep(", las$",censusdt$mundesc)] <- 
  paste("las", gsub(", las$","",censusdt$mundesc[grep(", las$",censusdt$mundesc)]))

#########################################################
## Check remaining discrepancies in municipality names ##
#########################################################

# Unmatched municipalities
unique(coviddt$mundesc[which(!coviddt$mundesc %in% demodt$mundesc)])
unique(coviddt$munid[which(!coviddt$munid %in% shapedt$munid)]) # All match!
unique(coviddt$mundesc[which(!coviddt$mundesc %in% censusdt$mundesc)])
# Candidates
sort(demodt$mundesc[which(!demodt$mundesc %in% coviddt$mundesc)])
sort(censusdt$mundesc[which(!censusdt$mundesc %in% coviddt$mundesc)])

## The easiest solution is to dissolve all districts within madrid into one 
## and match with "madrid" in demodt and censusdt.

## Alternative is to go at the level of details shown in demodt. Not 
## sure how each within-madrid regions can be matched with each other.

##########
## Save ##
##########

saveRDS(demodt, "../data/demo_madrid_rev.rds")
saveRDS(censusdt, "../data/census2011_madrid_rev.rds")