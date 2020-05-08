#' ---
#' title: "Revise Demographic Data (Cataluna)"
#' author: "Gento Kato"
#' date: "April 17, 2020"
#' ---

#################
## PREPARATION ##
#################

## Set Working Directory to the current directory 
## (If using RStudio, can be set automatically)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Clear Workspace
rm(list=ls())

# Data Import 
require(readr)

# Import COVID19 Data 
d <- read_tsv("../data/covid_cataluna/covid_cataluna_latest.tsv", 
              locale=locale(encoding = "UTF-8"))

## Import Relevant Data
shapedt <- readRDS("../data/shapefile/shape_cataluna.rds")
demodt <- readRDS("../data/demo_cataluna/demo_cataluna_wide.rds")
pop18 <- read.csv("../data/demo_cataluna/pop2018_cataluna.csv", 
                  sep=";", skip=6, nrows=947, header = FALSE,
                  col.names = c("munid","munname",
                                "f176_18","f33_18","f34and35_18",
                                "f38_18","f39_18","f40and41_18",
                                "f167_18","f27_18","f28and29_18","mis"),
                  stringsAsFactors = FALSE, encoding = "UTF-8")
pop18 <- pop18[,-ncol(pop18)]
pop18$f36_18 <- pop18$f176_18 + pop18$f33_18 + pop18$f34and35_18
pop18$f42_18 <- pop18$f38_18 + pop18$f39_18 + pop18$f40and41_18
pop18$f171_18 <- pop18$f167_18 + pop18$f27_18 + pop18$f28and29_18
head(pop18)

##########################################
## Revise Municipality Names for demodt ##
##########################################

# 
demodt$munid <- as.numeric(demodt$munid)

# New Variable for Matching
demodt$mundesc <- demodt$munname

## Rename Municipality Names in demodt to match with coviddt
demodt$mundesc[grep(", la$",demodt$munname)] <- 
  paste("La", gsub(", la$","",demodt$munname[grep(", la$",demodt$munname)]))
demodt$mundesc[grep(", l$",demodt$munname)] <- 
  paste("L", gsub(", l$","",demodt$munname[grep(", l$",demodt$munname)]))
demodt$mundesc[grep(", l'$",demodt$munname)] <- 
  paste0("L'", gsub(", l'$","",demodt$munname[grep(", l'$",demodt$munname)]))
demodt$mundesc[grep(", les$",demodt$munname)] <- 
  paste("Les", gsub(", les$","",demodt$munname[grep(", les$",demodt$munname)]))
demodt$mundesc[grep(", el$",demodt$munname)] <- 
  paste("El", gsub(", el$","",demodt$munname[grep(", el$",demodt$munname)]))
demodt$mundesc[grep(", els$",demodt$munname)] <- 
  paste("Els", gsub(", els$","",demodt$munname[grep(", els$",demodt$munname)]))
demodt$mundesc[grep(", Es$",demodt$munname)] <- 
  paste("Es", gsub(", Es$","",demodt$munname[grep(", Es$",demodt$munname)]))
demodt$mundesc[which(demodt$munname=="Brunyola i Sant Martí Sapresa")] <-
  "Brunyola"
demodt$mundesc[which(demodt$munname=="Saus, Camallera i Llampaies")] <-
  "Saus Camallera i Llampaies"
demodt$mundesc[which(demodt$munname=="Cruïlles, Monells i Sant Sadurní de l'Heura")] <-
  "Cruïlles Monells i Sant Sadurní de l'Heura"

## All Lower Case
demodt$mundesc <- tolower(demodt$mundesc)

## Add one row with (altres municipis)
demodt[nrow(demodt)+1,] <- NA
demodt$mundesc[nrow(demodt)] <- "(altres municipis)"

##########################################
## Revise Municipality Names for pop18 ##
##########################################

# New Variable for Matching
pop18$mundesc <- pop18$munname

## Rename Municipality Names in pop18 to match with coviddt
pop18$mundesc[grep(", la$",pop18$munname)] <- 
  paste("La", gsub(", la$","",pop18$munname[grep(", la$",pop18$munname)]))
pop18$mundesc[grep(", l$",pop18$munname)] <- 
  paste("L", gsub(", l$","",pop18$munname[grep(", l$",pop18$munname)]))
pop18$mundesc[grep(", l'$",pop18$munname)] <- 
  paste0("L'", gsub(", l'$","",pop18$munname[grep(", l'$",pop18$munname)]))
pop18$mundesc[grep(", les$",pop18$munname)] <- 
  paste("Les", gsub(", les$","",pop18$munname[grep(", les$",pop18$munname)]))
pop18$mundesc[grep(", el$",pop18$munname)] <- 
  paste("El", gsub(", el$","",pop18$munname[grep(", el$",pop18$munname)]))
pop18$mundesc[grep(", els$",pop18$munname)] <- 
  paste("Els", gsub(", els$","",pop18$munname[grep(", els$",pop18$munname)]))
pop18$mundesc[grep(", Es$",pop18$munname)] <- 
  paste("Es", gsub(", Es$","",pop18$munname[grep(", Es$",pop18$munname)]))
pop18$mundesc[which(pop18$munname=="Brunyola i Sant Martí Sapresa")] <-
  "Brunyola"
pop18$mundesc[which(pop18$munname=="Saus, Camallera i Llampaies")] <-
  "Saus Camallera i Llampaies"
pop18$mundesc[which(pop18$munname=="Cruïlles, Monells i Sant Sadurní de l'Heura")] <-
  "Cruïlles Monells i Sant Sadurní de l'Heura"

## All Lower Case
pop18$mundesc <- tolower(pop18$mundesc)

## Add one row with (altres municipis)
pop18[nrow(pop18)+1,] <- NA
pop18$mundesc[nrow(pop18)] <- "(altres municipis)"

###############################
## Combine demodt with pop18 ##
###############################

demoall <- merge(demodt,pop18)

##########
## Save ##
##########

saveRDS(demoall, "../data/demo_cataluna_wide_rev.rds")