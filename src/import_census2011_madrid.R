#' ---
#' title: "Import Census Files for Madrid"
#' author: "Gento Kato"
#' date: "May 4, 2020"
#' ---

## Set Working Directory to the current directory 
## (If using RStudio, can be set automatically)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Clear Workspace
rm(list=ls())

## Packages
require(readxl)

# 2011 Census Data
ce <- read.csv("../data/shapefile/national_data/indicadores_seccion_censal_csv/C2011_ccaa13_Indicadores.csv",
               stringsAsFactors = FALSE, encoding = "UTF-8")

## Dissolve File by Municipalities
ce_mun <- ce %>% group_by(ccaa,cpro,cmun) %>% summarise_all(sum, na.rm=TRUE)

### Add Area Names
canames <- read_excel("../data/shapefile/national_data/codccaa.xls", skip=1)
ce_mun$CCA_NAME <- canames$LITERAL[match(ce_mun$ccaa,as.numeric(canames$CODIGO))]
provnames <- read_excel("../data/shapefile/national_data/codprov.xls", skip=1)
ce_mun$CPRO_NAME <- provnames$LITERAL[match(ce_mun$cpro,as.numeric(provnames$CODIGO))]
munnames <- read_excel("../data/shapefile/national_data/Municipios_Censo_2011.xls")
head(munnames)
ce_mun$CMUN_NAME <- munnames$NOMBRE[match(ce_mun$cpro*1000 + ce_mun$cmun,
                                          as.numeric(munnames$COD_MUN))]
ce_mun$CMUN_NAME <- gsub(" \\(.*$", "", ce_mun$CMUN_NAME)

ce_mun$mundesc <- ce_mun$CMUN_NAME

## Rename Municipality Names in shapedt to match with coviddt
# ce_mun$mundesc[grep(", La$",ce_mun$CMUN_NAME)] <-
#   paste("La", gsub(", La$","",ce_mun$CMUN_NAME[grep(", La$",ce_mun$CMUN_NAME)]))
# ce_mun$mundesc[grep(", L$",ce_mun$CMUN_NAME)] <-
#   paste("L", gsub(", L$","",ce_mun$CMUN_NAME[grep(", L$",ce_mun$CMUN_NAME)]))
# ce_mun$mundesc[grep(", L'$",ce_mun$CMUN_NAME)] <-
#   paste0("L'", gsub(", L'$","",ce_mun$CMUN_NAME[grep(", L'$",ce_mun$CMUN_NAME)]))
# ce_mun$mundesc[grep(", Les$",ce_mun$CMUN_NAME)] <-
#   paste("Les", gsub(", Les$","",ce_mun$CMUN_NAME[grep(", Les$",ce_mun$CMUN_NAME)]))
# ce_mun$mundesc[grep(", El$",ce_mun$CMUN_NAME)] <-
#   paste("El", gsub(", El$","",ce_mun$CMUN_NAME[grep(", El$",ce_mun$CMUN_NAME)]))
# ce_mun$mundesc[grep(", Els$",ce_mun$CMUN_NAME)] <-
#   paste("Els", gsub(", Els$","",ce_mun$CMUN_NAME[grep(", Els$",ce_mun$CMUN_NAME)]))
# ce_mun$mundesc[grep(", Es$",ce_mun$CMUN_NAME)] <-
#   paste("Es", gsub(", Es$","",ce_mun$CMUN_NAME[grep(", Es$",ce_mun$CMUN_NAME)]))
# ce_mun$mundesc[which(ce_mun$CMUN_NAME=="Santa Maria de Corcó")] <-
#   "L'Esquirol"
# ce_mun$mundesc[which(ce_mun$CMUN_NAME=="Saus,  Camallera i Llampaies")] <-
#   "Saus Camallera i Llampaies"
# ce_mun$mundesc[which(ce_mun$CMUN_NAME=="Cruïlles,  Monells i Sant Sadurní de l'Heura")] <-
#   "Cruïlles Monells i Sant Sadurní de l'Heura"
# ce_mun$mundesc[which(ce_mun$CMUN_NAME=="Roda de Barà")] <-
#   "Roda de Berà"
# ce_mun$mundesc[which(ce_mun$CMUN_NAME=="Calonge")] <-
#   "Calonge i Sant Antoni"

ce_mun$mundesc <- tolower(ce_mun$mundesc)

saveRDS(ce_mun, "../data/census2011_madrid.rds")
