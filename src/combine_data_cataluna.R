#' ---
#' title: "Import amd Recode COVID-19 PCR Test Data (Cataluna)"
#' author: "Gento Kato"
#' date: "April 16, 2020"
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

###########################################
## Revise Municipality Names for shapedt ##
###########################################

# New Variable for Matching
shapedt$mundesc <- shapedt$NOMMUNI

## Rename Municipality Names in shapedt to match with coviddt
# shapedt$mundesc[grep(", La$",shapedt$NOMMUNI)] <- 
#   paste("La", gsub(", La$","",shapedt$NOMMUNI[grep(", La$",shapedt$NOMMUNI)]))
# shapedt$mundesc[grep(", L$",shapedt$NOMMUNI)] <- 
#   paste("L", gsub(", L$","",shapedt$NOMMUNI[grep(", L$",shapedt$NOMMUNI)]))
# shapedt$mundesc[grep(", L'$",shapedt$NOMMUNI)] <- 
#   paste0("L'", gsub(", L'$","",shapedt$NOMMUNI[grep(", L'$",shapedt$NOMMUNI)]))
# shapedt$mundesc[grep(", Les$",shapedt$NOMMUNI)] <- 
#   paste("Les", gsub(", Les$","",shapedt$NOMMUNI[grep(", Les$",shapedt$NOMMUNI)]))
# shapedt$mundesc[grep(", El$",shapedt$NOMMUNI)] <- 
#   paste("El", gsub(", El$","",shapedt$NOMMUNI[grep(", El$",shapedt$NOMMUNI)]))
# shapedt$mundesc[grep(", Els$",shapedt$NOMMUNI)] <- 
#   paste("Els", gsub(", Els$","",shapedt$NOMMUNI[grep(", Els$",shapedt$NOMMUNI)]))
# shapedt$mundesc[grep(", Es$",shapedt$NOMMUNI)] <- 
#   paste("Es", gsub(", Es$","",shapedt$NOMMUNI[grep(", Es$",shapedt$NOMMUNI)]))
# shapedt$mundesc[which(shapedt$NOMMUNI=="Santa Maria de Corcó")] <-
#   "L'Esquirol"
# shapedt$mundesc[which(shapedt$NOMMUNI=="Saus,  Camallera i Llampaies")] <-
#   "Saus Camallera i Llampaies"
# shapedt$mundesc[which(shapedt$NOMMUNI=="Cruïlles,  Monells i Sant Sadurní de l'Heura")] <-
#   "Cruïlles Monells i Sant Sadurní de l'Heura"
# shapedt$mundesc[which(shapedt$NOMMUNI=="Roda de Barà")] <-
#   "Roda de Berà"
# shapedt$mundesc[which(shapedt$NOMMUNI=="Calonge")] <-
#   "Calonge i Sant Antoni"
shapedt$mundesc[which(shapedt$NOMMUNI=="Brunyola i Sant Martí Sapresa")] <-
  "Brunyola"
shapedt$mundesc[which(shapedt$NOMMUNI=="Saus, Camallera i Llampaies")] <-
  "Saus Camallera i Llampaies"
shapedt$mundesc[which(shapedt$NOMMUNI=="Cruïlles, Monells i Sant Sadurní de l'Heura")] <-
  "Cruïlles Monells i Sant Sadurní de l'Heura"

## All lower case
shapedt$mundesc <- tolower(shapedt$mundesc)

## Add one row with (altres municipis)
shapedt[nrow(shapedt)+1,] <- NA
shapedt$mundesc[nrow(shapedt)] <- "(altres municipis)"

##########################################
## Revise Municipality Names for demodt ##
##########################################

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

##########################
## Recode Covid-19 Data ## 
##########################

## Date as Date Format
d$Data <- as.Date(d$Data, format="%d/%m/%Y")
head(d$Data)

## New Varible for Dataset Matching
d$mundesc <- d$MunicipiDescripcio

## Correct Errors in Municipality Names
d$mundesc[which(d$MunicipiDescripcio=="Catellví de Rosanes")] <-
  "Castellví de Rosanes"
d$mundesc[which(d$MunicipiDescripcio=="Fogars de Tordera")] <-
  "Fogars de la Selva"
d$mundesc[which(d$MunicipiDescripcio=="Sant Quirze  Safaja")] <-
  "Sant Quirze Safaja"
d$mundesc[which(d$MunicipiDescripcio=="Sant Adrià del Besòs")] <-
  "Sant Adrià de Besòs"
d$mundesc[which(d$MunicipiDescripcio=="Santa Perpétua de Mogoda")] <-
  "Santa Perpètua de Mogoda"
d$mundesc[which(d$MunicipiDescripcio=="Mortellà i Martinet")] <-
  "Montellà i Martinet"
d$mundesc[which(d$MunicipiDescripcio=="Torrefeta i florejacs")] <-
  "Torrefeta i Florejacs"
d$mundesc[which(d$MunicipiDescripcio=="Vespella")] <-
  "Vespella de Gaià"
d$mundesc[which(d$MunicipiDescripcio=="Vinyols i  els Arcs")] <-
  "Vinyols i els Arcs"
d$mundesc[which(d$MunicipiDescripcio=="La Vall de Cardós")] <-
  "Vall de Cardós"
d$mundesc[which(d$MunicipiDescripcio=="Torres del Segre")] <-
  "Torres de Segre"
d$mundesc[which(d$MunicipiDescripcio=="Boadella d'Empordà")] <-
  "Boadella i les Escaules"

## Missing Municipality == (Altres municipis)'s Municipality code as 999999 and Comarca Code as 99
d$MunicipiCodi[d$MunicipiDescripcio=="(Altres municipis)"] <- 99999
d$ComarcaCodi[d$MunicipiDescripcio=="(Altres municipis)"] <- 99

## All Lower Case Municipalities
d$mundesc <- tolower(d$mundesc)

## Check if all matched.
unique(d$MunicipiDescripcio[which(!d$mundesc %in% demodt$mundesc)])
unique(d$MunicipiDescripcio[which(!d$mundesc %in% shapedt$mundesc)])
unique(d$MunicipiDescripcio[which(!d$mundesc %in% pop18$mundesc)])
sort(demodt$munname[which(!demodt$mundesc %in% shapedt$mundesc)])
sort(pop18$munname[which(!shapedt$mundesc %in% pop18$mundesc)])
sort(shapedt$NOMMUNI[which(!shapedt$mundesc %in% demodt$mundesc)])

## Reorder Dataset by: 1 Municipaclity Code, 2 Date, 3 Sex, and 4 Test Result
d <- d[order(as.character(demodt$munid[match(d$mundesc, demodt$mundesc)]), d$Data, d$SexeCodi, d$ResultatCovidCodi), 
       c("mundesc",names(d)[-c(length(names(d)))])]

## Check
head(d)
tail(d)
##* It turns out that if no test is reported, such row is missing in data


## Complete Missing Rows by Inserting 0s
dcomp <- merge(expand.grid(mundesc = demodt$mundesc, 
                           Data = seq(min(d$Data),max(d$Data),by=1), 
                           SexeCodi = unique(d$SexeCodi),
                           ResultatCovidCodi = unique(d$ResultatCovidCodi)), 
               d, all.x = TRUE)
dcomp <- dcomp[,names(d)]
dcomp$munname <- as.character(dcomp$munname)

## Replace Missing Values in Completed Data
dcomp$NumCasos[is.na(dcomp$NumCasos)] <- 0
for(k in unique(na.omit(d$MunicipiCodi))) {
  dcomp$ComarcaCodi[dcomp$MunicipiCodi==k] <- na.omit(d$ComarcaCodi[d$MunicipiCodi==k])[1]
  dcomp$ComarcaDescripcio[dcomp$MunicipiCodi==k] <- na.omit(d$ComarcaDescripcio[d$MunicipiCodi==k])[1]
  dcomp$MunicipiDescripcio[dcomp$MunicipiCodi==k] <- na.omit(d$MunicipiDescripcio[d$MunicipiCodi==k])[1]
}
for (k in unique(d$SexeCodi)) {
  dcomp$SexeDescripcio[dcomp$SexeCodi==k] <- na.omit(d$SexeDescripcio[d$SexeCodi==k])[1]
}
for (k in unique(d$ResultatCovidCodi)) {
  dcomp$ResultatCovidDescripcio[dcomp$ResultatCovidCodi==k] <- 
    na.omit(d$ResultatCovidDescripcio[d$ResultatCovidCodi==k])[1]
}

## Cummulative Cases
dcomp$CumCasos <- NA
for (i in unique(dcomp$munname)) {
  for (j in unique(dcomp$SexeCodi)) {
    for (k in unique(dcomp$ResultatCovidCodi)) {
      dcomp$CumCasos[which(dcomp$munname==i & dcomp$SexeCodi==j & dcomp$ResultatCovidCodi==k)] <- 
        cumsum(dcomp$NumCasos[which(dcomp$munname==i & dcomp$SexeCodi==j & dcomp$ResultatCovidCodi==k)])
    }
  }
}
tail(dcomp)

## Make it Wide Data for Sex and Pos Neg
dwide <- dcomp[which(dcomp$SexeCodi==0 & dcomp$ResultatCovidCodi==0),-c(8:11)] # Subset Male Negative
names(dwide)[c(8,9)] <- c("NumCasos_MN", "CumCasos_MN")
dwide$NumCasos_MP <- dcomp$NumCasos[which(dcomp$SexeCodi==0 & dcomp$ResultatCovidCodi==1)]
dwide$CumCasos_MP <- dcomp$CumCasos[which(dcomp$SexeCodi==0 & dcomp$ResultatCovidCodi==1)]
dwide$NumCasos_FN <- dcomp$NumCasos[which(dcomp$SexeCodi==1 & dcomp$ResultatCovidCodi==0)]
dwide$CumCasos_FN <- dcomp$CumCasos[which(dcomp$SexeCodi==1 & dcomp$ResultatCovidCodi==0)]
dwide$NumCasos_FP <- dcomp$NumCasos[which(dcomp$SexeCodi==1 & dcomp$ResultatCovidCodi==1)]
dwide$CumCasos_FP <- dcomp$CumCasos[which(dcomp$SexeCodi==1 & dcomp$ResultatCovidCodi==1)]
dwide$NumCasos_M <- dwide$NumCasos_MN + dwide$NumCasos_MP
dwide$CumCasos_M <- dwide$CumCasos_MN + dwide$CumCasos_MP
dwide$NumCasos_F <- dwide$NumCasos_FN + dwide$NumCasos_FP
dwide$CumCasos_F <- dwide$CumCasos_FN + dwide$CumCasos_FP
dwide$NumCasos_N <- dwide$NumCasos_MN + dwide$NumCasos_FN
dwide$CumCasos_N <- dwide$CumCasos_MN + dwide$CumCasos_FN
dwide$NumCasos_P <- dwide$NumCasos_MP + dwide$NumCasos_FP
dwide$CumCasos_P <- dwide$CumCasos_MP + dwide$CumCasos_FP
dwide$NumCasos <- dwide$NumCasos_N + dwide$NumCasos_P
dwide$CumCasos <- dwide$CumCasos_N + dwide$CumCasos_P

## Save dwide & dlong
saveRDS(dwide, "../data/covid_cataluna_wide.rds")
saveRDS(dcomp, "../data/covid_cataluna_long.rds")

# ## Plot ##
# 
# require(ggplot2)
# 
# ## Before Starting, Exclue Barcelona, since it is an outlier
# dwide$MunicipiDescripcio[dwide$CumCasos>10000]
# dwide$MunicipiDescripcio[dwide$CumCasos>5000]
# 
# ## Cummulative Positve Cases
# p <- ggplot(dwide[dwide$MunicipiDescripcio!="Barcelona",], 
#        aes(x=Data, y=CumCasos_P)) + 
#   geom_line(aes(color=as.factor(MunicipiCodi))) + 
#   labs(x="Date", y="Cummulative Numbmer of Positive Cases") + 
#   theme_bw() + theme(legend.position = "none")
# p
# ggsave("../out/poscasescum.png", p, width=8, height=6)
# 
# ## Cummulative Test Cases
# p <- ggplot(dwide[dwide$MunicipiDescripcio!="Barcelona",], 
#        aes(x=Data, y=CumCasos)) + 
#   geom_line(aes(color=as.factor(MunicipiCodi))) + 
#   labs(x="Date", y="Cummulative Numbmer of Tested Cases") + 
#   theme_bw() + theme(legend.position = "none")
# p
# ggsave("../out/testcasescum.png", p, width=8, height=6)  
# 
# ## Number of Positive Cases 
# p <- ggplot(dwide[dwide$MunicipiDescripcio!="Barcelona",], 
#        aes(x=Data, y=NumCasos_P)) + 
#   geom_line(aes(color=as.factor(MunicipiCodi))) + 
#   labs(x="Date", y="Daily Numbmer of Positive Cases") + 
#   theme_bw() + theme(legend.position = "none")
# p
# ggsave("../out/poscasesnum.png", p, width=8, height=6)
# 
# ## Number of Test Cases 
# p <- ggplot(dwide[dwide$MunicipiDescripcio!="Barcelona",], 
#        aes(x=Data, y=NumCasos)) + 
#   geom_line(aes(color=as.factor(MunicipiCodi))) + 
#   labs(x="Date", y="Daily Numbmer of Tested Cases") + 
#   theme_bw() + theme(legend.position = "none")
# p
# ggsave("../out/testcasesnum.png", p, width=8, height=6)
# 
# ## Total Number of Test Cases versus Positive Rate (Exclude Municipality with no positive case)
# require(ggrepel)
# ## Using Log axes
# target <- dwide[which(dwide$Data==max(dwide$Data) & dwide$CumCasos_P>=1),]
# p <- ggplot(target, 
#        aes(x=CumCasos, y=CumCasos_P)) + 
#   geom_point(alpha=0.5) + 
#   geom_text_repel(data=subset(target, CumCasos > 800 | CumCasos > 30 & CumCasos_P/CumCasos < 0.2),
#             aes(x=CumCasos,y=CumCasos_P,label=MunicipiDescripcio), size=3) + 
#   scale_x_log10() + 
#   scale_y_log10() + 
#   labs(x="Total Tested Cases", y="Total Positive Cases", 
#        title="Relationship between Tested and Positive Cases",
#        subtitle="(Names shown for >800 tested cases OR positve rate < 0.2 & >30 tested cases)") + 
#   theme_bw() + theme(legend.position = "none", 
#                      plot.title = element_text(hjust=0.5),
#                      plot.subtitle = element_text(hjust=0.5))
# p
# ggsave("../out/testpositiverel.png", p, width=8, height=8)
