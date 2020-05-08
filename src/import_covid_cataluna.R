#' ---
#' title: "Import amd Recode COVID-19 PCR Test Data (Cataluna)"
#' author: "Gento Kato"
#' date: "April 16, 2020"
#' ---

## Set Working Directory to the current directory 
## (If using RStudio, can be set automatically)
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Clear Workspace
rm(list=ls())

require(readr)

## File Location (Need / at the end)
dloc <- "../data/covid_cataluna/"
## File Name
dfile <- "covid_cataluna_latest.tsv"

# Import Data 
d <- read_tsv(paste0(dloc, dfile), locale=locale(encoding = "UTF-8"))
## Adjust Error
#d <- d[which(str_squish(d$ComarcaDescripcio)!="Alt Emp"),]

# Check if everything can be matched 
demodt <- readRDS("../data/demo_cataluna_wide_rev.rds")
shapedt <- readRDS("../data/shapefile/shape_cataluna_rev.rds")
censusdt <- readRDS("../data/census2011_cataluna.rds")

##########################
## Recode Covid-19 Data ## 
##########################

## Date as Date Format
if ("TipusCasData"%in%colnames(d)) {
  d$Data <- as.Date(d$TipusCasData, format="%d/%m/%Y")
  d$ResultatCovidCodi <- ifelse(d$TipusCasDescripcio=="Positiu",1,0)
  d$ResultatCovidDescripcio <- ifelse(d$TipusCasDescripcio=="Positiu","Positiu","Negatiu")
  d <- d[,c("Data", "ComarcaCodi", "ComarcaDescripcio", "MunicipiCodi",      
            "MunicipiDescripcio", "SexeCodi", "SexeDescripcio", "ResultatCovidCodi", 
            "ResultatCovidDescripcio", "NumCasos")]
} else {
  d$Data <- as.Date(d$Data, format="%d/%m/%Y")
}
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
unique(d$MunicipiDescripcio[which(!d$mundesc %in% censusdt$mundesc)])
sort(demodt$munname[which(!demodt$mundesc %in% shapedt$mundesc)])
sort(shapedt$NOMMUNI[which(!shapedt$mundesc %in% demodt$mundesc)])
sort(censusdt$CCA_NAME[which(!censusdt$mundesc %in% demodt$mundesc)])

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
dcomp$mundesc <- as.character(dcomp$mundesc)

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
for (i in unique(dcomp$mundesc)) {
  for (j in unique(dcomp$SexeCodi)) {
    for (k in unique(dcomp$ResultatCovidCodi)) {
      dcomp$CumCasos[which(dcomp$mundesc==i & dcomp$SexeCodi==j & dcomp$ResultatCovidCodi==k)] <- 
        cumsum(dcomp$NumCasos[which(dcomp$mundesc==i & dcomp$SexeCodi==j & dcomp$ResultatCovidCodi==k)])
    }
  }
}
tail(dcomp)

## Make it Wide Data for Sex and Pos Neg
targetname <- names(dcomp)[!names(dcomp)%in%c("SexeCodi","SexeDescripcio","ResultatCovidCodi","ResultatCovidDescripcio")]
dwide <- dcomp[which(dcomp$SexeCodi==0 & dcomp$ResultatCovidCodi==0),targetname] # Subset Male Negative
names(dwide)[names(dwide)%in%c("NumCasos","CumCasos")] <- c("NumCasos_MN", "CumCasos_MN")
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
