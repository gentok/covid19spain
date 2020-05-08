#' ---
#' title: "Import amd Recode COVID-19 PCR Test Data (Madrid)"
#' author: "Gento Kato"
#' date: "May 4, 2020"
#' ---

## Set Working Directory to the current directory 
## (If using RStudio, can be set automatically)
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Clear Workspace
rm(list=ls())

require(readr)

## File Location (Need / at the end)
dloc <- "../data/covid_madrid/"
## File Name
dfile <- "covid_madrid_latest.csv"

# Import Data 
d <- read_csv(paste0(dloc, dfile), locale=locale(encoding = "UTF-8"))
head(d)

##########################
## Recode Covid-19 Data ## 
##########################

## Date Data
d$Data <- as.Date(d$fecha_informe)

## All Lower Case Municipalities
d$munid <- d$codigo_geometria
d$mundesc <- tolower(d$municipio_distrito)

## Cummulative Cases
d$CumCasos <- d$casos_confirmados_totales 

## Reorder Dataset by: 1 Municipaclity Code, 2 Date
dcomp <- d[order(d$munid, d$Data), c("munid","mundesc","municipio_distrito","Data","CumCasos")]

## Save dcomp
saveRDS(dcomp, "../data/covid_madrid.rds")

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
