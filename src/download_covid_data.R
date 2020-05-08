#' ---
#' title: "Download Spanish COVID-19 PCR Test Data"
#' author: "Gento Kato"
#' date: "April 16, 2020"
#' ---

## Set Working Directory to the current directory 
## (If using RStudio, can be set automatically)
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("/home/gentok/GoogleDrive/Projects/Coronavirus_Project/Coronavirus_spain/src")

require(rvest)
require(stringr)
require(readr)

# Madrid Data
tmp <- read_html(RCurl::getURL("https://datos.comunidad.madrid/catalogo/dataset/covid19_tia_muni_y_distritos", ssl.verifypeer = FALSE))
tmp <- tmp %>% html_nodes("span") %>% html_attr("data-datetime")
madrid_date <- gsub("-","",as.Date(tmp[!is.na(tmp)][1]))
madrid_dloc <- "https://datos.comunidad.madrid/catalogo/dataset/7da43feb-8d4d-47e0-abd5-3d022d29d09e/resource/b2a3a3f9-1f82-42c2-89c7-cbd3ef801412/download/covid19_tia_muni_y_distritos.csv"
d <- RCurl::getURL(madrid_dloc, ssl.verifypeer = FALSE)
tmp <- tempfile()
writeLines(d, tmp)
d <- read.csv(tmp, stringsAsFactors = FALSE, sep=";")
write.csv(d, paste0("../data/covid_madrid/covid_madrid_",madrid_date,".csv"), row.names = FALSE)
write.csv(d, paste0("../data/covid_madrid/covid_madrid_latest.csv"), row.names = FALSE)
source("../src/import_covid_madrid.R")

# Cataluna Data
tmp <- read_html("https://analisi.transparenciacatalunya.cat/Salut/Registre-de-test-de-COVID-19-realitzats-a-Cataluny/jj6z-iyrp") %>% 
  html_text %>% str_extract('"dateModified"\\:"[0-9-T\\:\\+]+') %>% str_replace('"dateModified"\\:"','')
cataluna_date <- gsub("-","",as.Date(tmp))
cataluna_dloc <- "https://analisi.transparenciacatalunya.cat/api/views/jj6z-iyrp/rows.tsv?accessType=DOWNLOAD&sorting=true"
d <- RCurl::getURL(cataluna_dloc)
download.file(cataluna_dloc, "../data/covid_cataluna/covid_cataluna_latest.tsv")
d <- read_tsv("../data/covid_cataluna/covid_cataluna_latest.tsv")
write.csv(d, paste0("../data/covid_cataluna/covid_cataluna_",cataluna_date,".csv"), row.names = FALSE)
source("../src/import_covid_cataluna.R")

# Basque Data
tmp <- read_html("https://opendata.euskadi.eus/catalogo/-/evolucion-del-coronavirus-covid-19-en-euskadi/")
tmp <- tmp %>% html_nodes("li") %>% html_text() 
euskadi_date <- sub("20","",gsub("Fecha de fin de datos |/","",str_squish(tmp[grep("Fecha de fin de datos",tmp)])))
euskadi_dloc <- "https://opendata.euskadi.eus/contenidos/ds_informes_estudios/covid_19_2020/opendata/covid19.xlsx"
download.file(euskadi_dloc, paste0("../data/covid_euskadi/covid19-",euskadi_date,".xlsx"))
require(readxl)
datalist <- paste0("../data/covid_euskadi/",list.files("../data/covid_euskadi/"))
datalist <- datalist[-grep("latest", datalist)]
importdt <- function(k) {
  dtmp <- read_xlsx(k, sheet=2, skip=1, n_max=256)
  if (ncol(dtmp)>3) dtmp <- read_xlsx(k, sheet=3, skip=1, n_max=256)
  dtmp$date <- as.Date(str_extract(k,"[0-9]+20"),"%d%m%y")
  dtmp
}
d <- do.call("rbind",lapply(datalist, importdt))
names(d)[1:3] <- c("muncode","munname","cumpos")
write.csv(d, "../data/covid_euskadi/covid_euskadi_latest.csv", row.names=FALSE)

# Today's date in Spanish time
todayspain <- gsub("-","",as.Date(as.character(`attr<-`(Sys.time(),"tzone","Spain"))))
# Murcia Data 
require(pdftools)
murcia_dloc <- "http://www.murciasalud.es/covid19/informe_epidemiologico"
download.file(murcia_dloc, paste0("../data/covid_murcia/covid_murcia_",todayspain,".pdf"))
# dpdf <- pdf_text(paste0("../data/covid_murcia/covid_murcia_",todayspain,".pdf"))
# dtab <- sub("^.*Tabla 1\\.", "Tabla 1\\.", dpdf[grep("Tabla 1\\.", dpdf)])
# dtab <- sub("^.*Municipio", "Municipio", dtab)
# dtab <- sub("\\*Incidencia.*$|\\(\\-\\) Aquellos municipios.*$", "", dtab)
# dtab <- str_split(dtab, "\n")[[1]]
# dtab <- str_split(gsub("  ", "&", gsub("^[ ]+", "", dtab[-length(dtab)])),"[&]+")
# dtab
# dtab[[grep("Otras", dtab)]][4:5] <- ""
# dtab[[grep("Otros", dtab)]][4:5] <- ""
# dtab <- do.call("rbind",dtab)
# setcolname <- gsub("\\*","",str_squish(dtab[1,]))
# setcolname[3] <- "Percent"
# dtab <- as.data.frame(dtab[-1,])
# colnames(dtab) <- setcolname
# dtab$Municipio <- as.character(dtab$Municipio)
# dtab$Casos <- as.numeric(as.character(dtab$Casos))
# dtab$Percent <- as.numeric(as.character(gsub("%","",gsub(",",".",dtab$Percent))))
# dtab$Población <- as.numeric(as.character(dtab$Población))
# dtab$IA <- as.numeric(gsub(",",".",as.character(dtab$IA)))
# write.csv(dtab, paste0("../data/covid_murcia/covid_murcia_",todayspain,".csv"), row.names = FALSE)
# write.csv(dtab, paste0("../data/covid_murcia/covid_murcia_latest.csv"), row.names = FALSE)
