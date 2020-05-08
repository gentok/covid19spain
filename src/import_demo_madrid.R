#' ---
#' title: "Import Demographic Data (Madrid)"
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

# Data Import 
require(readxl)
require(stringr)

# Importing Data Sets

# 1
tmp <- t(read_excel("../data/demo_madrid/indimuni19t01.xls", range="B6:J7", 
                 col_names = FALSE))
tmp[2,1] <- tmp[1,1]
tmp <- ifelse(is.na(tmp),"",tmp)
tmp <- str_squish(paste(tmp[,1],tmp[,2]))
d <- read_excel("../data/demo_madrid/indimuni19t01.xls", skip=8, n_max=193, 
                 col_names = c("munname",tmp), na=c("","-"))
d <- d %>% filter(!is.na(d$munname))
head(d)
dim(d)

# 2
tmp <- as.character(t(read_excel("../data/demo_madrid/indimuni19t02.xls", range="B6:F6", 
                    col_names = FALSE)))
dtmp <- read_excel("../data/demo_madrid/indimuni19t02.xls", skip=7, n_max=193,
                col_names = c("munname",tmp), na=c("","-"))
dtmp <- dtmp %>% filter(!is.na(dtmp$munname))
head(dtmp)
d <- d %>% inner_join(dtmp)
dim(d)

# 3
tmp <- t(read_excel("../data/demo_madrid/indimuni19t03.xls", range="B7:F8", 
                    col_names = FALSE))
tmp[2:3,1] <- tmp[1,1]
tmp <- ifelse(is.na(tmp),"",tmp)
tmp <- str_squish(paste(tmp[,1],tmp[,2]))
dtmp <- read_excel("../data/demo_madrid/indimuni19t03.xls", skip=9, n_max=193, 
                   col_names = c("munname",tmp), na=c("","-"))
dtmp <- dtmp %>% filter(!is.na(dtmp$munname))
head(dtmp)
d <- d %>% inner_join(dtmp)
dim(d)

# 4
tmp <- t(read_excel("../data/demo_madrid/indimuni19t04.xls", range="B6:K7", 
                    col_names = FALSE))
tmp[6:10,1] <- tmp[5,1]
tmp <- ifelse(is.na(tmp),"",tmp)
tmp <- str_squish(paste(tmp[,1],tmp[,2]))
dtmp <- read_excel("../data/demo_madrid/indimuni19t04.xls", skip=8, n_max=193, 
                   col_names = c("munname",tmp), na=c("","-"))
dtmp <- dtmp %>% filter(!is.na(dtmp$munname))
head(dtmp)
d <- d %>% inner_join(dtmp)
dim(d)

# 5
tmp <- t(read_excel("../data/demo_madrid/indimuni19t05.xls", range="B7:G8", 
                    col_names = FALSE))
tmp[3:6,1] <- tmp[2,1]
tmp <- ifelse(is.na(tmp),"",tmp)
tmp <- str_squish(paste(tmp[,1],tmp[,2]))
dtmp <- read_excel("../data/demo_madrid/indimuni19t05.xls", skip=9, n_max=193, 
                   col_names = c("munname",tmp), na=c("","-"))
dtmp <- dtmp %>% filter(!is.na(dtmp$munname))
head(dtmp)
d <- d %>% inner_join(dtmp)
dim(d)

# 6 
tmp <- t(read_excel("../data/demo_madrid/indimuni19t06.xls", range="B6:G7", 
                    col_names = FALSE))
tmp[3:6,1] <- tmp[2,1]
tmp <- ifelse(is.na(tmp),"",tmp)
tmp <- str_squish(paste(tmp[,1],tmp[,2]))
dtmp <- read_excel("../data/demo_madrid/indimuni19t06.xls", skip=8, n_max=195, # There are two not-assigned mun
                   col_names = c("munname",tmp), na=c("","-"))
dtmp <- dtmp %>% filter(!is.na(dtmp$munname))
head(dtmp)
d <- d %>% inner_join(dtmp)
dim(d)

# 7 
tmp <- t(read_excel("../data/demo_madrid/indimuni19t07.xls", range="B6:F7", 
                    col_names = FALSE))
tmp[3:5,1] <- tmp[2,1]
tmp <- ifelse(is.na(tmp),"",tmp)
tmp <- str_squish(paste(tmp[,1],tmp[,2]))
dtmp <- read_excel("../data/demo_madrid/indimuni19t07.xls", skip=8, n_max=193, 
                   col_names = c("munname",tmp), na=c("","-"))
dtmp <- dtmp %>% filter(!is.na(dtmp$munname))
head(dtmp)
d <- d %>% inner_join(dtmp)
dim(d)

# 8 
tmp <- t(read_excel("../data/demo_madrid/indimuni19t08.xls", range="B8:P9", 
                    col_names = FALSE))
tmp[2:7,1] <- tmp[1,1]
tmp[9:12,1] <- tmp[8,1]
tmp[14:15,1] <- tmp[13,1]
tmp <- ifelse(is.na(tmp),"",tmp)
tmp <- str_squish(paste(tmp[,1],tmp[,2]))
dtmp <- read_excel("../data/demo_madrid/indimuni19t08.xls", skip=10, n_max=195, # Not assigned muns 
                   col_names = c("munname",tmp), na=c("","-"))
dtmp <- dtmp %>% filter(!is.na(dtmp$munname))
head(dtmp)
d <- d %>% inner_join(dtmp)
dim(d)

# 9 
tmp <- t(read_excel("../data/demo_madrid/indimuni19t09.xls", range="B6:H7", 
                    col_names = FALSE))
tmp[2,1] <- tmp[1,1]
tmp <- ifelse(is.na(tmp),"",tmp)
tmp <- str_squish(paste(tmp[,1],tmp[,2]))
dtmp <- read_excel("../data/demo_madrid/indimuni19t09.xls", skip=8, n_max=195, # Not assigned muns 
                   col_names = c("munname",tmp), na=c("","-"))
dtmp <- dtmp %>% filter(!is.na(dtmp$munname))
head(dtmp)
d <- d %>% inner_join(dtmp)
dim(d)

# 10
tmp <- t(read_excel("../data/demo_madrid/indimuni19t10.xls", range="B6:G7", 
                    col_names = FALSE))
tmp[2:3,1] <- tmp[1,1]
tmp <- ifelse(is.na(tmp),"",tmp)
tmp <- str_squish(paste(tmp[,1],tmp[,2]))
dtmp <- read_excel("../data/demo_madrid/indimuni19t10.xls", skip=8, n_max=193, 
                   col_names = c("munname",tmp), na=c("","-"))
dtmp <- dtmp %>% filter(!is.na(dtmp$munname))
head(dtmp)
d <- d %>% inner_join(dtmp)
dim(d)

# 11
tmp <- t(read_excel("../data/demo_madrid/indimuni19t11.xls", range="B7:H8", 
                    col_names = FALSE))
tmp[2:7,1] <- tmp[1,1]
tmp <- ifelse(is.na(tmp),"",tmp)
tmp <- str_squish(paste(tmp[,1],tmp[,2]))
dtmp <- read_excel("../data/demo_madrid/indimuni19t11.xls", skip=9, n_max=193, 
                   col_names = c("munname",tmp), na=c("","-"))
dtmp <- dtmp %>% filter(!is.na(dtmp$munname))
head(dtmp)
d <- d %>% inner_join(dtmp)
dim(d)

# 12
tmp <- as.character(t(read_excel("../data/demo_madrid/indimuni19t12.xls", range="B6:D6", 
                                 col_names = FALSE)))
dtmp <- read_excel("../data/demo_madrid/indimuni19t12.xls", skip=7, n_max=193, 
                   col_names = c("munname",tmp), na=c("","-"))
dtmp <- dtmp %>% filter(!is.na(dtmp$munname))
head(dtmp)
d <- d %>% inner_join(dtmp)
dim(d)

# 13
tmp <- t(read_excel("../data/demo_madrid/indimuni19t13.xls", range="B7:I8", 
                    col_names = FALSE))
tmp[2:4,1] <- tmp[1,1]
tmp[6:8,1] <- tmp[5,1]
tmp <- ifelse(is.na(tmp),"",tmp)
tmp <- str_squish(paste(tmp[,1],tmp[,2]))
dtmp <- read_excel("../data/demo_madrid/indimuni19t13.xls", skip=9, n_max=193, 
                   col_names = c("munname",tmp), na=c("","-"))
dtmp <- dtmp %>% filter(!is.na(dtmp$munname))
head(dtmp)
d <- d %>% inner_join(dtmp)
dim(d)

# 14
tmp <- t(read_excel("../data/demo_madrid/indimuni19t14.xls", range="B6:F7", 
                    col_names = FALSE))
tmp[2:5,1] <- tmp[1,1]
tmp <- ifelse(is.na(tmp),"",tmp)
tmp <- str_squish(paste(tmp[,1],tmp[,2]))
dtmp <- read_excel("../data/demo_madrid/indimuni19t14.xls", skip=8, n_max=193, 
                   col_names = c("munname",tmp), na=c("","-"))
dtmp <- dtmp %>% filter(!is.na(dtmp$munname))
head(dtmp)
d <- d %>% inner_join(dtmp)
dim(d)

# 15
tmp <- as.character(t(read_excel("../data/demo_madrid/indimuni19t15.xls", range="B6:I6", 
                                 col_names = FALSE)))
dtmp <- read_excel("../data/demo_madrid/indimuni19t15.xls", skip=7, n_max=193, 
                   col_names = c("munname",tmp), na=c("","-"))
dtmp <- dtmp %>% filter(!is.na(dtmp$munname))
head(dtmp)
d <- d %>% inner_join(dtmp)
dim(d)

# 16
tmp <- t(read_excel("../data/demo_madrid/indimuni19t16.xls", range="B6:H7", 
                    col_names = FALSE))
tmp[7,1] <- tmp[6,1]
tmp <- ifelse(is.na(tmp),"",tmp)
tmp <- str_squish(paste(tmp[,1],tmp[,2]))
dtmp <- read_excel("../data/demo_madrid/indimuni19t16.xls", skip=8, n_max=193, 
                   col_names = c("munname",tmp), na=c("","-"))
dtmp <- dtmp %>% filter(!is.na(dtmp$munname))
head(dtmp)
d <- d %>% inner_join(dtmp)
dim(d)

## Create mundesc variable
d$mundesc <- tolower(d$munname)
d <- d[,c("mundesc", names(d)[-ncol(d)])]

## Save Data
saveRDS(d, "../data/demo_madrid.rds")