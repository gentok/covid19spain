#' ---
#' title: "Import and Save Demographic Data for Calalonia"
#' author: "Gento Kato"
#' date: "April 11, 2020"
#' ---

## Set Working Directory to the current directory 
## (If using RStudio, can be set automatically)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Clear Workspace
rm(list=ls())

##
library(rvest)
library(rjson)
library(tidyr)

# Extract the List of Municipality IDs
tmp <- read_html("https://api.idescat.cat/emex/v1/dades.json?lang=en&tipus=mun")
tmp <- fromJSON(tmp %>% html_nodes("p") %>% html_text)
munids <- data.frame(id = sapply(tmp$fitxes$cols$col, function(k) k$id),
                     content = sapply(tmp$fitxes$cols$col, function(k) k$content))
munids

dlong <- data.frame(munid=character(),munname=character(),comid=character(),comname=character(),
                    gid=character(), gdesc1=character(), gdesc2=character(),
                    tid=character(), tdesc1=character(), tdesc2=character(), tyear=character(), tupdate=character(),
                    turl=character(),
                    fid=character(), fdesc1=character(), fdesc2=character(), fyear=character(), fupdate=character(), 
                    fscale=character(), fval=numeric(), fval_com=numeric(), fval_cat=numeric())

for (muncase in 1:nrow(munids)) {
  
  munid = munids$id[muncase]
  munname = munids$content[muncase]
  
  Sys.sleep(0.01)
  cat(paste("\n\nExtract Municipality", munid, "...\n\n"))
  
  apitarget <- paste0("https://api.idescat.cat/emex/v1/dades.json?id=",munid,"&lang=en&tipus=mun")
  readjsondata <- function(apitarget) {
    tmp <- try(read_html(apitarget), silent =TRUE)
    tmp <- fromJSON(tmp %>% html_nodes("p") %>% html_text)
    if (!is.null(tmp$emex)) stop("ERROR!")
    return(tmp)
  }
  tmp <- try(readjsondata(apitarget), silent =TRUE)
  while (class(tmp)[1]=="try-error") tmp <- try(readjsondata(apitarget), silent =TRUE)

  for (i in seq(1,length(tmp$fitxes$gg$g))) {
    
    Sys.sleep(0.01)
    cat(paste("\tExtract Group", i, "...\n"))
    
    if(is.null(tmp$fitxes$gg$g[[i]]$tt$t$ff)) {
      
      for(j in seq(1,length(tmp$fitxes$gg$g[[i]]$tt$t))) {

        Sys.sleep(0.01)
        cat(paste("\t\tExtract SubGroup", j, "..."))

        if (is.null(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$ff$f$v)) {
          
          dlong_add <- data.frame(
            munid = munid,
            munname = munname,
            comid = tmp$fitxes$cols$col[[2]]$id,
            comname = tmp$fitxes$cols$col[[2]]$content,
            gid = tmp$fitxes$gg$g[[i]]$id,
            gdesc1 = ifelse(is.null(tmp$fitxes$gg$g[[i]]$c),NA,tmp$fitxes$gg$g[[i]]$c),
            gdesc2 = ifelse(is.null(tmp$fitxes$gg$g[[i]]$calt),NA,tmp$fitxes$gg$g[[i]]$calt),
            tid = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$id),NA,tmp$fitxes$gg$g[[i]]$tt$t[[j]]$id),
            tdesc1 = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$c),NA,tmp$fitxes$gg$g[[i]]$tt$t[[j]]$c),
            tdesc2 = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$calt),NA,tmp$fitxes$gg$g[[i]]$tt$t[[j]]$calt),
            tyear = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$r),NA,tmp$fitxes$gg$g[[i]]$tt$t[[j]]$r),
            turl = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$l),NA,tmp$fitxes$gg$g[[i]]$tt$t[[j]]$l),
            tupdate = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$updated),NA,tmp$fitxes$gg$g[[i]]$tt$t[[j]]$updated),
            fid = sapply(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$ff$f, function(k) ifelse(is.null(k$id),NA,k$id)),
            fdesc1 = sapply(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$ff$f, function(k) ifelse(is.null(k$c),NA,k$c)),
            fdesc2 = sapply(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$ff$f, function(k) ifelse(is.null(k$calt),NA,k$calt)),
            fyear = sapply(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$ff$f, function(k) ifelse(is.null(k$r),NA,k$r)),
            fupdate = sapply(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$ff$f, function(k) ifelse(is.null(k$updated),NA,k$updated)),
            fscale = sapply(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$ff$f, function(k) ifelse(is.null(k$u),NA,k$u)),
            fval = sapply(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$ff$f, function(k) ifelse(is.null(k$v),NA,gsub("\\,.*$","",k$v))),
            fval_com = sapply(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$ff$f, function(k) ifelse(is.null(k$v),NA,gsub("^[0-9\\.]+\\,|\\,[0-9\\.]+$","",k$v))),
            fval_cat = sapply(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$ff$f, function(k) ifelse(is.null(k$v),NA,gsub("^.*\\,","",k$v)))
          )
          
        } else {
          
          dlong_add <- data.frame(
            munid = munid,
            munname = munname,
            comid = tmp$fitxes$cols$col[[2]]$id,
            comname = tmp$fitxes$cols$col[[2]]$content,
            gid = tmp$fitxes$gg$g[[i]]$id,
            gdesc1 = ifelse(is.null(tmp$fitxes$gg$g[[i]]$c),NA,tmp$fitxes$gg$g[[i]]$c),
            gdesc2 = ifelse(is.null(tmp$fitxes$gg$g[[i]]$calt),NA,tmp$fitxes$gg$g[[i]]$calt),
            tid = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$id),NA,tmp$fitxes$gg$g[[i]]$tt$t[[j]]$id),
            tdesc1 = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$c),NA,tmp$fitxes$gg$g[[i]]$tt$t[[j]]$c),
            tdesc2 = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$calt),NA,tmp$fitxes$gg$g[[i]]$tt$t[[j]]$calt),
            tyear = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$r),NA,tmp$fitxes$gg$g[[i]]$tt$t[[j]]$r),
            tupdate = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$updated),NA,tmp$fitxes$gg$g[[i]]$tt$t[[j]]$updated),
            turl = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$l),NA,tmp$fitxes$gg$g[[i]]$tt$t[[j]]$l),
            fid = tmp$fitxes$gg$g[[i]]$tt$t[[j]]$ff$f$id,
            fdesc1 = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$ff$f$c),NA,tmp$fitxes$gg$g[[i]]$tt$t[[j]]$ff$f$c),
            fdesc2 = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$ff$f$calt),NA,tmp$fitxes$gg$g[[i]]$tt$t[[j]]$ff$f$calt),
            fyear = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$ff$f$r),NA,tmp$fitxes$gg$g[[i]]$tt$t[[j]]$ff$f$r),
            fupdate = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$ff$f$updated),NA,tmp$fitxes$gg$g[[i]]$tt$t[[j]]$ff$f$updated),
            fscale = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t[[j]]$ff$f$u),NA,tmp$fitxes$gg$g[[i]]$tt$t[[j]]$ff$f$u),
            fval = gsub("\\,.*$","",tmp$fitxes$gg$g[[i]]$tt$t[[j]]$ff$f$v),
            fval_com = gsub("^[0-9\\.]+\\,|\\,[0-9\\.]+$","",tmp$fitxes$gg$g[[i]]$tt$t[[j]]$ff$f$v),
            fval_cat = gsub("^.*\\,","",tmp$fitxes$gg$g[[i]]$tt$t[[j]]$ff$f$v)
          )
          
        } 
        
        dlong = rbind(dlong,dlong_add)
        
        cat(paste("DONE!\n"))

      }
      
    } else {
      
      if (is.null(tmp$fitxes$gg$g[[i]]$tt$t$ff$f$v)) {
        
        dlong_add <- data.frame(
          munid = munid,
          munname = munname,
          comid = tmp$fitxes$cols$col[[2]]$id,
          comname = tmp$fitxes$cols$col[[2]]$content,
          gid = tmp$fitxes$gg$g[[i]]$id,
          gdesc1 = ifelse(is.null(tmp$fitxes$gg$g[[i]]$c),NA,tmp$fitxes$gg$g[[i]]$c),
          gdesc2 = ifelse(is.null(tmp$fitxes$gg$g[[i]]$calt),NA,tmp$fitxes$gg$g[[i]]$calt),
          tid = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t$id),NA,tmp$fitxes$gg$g[[i]]$tt$t$id),
          tdesc1 = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t$c),NA,tmp$fitxes$gg$g[[i]]$tt$t$c),
          tdesc2 = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t$calt),NA,tmp$fitxes$gg$g[[i]]$tt$t$calt),
          tyear = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t$r),NA,tmp$fitxes$gg$g[[i]]$tt$t$r),
          tupdate = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t$updated),NA,tmp$fitxes$gg$g[[i]]$tt$t$updated),
          turl = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t$l),NA,tmp$fitxes$gg$g[[i]]$tt$t$l),
          fid = sapply(tmp$fitxes$gg$g[[i]]$tt$t$ff$f, function(k) ifelse(is.null(k$id),NA,k$id)),
          fdesc1 = sapply(tmp$fitxes$gg$g[[i]]$tt$t$ff$f, function(k) ifelse(is.null(k$c),NA,k$c)),
          fdesc2 = sapply(tmp$fitxes$gg$g[[i]]$tt$t$ff$f, function(k) ifelse(is.null(k$calt),NA,k$calt)),
          fyear = sapply(tmp$fitxes$gg$g[[i]]$tt$t$ff$f, function(k) ifelse(is.null(k$r),NA,k$r)),
          fupdate = sapply(tmp$fitxes$gg$g[[i]]$tt$t$ff$f, function(k) ifelse(is.null(k$updated),NA,k$updated)),
          fscale = sapply(tmp$fitxes$gg$g[[i]]$tt$t$ff$f, function(k) ifelse(is.null(k$u),NA,k$u)),
          fval = sapply(tmp$fitxes$gg$g[[i]]$tt$t$ff$f, function(k) ifelse(is.null(k$v),NA,gsub("\\,.*$","",k$v))),
          fval_com = sapply(tmp$fitxes$gg$g[[i]]$tt$t$ff$f, function(k) ifelse(is.null(k$v),NA,gsub("^[0-9\\.]+\\,|\\,[0-9\\.]+$","",k$v))),
          fval_cat = sapply(tmp$fitxes$gg$g[[i]]$tt$t$ff$f, function(k) ifelse(is.null(k$v),NA,gsub("^.*\\,","",k$v)))
          
        )

      } else {
        
        dlong_add <- data.frame(
          munid = munid,
          munname = munname,
          comid = tmp$fitxes$cols$col[[2]]$id,
          comname = tmp$fitxes$cols$col[[2]]$content,
          gid = tmp$fitxes$gg$g[[i]]$id,
          gdesc1 = ifelse(is.null(tmp$fitxes$gg$g[[i]]$c),NA,tmp$fitxes$gg$g[[i]]$c),
          gdesc2 = ifelse(is.null(tmp$fitxes$gg$g[[i]]$calt),NA,tmp$fitxes$gg$g[[i]]$calt),
          tid = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t$id),NA,tmp$fitxes$gg$g[[i]]$tt$t$id),
          tdesc1 = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t$c),NA,tmp$fitxes$gg$g[[i]]$tt$t$c),
          tdesc2 = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t$calt),NA,tmp$fitxes$gg$g[[i]]$tt$t$calt),
          tyear = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t$r),NA,tmp$fitxes$gg$g[[i]]$tt$t$r),
          tupdate = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t$updated),NA,tmp$fitxes$gg$g[[i]]$tt$t$updated),
          turl = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t$l),NA,tmp$fitxes$gg$g[[i]]$tt$t$l),
          fid = tmp$fitxes$gg$g[[i]]$tt$t$ff$f$id,
          fdesc1 = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t$ff$f$c),NA,tmp$fitxes$gg$g[[i]]$tt$t$ff$f$c),
          fdesc2 = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t$ff$f$calt),NA,tmp$fitxes$gg$g[[i]]$tt$t$ff$f$calt),
          fyear = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t$ff$f$r),NA,tmp$fitxes$gg$g[[i]]$tt$t$ff$f$r),
          fupdate = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t$ff$f$updated),NA,tmp$fitxes$gg$g[[i]]$tt$t$ff$f$updated),
          fscale = ifelse(is.null(tmp$fitxes$gg$g[[i]]$tt$t$ff$f$u),NA,tmp$fitxes$gg$g[[i]]$tt$t$ff$f$u),
          fval = gsub("\\,.*$","",tmp$fitxes$gg$g[[i]]$tt$t$ff$f$v),
          fval_com = gsub("^[0-9\\.]+\\,|\\,[0-9\\.]+$","",tmp$fitxes$gg$g[[i]]$tt$t$ff$f$v),
          fval_cat = gsub("^.*\\,","",tmp$fitxes$gg$g[[i]]$tt$t$ff$f$v)
        )

      } 
      
      dlong = rbind(dlong,dlong_add)
      
    }
    
    cat(paste("\tDONE!\n\n"))
    
  }
  
  saveRDS(dlong, "../data/demo_cataluna/demo_cataluna_long.rds")
  
  cat(paste0("DONE! ", "(", round((muncase/nrow(munids))*100,2), "%)\n"))
  
}


## The process failed several times. If so, restarted from error location
# which(munids$id=="250169") # The Location of Municipality with Error (restart for loop from this number)
# which(munids$id==tail(dlong$munid,1)) # The last Imported Municipality Location

## All Character Variables and Save
for(i in 1:ncol(dlong)) dlong[,i] <- as.character(dlong[,i])
saveRDS(dlong, "../data/demo_cataluna/demo_cataluna_long.rds")

## Create Codebook (Just Use the First Municipality)
dlong <- readRDS("../data/demo_cataluna/demo_cataluna_long.rds")
codebook <- dlong[which(!duplicated(dlong$fid)),
                  names(dlong)[!names(dlong)%in%c("munid","munname","fval","tupdate","turl","fupdate")]]
write.csv(codebook, "../data/demo_cataluna/demo_cataluna_codebook.csv", row.names = FALSE)

## Wide Data
dwide <- spread(dlong[,c("munid","munname","fid","fval")], fid, fval)
saveRDS(dwide, "../data/demo_cataluna/demo_cataluna_wide.rds")

