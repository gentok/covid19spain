#' ---
#' title: "Analysis of COVID-19 PCR Test Data"
#' author: "Gento Kato"
#' date: "April 15, 2020"
#' ---

library(tidyverse)
library(broom)
library(sf)
library(fts)

## Set Working Directory to the current directory 
## (If using RStudio, can be set automatically)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Clear Workspace
rm(list=ls())

## Import Relevant Data
coviddt <- readRDS("../data/covid_cataluna_wide.rds") %>% filter(mundesc != "(altres municipis)")
shapedt <- readRDS("../data/shapefile/shape_cataluna_rev.rds") %>% filter(mundesc != "(altres municipis)")
demodt <- readRDS("../data/demo_cataluna_wide_rev.rds") %>% filter(mundesc != "(altres municipis)")
censusdt <- readRDS("../data/census2011_cataluna.rds") %>% filter(mundesc != "(altres municipis)")

# Merge ALl Data
granddt = coviddt %>% 
  inner_join(demodt, by = c("mundesc"="mundesc")) %>% 
  inner_join(shapedt, by = c("mundesc"="mundesc")) %>% 
  inner_join(censusdt, by = c("mundesc"="mundesc"))

# Recode Data
granddt <- granddt %>% 
  mutate(pos_day = ifelse(is.na(NumCasos_P),0,NumCasos_P),
         test_day = ifelse(is.na(NumCasos),0,NumCasos),
         pos = ifelse(is.na(CumCasos_P),0,CumCasos_P),
         test = ifelse(is.na(CumCasos),0,CumCasos),
         posN = ifelse(is.na(NumCasos_P),0,NumCasos_P),
         testN = ifelse(is.na(NumCasos),0,NumCasos),
         pop = as.numeric(as.character(f321)),
         pop18 = granddt$f171_18, # Population 2018
         pop_15to64 = as.numeric(as.character(f27)),
         f28 = as.numeric(as.character(f28)),
         f29 = as.numeric(as.character(f29)),
         pop_65plus = f28 + f29,
         poppr_65plus = (pop_65plus / pop) * 100,
         popdens = as.numeric(as.character(f262)),
         pop_unemp = as.numeric(as.character(f222)),
         pop_active = as.numeric(as.character(f223)),
         unemprate = (pop_unemp / pop_active) * 100,
         pop_regunemp = as.numeric(as.character(f242)), # Annual Average N of Registered Unemployment
         regunemprate = (pop_regunemp / pop_15to64) * 100,
         avincome = as.numeric(as.character(f7)),
         taxbaseincome = as.numeric(as.character(f10))/10000,
         totalwealth = as.numeric(as.character(f180))/10000,
         posrate = (pos / test) * 100,
         pos_100k = pos / (pop / 100000),
         test_100k = test / (pop / 100000),
         hotelbeds = as.numeric(as.character(f216)),
         hotelbeds_dum = ifelse(hotelbeds >= median(hotelbeds), 1, 0),
         jobcat_service = as.numeric(gsub("_",NA,as.character(f280))),
         jobcat_totalpop = as.numeric(gsub("_",NA,as.character(f282))),
         prop_service = (jobcat_service / jobcat_totalpop)*100,
         men_15to64 = as.numeric(as.character(f33)),
         men_regunemp = as.numeric(as.character(f240)),
         regunemprate_men = men_regunemp / men_15to64,
         women_15to64 = as.numeric(as.character(f39)),
         women_regunemp = as.numeric(as.character(f241)),
         regunemprate_women = women_regunemp / women_15to64,
         death2018 = as.numeric(as.character(f188)),
         immig_noEU = as.numeric(as.character(f316)),
         pop_noESP = as.numeric(as.character(f73)),
         deathrate = (death2018 / pop18)*100, 
         prop_immig_noEU = (immig_noEU / pop18)*100, 
         prop_pop_noESP = (pop_noESP / pop18)*100, 
         mainhh = as.numeric(as.character(f250)), # Number of main households in 2011
         numhh = pop / mainhh,
         pop_smallhouse = t19_1 + t19_2 + t19_3 + t19_4 + t19_5,
         pop_housesizeall = t19_1 + t19_2 + t19_3 + t19_4 + t19_5 + t19_6 + t19_7 + t19_8 + t19_9 + t19_10,
         prop_smallhouse = (pop_smallhouse / pop_housesizeall)*100,
         pop_lowpricerent = t18_5,
         pop_housingall = t18_1 + t18_2 + t18_3 + t18_4 + t18_5 + t18_6,
         prop_lowpricerent = (pop_lowpricerent / pop_housingall)*100,
         pop_univ = t12_5,
         pop_eduall = t12_1 + t12_2 + t12_3 + t12_4 + t12_5,
         prop_univ = (pop_univ/pop_eduall)*100)

## Moving Sum by 7 days
granddt$pos_wk <- NA
for (i in unique(granddt$mundesc)) {
  granddt$pos_wk[granddt$mundesc==i] <- c(rep(NA,6),as.vector(moving.sum(fts(granddt$Data[granddt$mundesc==i], 
                                                         granddt$pos_day[granddt$mundesc==i]), periods=7)))
  granddt$test_wk[granddt$mundesc==i] <- c(rep(NA,6),as.vector(moving.sum(fts(granddt$Data[granddt$mundesc==i], 
                                                         granddt$test_day[granddt$mundesc==i]), periods=7)))
}
tail(granddt$pos_wk,30)

# Plotting Function
plotEstMod <- function(EstMod,startdate,type,modname,dvtext,varlab) {
  names(EstMod)[names(EstMod)==modname] <- "targetmod"
  if (type=="count") TYPE="Count" 
  if (type=="zero") TYPE="Zero Inflation" 
  EstOut <- data.frame(estimate=numeric(), term=character(),
                       std.error=numeric(), type=character())
  for (i in startdate:length(EstMod$targetmod)) {
    c <- EstMod$targetmod[[i]]$coefficients[[type]]
    k <- EstMod$targetmod[[i]]$vcov
    k <- sqrt(diag(k[grep(type,rownames(k)),grep(type,colnames(k))]))
    EstOut <- rbind(EstOut, data.frame(estimate=c, term=names(c), 
                                       std.error=k, type=type, Data=EstMod$Data[i]))
  }
  EstOut <- EstOut %>% 
    mutate(up95 = estimate + qnorm(0.975)*std.error,
           lo95 = estimate - qnorm(0.975)*std.error,
           up90 = estimate + qnorm(0.95)*std.error,
           lo90 = estimate - qnorm(0.95)*std.error)
  ggplot(EstOut, aes_string(x = "Data", y = "estimate")) +
    geom_hline(aes(yintercept=0), linetype=2) + 
    geom_ribbon(aes_string(ymax="up95", ymin="lo95", fill = "term"), alpha=0.2) + 
    geom_ribbon(aes_string(ymax="up90", ymin="lo90", fill = "term"), alpha=0.2) + 
    geom_line(aes_string(colour = "term"))  + 
    facet_wrap(vars(factor(EstOut$term, levels=unique(EstOut$term),
                           labels = varlab)), 
               scales="free_y") + 
    labs(title = paste0("Relationship between Municipality Statistics and \nCummulative COVID-19 ", dvtext, " Cases"),
         x = "Date", y = paste0(TYPE, " Model Coefficient with 90% and 95% CI"),
         caption = paste0("Models estimated daily by hurdle model with binomial logit for zero-inflation and negative binomial regression for count.\n", 
                          "Included cases recored by ", 
                          gsub("-","/",as.character(max(EstMod$Data))),
                          " (N=", EstMod$targetmod[[1]]$n,").")) + 
    theme_classic() + theme(legend.position = "none",
                            plot.title = element_text(hjust=0.5),
                            axis.text.x = element_text(angle=30,vjust=0.5)) 
}

# Estimate Models by Day
require(pscl)
doEstMod_hurdle <- function(threshold_Data) {
  granddt %>%
    filter(Data>=threshold_Data) %>%  
    group_by(Data) %>% 
    do(m1 = hurdle(pos ~ regunemprate + taxbaseincome + #prop_lowpricerent +
                     poppr_65plus + deathrate + 
                     prop_univ + 
                     hotelbeds_dum + prop_immig_noEU +  #prop_service + 
                     log(pop) + log(popdens) + numhh + prop_smallhouse, 
                   dist = "negbin", data = .),
       m2 = hurdle(pos ~ unemprate + taxbaseincome + #prop_lowpricerent +
                     poppr_65plus + deathrate + 
                     prop_univ + 
                     hotelbeds_dum + prop_immig_noEU +  #prop_service + 
                     log(pop) + log(popdens) + numhh + prop_smallhouse, 
                   dist = "negbin", data = .),
       m3 = hurdle(pos ~ regunemprate + totalwealth + #prop_lowpricerent +
                     poppr_65plus + deathrate + 
                     prop_univ + 
                     hotelbeds_dum + prop_immig_noEU +  #prop_service + 
                     log(pop) + log(popdens) + numhh + prop_smallhouse, 
                   dist = "negbin", data = .),
    )
}

## Run Estimation 
EstMod_hurdle <- doEstMod_hurdle("2020-03-10")

## By Week
doEstMod_hurdle_wk <- function(threshold_Data) {
  granddt %>%
    filter(Data>=threshold_Data & !is.na(pos_wk)) %>%  
    group_by(Data) %>% 
    do(m1 = hurdle(pos_wk ~ regunemprate + taxbaseincome + #prop_lowpricerent +
                     poppr_65plus + deathrate + 
                     prop_univ + 
                     hotelbeds_dum + prop_immig_noEU +  #prop_service + 
                     log(pop) + log(popdens) + numhh + prop_smallhouse, 
                   dist = "negbin", data = .),
       m2 = hurdle(pos_wk ~ unemprate + taxbaseincome + #prop_lowpricerent +
                     poppr_65plus + deathrate + 
                     prop_univ + 
                     hotelbeds_dum + prop_immig_noEU +  #prop_service + 
                     log(pop) + log(popdens) + numhh + prop_smallhouse, 
                   dist = "negbin", data = .),
       m3 = hurdle(pos_wk ~ regunemprate + totalwealth + #prop_lowpricerent +
                     poppr_65plus + deathrate + 
                     prop_univ + 
                     hotelbeds_dum + prop_immig_noEU +  #prop_service + 
                     log(pop) + log(popdens) + numhh + prop_smallhouse, 
                   dist = "negbin", data = .),
    )
}

## Run Estimation 
EstMod_hurdle_wk <- doEstMod_hurdle_wk("2020-03-10")

## Variable Labels
varlab_1 <- c("(Intercept)", "% Unemployment \n(Registered)", 
              "Personal income tax \n(100k Euro)", #"% Low-Cost Rent",
              "Age 65+ Pop. %", "Death rate",
              "Prop. University",  
              "Hotel Beds N >= Median", "% Immigrants out of EU", 
              #"Prop. Service Industry", 
              "Population (Log)","Pop. Density (Log)",
              "Av. N in Household", "% Small House (<=90sq.m)")
varlab_2 <- varlab_1
varlab_2[2] <- "% Unemployment \n(Official)"
varlab_3 <- varlab_1
varlab_3[3] <- "Total Wealth \n(100k Euro)"

which(EstMod_hurdle$Data=="2020-03-14")
max(EstMod_hurdle$Data)

(p <- plotEstMod(EstMod_hurdle,5,"count","m1","Positive",varlab_1))
ggsave("../out/dailyreg_hurdle_pos_regunemp_count.png", p, width=8, height=6)
(p <- plotEstMod(EstMod_hurdle,5,"zero","m1","Positive",varlab_1))
ggsave("../out/dailyreg_hurdle_pos_regunemp_zero.png", p, width=8, height=6)

(p <- plotEstMod(EstMod_hurdle,5,"count","m2","Positive",varlab_2))
ggsave("../out/dailyreg_hurdle_pos_unemp_count.png", p, width=8, height=6)
(p <- plotEstMod(EstMod_hurdle,5,"zero","m2","Positive",varlab_2))
ggsave("../out/dailyreg_hurdle_pos_unemp_zero.png", p, width=8, height=6)

(p <- plotEstMod(EstMod_hurdle,5,"count","m3","Tested",varlab_3))
ggsave("../out/dailyreg_hurdle_pos_wealth_count.png", p, width=8, height=6)
(p <- plotEstMod(EstMod_hurdle,5,"zero","m3","Tested",varlab_3))
ggsave("../out/dailyreg_hurdle_pos_wealth_zero.png", p, width=8, height=6)

(p <- plotEstMod(EstMod_hurdle_wk,5,"count","m1","Positive",varlab_1))
ggsave("../out/dailyreg_hurdle_wk_pos_regunemp_count.png", p, width=8, height=6)
(p <- plotEstMod(EstMod_hurdle_wk,5,"zero","m1","Positive",varlab_1))
ggsave("../out/dailyreg_hurdle_wk_pos_regunemp_zero.png", p, width=8, height=6)

(p <- plotEstMod(EstMod_hurdle_wk,5,"count","m2","Positive",varlab_2))
ggsave("../out/dailyreg_hurdle_wk_pos_unemp_count.png", p, width=8, height=6)
(p <- plotEstMod(EstMod_hurdle_wk,5,"zero","m2","Positive",varlab_2))
ggsave("../out/dailyreg_hurdle_wk_pos_unemp_zero.png", p, width=8, height=6)

(p <- plotEstMod(EstMod_hurdle_wk,5,"count","m3","Tested",varlab_3))
ggsave("../out/dailyreg_hurdle_wk_pos_wealth_count.png", p, width=8, height=6)
(p <- plotEstMod(EstMod_hurdle_wk,5,"zero","m3","Tested",varlab_3))
ggsave("../out/dailyreg_hurdle_wk_pos_wealth_zero.png", p, width=8, height=6)
  
