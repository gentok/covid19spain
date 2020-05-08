#' ---
#' title: "Analysis of COVID-19 PCR Test Data"
#' author: "Gento Kato"
#' date: "April 15, 2020"
#' ---

library(tidyverse)
library(broom)

## Set Working Directory to the current directory 
## (If using RStudio, can be set automatically)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Clear Workspace
#rm(list=ls())

## Import Relevant Data
coviddt <- readRDS("../data/covid_cataluna_wide.rds") 
demodt <- readRDS("../data/demo_cataluna_wide_rev.rds") %>% filter(mundesc != "(altres municipis)")

# Recode demodt 
D_coviddt <- coviddt %>% 
  as.data.frame() %>% 
  filter(mundesc != "(Altres municipis)")

# Combine coviddt & demodt and Recode and Filter
D_granddt <- demodt %>% 
  as.data.frame() %>% 
  inner_join(D_coviddt, by = c("mundesc" = "mundesc")) %>% 
  mutate(pos = ifelse(is.na(CumCasos_P), 0, CumCasos_P),
         test = ifelse(is.na(CumCasos), 0, CumCasos),
         posN = ifelse(is.na(NumCasos_P), 0, NumCasos_P),
         testN = ifelse(is.na(NumCasos), 0, NumCasos),
         pop = as.numeric(as.character(f321)),
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
         posrate = (pos / test) * 100,
         pos_100k = pos / (pop / 100000),
         test_100k = test / (pop / 100000),
         hotelbeds = as.numeric(as.character(f216)),
         hotelbeds_dum = ifelse(hotelbeds >= median(hotelbeds), 1, 0),
         jobcat_service = as.numeric(gsub("_", NA, as.character(f280))),
         jobcat_totalpop = as.numeric(gsub("_", NA, as.character(f282))),
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
         deathrate = death2018 / pop, # The denominator is Popultion 2019
         prop_immig_noEU = immig_noEU / pop, # The denominator is Popultion 2019
         prop_pop_noESP = pop_noESP / pop, # The denominator is Popultion 2019
         mainhh = as.numeric(as.character(f250)), # Number of main households in 2011
         numhh = pop / mainhh
         )   
  # filter(test >= 10) 
hist(as.numeric(D_granddt[D_granddt$Data==max(D_granddt$Data),]$f10))
 table(D_granddt[D_granddt$Data==max(D_granddt$Data),]$f14)

# Estimate Models by Day
require(pscl)
doEstMod_hurdle <- function(threshold_N) {
  D_granddt %>%
    filter(mundesc %in% mundesc[Data==max(Data) & test >= threshold_N]) %>%  
    group_by(Data) %>% 
    do(m1_1 = hurdle(pos ~ regunemprate + taxbaseincome + I(popdens/1000) + poppr_65plus + 
                       hotelbeds_dum + prop_service + I(pop/10000) + deathrate + prop_immig_noEU +
                       numhh, data = .),
       m2_1 = hurdle(test ~ regunemprate + taxbaseincome + I(popdens/1000) + poppr_65plus + 
                       hotelbeds_dum + prop_service + I(pop/10000) + deathrate + prop_immig_noEU +
                       numhh, data = .))
}

plotEstMod <- function(EstMod, startdate, type, modname, dvtext, varlab) {
  names(EstMod)[names(EstMod) == modname] <- "targetmod"
  if (type == "count") TYPE = "Count" 
  if (type == "zero") TYPE = "Zero Inflation" 
  EstOut <- data.frame(estimate = numeric(), term = character(),
                       std.error = numeric(), type = character())
  for (i in startdate:length(EstMod$targetmod)) {
    c <- EstMod$targetmod[[i]]$coefficients[[type]]
    k <- EstMod$targetmod[[i]]$vcov
    k <- sqrt(diag(k[grep(type,rownames(k)), grep(type,colnames(k))]))
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

EstMod_hurdle <- doEstMod_hurdle(0) 

varlab <- c("(Intercept)", "Unemployment Rate (All)", "Tax Base Income (10000 Euro)",
            "Population Density (1000/sq. km)","65+ Years Old Population %",
            "Hotel Beds N >= Median", "Prop. Service Industry","Population (by 10000)",
            "Death rate", "% Immigrants outside of EU", "Avg. Num. Household members")

which(EstMod_hurdle$Data=="2020-03-14")

(p <- plotEstMod(EstMod_hurdle, 38, "count", "m1_1", "Positive", varlab))
ggsave("../out/dailyreg_hurdle_pos_unempall_count.png", p, width=8, height = 5)
(p <- plotEstMod(EstMod_hurdle, 38, "zero", "m1_1", "Positive", varlab))
ggsave("../out/dailyreg_hurdle_pos_unempall_zero.png", p, width = 8, height = 5)

(p <- plotEstMod(EstMod_hurdle,38,"count","m2_1","Tested",varlab))
ggsave("../out/dailyreg_hurdle_test_unempall_count.png", p, width=8, height = 5)
(p <- plotEstMod(EstMod_hurdle,38,"zero","m2_1","Tested",varlab))
ggsave("../out/dailyreg_hurdle_test_unempall_zero.png", p, width=8, height = 5)
