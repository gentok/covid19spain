#' ---
#' title: "Analysis of COVID-19 PCR Test Data"
#' author: "Gento Kato"
#' date: "April 14, 2020"
#' ---

library(tidyverse)
library(broom)

## Set Working Directory to the current directory 
## (If using RStudio, can be set automatically)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Clear Workspace
rm(list=ls())

## Import Relevant Data
coviddt <- readRDS("../data/test_cataluna_wide_rev.rds")
shapedt <- readRDS("../data/mun_cataluna_rev.rds")
demodt <- readRDS("../data/demo_cataluna_wide_rev.rds")

# Recode demodt 
D_demodt <- demodt %>% 
  as.data.frame() %>% 
  mutate(munname = tolower(munname)) #%>% 
  # select(munname, f321, f27, f28, f29, f262, f222, f223, f242, f7, f280, f216, f33, f240)

# Combine coviddt & demodt and Recode and Filter
D_granddt <- coviddt %>% 
  as.data.frame() %>% 
  mutate (MunicipiDescripcio = tolower(MunicipiDescripcio)) %>% 
  filter(MunicipiDescripcio != "(Altres municipis)") %>% 
  inner_join(D_demodt, by = c("MunicipiDescripcio" = "munname")) %>% 
  mutate(pop = as.numeric(as.character(f321)),
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
         pos = CumCasos_P,
         test = CumCasos,
         posrate = (pos / test) * 100,
         pos_100k = pos / (pop / 100000),
         test_100k = test / (pop / 100000),
         hotelbeds = as.numeric(as.character(f216)),
         hotelbeds_dum = ifelse(hotelbeds >= median(hotelbeds), 1, 0),
         jobcat_service = as.numeric(as.character(f280)),
         jobcat_totalpop = as.numeric(as.character(f282)),
         prop_service = (jobcat_service / jobcat_totalpop)*100,
         men_15to64 = as.numeric(as.character(f33)),
         men_regunemp = as.numeric(as.character(f240)),
         regunemprate_men = men_regunemp / men_15to64,
         women_15to64 = as.numeric(as.character(f39)),
         women_regunemp = as.numeric(as.character(f241)),
         regunemprate_women = women_regunemp / women_15to64)   
  # filter(test >= 10) 

# Estimate Models by Day  
doEstMod <- function(threshold_N) {
  D_granddt %>%
    filter(MunicipiDescripcio %in% MunicipiDescripcio[Data==max(Data) & test >= threshold_N]) %>%  
    group_by(Data) %>% 
    do(m1_1 = lm(pos_100k ~ regunemprate + I(popdens/1000) + poppr_65plus + hotelbeds_dum + prop_service, data = .),
       m2_1 = lm(test_100k ~ regunemprate + I(popdens/1000) + poppr_65plus + hotelbeds_dum + prop_service, data = .),
       m1_2 = lm(pos_100k ~ regunemprate_men + I(popdens/1000) + poppr_65plus + hotelbeds_dum + prop_service, data = .),
       m2_2 = lm(test_100k ~ regunemprate_men + I(popdens/1000) + poppr_65plus + hotelbeds_dum + prop_service, data = .),
       m1_3 = lm(pos_100k ~ regunemprate_women + I(popdens/1000) + poppr_65plus + hotelbeds_dum + prop_service, data = .),
       m2_3 = lm(test_100k ~ regunemprate_women + I(popdens/1000) + poppr_65plus + hotelbeds_dum + prop_service, data = .))
}

EstMod_5plus <- doEstMod(5) 
EstMod_10plus <- doEstMod(10) 
EstMod_30plus <- doEstMod(30) 
EstMod_50plus <- doEstMod(50) 

# All Same N of cases (just to make sure)
sapply(EstMod_5plus$m1_1, function(k) nobs(k))
sapply(EstMod_10plus$m1_1, function(k) nobs(k))
sapply(EstMod_30plus$m1_1, function(k) nobs(k))
sapply(EstMod_50plus$m1_1, function(k) nobs(k))

plotEstMod <- function(EstMod,threshold_N,modname,dvtext,unemptext) {
  names(EstMod)[names(EstMod)==modname] <- "targetmod"
  EstOut <- tidy(EstMod, targetmod) %>% 
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
                           labels = c("(Intercept)", paste0("Unemployment Rate (", unemptext,")"),
                                      "Population Density (1000/sq. km)","65+ Years Old Population %",
                                      "Hotel Beds N >= Median", "Prop. Service Industry"))), 
               scales="free_y") + 
    labs(title = paste0("Relationship between Municipality Statistics and \nCummulative COVID-19 ", dvtext, " Cases per 100,000 Population"),
         x = "Date", y = "Coefficient with 90% and 95% CI",
         caption = paste0("Models estimated daily by OLS regression. ", 
                          "Include municipalities with ", threshold_N, " or more PCR tests by ", 
                          gsub("-","/",as.character(max(EstMod$Data))),
                          " (N=", nobs(EstMod$targetmod[[1]]),").")) + 
    theme_classic() + theme(legend.position = "none",
                            plot.title = element_text(hjust=0.5),
                            axis.text.x = element_text(angle=45,vjust=0.1)) 
}

(p <- plotEstMod(EstMod_5plus,5,"m1_1","Positive","All"))
ggsave("../out/dailyreg_5plus_pos_unempall.png", p, width=8, height=5)
(p <- plotEstMod(EstMod_5plus,5,"m2_1","Tested","All"))
ggsave("../out/dailyreg_5plus_test_unempall.png", p, width=8, height=5)
(p <- plotEstMod(EstMod_5plus,5,"m1_2","Positive","Men"))
ggsave("../out/dailyreg_5plus_pos_unempmen.png", p, width=8, height=5)
(p <- plotEstMod(EstMod_5plus,5,"m2_2","Tested","Men"))
ggsave("../out/dailyreg_5plus_test_unempmen.png", p, width=8, height=5)
(p <- plotEstMod(EstMod_5plus,5,"m1_3","Positive","Women"))
ggsave("../out/dailyreg_5plus_pos_unempwomen.png", p, width=8, height=5)
(p <- plotEstMod(EstMod_5plus,5,"m2_3","Tested","Women"))
ggsave("../out/dailyreg_5plus_test_unempwomen.png", p, width=8, height=5)

(p <- plotEstMod(EstMod_10plus,10,"m1_1","Positive","All"))
ggsave("../out/dailyreg_10plus_pos_unempall.png", p, width=8, height=5)
(p <- plotEstMod(EstMod_10plus,10,"m2_1","Tested","All"))
ggsave("../out/dailyreg_10plus_test_unempall.png", p, width=8, height=5)
(p <- plotEstMod(EstMod_10plus,10,"m1_2","Positive","Men"))
ggsave("../out/dailyreg_10plus_pos_unempmen.png", p, width=8, height=5)
(p <- plotEstMod(EstMod_10plus,10,"m2_2","Tested","Men"))
ggsave("../out/dailyreg_10plus_test_unempmen.png", p, width=8, height=5)
(p <- plotEstMod(EstMod_10plus,10,"m1_3","Positive","Women"))
ggsave("../out/dailyreg_10plus_pos_unempwomen.png", p, width=8, height=5)
(p <- plotEstMod(EstMod_10plus,10,"m2_3","Tested","Women"))
ggsave("../out/dailyreg_10plus_test_unempwomen.png", p, width=8, height=5)

(p <- plotEstMod(EstMod_30plus,30,"m1_1","Positive","All"))
ggsave("../out/dailyreg_30plus_pos_unempall.png", p, width=8, height=5)
(p <- plotEstMod(EstMod_30plus,30,"m2_1","Tested","All"))
ggsave("../out/dailyreg_30plus_test_unempall.png", p, width=8, height=5)
(p <- plotEstMod(EstMod_30plus,30,"m1_2","Positive","Men"))
ggsave("../out/dailyreg_30plus_pos_unempmen.png", p, width=8, height=5)
(p <- plotEstMod(EstMod_30plus,30,"m2_2","Tested","Men"))
ggsave("../out/dailyreg_30plus_test_unempmen.png", p, width=8, height=5)
(p <- plotEstMod(EstMod_30plus,30,"m1_3","Positive","Women"))
ggsave("../out/dailyreg_30plus_pos_unempwomen.png", p, width=8, height=5)
(p <- plotEstMod(EstMod_30plus,30,"m2_3","Tested","Women"))
ggsave("../out/dailyreg_30plus_test_unempwomen.png", p, width=8, height=5)

(p <- plotEstMod(EstMod_50plus,50,"m1_1","Positive","All"))
ggsave("../out/dailyreg_50plus_pos_unempall.png", p, width=8, height=5)
(p <- plotEstMod(EstMod_50plus,50,"m2_1","Tested","All"))
ggsave("../out/dailyreg_50plus_test_unempall.png", p, width=8, height=5)
(p <- plotEstMod(EstMod_50plus,50,"m1_2","Positive","Men"))
ggsave("../out/dailyreg_50plus_pos_unempmen.png", p, width=8, height=5)
(p <- plotEstMod(EstMod_50plus,50,"m2_2","Tested","Men"))
ggsave("../out/dailyreg_50plus_test_unempmen.png", p, width=8, height=5)
(p <- plotEstMod(EstMod_50plus,50,"m1_3","Positive","Women"))
ggsave("../out/dailyreg_50plus_pos_unempwomen.png", p, width=8, height=5)
(p <- plotEstMod(EstMod_50plus,50,"m2_3","Tested","Women"))
ggsave("../out/dailyreg_50plus_test_unempwomen.png", p, width=8, height=5)
