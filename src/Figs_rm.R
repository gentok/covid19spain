#' ---
#' title: "Analysis of COVID-19 PCR Test Data"
#' author: "Gento Kato"
#' date: "April 15, 2020"
#' ---
# upd: 2020/05/08 by Ryohei Mogi

library(tidyverse)
library(broom)
library(sf)
library(fts)
library(dotwhisker)
library(cowplot)

#### ---- Data cleaning ----
## Import Relevant Data
coviddt <- readRDS("Data/covid_cataluna_wide.rds") %>% filter(mundesc != "(altres municipis)")
#shapedt <- readRDS("Data/shapefile/shape_cataluna_rev.rds") %>% filter(mundesc != "(altres municipis)")
demodt <- readRDS("Data/demo_cataluna_wide_rev.rds") %>% filter(mundesc != "(altres municipis)")
censusdt <- readRDS("Data/census2011_cataluna.rds") %>% filter(mundesc != "(altres municipis)")

# Merge ALl Data
granddt <- coviddt %>% 
  inner_join(demodt, by = c("mundesc"="mundesc")) %>% 
  #inner_join(shapedt, by = c("mundesc"="mundesc")) %>% 
  inner_join(censusdt, by = c("mundesc"="mundesc"))

# Recode Data
granddt <- granddt %>% 
  mutate(pos_day = ifelse(is.na(NumCasos_P), 0, NumCasos_P),
         test_day = ifelse(is.na(NumCasos), 0, NumCasos),
         pos = ifelse(is.na(CumCasos_P), 0, CumCasos_P),
         test = ifelse(is.na(CumCasos), 0, CumCasos),
         posN = ifelse(is.na(NumCasos_P), 0, NumCasos_P),
         testN = ifelse(is.na(NumCasos), 0, NumCasos),
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
         pos_day100k = pos_day / (pop / 100000),
         hotelbeds = as.numeric(as.character(f216)),
         hotelbeds_dum = ifelse(hotelbeds >= median(hotelbeds), 1, 0),
         jobcat_service = as.numeric(gsub("_", NA, as.character(f280))),
         jobcat_totalpop = as.numeric(gsub("_", NA, as.character(f282))),
         prop_service = (jobcat_service / jobcat_totalpop) * 100,
         men_15to64 = as.numeric(as.character(f33)),
         men_regunemp = as.numeric(as.character(f240)),
         regunemprate_men = men_regunemp / men_15to64,
         women_15to64 = as.numeric(as.character(f39)),
         women_regunemp = as.numeric(as.character(f241)),
         regunemprate_women = women_regunemp / women_15to64,
         death2018 = as.numeric(as.character(f188)),
         immig_noEU = as.numeric(as.character(f316)),
         pop_noESP = as.numeric(as.character(f73)),
         deathrate = (death2018 / pop18) * 100, 
         prop_immig_noEU = (immig_noEU / pop18) * 100, 
         prop_pop_noESP = (pop_noESP / pop18) * 100, 
         mainhh = as.numeric(as.character(f250)), # Number of main households in 2011
         numhh = pop / mainhh,
         pop_smallhouse = t19_1 + t19_2 + t19_3 + t19_4 + t19_5,
         pop_housesizeall = t19_1 + t19_2 + t19_3 + t19_4 + t19_5 + t19_6 + t19_7 + t19_8 + t19_9 + t19_10,
         prop_smallhouse = (pop_smallhouse / pop_housesizeall) * 100,
         pop_lowpricerent = t18_5,
         pop_housingall = t18_1 + t18_2 + t18_3 + t18_4 + t18_5 + t18_6,
         prop_lowpricerent = (pop_lowpricerent / pop_housingall) * 100,
         pop_univ = t12_5,
         pop_eduall = t12_1 + t12_2 + t12_3 + t12_4 + t12_5,
         prop_univ = (pop_univ/pop_eduall) * 100)

## Moving Sum by 7 days
granddt$pos_wk <- NA
for (i in unique(granddt$mundesc)) {
  granddt$pos_wk[granddt$mundesc == i] <- c(rep(NA, 6), as.vector(moving.sum(fts(granddt$Data[granddt$mundesc==i], 
                                                         granddt$pos_day[granddt$mundesc==i]), periods=7)))
  granddt$test_wk[granddt$mundesc == i] <- c(rep(NA, 6), as.vector(moving.sum(fts(granddt$Data[granddt$mundesc==i], 
                                                         granddt$test_day[granddt$mundesc==i]), periods=7)))
}
tail(granddt$pos_wk, 30)

#### ---- Daily trend of confirmed cases ----
sel <- granddt %>% 
  select(mundesc, Data, pos, pos_day, pos_100k, pos_day100k, pos_wk) %>% 
  filter(Data == max(Data)) %>% 
  arrange(desc(pos)) %>% 
  top_n(n = 50, wt = pos) %>% 
  select(mundesc) %>% 
  unlist()

D_Fig <- granddt %>% 
  select(mundesc, Data, pos, pos_day, pos_100k, pos_day100k, pos_wk) %>% 
  filter(mundesc %in% sel)

casetiles <- D_Fig %>% 
  filter(mundesc != "barcelona") %>% 
  ggplot(aes(x = Data, y = fct_reorder(mundesc, pos), fill = pos)) +
  geom_tile(colour = "White", show.legend = F) +
  theme_classic() +
  scale_fill_distiller(palette = "Spectral") +
  labs(x = "Cumulative confirmed cases at each day") +
  theme(axis.line.y = element_blank(),
        axis.title.y = element_blank())

casebars <- D_Fig %>% 
  filter(mundesc != "barcelona") %>% 
  filter(Data == max(Data)) %>% 
  ggplot(aes(x = pos, y = fct_reorder(mundesc, pos), fill = pos)) +
  geom_col(show.legend = F) +
  theme_classic() +
  scale_fill_distiller(palette = "Spectral") +
  scale_x_continuous(name = "Total confirmed cases", breaks = c(0, 1000, 2000, 3000)) +
  theme(axis.title.y = element_blank(), axis.line.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.x = element_text(colour = "Black"))

p <- plot_grid(casetiles, casebars, align = "h", rel_widths = c(1, 0.2))
ggsave("Graph/Timetrend_cumcases.png", p, width = 10, height = 6)


#### ---- Function for the hurdle model ----
# Estimate Models by Day
require(pscl)
doEstMod_hurdle <- function(threshold_Data) {
  granddt %>%
    filter(Data >= threshold_Data) %>%  
    group_by(Data) %>% 
    do(m1 = hurdle(pos ~ regunemprate + taxbaseincome + #prop_lowpricerent +
                     prop_immig_noEU +
                     deathrate + poppr_65plus + 
                     prop_univ + 
                     hotelbeds_dum +
                     log(pop) + log(popdens) + numhh + prop_smallhouse, 
                   dist = "negbin", data = .),
       m2 = hurdle(pos ~ regunemprate + taxbaseincome +
                     prop_immig_noEU + prop_service +
                     deathrate + poppr_65plus + 
                     prop_univ + 
                     log(pop) + log(popdens) + numhh + prop_smallhouse, 
                   dist = "negbin", data = .)
    )
}

## By Week
doEstMod_hurdle_wk <- function(threshold_Data) {
  granddt %>%
    filter(Data >= threshold_Data & !is.na(pos_wk)) %>%  
    group_by(Data) %>% 
    do(m1 = hurdle(pos_wk ~ regunemprate + taxbaseincome + #prop_lowpricerent +
                     prop_immig_noEU +
                     deathrate + poppr_65plus + 
                     prop_univ + 
                     hotelbeds_dum +
                     log(pop) + log(popdens) + numhh + prop_smallhouse, 
                   dist = "negbin", data = .),
       m2 = hurdle(pos_wk ~ regunemprate + taxbaseincome +
                     prop_immig_noEU + prop_service +
                     deathrate + poppr_65plus + 
                     prop_univ + 
                     log(pop) + log(popdens) + numhh + prop_smallhouse, 
                   dist = "negbin", data = .)
    )
}

#### ---- Regression at the current date ----
Results_currentD <- hurdle(pos ~ regunemprate + taxbaseincome +
                                 prop_immig_noEU + prop_service +
                                 deathrate + poppr_65plus + 
                                 prop_univ + 
                                 log(pop) + log(popdens) + numhh + prop_smallhouse, 
                           dist = "negbin", data = granddt %>% filter(Data == max(EstMod_hurdle$Data)))



Fig_hurdle_currentD <- function(type){
  c <- Results_currentD$coefficients$count
  target_row <- ifelse(type == "count", 1, 13)
  k <- Results_currentD$vcov[target_row:(target_row + 11), target_row:(target_row + 11)]
  k <- sqrt(diag(k))
  
  names(c) <- varlab_2
  
  EstOut <- data.frame(estimate = c, term = names(c), std.error = k,
                       type = type)
  
  D_Fig <- EstOut %>% 
    mutate(up95 = estimate + qnorm(0.975) * std.error,
           lo95 = estimate - qnorm(0.975) * std.error,
           up90 = estimate + qnorm(0.95) * std.error,
           lo90 = estimate - qnorm(0.95) * std.error,
           term = factor(term, levels = rev(varlab_2))) %>% 
    filter(term != "(Intercept)")
  
  Fig <- D_Fig %>% 
    ggplot() +
    geom_segment(aes(x = lo90, xend = up90, y = term, yend = term), col = "#FD8D3C", alpha = 0.8, size = 2) +
    geom_segment(aes(x = lo95, xend = up95, y = term, yend = term), col = "#ffa869", alpha = 0.8, size = 1.4) +
    geom_point(aes(x = estimate, y = term)) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    theme_bw() +
    labs(x = "Coefficient estimate") +
    theme(axis.title.y = element_blank())
    
  return(Fig)
}

p <- Fig_hurdle_currentD(type = "count")
ggsave("Graph/currentreg_hurdle_pos_regunemp_count_wotitle.png", p, width = 8, height = 6)

#### ---- Regression dynamic trend ----
## Plotting Function
# with title
plotEstMod_title <- function(EstMod,startdate,type,modname,dvtext,varlab) {
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

# without title
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
    geom_hline(aes(yintercept = 0), linetype = 2) + 
    geom_ribbon(aes_string(ymax = "up95", ymin = "lo95", fill = "term"), alpha = 0.2) + 
    geom_ribbon(aes_string(ymax = "up90", ymin = "lo90", fill = "term"), alpha = 0.2) + 
    geom_line(aes_string(colour = "term"))  + 
    facet_wrap(vars(factor(EstOut$term, levels = unique(EstOut$term),
                           labels = varlab)), 
               scales = "free_y") + 
    labs(x = "Date", y = paste0(TYPE, " Model Coefficient with 90% and 95% CI")) + 
    theme_classic() + 
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 30, vjust = 0.5)) 
}

## Run Estimation 
EstMod_hurdle <- doEstMod_hurdle("2020-03-09") # One week before the lockdown

## Run Estimation 
EstMod_hurdle_wk <- doEstMod_hurdle_wk("2020-03-09")

## Variable Labels
varlab_1 <- c("(Intercept)", "% Unemployment \n(Registered)", 
              "Personal income tax \n(100k Euro)", #"% Low-Cost Rent",
              "% Immigrants out of EU", 
              "Death rate", "Age 65+ Pop. %", 
              "% University",  
              "Hotel Beds N >= Median", 
              "Population (Log)","Pop. Density (Log)",
              "Av. N in Household", "% Small House \n(<=90sq.m)")

varlab_2 <- c("(Intercept)", "% Unemployment \n(Registered)", 
              "Personal income tax \n(100k Euro)",
              "% Immigrants out of EU", "% Service Industry",
              "Death rate", "% Age 65+", 
              "% University",
              "Population (Log)","Pop. Density (Log)",
              "Av. N in Household", "% Small House \n(<=90sq.m)")

which(EstMod_hurdle$Data=="2020-03-14")
max(EstMod_hurdle$Data)

#### ---- Plot ----
## For the main text
p <- plotEstMod(EstMod_hurdle_wk, 5, "count", "m2", "Positive", varlab_2)
ggsave("Graph/dailyreg_hurdle_wk_pos_regunemp_count_wotitle.png", p, width = 8, height = 6)

## For the appendix
p <- plotEstMod(EstMod_hurdle_wk, 5, "zero", "m2", "Positive", varlab_2)
ggsave("Graph/dailyreg_hurdle_wk_pos_regunemp_zero_wotitle.png", p, width = 8, height = 6)

# cumulative
p <- plotEstMod(EstMod_hurdle, 5, "count", "m2", "Positive", varlab_2)
ggsave("Graph/dailyreg_hurdle_pos_regunemp_count_wotitle.png", p, width = 8, height = 6)

p <- plotEstMod(EstMod_hurdle, 5, "zero", "m2", "Positive", varlab_2)
ggsave("Graph/dailyreg_hurdle_pos_regunemp_zero_wotitle.png", p, width = 8, height = 6)

