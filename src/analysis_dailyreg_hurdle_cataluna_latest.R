#' ---
#' title: "Analysis of COVID-19 PCR Test Data (Latest)"
#' author: "Gento Kato and Ryohei Mogi"
#' date: "May 20, 2020"
#' ---
#' 
#' # Preparation
#' 

## Clear Workspace
rm(list=ls())

## Set Working Directory (Automatically to Project Home) ##
library(rprojroot)
if (rstudioapi::isAvailable()==TRUE) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); 
} 
projdir <- find_root(has_file("thisishome.txt"))
cat(paste("Working Directory Set to:\n",projdir))
setwd(projdir)

## Library Required Packages
library(tidyverse)
library(broom)
library(sf)
library(fts)
library(dotwhisker)
library(cowplot)
library(GGally)

## Import Relevant Data
coviddt <- readRDS(paste0(projdir,"/","data/covid_cataluna_wide.rds")) %>% 
  filter(mundesc != "(altres municipis)")
# shapedt <- readRDS(paste0(projdir,"/","data/shapefile/shape_cataluna_rev.rds")) %>% 
#   filter(mundesc != "(altres municipis)")
demodt <- readRDS(paste0(projdir,"/","data/demo_cataluna_wide_rev.rds")) %>% 
  filter(mundesc != "(altres municipis)")
censusdt <- readRDS(paste0(projdir,"/","data/census2011_cataluna.rds")) %>% 
  filter(mundesc != "(altres municipis)")

# For Mogi
#coviddt <- readRDS("Data/covid_cataluna_wide.rds") %>% 
#  filter(mundesc != "(altres municipis)")
## shapedt <- readRDS(paste0(projdir,"/","data/shapefile/shape_cataluna_rev.rds")) %>% 
##   filter(mundesc != "(altres municipis)")
#demodt <- readRDS("Data/demo_cataluna_wide_rev.rds") %>% 
#  filter(mundesc != "(altres municipis)")
#censusdt <- readRDS("Data/census2011_cataluna.rds") %>% 
#  filter(mundesc != "(altres municipis)")

#'
#' # Date of Analysis
#'

Date_analy <- max(coviddt$Data)
Date_analy_simple <- "latest"
# Date_analy <- "2020-05-14"
# Date_analy_simple <- gsub("-","", Date_analy)
Date_analy

#'
#' # Data Cleaning/Manipulation
#'

# Merge ALl Data
granddt <- coviddt %>% 
  inner_join(demodt, by = c("mundesc"="mundesc")) %>% 
  #inner_join(shapedt, by = c("mundesc"="mundesc")) %>% 
  inner_join(censusdt, by = c("mundesc"="mundesc")) %>%
  filter(Data<=Date_analy)

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
         taxbaseincome = as.numeric(as.character(f10))/ 10000,
         totalwealth = as.numeric(as.character(f180))/ 10000,
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
         deathrate = death2018 / (pop18/1000), 
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
         prop_univ = (pop_univ / pop_eduall) * 100,
         mean_numhh_census = ((1 * t22_1) + (2 * t22_2) + (3 * t22_3) + (4 * t22_4) + 
                              (5 * t22_5) + (7 * t22_6)) / 
                              ((1 * t22_1) + (1 * t22_2) + (1 * t22_3) + (1 * t22_4) + 
                               (1 * t22_5) + (1 * t22_6)), # how to define the last category (6+)
         log_pop = log(pop),
         log_popdens = log(popdens))


## Moving Sum by 7 days
granddt$pos_wk <- NA
granddt$test_wk <- NA
for (i in unique(granddt$mundesc)) {
  granddt$pos_wk[granddt$mundesc == i] <- c(rep(NA, 6), as.vector(moving.sum(fts(granddt$Data[granddt$mundesc==i], 
                                                         granddt$pos_day[granddt$mundesc==i]), periods=7)))
  granddt$test_wk[granddt$mundesc == i] <- c(rep(NA, 6), as.vector(moving.sum(fts(granddt$Data[granddt$mundesc==i], 
                                                         granddt$test_day[granddt$mundesc==i]), periods=7)))
}
tail(granddt$pos_wk, 30)
tail(granddt$test_wk, 30)

# Check mean number of household members
#plot(granddt[granddt$Data == "2020-05-09", ]$numhh, 
#granddt[granddt$Data == "2020-05-09", ]$mean_numhh_census, xlim = c(0, 7))

#+ eval = FALSE
## Save Grand Dataset
saveRDS(granddt, paste0(projdir,"/data/granddt_", Date_analy_simple, ".rds"))

#'
#' # Plot: Daily trend of cumulative confirmed cases
#' 

sel <- granddt %>% 
  dplyr::select(munname, Data, pos, pos_day, pos_100k, pos_day100k, pos_wk) %>% 
  filter(Data == Date_analy) %>% 
  arrange(desc(pos)) %>% 
  top_n(n = 50, wt = pos) %>% 
  dplyr::select(munname) %>% 
  unlist()

D_Fig <- granddt %>% 
  dplyr::select(munname, Data, pos, pos_day, pos_100k, pos_day100k, pos_wk) %>% 
  filter(munname %in% sel)

casetiles <- D_Fig %>% 
  filter(munname != "Barcelona",
         Data <= Date_analy) %>% 
  mutate(munname = case_when(munname == "Hospitalet de Llobregat, l'" ~ "L'Hospitalet de Llobregat", 
                             munname == "Prat de Llobregat, el" ~ "El Prat de Llobregat",
                             T ~ munname)) %>% 
  ggplot(aes(x = Data, y = fct_reorder(munname, pos), fill = pos)) +
  geom_tile(colour = "White", show.legend = F) +
  theme_classic() +
  scale_fill_distiller(palette = "Spectral") +
  labs(x = "Cumulative confirmed cases at each day") +
  theme(axis.line.y = element_blank(),
        axis.title.y = element_blank())

casebars <- D_Fig %>% 
  filter(munname != "Barcelona") %>% 
  filter(Data == Date_analy) %>% 
  ggplot(aes(x = pos, y = fct_reorder(munname, pos), fill = pos)) +
  geom_col(show.legend = F) +
  theme_classic() +
  scale_fill_distiller(palette = "Spectral") +
  scale_x_continuous(name = "Total confirmed cases", breaks = c(0, 1000, 2000, 3000)) +
  theme(axis.title.y = element_blank(), axis.line.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.x = element_text(colour = "Black"))

p <- plot_grid(casetiles, casebars, align = "h", rel_widths = c(1, 0.2))
p
ggsave(paste0(projdir,"/","out/ForArticle/timetrend_cumcases_", Date_analy_simple, ".png"), p, width = 10, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/timetrend_cumcases_", Date_analy_simple, ".pdf"), p, width = 10, height = 6)
#ggsave("Graph/Timetrend_cumcases_", Date_analy_simple, ".png", p, width = 10, height = 6)
#ggsave("Graph/Timetrend_cumcases_", Date_analy_simple, ".pdf", p, width = 10, height = 6)

#'
#' # Analysis
#'
#' ## Functions to Estimate the Hurdle Model
#'

## Function to Standardize Predictors
stdv <- function(var) (var - mean(var, na.rm = TRUE)) / sd(var, na.rm = TRUE)

# Estimate Models with Cumulative Positive Cases
require(pscl)
doEstMod_hurdle <- function(threshold_Data) {
  granddt %>%
    filter(Data >= threshold_Data & Data <= Date_analy) %>%  
    group_by(Data) %>% 
    do(m1 = hurdle(pos ~ stdv(regunemprate) + stdv(taxbaseincome) + 
                     stdv(prop_smallhouse) + 
                     stdv(prop_immig_noEU) + #stdv(prop_service) +
                     stdv(deathrate) + stdv(poppr_65plus) + 
                     stdv(prop_univ) + 
                     stdv(log(pop)) + stdv(log(popdens)) + 
                     stdv(mean_numhh_census), 
                   dist = "negbin", data = .),
       m2 = hurdle(pos ~ stdv(regunemprate) + stdv(taxbaseincome) + 
                     stdv(prop_smallhouse) + 
                     stdv(prop_immig_noEU) + stdv(prop_service) +
                     stdv(deathrate) + stdv(poppr_65plus) + 
                     stdv(prop_univ) + 
                     stdv(log(pop)) + stdv(log(popdens)) + 
                     stdv(mean_numhh_census), 
                   dist = "negbin", data = .),
       m3 = hurdle(pos ~ stdv(unemprate) + stdv(taxbaseincome) + 
                     stdv(prop_smallhouse) + 
                     stdv(prop_immig_noEU) + #stdv(prop_service) +
                     stdv(deathrate) + stdv(poppr_65plus) + 
                     stdv(prop_univ) + 
                     stdv(log(pop)) + stdv(log(popdens)) + 
                     stdv(mean_numhh_census), 
                   dist = "negbin", data = .),
       # m4: not divided by population
       m4 = hurdle(pos ~ stdv(pop_regunemp) + stdv(taxbaseincome) +
                     stdv(prop_smallhouse) + 
                     stdv(immig_noEU) + 
                     stdv(death2018) + stdv(pop_65plus) + 
                     stdv(pop_univ) + 
                     stdv(log(pop)) + stdv(log(popdens)) + 
                     stdv(mean_numhh_census), 
                   dist = "negbin", data = .),
       m5 = hurdle(test ~ stdv(regunemprate) + stdv(taxbaseincome) + 
                     stdv(prop_smallhouse) + 
                     stdv(prop_immig_noEU) + #stdv(prop_service) +
                     stdv(deathrate) + stdv(poppr_65plus) + 
                     stdv(prop_univ) + 
                     stdv(log(pop)) + stdv(log(popdens)) + 
                     stdv(mean_numhh_census) + 
                     stdv((pos>0)*1) + 
                     stdv(log(pos+1))
                     | stdv(regunemprate) + stdv(taxbaseincome) + 
                     stdv(prop_smallhouse) + 
                     stdv(prop_immig_noEU) + #stdv(prop_service) +
                     stdv(deathrate) + stdv(poppr_65plus) + 
                     stdv(prop_univ) + 
                     stdv(log(pop)) + stdv(log(popdens)) + 
                     stdv(mean_numhh_census) , 
                   dist = "negbin", data = .)
    )
}

## By Moving Weekly Sum of Positive Cases
doEstMod_hurdle_wk <- function(threshold_Data) {
  granddt %>%
    filter(Data >= threshold_Data & Data <= Date_analy,
           !is.na(pos_wk)) %>%  
    group_by(Data) %>% 
    do(m1 = hurdle(pos_wk ~ stdv(regunemprate) + stdv(taxbaseincome) + 
                     stdv(prop_smallhouse) + 
                     stdv(prop_immig_noEU) + #stdv(prop_service) +
                     stdv(deathrate) + stdv(poppr_65plus) + 
                     stdv(prop_univ) + 
                     stdv(log(pop)) + stdv(log(popdens)) + 
                     stdv(mean_numhh_census), 
                   dist = "negbin", data = .),
       m2 = hurdle(pos_wk ~ stdv(regunemprate) + stdv(taxbaseincome) + 
                     stdv(prop_smallhouse) + 
                     stdv(prop_immig_noEU) + stdv(prop_service) +
                     stdv(deathrate) + stdv(poppr_65plus) + 
                     stdv(prop_univ) + 
                     stdv(log(pop)) + stdv(log(popdens)) + 
                     stdv(mean_numhh_census), 
                   dist = "negbin", data = .),
       m3 = hurdle(pos_wk ~ stdv(unemprate) + stdv(taxbaseincome) + 
                     stdv(prop_smallhouse) + 
                     stdv(prop_immig_noEU) + #stdv(prop_service) +
                     stdv(deathrate) + stdv(poppr_65plus) + 
                     stdv(prop_univ) + 
                     stdv(log(pop)) + stdv(log(popdens)) + 
                     stdv(mean_numhh_census), 
                   dist = "negbin", data = .),
       m4 = hurdle(pos_wk ~ stdv(pop_regunemp) + stdv(taxbaseincome) +
                     stdv(prop_smallhouse) + 
                     stdv(immig_noEU) + 
                     stdv(death2018) + stdv(pop_65plus) + 
                     stdv(pop_univ) + 
                     stdv(log(pop)) + stdv(log(popdens)) + 
                     stdv(mean_numhh_census), 
                   dist = "negbin", data = .),
       m5 = hurdle(test_wk ~ stdv(regunemprate) + stdv(taxbaseincome) + 
                     stdv(prop_smallhouse) + 
                     stdv(prop_immig_noEU) + #stdv(prop_service) +
                     stdv(deathrate) + stdv(poppr_65plus) + 
                     stdv(prop_univ) + 
                     stdv(log(pop)) + stdv(log(popdens)) + 
                     stdv(mean_numhh_census)  + 
                     stdv((pos_wk>0)*1)  + 
                     stdv(log(pos_wk+1)) | stdv(regunemprate) + stdv(taxbaseincome) + 
                     stdv(prop_smallhouse) + 
                     stdv(prop_immig_noEU) + #stdv(prop_service) +
                     stdv(deathrate) + stdv(poppr_65plus) + 
                     stdv(prop_univ) + 
                     stdv(log(pop)) + stdv(log(popdens)) + 
                     stdv(mean_numhh_census), 
                   dist = "negbin", data = .)
    )
}


## Plotting Functions

## Variable Labels for each model
varlab_1 <- c("(Intercept)", "% Unemployment \n(Registered)", 
            "Av. Taxable Base Income \n(10k euro)", #"% Low-Cost Rent",
            "% Small House \n(<=90sq.m)",
            "% Immigrants out of EU", #"% Service Industry",
            "Crude death rate", "% Age 65+", 
            "% University degree",
            "Population (Log)","Pop. Density (Log)",
            "Av. N in Household")
varlab_2 <- c(varlab_1[1:5],"% Service Industry", varlab_1[6:length(varlab_1)])
varlab_3 <- ifelse(varlab_1=="% Unemployment \n(Registered)",
                   "% Unemployment \n(Alternative)", varlab_1)
varlab_4 <- c("(Intercept)", "N. Unemployed pop\n(Registered)", 
              "Av. Taxable Base Income \n(10k euro)", #"% Low-Cost Rent",
              "% Small House \n(<=90sq.m)",
              "Immigrants out of EU", #"% Service Industry",
              "N. Death", "N. Age 65+", 
              "N. University degree",
              "Population (Log)","Pop. Density (Log)",
              "Av. N in Household")
varlab_5 <- c(varlab_1, "Have positive cases \n(Dummy)", "N. positive cases (Log)")

# Plotting Only One Day
plotEstMod_oneshot <- function(EstMod, date, modname, dvtxt1, dvtxt2, varlab){
  
  names(EstMod)[names(EstMod)==modname] <- "targetmod"
  mod = EstMod$targetmod[[which(EstMod$Data==date)]]
  
  # Count Model 
  c1 <- mod$coefficients$count
  k <- vcov(mod)
  k1 <- sqrt(diag(k[grep("count",rownames(k)),grep("count",colnames(k))]))
  
  # Zero-Inflation Model
  c2 <- mod$coefficients$zero
  k <- vcov(mod)
  k2 <- sqrt(diag(k[grep("zero",rownames(k)),grep("zero",colnames(k))]))
  if (modname=="m5") {
    c2 <- c(c2,NA,NA)
    k2 <- c(k2,NA,NA)
  }
  
  EstOut <- data.frame(estimate = c(c1,c2), term = varlab, std.error = c(k1,k2),
                       type = rep(c("Positive Count \n(Negative Binomial)",
                                    "Hurdle Component \n(Logit)"), each=length(c1)))
  
  D_Fig <- EstOut %>% 
    mutate(up95 = estimate + qnorm(0.975) * std.error,
           lo95 = estimate - qnorm(0.975) * std.error,
           up90 = estimate + qnorm(0.95) * std.error,
           lo90 = estimate - qnorm(0.95) * std.error,
           term = factor(term, levels = rev(varlab)),
           sig = ifelse((lo95 > 0)|(up95 < 0),"p<.05",
                        ifelse((lo90 > 0)|(up90 < 0),"p<.10","n.s.")),
           sig = factor(sig, levels=c("p<.05","p<.10","n.s."))) %>% 
    filter(term != "(Intercept)")
  
  captiontxt = paste0("Predictors' scales are standardized: odds/incidence-rate ratios correspond to 1 SD increase in each predictor.",
                      "\nDependent variable is all confirmed cases as of ",date,". Municipality N = ", mod$n,".")
  
  Fig <- D_Fig %>% 
    ggplot() +
    # geom_segment(aes(x = exp(lo90), xend = exp(up90), y = term, yend = term), col = "#FD8D3C", alpha = 0.8, size = 2) +
    # geom_segment(aes(x = exp(lo95), xend = exp(up95), y = term, yend = term), col = "#ffa869", alpha = 0.8, size = 1.4) +
    geom_segment(aes(x = exp(lo90), xend = exp(up90), y = term, yend = term, col = sig), alpha = 0.8, size = 2.2) +
    geom_segment(aes(x = exp(lo95), xend = exp(up95), y = term, yend = term, col = sig), alpha = 0.8, size = 1.4) +
    geom_point(aes(x = exp(estimate), y = term, shape = sig), size=2) +
    geom_vline(xintercept = 1, linetype = "dotted") +
    facet_grid(~ type, scales = "free_x") + 
    theme_bw() +
    labs(x = "Odds/incidence-rate ratio (logged scale) \nwith 95% and 90% confidence intervals",
         title = paste0("Association between municipality characteristics and \n", dvtxt1, " COVID-19 ", dvtxt2, " cases"),
         caption = captiontxt) + 
    scale_shape_discrete(name="Statistical Significance", drop=FALSE, na.translate=FALSE) + 
    scale_color_discrete(name="Statistical Significance", drop=FALSE, na.translate=FALSE) + 
    scale_x_continuous(trans="log10") + 
    theme(axis.title.y = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust=0.5))
  
  return(Fig)
}

# Plotting dynamic trend
plotEstMod <- function(EstMod,startdate,type,modname,dvtxt1,dvtxt2,varlab) {
  
  names(EstMod)[names(EstMod)==modname] <- "targetmod"
  if (type=="count") TYPE="Positive count model incidence rate " 
  if (type=="zero") TYPE="Hurdle component model odds "
  
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
    mutate(up95 = exp(estimate + qnorm(0.975)*std.error),
           lo95 = exp(estimate - qnorm(0.975)*std.error),
           up90 = exp(estimate + qnorm(0.95)*std.error),
           lo90 = exp(estimate - qnorm(0.95)*std.error),
           estimate = exp(estimate))
  
  ggplot(EstOut, aes_string(x = "Data", y = "estimate")) +
    geom_hline(aes(yintercept=1), linetype=2) + 
    geom_ribbon(aes_string(ymax="up95", ymin="lo95", fill = "term"), alpha=0.2) + 
    geom_ribbon(aes_string(ymax="up90", ymin="lo90", fill = "term"), alpha=0.2) + 
    geom_line(aes_string(colour = "term"))  + 
    facet_wrap(vars(factor(EstOut$term, levels=unique(EstOut$term),
                           labels = varlab)), 
               scales="free_y") + 
    scale_y_continuous(trans="log10") + 
    labs(title = paste0("Association between municipality characteristics and \n", dvtxt1, " COVID-19 ", dvtxt2, " cases"),
         x = "Date", y = paste0(TYPE, "ratio \nwith 90% and 95% CI (logged scale)"),
         caption = paste0("Models estimated daily by hurdle model with logit for hurdle component and negative binomial regression for positive count.\n",
                          "Predictors' scales are standardized: odds/incidence-rate ratios correspond to 1 SD increase in each predictor.\n",
                          "Includes cases recored by ", 
                          gsub("-","/",as.character(max(EstMod$Data))),
                          " (N=", EstMod$targetmod[[1]]$n," in each model).")) + 
    theme_classic() + theme(legend.position = "none",
                            plot.title = element_text(hjust=0.5),
                            axis.text.x = element_text(angle=30,vjust=0.8,hjust=0.9)) 
}

#'
#' # Plot: Correlation matrix
#' 

corrdt <- granddt %>% 
  dplyr::select(regunemprate, taxbaseincome, 
         prop_smallhouse, prop_immig_noEU, 
         deathrate, poppr_65plus, 
         prop_univ,
         log_pop, log_popdens, 
         mean_numhh_census)
colnames(corrdt) <- c("A. Unemployment (%)",
                      "B. Av. taxable income (10k euro)",
                      "C. House <= 90sq.m (%)",           
                      "D.Immigrants out of EU (%)",
                      "E. Crude death rate",                   
                      "F. Age 65+ (%)",
                      "G. University degree (%)",
                      "H. Population (Log)",
                      "I. Pop. Density (Log)",                   
                      "J. Av. N in Household")
corrmat <- cor(corrdt[,rev(colnames(corrdt))], use="pairwise")
rownames(corrmat) <- sub("\\..*$","", rev(colnames(corrdt)))

## Correlation matrix
# p <- ggcorr(corrmat, palette = "RdBu", label = T, hjust=1)
require(ggcorrplot)
p <- ggcorrplot(corrmat, type = "full", show.diag = TRUE, lab = TRUE, 
                tl.srt = 0, legend.title = "") + 
  theme_classic() + labs(x=NULL, y=NULL) + 
  scale_x_discrete(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) + 
  geom_hline(aes(yintercept=6.5), linetype=1) + 
  geom_vline(aes(xintercept=6.5), linetype=1) + 
  geom_hline(aes(yintercept=4.5), linetype=1) + 
  geom_vline(aes(xintercept=4.5), linetype=1) + 
  geom_hline(aes(yintercept=3.5), linetype=1) + 
  geom_vline(aes(xintercept=3.5), linetype=1) 

#+ fig.width=7, fig.height=7
p

#+ eval=FALSE
ggsave(paste0(projdir,"/","out/ForArticle/corrmat_", Date_analy_simple, ".png"), p, width = 8, height = 5)
ggsave(paste0(projdir,"/","out/ForArticle/corrmat_", Date_analy_simple, ".pdf"), p, width = 8, height = 5)
# ggsave("Graph/Correlation_Matrix.png", p, width = 7, height = 7)

## scatter plot of tax base income and % of small house
# p <- plot(corrmat$taxbaseincome, corrmat$prop_smallhouse)

#'
#' ## Run Estimation
#'

#+ eval = FALSE

## By Cumulative 
EstMod_hurdle <- doEstMod_hurdle("2020-03-09") # One week before the lockdown

## By Moving Weekly Sum 
EstMod_hurdle_wk <- doEstMod_hurdle_wk("2020-03-09")

#+ echo = FALSE, eval = FALSE
## Save Models
saveRDS(EstMod_hurdle, paste0(projdir,"/","out/dailyreg_hurdle_cum_pos_mods_", Date_analy_simple, ".rds"))
saveRDS(EstMod_hurdle_wk, paste0(projdir,"/","out/dailyreg_hurdle_wk_pos_mods_", Date_analy_simple, ".rds"))

#+ echo = FALSE
## Read Models
EstMod_hurdle <- readRDS(paste0(projdir,"/","out/dailyreg_hurdle_cum_pos_mods_", Date_analy_simple, ".rds"))
EstMod_hurdle_wk <- readRDS(paste0(projdir,"/","out/dailyreg_hurdle_wk_pos_mods_", Date_analy_simple, ".rds"))

#'
#' ## Coefficient Plot at the current (latest) date
#'

## The Current Date
#max(EstMod_hurdle$Data)

## The Date for the analysis
Date_analy

#+ Model 1
p <- plotEstMod_oneshot(EstMod_hurdle, Date_analy, "m1", 
                        "cumulative", "positive", varlab_1)
#+ fig.width = 8, fig.height = 5
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/currentreg_hurdle_cum_pos_m1_", Date_analy_simple, ".png"), p, width = 8, height = 5)
ggsave(paste0(projdir,"/","out/currentreg_hurdle_cum_pos_m1_", Date_analy_simple, ".pdf"), p, width = 8, height = 5)
ggsave(paste0(projdir,"/","out/ForArticle/currentreg_hurdle_cum_pos_m1_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 5)
ggsave(paste0(projdir,"/","out/ForArticle/currentreg_hurdle_cum_pos_m1_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 5)
#ggsave(paste0("Graph/currentreg_hurdle_cum_pos_m1_wotitle_", Date_analy_simple, ".png"), 
#       p + labs(title=NULL, caption=NULL), width = 8, height = 5)

#+ Model 2
p <- plotEstMod_oneshot(EstMod_hurdle, max(EstMod_hurdle$Data), "m2", 
                        "cumulative", "positive", varlab_2)
#+ fig.width = 8, fig.height = 5
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/currentreg_hurdle_cum_pos_m2_", Date_analy_simple, ".png"), p, width = 8, height = 5)
ggsave(paste0(projdir,"/","out/currentreg_hurdle_cum_pos_m2_", Date_analy_simple, ".pdf"), p, width = 8, height = 5)
ggsave(paste0(projdir,"/","out/ForArticle/currentreg_hurdle_cum_pos_m2_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 5)
ggsave(paste0(projdir,"/","out/ForArticle/currentreg_hurdle_cum_pos_m2_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 5)

#+ Model 3
p <- plotEstMod_oneshot(EstMod_hurdle, max(EstMod_hurdle$Data), "m3", 
                        "cumulative", "positive", varlab_3)
#+ fig.width = 8, fig.height = 5
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/currentreg_hurdle_cum_pos_m3_", Date_analy_simple, ".png"), p, width = 8, height = 5)
ggsave(paste0(projdir,"/","out/currentreg_hurdle_cum_pos_m3_", Date_analy_simple, ".pdf"), p, width = 8, height = 5)
ggsave(paste0(projdir,"/","out/ForArticle/currentreg_hurdle_cum_pos_m3_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 5)
ggsave(paste0(projdir,"/","out/ForArticle/currentreg_hurdle_cum_pos_m3_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 5)

#+ Model 4
p <- plotEstMod_oneshot(EstMod_hurdle, Date_analy, "m4", 
                        "cumulative", "positive", varlab_4)
#+ fig.width = 8, fig.height = 5
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/currentreg_hurdle_cum_pos_m4_", Date_analy_simple, ".png"), p, width = 8, height = 5)
ggsave(paste0(projdir,"/","out/currentreg_hurdle_cum_pos_m4_", Date_analy_simple, ".pdf"), p, width = 8, height = 5)
ggsave(paste0(projdir,"/","out/ForArticle/currentreg_hurdle_cum_pos_m4_wotitle_", Date_analy_simple, ".png"),
       p + labs(title=NULL, caption=NULL), width = 8, height = 5)
ggsave(paste0(projdir,"/","out/ForArticle/currentreg_hurdle_cum_pos_m4_wotitle_", Date_analy_simple, ".pdf"),
       p + labs(title=NULL, caption=NULL), width = 8, height = 5)
# ggsave("Graph/currentreg_hurdle_cum_pos_m4_wotitle_", Date_analy_simple, ".png", 
#        p + labs(title=NULL, caption=NULL), width = 8, height = 5)

#+ Model 5
p <- plotEstMod_oneshot(EstMod_hurdle, Date_analy, "m5", 
                        "cumulative", "test", varlab_5)
#+ fig.width = 8, fig.height = 5
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/currentreg_hurdle_cum_pos_m5_", Date_analy_simple, ".png"), p, width = 8, height = 5)
ggsave(paste0(projdir,"/","out/currentreg_hurdle_cum_pos_m5_", Date_analy_simple, ".pdf"), p, width = 8, height = 5)
ggsave(paste0(projdir,"/","out/ForArticle/currentreg_hurdle_cum_pos_m5_wotitle_", Date_analy_simple, ".png"),
       p + labs(title=NULL, caption=NULL), width = 8, height = 5)
ggsave(paste0(projdir,"/","out/ForArticle/currentreg_hurdle_cum_pos_m5_wotitle_", Date_analy_simple, ".pdf"),
       p + labs(title=NULL, caption=NULL), width = 8, height = 5)
# ggsave("Graph/currentreg_hurdle_cum_pos_m5_wotitle_", Date_analy_simple, ".png", 
#        p + labs(title=NULL, caption=NULL), width = 8, height = 5)

#'
#' ## Dynamic Plots
#'

## Start date of Lockdown
(startdate = which(EstMod_hurdle$Data=="2020-03-14"))

#'
#' ### Weekly N of Cases
#'

#+ 
## Model 1 (Count)
p <- plotEstMod(EstMod_hurdle_wk, startdate, "count", "m1", "weekly", "positive", varlab_1)
#+ fig.width = 8, fig.height = 6
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_wk_pos_m1_count_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_wk_pos_m1_count_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_wk_pos_m1_count_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_wk_pos_m1_count_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)

#ggsave(paste0("Graph/dailyreg_hurdle_wk_pos_m1_count_wotitle_census_", Date_analy_simple, ".png"), 
#       p + labs(title=NULL, caption=NULL), width = 8, height = 6)

#+ 
## Model 1 (Zero)
p <- plotEstMod(EstMod_hurdle_wk, startdate, "zero", "m1", "weekly", "positive", varlab_1)
#+ fig.width = 8, fig.height = 6
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_wk_pos_m1_zero_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_wk_pos_m1_zero_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_wk_pos_m1_zero_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_wk_pos_m1_zero_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)

#ggsave(paste0("Graph/dailyreg_hurdle_wk_pos_m1_zero_wotitle_census_", Date_analy_simple, ".png"), 
#       p + labs(title=NULL, caption=NULL), width = 8, height = 6)

#+ 
## Model 2 (Count)
p <- plotEstMod(EstMod_hurdle_wk, startdate, "count", "m2", "weekly", "positive", varlab_2)
#+ fig.width = 8, fig.height = 6
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_wk_pos_m2_count_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_wk_pos_m2_count_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_wk_pos_m2_count_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_wk_pos_m2_count_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
#+ 
## Model 2 (Zero)
p <- plotEstMod(EstMod_hurdle_wk, startdate, "zero", "m2", "weekly", "positive", varlab_2)
#+ fig.width = 8, fig.height = 6
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_wk_pos_m2_zero_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_wk_pos_m2_zero_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_wk_pos_m2_zero_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_wk_pos_m2_zero_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)

#+ 
## Model 3 (Count)
p <- plotEstMod(EstMod_hurdle_wk, startdate, "count", "m3", "weekly", "positive", varlab_3)
#+ fig.width = 8, fig.height = 6
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_wk_pos_m3_count_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_wk_pos_m3_count_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_wk_pos_m3_count_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_wk_pos_m3_count_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
#+ 
## Model 3 (Zero)
p <- plotEstMod(EstMod_hurdle_wk, startdate, "zero", "m3", "weekly", "positive", varlab_3)
#+ fig.width = 8, fig.height = 6
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_wk_pos_m3_zero_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_wk_pos_m3_zero_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_wk_pos_m3_zero_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_wk_pos_m3_zero_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)

#+ 
## Model 4 (Count)
p <- plotEstMod(EstMod_hurdle_wk, startdate, "count", "m4", "weekly", "positive", varlab_4)
#+ fig.width = 8, fig.height = 6
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_wk_pos_m4_count_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_wk_pos_m4_count_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_wk_pos_m4_count_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_wk_pos_m4_count_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
# ggsave("Graph/dailyreg_hurdle_wk_pos_m4_count_wotitle_census.png", 
#        p + labs(title=NULL, caption=NULL), width = 8, height = 6)
#+ 
## Model 4 (Zero)
p <- plotEstMod(EstMod_hurdle_wk, startdate, "zero", "m4", "weekly", "positive", varlab_4)
#+ fig.width = 8, fig.height = 6
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_wk_pos_m4_zero_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_wk_pos_m4_zero_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_wk_pos_m4_zero_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_wk_pos_m4_zero_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)

#+ 
## Model 5 (Count)
p <- plotEstMod(EstMod_hurdle_wk, startdate, "count", "m5", "weekly", "test", varlab_5)
#+ fig.width = 8, fig.height = 6
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_wk_pos_m5_count_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_wk_pos_m5_count_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_wk_pos_m5_count_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_wk_pos_m5_count_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
# ggsave("Graph/dailyreg_hurdle_wk_pos_m5_count_wotitle_census.png", 
#        p + labs(title=NULL, caption=NULL), width = 8, height = 6)
#+ 
## Model 5 (Zero)
p <- plotEstMod(EstMod_hurdle_wk, startdate, "zero", "m5", "weekly", "test", varlab_1)
#+ fig.width = 8, fig.height = 6
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_wk_pos_m5_zero_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_wk_pos_m5_zero_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_wk_pos_m5_zero_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_wk_pos_m5_zero_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)

#'
#' ### Cumulative N of Cases
#'

#+ 
## Model 1 (Count)
p <- plotEstMod(EstMod_hurdle, startdate, "count", "m1", "cumulative", "positive", varlab_1)
#+ fig.width = 8, fig.height = 6
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_cum_pos_m1_count_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_cum_pos_m1_count_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_cum_pos_m1_count_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_cum_pos_m1_count_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)

#ggsave(paste0("Graph/dailyreg_hurdle_cum_pos_m1_count_wotitle_census_", Date_analy_simple, ".png"), 
#       p + labs(title=NULL, caption=NULL), width = 8, height = 6)

#+ 
## Model 1 (Zero)
p <- plotEstMod(EstMod_hurdle, startdate, "zero", "m1", "cumulative", "positive", varlab_1)
#+ fig.width = 8, fig.height = 6
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_cum_pos_m1_zero_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_cum_pos_m1_zero_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_cum_pos_m1_zero_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_cum_pos_m1_zero_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
#ggsave(paste0("Graph/dailyreg_hurdle_cum_pos_m1_zero_wotitle_census_", Date_analy_simple, ".png"), 
#       p + labs(title=NULL, caption=NULL), width = 8, height = 6)

#+ 
## Model 2 (Count)
p <- plotEstMod(EstMod_hurdle, startdate, "count", "m2", "cumulative", "positive", varlab_2)
#+ fig.width = 8, fig.height = 6
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_cum_pos_m2_count_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_cum_pos_m2_count_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_cum_pos_m2_count_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_cum_pos_m2_count_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
#+ 
## Model 2 (Zero)
p <- plotEstMod(EstMod_hurdle, startdate, "zero", "m2", "cumulative", "positive", varlab_2)
#+ fig.width = 8, fig.height = 6
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_cum_pos_m2_zero_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_cum_pos_m2_zero_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_cum_pos_m2_zero_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_cum_pos_m2_zero_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)

#+ 
## Model 3 (Count)
p <- plotEstMod(EstMod_hurdle, startdate, "count", "m3", "cumulative", "positive", varlab_3)
#+ fig.width = 8, fig.height = 6
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_cum_pos_m3_count_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_cum_pos_m3_count_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_cum_pos_m3_count_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_cum_pos_m3_count_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
#+ 
## Model 3 (Zero)
p <- plotEstMod(EstMod_hurdle, startdate, "zero", "m3", "cumulative", "positive", varlab_3)
#+ fig.width = 8, fig.height = 6
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_cum_pos_m3_zero_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_cum_pos_m3_zero_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_cum_pos_m3_zero_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_cum_pos_m3_zero_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)

#+ 
## Model 4 (Count)
p <- plotEstMod(EstMod_hurdle, startdate, "count", "m4", "cumulative", "positive", varlab_4)
#+ fig.width = 8, fig.height = 6
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_cum_pos_m4_count_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_cum_pos_m4_count_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_cum_pos_m4_count_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_cum_pos_m4_count_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
#+ 
## Model 4 (Zero)
p <- plotEstMod(EstMod_hurdle, startdate, "zero", "m4", "cumulative", "positive", varlab_4)
#+ fig.width = 8, fig.height = 6
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_cum_pos_m4_zero_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_cum_pos_m4_zero_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_cum_pos_m4_zero_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_cum_pos_m4_zero_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)

#+ 
## Model 5 (Count)
p <- plotEstMod(EstMod_hurdle, startdate, "count", "m5", "cumulative", "test", varlab_5)
#+ fig.width = 8, fig.height = 6
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_cum_pos_m5_count_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_cum_pos_m5_count_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_cum_pos_m5_count_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_cum_pos_m5_count_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
#+ 
## Model 5 (Zero)
p <- plotEstMod(EstMod_hurdle, startdate, "zero", "m5", "cumulative", "test", varlab_1)
#+ fig.width = 8, fig.height = 6
p
#+ eval = FALSE
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_cum_pos_m5_zero_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/dailyreg_hurdle_cum_pos_m5_zero_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_cum_pos_m5_zero_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/dailyreg_hurdle_cum_pos_m5_zero_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)


#'
#' # Analysis: Positive rate (OLS)
#' 

PlotEst_posrate <- function(data, date, testmin, posmin=1, mod = "1"){
  
  D <- data %>% 
    filter(Data == date, test >= testmin, pos >= posmin)
  
  if (mod=="1") {
    m1 <- lm(posrate ~ stdv(regunemprate) + stdv(taxbaseincome) + 
               stdv(prop_smallhouse) + 
               stdv(prop_immig_noEU) + #stdv(prop_service) +
               stdv(deathrate) + stdv(poppr_65plus) + 
               stdv(prop_univ) + 
               stdv(log(pop)) + stdv(log(popdens)) + 
               stdv(mean_numhh_census), 
             data = D)
    varlab_set <- varlab_1
    dv <- c("positive rate","positive rates")
    estmeth <- "OLS"
  } else if (mod=="1x") {
    m1 <- lm(posrate ~ stdv(regunemprate) + stdv(taxbaseincome) + 
               stdv(prop_smallhouse) + 
               stdv(prop_immig_noEU) + #stdv(prop_service) +
               stdv(deathrate) + stdv(poppr_65plus) + 
               stdv(prop_univ) + 
               stdv(log(pop)) + stdv(log(popdens)) + 
               stdv(mean_numhh_census) + 
               stdv(log(test_100k)), 
             data = D)
    varlab_set <- c(varlab_1, "Tests/100k pop. (Log)")
    dv <- c("positive rate","positive rates")
    estmeth <- "OLS"
  } else if (mod=="6") {
    require(MASS)
    m1 <- glm.nb(pos ~ stdv(regunemprate) + stdv(taxbaseincome) + 
               stdv(prop_smallhouse) + 
               stdv(prop_immig_noEU) + #stdv(prop_service) +
               stdv(deathrate) + stdv(poppr_65plus) + 
               stdv(prop_univ) + 
               stdv(log(pop)) + stdv(log(popdens)) + 
               stdv(mean_numhh_census) + 
               stdv(log(test_100k)), 
             data = D)
    varlab_set <- c(varlab_1, "Tests/100k pop. (Log)")
    dv <- c("positive case","positive cases")
    estmeth <- "negative binomial"
  } else if (mod=="7") {
    require(MASS)
    m1 <- glm.nb(test ~ stdv(regunemprate) + stdv(taxbaseincome) + 
                   stdv(prop_smallhouse) + 
                   stdv(prop_immig_noEU) + #stdv(prop_service) +
                   stdv(deathrate) + stdv(poppr_65plus) + 
                   stdv(prop_univ) + 
                   stdv(log(pop)) + stdv(log(popdens)) + 
                   stdv(mean_numhh_census) + 
                   stdv(log(pos)), 
                 data = D)
    varlab_set <- c(varlab_1, "N of positive cases (Log)")
    dv <- c("test case","test cases")
    estmeth <- "negative binomial"
  } else {
    stop("Invalid mod value!")
  }

  c1 <- coef(m1)
  k <- vcov(m1)
  k1 <- sqrt(diag(k))
  
  EstOut <- data.frame(estimate = c1, term = varlab_set, std.error = k1)
  D_Fig <- EstOut %>% 
    mutate(up95 = estimate + qnorm(0.975) * std.error,
           lo95 = estimate - qnorm(0.975) * std.error,
           up90 = estimate + qnorm(0.95) * std.error,
           lo90 = estimate - qnorm(0.95) * std.error,
           term = factor(term, levels = rev(varlab_set)),
           sig = ifelse((lo95 > 0)|(up95 < 0), "p<.05",
                        ifelse((lo90 > 0)|(up90 < 0), "p<.10", "n.s.")),
           sig = factor(sig, levels = c("p<.05","p<.10","n.s."))) %>% 
    filter(term != "(Intercept)")
  
  captiontxt <- paste0("Predictors' scales are standardized: coefficients correspond to 1 SD increase in each predictor.",
                       "\nDependent variable is COIVD-19 ", dv[1], " at ", date, ". Model estimated by ", estmeth, " regression.", 
                       "\nMunicipalities with less than ", testmin, " tests or ", posmin,
                       " positive cases are dropped. N = ", nobs(m1),".")
  
  if (mod %in% c("6","7")) {
    Fig <- D_Fig %>% 
      ggplot() +
      # geom_segment(aes(x = exp(lo90), xend = exp(up90), y = term, yend = term), col = "#FD8D3C", alpha = 0.8, size = 2) +
      # geom_segment(aes(x = exp(lo95), xend = exp(up95), y = term, yend = term), col = "#ffa869", alpha = 0.8, size = 1.4) +
      geom_segment(aes(x = exp(lo90), xend = exp(up90), y = term, yend = term, col = sig), alpha = 0.8, size = 2.2) +
      geom_segment(aes(x = exp(lo95), xend = exp(up95), y = term, yend = term, col = sig), alpha = 0.8, size = 1.4) +
      geom_point(aes(x = exp(estimate), y = term, shape = sig), size=2) +
      geom_vline(xintercept = 1, linetype = "dotted") +
      theme_bw() +
      labs(x = "Incidence-rate ratio (logged scale) \nwith 95% and 90% confidence intervals",
           title = paste0("Association between municipality characteristics and \n COVID-19 ", dv[2], " on ", date),
           caption = captiontxt) + 
      scale_shape_discrete(name="Statistical Significance", drop=FALSE, na.translate=FALSE) + 
      scale_color_discrete(name="Statistical Significance", drop=FALSE, na.translate=FALSE) + 
      scale_x_continuous(trans="log10") + 
      theme(axis.title.y = element_blank(),
            legend.position = "bottom",
            plot.title = element_text(hjust=0.5))
  } else {
    Fig <- D_Fig %>% 
      ggplot() +
      geom_segment(aes(x = lo90, xend = up90, y = term, yend = term, col = sig), alpha = 0.8, size = 2.2) +
      geom_segment(aes(x = lo95, xend = up95, y = term, yend = term, col = sig), alpha = 0.8, size = 1.4) +
      geom_point(aes(x = estimate, y = term, shape = sig), size = 2) +
      geom_vline(xintercept = 0, linetype = "dotted") +
      theme_bw() +
      labs(x = "Coefficient Estimates\nwith 95% and 90% confidence intervals",
           title = paste0("Association between municipality characteristics and \n COVID-19 ", dv[2], " on ", date),
           caption = captiontxt) + 
      scale_shape_discrete(name = "Statistical Significance", drop = FALSE) + 
      scale_color_discrete(name = "Statistical Significance", drop = FALSE) + 
      scale_x_continuous() + 
      theme(axis.title.y = element_blank(),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5))
  }

  return(Fig)
}

#+ 
## Model 1
p <- PlotEst_posrate(data = granddt, date = Date_analy, testmin = 10, posmin=0)

#+ fig.width = 8, fig.height = 6
p

#+ eval = FALSE
ggsave(paste0(projdir,"/","out/currentreg_ols_posrate_m1_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/currentreg_ols_posrate_m1_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/currentreg_ols_posrate_m1_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/currentreg_ols_posrate_m1_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)

### For Mogi
#ggsave(paste0("Graph/currentreg_ols_posrate_m1_wotitle_", Date_analy_simple, ".png"), 
#       p + labs(title=NULL, caption=NULL), width = 8, height = 5)

#+ 
## Model 1 (with 5 or more positive cases)
p <- PlotEst_posrate(data = granddt, date = Date_analy, testmin = 10, posmin=5)

#+ fig.width = 8, fig.height = 6
p

#+ eval = FALSE
ggsave(paste0(projdir,"/","out/currentreg_ols_posrate_m1_pos1_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/currentreg_ols_posrate_m1_pos1_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/currentreg_ols_posrate_m1_pos1_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/currentreg_ols_posrate_m1_pos1_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)

#+ 
## Model 1x (with 5 or more positive cases)
p <- PlotEst_posrate(data = granddt, date = Date_analy, testmin = 10, posmin=0, mod = "1x")

#+ fig.width = 8, fig.height = 6
p

#+ eval = FALSE
ggsave(paste0(projdir,"/","out/currentreg_ols_posrate_m1x_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/currentreg_ols_posrate_m1x_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/currentreg_ols_posrate_m1x_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/currentreg_ols_posrate_m1x_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)

#+ 
## Model 6 (with 5 or more positive cases)
p <- PlotEst_posrate(data = granddt, date = Date_analy, testmin = 1, posmin=0, mod = "6")

#+ fig.width = 8, fig.height = 6
p

#+ eval = FALSE
ggsave(paste0(projdir,"/","out/currentreg_nb_posrate_m6_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/currentreg_nb_posrate_m6_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/currentreg_nb_posrate_m6_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/currentreg_nb_posrate_m6_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)

#+ 
## Model 7 (with 5 or more positive cases)
p <- PlotEst_posrate(data = granddt, date = Date_analy, testmin = 1, posmin=10, mod = "7")

#+ fig.width = 8, fig.height = 6
p

#+ eval = FALSE
ggsave(paste0(projdir,"/","out/currentreg_nb_posrate_m7_", Date_analy_simple, ".png"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/currentreg_nb_posrate_m7_", Date_analy_simple, ".pdf"), p, width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/currentreg_nb_posrate_m7_wotitle_", Date_analy_simple, ".png"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)
ggsave(paste0(projdir,"/","out/ForArticle/currentreg_nb_posrate_m7_wotitle_", Date_analy_simple, ".pdf"), 
       p + labs(title=NULL, caption=NULL), width = 8, height = 6)

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render(paste0(projdir, '/src/analysis_dailyreg_hurdle_cataluna_latest.R'),
#                   rmarkdown::github_document(toc=TRUE),
#                   clean=FALSE)
# tmp <- list.files(paste0(projdir,"/src"))
# tmp <- tmp[grep("\\.spin\\.R$|\\.spin\\.Rmd$|\\.utf8\\.md$|\\.knit\\.md$|\\.tex",tmp)]
# for (i in 1:length(tmp)) file.remove(paste0(projdir,"/src/",tmp[i]))
