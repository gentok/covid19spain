#' ---
#' title: "Visualize COVID-19 PCR Test Data"
#' author: "Gento Kato"
#' date: "April 12, 2020"
#' ---

## Set Working Directory to the current directory 
## (If using RStudio, can be set automatically)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Clear Workspace
rm(list=ls())

## Import Relevant Data
coviddt <- readRDS("../data/test_cataluna_wide_rev.rds")
shapedt <- readRDS("../data/shapefile/mun_cataluna_rev.rds")
demodt <- readRDS("../data/demo_cataluna_wide_rev.rds")

## Drop Cases with Unknown Municipality
coviddt <- coviddt[which(coviddt$MunicipiDescripcio!="(Altres municipis)"),]

## Extract Only The Latest Date
coviddt_latest <- coviddt[which(coviddt$Data==max(coviddt$Data)),]

## Merge demodt and shape file to Create Grand Dataset
all(tolower(shapedt$CMUN_NAME)==tolower(demodt$munname)) # All Municipality Names Identical
granddt <- cbind.data.frame(shapedt, demodt)

## Recode Some Relevant Variables
# Population
granddt$pop <- as.numeric(granddt$f321)
granddt$pop_15to64 <- as.numeric(granddt$f27) # 15 to 64
granddt$pop_65plus <- as.numeric(granddt$f28) + as.numeric(granddt$f29) # 65 years or over
granddt$poppr_65plus <- (granddt$pop_65plus/granddt$pop)*100
# Population Density (Population/km2)
granddt$popdens <- as.numeric(granddt$f262)
summary(granddt$popdens)
# Unemployment (Missing in Half of Cases)
granddt$pop_unemp <- as.numeric(granddt$f222)
granddt$pop_active <- as.numeric(granddt$f223)
granddt$unemprate <- (granddt$pop_unemp/granddt$pop_active)*100
summary(granddt$unemprate)
# Registerd Unemployment (No Missing Cases)
granddt$pop_regunemp <- as.numeric(granddt$f242) # Annual Average N of Registered Unemployment
granddt$regunemprate <- (granddt$pop_regunemp/granddt$pop_15to64)*100
summary(granddt$regunemprate)
# Disposable Household Income Per Capita (In Thousands of Euro) *Many Missing Cases, Not Used
granddt$avincome <- as.numeric(granddt$f7) 
summary(granddt$avincome)

## Add COVID-19 to Shape File ##

# Positive Cases
granddt$pos <- coviddt_latest$CumCasos_P[match(tolower(granddt$CMUN_NAME),tolower(coviddt_latest$MunicipiDescripcio))]
# Tested Cases
granddt$test <- coviddt_latest$CumCasos[match(tolower(granddt$CMUN_NAME),tolower(coviddt_latest$MunicipiDescripcio))]
# Positve Rate
granddt$posrate <- (granddt$pos/granddt$test)*100
# Positive Cases / 100k Population
granddt$pos_100k <- granddt$pos/(as.numeric(granddt$pop)/100000)
# Tested Cases / 100k 
granddt$test_100k <- granddt$test/(as.numeric(granddt$pop)/100000)

## Plots ## 
require(sf)
require(ggplot2)

## Limits COVID-19 Data to Those with >=10 Tests
target <- granddt
threshold_N <- 10
target$pos[which(granddt$test<threshold_N)] <- NA
target$test[which(granddt$test<threshold_N)] <- NA
target$posrate[which(granddt$test<threshold_N)] <- NA
target$pos_100k[which(granddt$test<threshold_N)] <- NA
target$test_100k[which(granddt$test<threshold_N)] <- NA

require(dplyr)
target2 <- target %>% 
  mutate(hotelbeds = as.numeric(as.character(f216)),
         service = as.numeric(as.character(f280)),
         prop_service = (service / as.numeric(as.character(f282)))*100,
         unemp_men = as.numeric(as.character(f240)),
         men15_64 = as.numeric(as.character(f33)),
         unemp_men_rate = (unemp_men / men15_64)*100,
         unemp_women = as.numeric(as.character(f241)),
         women15_64 = as.numeric(as.character(f39)),
         unemp_women_rate = (unemp_women / women15_64)*100)

## Positive Rate
p <- ggplot(target) + 
  geom_sf(aes(geometry=geometry, fill=posrate), color="white", size=0.1) + 
  scale_fill_viridis_c(name="Positive Rate", option="A", direction=-1) + 
  labs(title="COVID-19 Positive Rate in Catalonia",
       caption="Note: Municipalities with less than 10 tests are grayed out.") +
  theme_void() + theme(plot.title = element_text(hjust=0.5))
p
ggsave("../out/map_posrate.png", p, width=8, height=7)

## Positive Cases / 10k Population
p <- ggplot(target) + 
  geom_sf(aes(geometry=geometry, fill=pos_100k), color="white", size=0.1) + 
  scale_fill_viridis_c(name="Positive Cases/\n10,000 Population", option="A", direction=-1, 
                       trans=scales::trans_new(name = "sqrtsqrt", transform = function(x) sqrt(sqrt(x)),
                                               inverse = function(x) (x^2)^2), breaks=c(50,500,3000)) + 
  labs(title="COVID-19 Positive Cases/10,100 Population in Catalonia",
       caption="Note: Municipalities with less than 10 tests are grayed out.") +
  theme_void() + theme(plot.title = element_text(hjust=0.5))
p
ggsave("../out/map_pos_100k.png", p, width=8, height=7)

## Tested Cases / 10k Population
p <- ggplot(target) + 
  geom_sf(aes(geometry=geometry, fill=test_100k), color="white", size=0.1) + 
  scale_fill_viridis_c(name="Tested Cases/\n10,000 Population", option="A", direction=-1, 
                       trans=scales::trans_new(name = "sqrtsqrt", transform = function(x) sqrt(sqrt(x)),
                                               inverse = function(x) (x^2)^2), breaks=c(500,2000,5000)) + 
  labs(title="COVID-19 Tested Cases/10,100 Population in Catalonia",
       caption="Note: Municipalities with less than 10 tests are grayed out.") +
  theme_void() + theme(plot.title = element_text(hjust=0.5))
p
ggsave("../out/map_test_100k.png", p, width=8, height=7)

## Registered Unemployment Rate
p <- ggplot(target) + 
  geom_sf(aes(geometry=geometry, fill=regunemprate), color="white", size=0.1) + 
  scale_fill_viridis_c(name="Registered Unemployment Rate \nin Pop. of 15 to 65 Years Old", option="A", direction=-1,
                       trans="sqrt") + 
  labs(title="Unemployment Rate in Catalonia (2019 Average)") +
  theme_void() + theme(plot.title = element_text(hjust=0.5))
p
ggsave("../out/map_regunemprate.png", p, width=8, height=7)

## Population Density
p <- ggplot(target) + 
  geom_sf(aes(geometry=geometry, fill=popdens), color="white", size=0.1) + 
  scale_fill_viridis_c(name="Population Density \n(per sq. km)", option="A", direction=-1,
                       trans="log", breaks=c(10,150,3000)) + 
  labs(title="Population Density in Catalonia (2019)") +
  theme_void() + theme(plot.title = element_text(hjust=0.5))
p
ggsave("../out/map_popdens.png", p, width=8, height=7)

## Proportion of 65+ Residents
p <- ggplot(target) + 
  geom_sf(aes(geometry=geometry, fill=poppr_65plus), color="white", size=0.1) + 
  scale_fill_viridis_c(name="Proportion of 65+ \nYears Old Residents", option="A", direction=-1) + 
  labs(title="Proportion of Elderly Residents in Catalonia (2019)") +
  theme_void() + theme(plot.title = element_text(hjust=0.5))
p
ggsave("../out/map_poppr_65plus.png", p, width=8, height=7)

## Prediction ##

## OLS Regression with Predictors 
m1 <- lm(pos_100k ~ regunemprate + I(popdens/1000) + poppr_65plus + I(hotelbeds>=500) + prop_service, data=target2)
m2 <- lm(test_100k ~ regunemprate + I(popdens/1000) + poppr_65plus + I(hotelbeds>=500) + prop_service, data=target2)
m3 <- lm(posrate ~ regunemprate + I(popdens/1000) + poppr_65plus + I(hotelbeds>=500) + prop_service, data=target2)

# Regression Table
require(texreg)
screenreg(list(m1,m2,m3), digits = 3, symbol = "+", stars = c(0.001, 0.01, 0.05,0.1),
          custom.coef.names = c("(Intercept)","Unemployment Rate (All)",
                                "Population Density (1000/sq. km)","65+ Years Old Population %",
                                "Hotel Beds N >= 500", "Prop. Service Industry"),
          custom.model.names = c("Positive/100k","Tested/100k","Positive Rate"))

# Coefficient Plot (Custom Package by Me)
# devtools::install_github("gentok/estvis")
library(estvis)

p <- plot_coef(list(m1,m2,m3), digits = 3, 
          facet.names = "m.names", facet.x.scale = "free",
          m.names = c("Positive/100k","Tested/100k","Positive Rate"),
          title = "Predicting COVID-19 Cases in Catalonia\nthrogh Municipality Demographics",
          drop.intercept = TRUE,
          custom.variable.names = c("Unemployment Rate",
                                "Population Density (1000/sq. km)","65+ Years Old Population %"),
          footnote.gof = TRUE, custom.footnote = "Note: Municipalities with less than 10 PCR tests are excluded from the analysis.\nEstimated by OLS Regression and intercepts are omitted from outputs.",
          footnote.caption = FALSE)
p
ggsave("../out/coefplot_covidmod.png", p, width=8, height=5)
