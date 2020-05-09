#' ---
#' title: "Visualize COVID-19 PCR Test Data"
#' author: "Gento Kato"
#' date: "May 8, 2020"
#' ---

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

## Plots ## 
require(sf)
require(rmapshaper)
require(ggplot2)
require(dplyr)

## Import Relevant Data
granddt <- readRDS(paste0(projdir,"/data/granddt_latest.rds"))
shapedt <- readRDS(paste0(projdir,"/data/shapefile/shape_cataluna_rev.rds")) %>% 
  filter(mundesc != "(altres municipis)")
shapedt_simple <- ms_simplify(shapedt) %>% st_as_sf()

# Merge ALl Data
granddt <- granddt %>% 
  inner_join(shapedt_simple, by = c("mundesc"="mundesc"))
target <- granddt

## Limits COVID-19 Data to Those with >=10 Tests

## Positive Rate
target$posrate <- ifelse(target$test<10, NA, target$posrate)
p <- ggplot(target) + 
  geom_sf(aes(geometry=geometry, fill=posrate), color="white", size=0.1) + 
  scale_fill_viridis_c(name="Positive Rate", option="A", direction=-1, na.value = "grey80") + 
  labs(title=paste0("COVID-19 Positive Rate in Catalonia \n(As of ", max(granddt$Data), ")"),
       caption="Note: Municipalities with less than 10 reported PCR tests are grayed out.") +
  theme_void() + theme(plot.title = element_text(hjust=0.5))
p
ggsave(paste0(projdir,"/out/map_posrate.png"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/map_posrate.pdf"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_posrate_wotitle.png"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_posrate_wotitle.pdf"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)

## Positive Cases / 10k Population
target$pos_100k <- ifelse(target$test<10, NA, target$pos_100k)
p <- ggplot(target) + 
  geom_sf(aes(geometry=geometry, fill=pos_100k), color="white", size=0.1) + 
  scale_fill_viridis_c(name="Positive Cases/\n10,000 Population", option="A", direction=-1, na.value = "grey80",
                       trans=scales::trans_new(name = "sqrtsqrt", transform = function(x) sqrt(sqrt(x)),
                                               inverse = function(x) (x^2)^2 ), breaks=c(50,500,3000)) + 
  labs(title=paste0("COVID-19 Positive Cases/10,000 Population in Catalonia \n(As of ", max(granddt$Data), ")"),
       caption="Note: Municipalities with less than 10 reported PCR tests are grayed out.") +
  theme_void() + theme(plot.title = element_text(hjust=0.5))
p
ggsave(paste0(projdir,"/out/map_pos.png"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/map_pos.pdf"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_pos_wotitle.png"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_pos_wotitle.pdf"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)

## Tested Cases / 10k Population
target$test_100k <- ifelse(target$test<10, NA, target$test_100k)
p <- ggplot(target) + 
  geom_sf(aes(geometry=geometry, fill=test_100k), color="white", size=0.1) + 
  scale_fill_viridis_c(name="Tested Cases/\n10,000 Population", option="A", direction=-1, na.value = "grey80",
                       trans=scales::trans_new(name = "sqrtsqrt", transform = function(x) sqrt(sqrt(x)),
                                               inverse = function(x) (x^2)^2), breaks=c(500,2000,5000)) + 
  labs(title=paste0("COVID-19 Tested Cases/10,000 Population in Catalonia \n(As of ", max(granddt$Data), ")"),
       caption="Note: Municipalities with less than 10 reported PCR tests are grayed out.") +
  theme_void() + theme(plot.title = element_text(hjust=0.5))
p
ggsave(paste0(projdir,"/out/map_test.png"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/map_test.pdf"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_test_wotitle.png"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_test_wotitle.pdf"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)

## Registered Unemployment Rate
p <- ggplot(target) + 
  geom_sf(aes(geometry=geometry, fill=regunemprate), color="white", size=0.1) + 
  scale_fill_viridis_c(name="Registered Unemployment Rate in \nPop. of 15 to 65 Years Old (%)", 
                       option="A", direction=-1, trans="sqrt", na.value = "grey80") + 
  labs(title="Unemployment Rate in Catalonia (2019 Average)") +
  theme_void() + theme(plot.title = element_text(hjust=0.5))
p
ggsave(paste0(projdir,"/out/map_regunemprate.png"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/map_regunemprate.pdf"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_regunemprate_wotitle.png"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_regunemprate_wotitle.pdf"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)

## Alternative Unemployment Rate
p <- ggplot(target) + 
  geom_sf(aes(geometry=geometry, fill=unemprate), color="white", size=0.1) + 
  scale_fill_viridis_c(name="Alternative Unemployment Rate \nin Labor Population (%)", 
                       option="A", direction=-1, trans="sqrt", na.value = "grey80") + 
  labs(title="Unemployment Rate in Catalonia (2019 Alternative Measurement)") +
  theme_void() + theme(plot.title = element_text(hjust=0.5))
p
ggsave(paste0(projdir,"/out/map_unemprate.png"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/map_unemprate.pdf"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_unemprate_wotitle.png"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_unemprate_wotitle.pdf"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)

## Taxable Base Income
p <- ggplot(target) + 
  geom_sf(aes(geometry=geometry, fill=taxbaseincome), color="white", size=0.1) + 
  scale_fill_viridis_c(name="Av. Taxable Base Income \n(in 100k Euro)", 
                       option="A", direction=-1, trans="sqrt", na.value = "grey80") + 
  labs(title="Average Taxable Base Income in Catalonia (2017)") +
  theme_void() + theme(plot.title = element_text(hjust=0.5))
p
ggsave(paste0(projdir,"/out/map_taxbaseincome.png"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/map_taxbaseincome.pdf"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_taxbaseincome_wotitle.png"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_taxbaseincome_wotitle.pdf"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)

## Proportion Living In Small Housing
p <- ggplot(target) + 
  geom_sf(aes(geometry=geometry, fill=prop_smallhouse), color="white", size=0.1) + 
  scale_fill_viridis_c(name="Residence \n90 sq. meter or smaller (%)", 
                       option="A", direction=-1, na.value = "grey80") + 
  labs(title="Proportion of Residents Living in Small Housing \nin Catalonia (2011 Census)") +
  theme_void() + theme(plot.title = element_text(hjust=0.5))
p
ggsave(paste0(projdir,"/out/map_smallhouse.png"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/map_smallhouse.pdf"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_smallhouse_wotitle.png"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_smallhouse_wotitle.pdf"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)

## Proportion of Immigrants out of EU
p <- ggplot(target) + 
  geom_sf(aes(geometry=geometry, fill=prop_immig_noEU), color="white", size=0.1) + 
  scale_fill_viridis_c(name="Immigrants from \nout of EU (%)", 
                       option="A", direction=-1, trans="sqrt", na.value = "grey80") + 
  labs(title="Immigrants from out of EU in Catalonia (2018)") +
  theme_void() + theme(plot.title = element_text(hjust=0.5))
p
ggsave(paste0(projdir,"/out/map_immig_noEU.png"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/map_immig_noEU.pdf"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_immig_noEU_wotitle.png"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_immig_noEU_wotitle.pdf"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)

## Proportion of Service Industry Workers
p <- ggplot(target) + 
  geom_sf(aes(geometry=geometry, fill=prop_service), color="white", size=0.1) + 
  scale_fill_viridis_c(name="Working in Service Industry (%)", 
                       option="A", direction=-1, na.value = "grey80") + 
  labs(title="Proportion of Service Industry Workers in Catalonia (2018)") +
  theme_void() + theme(plot.title = element_text(hjust=0.5))
p
ggsave(paste0(projdir,"/out/map_service.png"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/map_service.pdf"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_service_wotitle.png"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_service_wotitle.pdf"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)

## Death Rate (2018)
p <- ggplot(target) + 
  geom_sf(aes(geometry=geometry, fill=deathrate), color="white", size=0.1) + 
  scale_fill_viridis_c(name="Death Rate (%)", 
                       option="A", direction=-1, trans="sqrt", na.value = "grey80") + 
  labs(title="Rough Death Rates in Catalonia (2018)") +
  theme_void() + theme(plot.title = element_text(hjust=0.5))
p
ggsave(paste0(projdir,"/out/map_deathrate.png"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/map_deathrate.pdf"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_deathrate_wotitle.png"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_deathrate_wotitle.pdf"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)

## Proportion of 65+ Residents
p <- ggplot(target) + 
  geom_sf(aes(geometry=geometry, fill=poppr_65plus), color="white", size=0.1) + 
  scale_fill_viridis_c(name="Proportion of 65+ \nYears Old Residents", option="A", direction=-1) + 
  labs(title="Proportion of Elderly Residents in Catalonia (2019)") +
  theme_void() + theme(plot.title = element_text(hjust=0.5))
p
ggsave(paste0(projdir,"/out/map_poppr_65plus.png"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/map_poppr_65plus.pdf"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_poppr_65plus_wotitle.png"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_poppr_65plus_wotitle.pdf"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)

## Proportion of University Educated
p <- ggplot(target) + 
  geom_sf(aes(geometry=geometry, fill=prop_univ), color="white", size=0.1) + 
  scale_fill_viridis_c(name="Attended university in \n16-65 years old pop. (%)", 
                       option="A", direction=-1, na.value = "grey80") + 
  labs(title="Proportion of University Educated in Catalonia (2011)") +
  theme_void() + theme(plot.title = element_text(hjust=0.5))
p
ggsave(paste0(projdir,"/out/map_univ.png"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/map_univ.pdf"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_univ_wotitle.png"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_univ_wotitle.pdf"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)

## Population Density
p <- ggplot(target) + 
  geom_sf(aes(geometry=geometry, fill=popdens), color="white", size=0.1) + 
  scale_fill_viridis_c(name="Population Density \n(per sq. km)", option="A", direction=-1,
                       trans="log", breaks=c(10,150,3000)) + 
  labs(title="Population Density in Catalonia (2019)") +
  theme_void() + theme(plot.title = element_text(hjust=0.5))
p
ggsave(paste0(projdir,"/out/map_popdens.png"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/map_popdens.pdf"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_popdens_wotitle.png"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_popdens_wotitle.pdf"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)

## Average Number in House Hold
p <- ggplot(target) + 
  geom_sf(aes(geometry=geometry, fill=numhh), color="white", size=0.1) + 
  scale_fill_viridis_c(name="Av. number of members \nin a household", 
                       option="A", direction=-1, trans="log", na.value = "grey80") + 
  labs(title="Average household size in Catalonia (2011)") +
  theme_void() + theme(plot.title = element_text(hjust=0.5))
p
ggsave(paste0(projdir,"/out/map_numhh.png"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/map_numhh.pdf"), p, width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_numhh_wotitle.png"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)
ggsave(paste0(projdir,"/out/ForArticle/map_numhh_wotitle.pdf"), 
       p + labs(title = NULL, caption = NULL), width=8, height=6)

