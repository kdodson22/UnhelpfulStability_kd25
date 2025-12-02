## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 1.0 Data Set-up

## Purpose: This script loads necessary data for analysis
## Author: K. Dodson 
## Date: Updated 12/1/2025


library(tidyverse)
library(vegan)
library(here)

##Read in data files #####

#coordinates for all plots 
mypoints <- read.csv("data/BRWMA_plotcoords.csv")

#plot-level features for all plots across all years 
df_divcov <- read.csv("data/df_divcov.csv")

#species attribute data 
plantattribs <- read.csv("data/plantattribs.csv")

#plot-level pretreatment features:
pretreat <- read.csv("data/pretreat.csv")

#PRISM (OSU) data for plots:
prism <- read.csv("data/prism.csv")  

#soil data, including water content, available nitrogen:
soildata <- read.csv("data/soildata.csv") 
soildata2 <- read.csv("data/soildata2.csv") 

#absolute species composition 
spp.comp <- read.csv("data/spp.comp.csv")
spp.comp2 <- read.csv("data/spp.comp2.csv") %>% column_to_rownames(var = "X")

#relative species composition
spp.rel.all <- read.csv("data/spp.rel.all.csv") #all species
spp.comp.rel <- read.csv("data/spp.comp.rel.csv") %>% column_to_rownames(var = "X")
spp.rel.inv <- read.csv("data/spp.rel.inv.csv") #invasive species

#relative cover of most abundant species
brch21 <- spp.comp.rel %>%
  rownames_to_column(var = "ObsID") %>%
  separate(col = "ObsID", 
           into = c("Site", "Plot","Year"), 
           sep = "-") %>%
  select(Site, Plot, Year, BRTE, CHJU, BRAR5, BRBR5, POBU, EPBR3) %>%
  filter(Year == 2021) %>%
  mutate(Year = as.numeric(Year), 
         Site = as.factor(Site),
         Plot = as.factor(Plot))%>% 
  group_by(Site, Plot, Year) %>% 
  mutate(Bromus = sum(BRTE, BRAR5, BRBR5)) %>%
  ungroup()
