## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 1.0 Data Set-up

## Purpose: This script loads necessary data for analysis
## Author: K. Dodson 
## Date: Updated 12/1/2025


## ASW: good to add a descriptive title to each script.

library(tidyverse)
library(vegan)
library(here)

##Read in data files #####


## ASW: Hey, Kay! Many of these are loading the same variables twice. For instance -- the plot-level features across all years already contain all of the needed N, WHC, and site-level (heatload, etc). variables. Do you need to load them again? if so, can you explain why/how they are different?

# if you don't need to reload variables, please avoid to simplify for the reader -- in particular, you may be able to get rid of the files indicated below?

#coordinates for all plots 
mypoints <- read.csv("data/BRWMA_plotcoords.csv")

#plot-level features for all plots across all years 
df_divcov <- read.csv("data/df_divcov.csv")

#species attribute data 
plantattribs <- read.csv("data/plantattribs.csv")

#plot-level pretreatment features:
pretreat <- read.csv("data/pretreat.csv")

#PRISM (OSU) data for plots:
prism <- read.csv("data/prism.csv") ## is this needed?

#plot-level local metadata 
sitedata <- read.csv("data/sitedata.csv") ## is this needed, separate from df_divcov?

#soil data, including water content, available nitrogen:
soildata <- read.csv("data/soildata.csv") ## is this needed, separate from df_divcov?
soildata2 <- read.csv("data/soildata2.csv") ## is this needed, separate from df_divcov?

#absolute species composition 
spp.comp <- read.csv("data/spp.comp.csv")
spp.comp2 <- read.csv("data/spp.comp2.csv") %>% column_to_rownames(var = "X")

#relative species composition
spp.rel.all <- read.csv("data/spp.rel.all.csv") #all species
spp.comp.rel <- read.csv("data/spp.comp.rel.csv") %>% column_to_rownames(var = "X")
spp.rel.inv <- read.csv("data/spp.rel.inv.csv") #invasive species

#relative cover of most abundant species
brch21 <- read.csv("data/brch21.csv") ## is this needed, separate from the other files above?
