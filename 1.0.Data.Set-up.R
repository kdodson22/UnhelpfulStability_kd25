library(tidyverse)
library(vegan)
library(here)

##Read in data files #####
#relative cover of notable species
brch21 <- read.csv("data/brch21.csv")

#coordinates for all plots 
mypoints <- read.csv("data/BRWMA_plotcoords.csv")

#comprehensive data for all plots across all years 
df_divcov <- read.csv("data/df_divcov.csv")

#species attribute data 
plantattribs <- read.csv("data/plantattribs.csv")

#plot-level pretreatment metadata
pretreat <- read.csv("data/pretreat.csv")

#PRISM (OSU) data for given area 
prism <- read.csv("data/prism.csv")

#plot-level local metadata 
sitedata <- read.csv("data/sitedata.csv")

#soil data 
soildata <- read.csv("data/soildata.csv")
soildata2 <- read.csv("data/soildata2.csv")

#absolute species composition 
spp.comp <- read.csv("data/spp.comp.csv")
spp.comp2 <- read.csv("data/spp.comp2.csv") %>% column_to_rownames(var = "X")

#relative species composition
spp.rel.all <- read.csv("data/spp.rel.all.csv") #all species
spp.comp.rel <- read.csv("data/spp.comp.rel.csv") %>% column_to_rownames(var = "X")
spp.rel.inv <- read.csv("data/spp.rel.inv.csv") #invasive species
