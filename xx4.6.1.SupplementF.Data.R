## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 4.6.1 Supplement F Data:

## Purpose: This script prepares data for supp fig 6 - interannual compositional change 

## Author: KED
## Updated: 11/8/2026
######

## This analysis requires use of Ecotraj 0.1.1
# library(remotes)
# remotes::install_version("ecotraj", version = "0.1.1")

library(tidyverse)
library(vegan)
library(here)
library(corrplot)
library(brms)
library(tidybayes)
library(ggtext)
library(ecotraj)


#data
source(here("1.0.Data.Set-up.R"))

## DATA PREP #####
df_divcov$Site <- factor(df_divcov$Site, 
                         levels = c("1","2","3","4","5",
                                    "6","7","8","9","10"))
df_divcov$Plot <- factor(df_divcov$Plot, 
                         levels = c("1","2","3","4","5",
                                    "6","7","8","9","10",
                                    "11","12","13"))
pretreat$Site <- factor(pretreat$Site, 
                        levels = c("1","2","3","4","5",
                                   "6","7","8","9","10"))
pretreat$Plot <- factor(pretreat$Plot, 
                        levels = c("1","2","3","4","5",
                                   "6","7","8","9","10",
                                   "11","12","13"))

#create 'metadata' df with site, plot, date, and treatment info
spp.comp.rel$Obs.ID <- row.names(spp.comp.rel)
meta1 <- spp.comp.rel %>% 
  dplyr::select(-OARTR, -OPSSP, -OERNA, -OELEL, -OACMI) %>% 
  separate(Obs.ID, into = c("Site", "Plot", "Year")) %>% 
  relocate(c("Site", "Plot", "Year"), .before = "AMSIN") %>%
  mutate(Year = as.numeric(Year),
         Site = as.numeric(Site),
         Plot = as.numeric(Plot)) %>% 
  dplyr::select(Site, Plot, Year) %>%
  mutate(ObsID = paste(Site, Plot, Year, sep = "-")) 

treat1 <- df_divcov %>%
  filter(Year == 2021) %>%
  mutate(Site = as.integer(Site),
         Plot = as.integer(Plot)) %>%
  filter(Plot <= 13) %>% ## remove 4x4 N-treatment plots, not used in this analysis.
  dplyr::select(Site, Plot, Sprayed)

meta <- left_join(meta1, treat1, by = c("Site", "Plot"))
meta <- meta %>% mutate(Year = as.numeric(Year))



## Calculate distances in community composition between time points using ecotraj functions:####

## 2021 -> 2022 Distance, representing Compositional Resistance ###### 
#create metadata df
plots1 <- meta %>% 
  filter(Year == 2021 | Year == 2022)

#filter to 2021 and 2022 years from spp.comp.rel
scr.1 <- spp.comp.rel %>% 
  separate(Obs.ID, into = c("Site", "Plot", "Year")) %>% 
  relocate(c("Site", "Plot", "Year"), .before = "AMSIN") %>%
  mutate(Year = as.numeric(Year),
         Site = as.numeric(Site),
         Plot = as.numeric(Plot)) %>%
  filter(Year == 2021 | Year == 2022)


#Calculate bray-curtis dissimilarity using vegan:
bray1 <- vegdist(scr.1[,-c(1:3)], method = "bray")

#as matrix
braymat1 <- as.matrix(bray1)
#as data frame
braydf1 <- as.data.frame(braymat1)

#combine
distmatrix1 <- cbind(plots1, braydf1)

#group by site-plot IDs
distmat1 <- distmatrix1 %>%
  mutate("Site-Plot" = paste(Site, Plot, Sprayed, sep = "-")) %>%
  relocate("Site-Plot", .after = Year)














