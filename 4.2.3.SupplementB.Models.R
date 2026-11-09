## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 4.2.3 Supplement B Models:

## Purpose: This script fits an alternate model specification: Estimated effect of diversity, dominance, and soil nitrogen on system stability when restoration-asssociated native plantings are included in measured of post-treatment native cover and composition

## Author: KED
## Updated: 11/8/2026
######

## This analysis requires use of Ecotraj 0.1.1
library(remotes)
remotes::install_version("ecotraj", version = "0.1.1")

library(tidyverse)
library(vegan)
library(here)
library(corrplot)
library(brms)
library(tidybayes)
library(ggtext)
library(ecotraj)
#

### Data ####
source(here("1.0.Data.Set-up.R"))
source(here("1.1.RRR.Cover.R"))
extranat_cover_df <- read.csv("data/onat_test_dfdivcov.csv") %>%
  select(-natcover_abs) %>%
  rename(extranatcover_abs = onatcover_abs) %>%
  mutate(Site = as.factor(Site), Plot = as.factor(Plot))

### Prepare native absolute cover df for "extranatives" (aka our plantings included) ####
#absolute cover dataframe
extranatabsdf <- df_divcov %>%
  select(Site, Plot, Year, -natcover_abs) %>% 
  left_join(extranat_cover_df, join_by(Site,Plot,Year)) %>%
  pivot_wider(names_from = "Year", 
              values_from = c("extranatcover_abs"),
              names_prefix = "extranatcover_abs_") %>%
  left_join(pretreat, by = c("Site", "Plot"))

# unlike in the invasive data, native absences are possible:
extranatabsdf <- extranatabsdf %>% filter(extranatcover_abs_2021 != 0 &
                                      extranatcover_abs_2022 != 0 &
                                      extranatcover_abs_2023 != 0 &
                                      extranatcover_abs_2024 != 0  )

#add more soil variables:
extranatabsdf_soil <- extranatabsdf  %>% 
  left_join(soildata, by = c("Site","Plot")) %>%
  ## Calculate changes in soil N:
  mutate(Fall21_mineralN = N_ug.g.incubated_2021Fall - N_ug.g.instant_2021Fall,
         Spr22_mineralN = N_ug.g.incubated_2022Spring - N_ug.g.instant_2022Spring, 
         DeltaN_21_22 = (N_ug.g.instant_2021Fall - N_ug.g.instant_2022Spring)/N_ug.g.instant_2021Fall,
         DeltaNH4_21_22 = (NH4.instant_2021Fall - NH4.instant_2022Spring)/NH4.instant_2021Fall,
         DeltaNO3_21_22 = (NO3.instant_2021Fall - NO3.instant_2022Spring)) 


#calculate mundlak/site-level means for variables 
mundlaks <- extranatabsdf_soil %>%
  group_by(Site) %>%
  filter(Sprayed=="Yes") %>%
  summarise(slmc.inv.cov = mean(pre_invcover_abs),
            slmd.all = mean(pre_totaldiversity),
            slmd.inv = mean(pre_invdiversity),
            slmd.nat = mean(pre_natdiversity),
            slmr.all = mean(pre_totalrichness),
            slmr.inv = mean(pre_invrichness),
            slmr.nat = mean(pre_natrichness),
            slm.NO3.21fall = mean(NO3.instant_2021Fall),
            slm.NO3.22sp = mean(NO3.instant_2022Spring),
            slm.N.21fall = mean(N_ug.g.instant_2021Fall),
            slm.N.22sp = mean(N_ug.g.instant_2022Spring),
            slm.NH4.21fall = mean(NH4.instant_2021Fall),
            slm.NH4.22sp = mean(NH4.instant_2022Spring),
            slm.MinN.22sp = mean(Spr22_mineralN),
            slm.TotN.21fall = mean(N_pct_2021Fall),
            slm.TotN.22sp = mean(N_pct_2022Spring),
            slm.wc.f21 = mean(watercontent_g.g.instant_2021Fall),
            slm.wc.s22 = mean(watercontent_g.g.instant_2022Spring),
            slm.whc.f21 = mean(WHC_g.g.instant_2021Fall),
            slm.whc.s22 = mean(WHC_g.g.instant_2022Spring))

#combine
extranatabsdf_soil <- extranatabsdf_soil %>% left_join(mundlaks)


## Calculate RRR metrics:
#calculate means of control plots for each year
controlmeans <- extranatabsdf_soil %>%
  group_by(Site) %>%
  filter(Sprayed=="No") %>%
  summarise(extranatcover_abs_2022.control = mean(extranatcover_abs_2022),
            extranatcover_abs_2023.control = mean(extranatcover_abs_2023),
            extranatcover_abs_2024.control = mean(extranatcover_abs_2024)) %>%
  select(Site, extranatcover_abs_2022.control, extranatcover_abs_2023.control, extranatcover_abs_2024.control)

#combine
extranatabsdf_soil <- extranatabsdf_soil %>% left_join(controlmeans)

#calculate log response ratio style RRR metrics:
# Resistance = compare sprayed and unsprayed in initial sampling after disturbance (2022)
# Resilience = change over time in the comparison of sprayed and unsprayed after disturbance (could be final, could be at intermediate timepoints) (change in LRR between 2023 and 2022 or between 2024 and 2022)
# Recovery = compare sprayed and unsprayed in final sampling (2024)

extranatabsdf_soil <- extranatabsdf_soil %>% 
  mutate(LRR.resistance = log(extranatcover_abs_2022 / extranatcover_abs_2022.control),
         
         LRR.recovery = log(extranatcover_abs_2024 / extranatcover_abs_2024.control),
         
         LRR.resilience24 = log(extranatcover_abs_2024 / extranatcover_abs_2024.control) - 
           log(extranatcover_abs_2022 / extranatcover_abs_2022.control) ,
         
         LRR.resilience23 = log(extranatcover_abs_2023 / extranatcover_abs_2023.control) - 
           log(extranatcover_abs_2022 / extranatcover_abs_2022.control) )

# Calculate pre-treatment % cover for focal dominant species:
extranatabsdf_soil <- extranatabsdf_soil %>%
  mutate(Site = as.factor(Site),
         Plot = as.factor(Plot)) %>%
  mutate(Year = as.integer(Year)) %>%
  left_join(brch21, by = c("Site", "Plot", "Year"))


extranatabsdf_soil <- invabsdf_soil %>% select(Site, Plot, Sprayed, invcover_abs_2022) %>%
  mutate(Site = as.factor(Site),
         Plot = as.factor(Plot)) %>%
  right_join(extranatabsdf_soil, by = c("Site", "Plot", "Sprayed")) %>% 
  relocate("invcover_abs_2022", .after = extranatcover_abs_2024)

extranatabsdf_soil$Site <- factor(extranatabsdf_soil$Site, 
                               levels = c("1","2","3","4","5",
                                          "6","7","8","9","10"))

#remove those without native richness & add site-level mean variables for focal dominants:
extranatabsdf_soil2 <- extranatabsdf_soil %>% 
  # filter(pre_natrichness != 0) %>%
  group_by(Site) %>% mutate(slm.brte = mean(BRTE),
                            slm.chju = mean(CHJU), 
                            slm.epbr = mean(EPBR3),
                            slm.br = mean(Bromus),
                            slm.pobu = mean(POBU)) %>% ungroup()

##KEEP
# invabsdf_soil
# extranatabsdf_soil2

### Prepare composition df with our plantings included ####
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
meta1 <- spp.rel.all %>% 
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



## Calculate distances in community composition between time points using ecotraj functions:

## 2021 -> 2022 Distance, representing Compositional Resistance 
#create metadata df
plots1 <- meta %>% 
  filter(Year == 2021 | Year == 2022)

#filter to 2021 and 2022 years from spp.rel.all
scr.1 <- spp.rel.all %>% 
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



#Calculate trajectory lengths across years for each plot ID.
tr_lengths1 <- trajectoryLengths(braymat1,
                                 distmat1$`Site-Plot`,
                                 distmat1$Year)

tr_lengths_df1 <- as.data.frame(tr_lengths1)
tr_lengths_df1$ObsID <- rownames(tr_lengths_df1)
tr_lengths_df1 <- tr_lengths_df1 %>% 
  separate(ObsID, into = c("Site", "Plot", "Sprayed"), sep = "-") %>%
  mutate(Site = as.factor(Site),
         Plot = as.factor(Plot))%>%
  relocate(c("Site", "Plot", "Sprayed"), .before = "S1") 

tr_lengths_df1$S1 <- replace(tr_lengths_df1$S1,tr_lengths_df1$S1==1, 0.99)



## 2021 -> 2024 Distance representing Compositional Recovery
#create metadata df
plots2 <- meta %>% 
  filter(Year == 2021 | Year == 2024)

#filter to 2021 and 2024 from spp.rel.all
scr.2 <- spp.rel.all %>%
  filter(Year == 2021 | Year == 2024)

#bray-curtis dissimilarity
bray2 <- vegdist(scr.2[,-c(1:3)], method = "bray")

#as matrix
braymat2 <- as.matrix(bray2)
#as data frame
braydf2 <- as.data.frame(braymat2)

#combine
distmatrix2 <- cbind(plots2, braydf2)

#group by site-plot combo
distmat2 <- distmatrix2 %>%
  mutate("Site-Plot" = paste(Site, Plot, Sprayed, sep = "-")) %>%
  relocate("Site-Plot", .after = Year)

#Calculate trajectory lengths across years for each plot ID.
tr_lengths2 <- trajectoryLengths(braymat2,
                                 distmat2$`Site-Plot`,
                                 distmat2$Year)

tr_lengths_df2 <- as.data.frame(tr_lengths2)
tr_lengths_df2$ObsID <- rownames(tr_lengths_df2)
tr_lengths_df2 <- tr_lengths_df2 %>% 
  separate(ObsID, into = c("Site", "Plot", "Sprayed"), sep = "-") %>%
  mutate(Site = as.factor(Site),
         Plot = as.factor(Plot))%>%
  relocate(c("Site", "Plot", "Sprayed"), .before = "S1") 



## Create dataframe 

#calculate means distances for control plots for 2021-2022 and 2021-2024:
controlmeanlengths <- tr_lengths_df1 %>%
  select(-Trajectory) %>%
  rename("Y21_Y22" = S1) %>% 
  filter(Sprayed == "No") %>%
  group_by(Site) %>%
  summarise("Y21_Y22_conmean" = mean(Y21_Y22))

controlmeanlengths <- tr_lengths_df2 %>%
  select(-Trajectory) %>%
  rename("Y21_Y24" = S1) %>% 
  filter(Sprayed == "No") %>%
  group_by(Site) %>%
  summarise("Y21_Y24_conmean" = mean(Y21_Y24)) %>%
  left_join(controlmeanlengths)

# Create dataframe with all lengths 
RRRo_df <- tr_lengths_df1 %>%
  select(-Trajectory) %>%
  rename("Y21_Y22" = S1) %>% 
  left_join(tr_lengths_df2) %>%
  select(-Trajectory) %>%
  rename("Y21_Y24" = S1) %>% 
  left_join(controlmeanlengths)

#join with pretreatment & calculate log response ratios based on composition:
RRRo_df <- df_divcov %>% filter(Year == 2021) %>% 
  left_join(RRRo_df) %>% 
  mutate(LRR.resistance = log((1-Y21_Y22) / (1-Y21_Y22_conmean)),
         
         LRR.resilience = log((1-Y21_Y24) / (1-Y21_Y24_conmean)) - 
           log((1-Y21_Y22) / (1-Y21_Y22_conmean)),
         
         LRR.recovery = log((1-Y21_Y24) / (1-Y21_Y24_conmean)))

# Add soil data
RRRo_df <- RRRo_df %>% left_join(soildata %>% mutate(Site = as.factor(Site), Plot = as.factor(Plot)), by = c("Site", "Plot")) 

#Calculate site-level means (mundlak variables):
RRRo_df <- RRRo_df %>% 
  group_by(Site) %>%
  rename("pre_natcover_abs" = natcover_abs,
         "pre_invcover_abs" = invcover_abs,
         "pre_natcover_rel" = natcover_rel,
         "pre_invcover_rel" = invcover_rel,
         "pre_totplantcov" = totplantcov ) %>%
  select(-totaldiversity, -natdiversity, -invdiversity) %>%
  mutate(slmc.all = mean(pre_totplantcov),
         slmc.invr = mean(pre_invcover_rel),
         slmc.natr = mean(pre_natcover_rel),
         slmc.inva = mean(pre_invcover_abs),
         slmc.nata = mean(pre_natcover_abs),
         slmd.all = mean(pre_totaldiversity),
         slmd.nat = mean(pre_natdiversity),
         slmd.inv = mean(pre_invdiversity),
         slmr.all = mean(pre_totalrichness),
         slmr.inv = mean(pre_invrichness),
         slmr.nat = mean(pre_natrichness),
         slm.No3.f21 = mean(NO3.instant_2021Fall),
         slm.Nh4.f21 = mean(NH4.instant_2021Fall),
         slm.N.f21 = mean(N_ug.g.instant_2021Fall),
         slm.N.s22 = mean(N_ug.g.instant_2022Spring),
         slm.No3.s22 = mean(NO3.instant_2022Spring),
         slm.Nh4.s22 = mean(NH4.instant_2022Spring),
         slm.wc.f21 = mean(watercontent_g.g.instant_2021Fall),
         slm.wc.s22 = mean(watercontent_g.g.instant_2022Spring),
         slm.whc.f21 = mean(WHC_g.g.instant_2021Fall),
         slm.whc.s22 = mean(WHC_g.g.instant_2022Spring))

# Add percent cover of dominant species:
RRRo_df <- RRRo_df %>% 
  mutate(Site = as.factor(Site),
         Plot = as.factor(Plot), 
         Year = as.integer(Year)) %>%
  left_join(brch21, by = c("Site", "Plot")) %>%
  select(-Year.x, -Year.y, -Depth, -Size) %>%
  relocate("Season", .after = "Plot")

# Calculate mineralization & Delta N:
RRRo_df <- RRRo_df %>%
  mutate(Fall21_mineralN = N_ug.g.incubated_2021Fall - N_ug.g.instant_2021Fall,
         Spr22_mineralN = N_ug.g.incubated_2022Spring - N_ug.g.instant_2022Spring, 
         DeltaN_21_22 = (N_ug.g.instant_2021Fall - N_ug.g.instant_2022Spring)/N_ug.g.instant_2021Fall,
         DeltaNH4_21_22 = (NH4.instant_2021Fall - NH4.instant_2022Spring)/NH4.instant_2021Fall,
         DeltaNO3_21_22 = (NO3.instant_2021Fall - NO3.instant_2022Spring)/NO3.instant_2021Fall)


RRRo_df$Site <- factor(RRRo_df$Site, 
                      levels = c("1","2","3","4","5",
                                 "6","7","8","9","10"))
RRRo_df$Plot <- factor(RRRo_df$Plot, 
                      levels = c("1","2","3","4","5",
                                 "6","7","8","9","10",
                                 "11","12","13"))
RRRo_df <- RRRo_df %>% relocate(c("LRR.resistance", "LRR.resilience", "LRR.recovery",
                                "Y21_Y22", "Y21_Y24", "Y21_Y22_conmean", "Y21_Y24_conmean"),
                              .after = Sprayed)

RRRo_df <- RRRo_df %>% group_by(Site) %>% mutate(slm.brte = mean(BRTE),
                                               slm.chju = mean(CHJU), 
                                               slm.epbr = mean(EPBR3),
                                               slm.br = mean(Bromus),
                                               slm.pobu = mean(POBU)) %>% ungroup()

#####
#
rm(bray1, bray2, braydf1, braydf2, braymat1, braymat2, brch21, controlmeans, 
   df_divcov, fun.comp.rel, invabsdf, natabsdf_soil2,
   controlmeanlengths, distmat1, distmat2, distmatrix1, distmatrix2,
   mundlaks, mypoints, natabsdf, natabsdf_soil,extranat_cover_df,
   extranatabsdf,extranatabsdf_soil, plantattribs, pretreat, prism, 
   soildata, soildata2, spp.comp, spp.rel.all, spp.comp.rel, spp.comp2, spp.rel.inv,
   meta, meta1, plots1, plots2, scr.1, scr.2, tr_lengths_df1, tr_lengths_df2, 
   tr_lengths1, tr_lengths2, treat1)
#




### MODELS - Invasive Cover ####
inv_resistmodr <- readRDS("data/outputs/inv_resistmodr.RDS")
inv_resilience24_modr <- readRDS("data/outputs/inv_resilience24_modr.RDS")
inv_recovermodr <- readRDS("data/outputs/inv_recovermodr.RDS")
#

### MODELS - Native Cover #####
# RESISTANCE based on native cover
nat_resistmodr_extracover <- brm(LRR.resistance ~ 
                                 scale(pre_invrichness) +
                                 scale(pre_natrichness) +
                                 scale(CHJU) + 
                                 scale(BRTE) +
                                 scale(N_ug.g.instant_2021Fall) +
                                 scale(slm.brte) +
                                 scale(slm.chju) +
                                 scale(slmr.inv) +
                                 scale(slmr.nat) +
                                 scale(Heatload) +
                                 scale(slm.N.21fall) +
                                 (1 | Site), 
                               data = subset(extranatabsdf_soil2,
                                             Sprayed=="Yes"), 
                               warmup = 1000, iter = 2000, chains = 4, seed = 123,
                               control = list(adapt_delta = 0.999, max_treedepth = 12))

#RESILIENCE (2024)  based on native cover
nat_resilience24_modr_extracover <- brm(LRR.resilience24 ~ 
                                        scale(pre_invrichness) +
                                        scale(pre_natrichness) +
                                        scale(CHJU) +
                                        scale(BRTE) +
                                        scale(N_ug.g.instant_2022Spring) +
                                        scale(slm.brte) +
                                        scale(slm.chju) +
                                        scale(LRR.resistance) +
                                        scale(slmr.inv) +
                                        scale(slmr.nat) +
                                        scale(Heatload) +
                                        scale(slm.N.22sp) +
                                        (1 | Site), 
                                      data = subset(extranatabsdf_soil2,
                                                    Sprayed=="Yes"),
                                      warmup = 1000, iter = 2000, chains = 4, seed = 123, 
                                      control = list(adapt_delta = 0.999, 
                                                     max_treedepth = 12))

#RECOVERY based on native cover
nat_recovermodr_extracover <- brm(LRR.recovery ~ 
                                  scale(pre_invrichness) +
                                  scale(pre_natrichness) +
                                  scale(CHJU) +
                                  scale(BRTE) +
                                  scale(N_ug.g.instant_2022Spring) +
                                  scale(slm.brte) +
                                  scale(slm.chju) +
                                  scale(slmr.inv) +
                                  scale(slmr.nat) +
                                  scale(Heatload) +
                                  scale(slm.N.22sp) +
                                  (1 | Site), 
                                data = subset(extranatabsdf_soil2,
                                              Sprayed=="Yes"),
                                warmup = 1000, iter = 2000, chains = 4, seed = 123,
                                control = list(adapt_delta = 0.999, 
                                               max_treedepth = 12))
##

### MODELS - Composition #####
#RESISTANCE based on composition
resist_modr_extracover <- brm(LRR.resistance ~ 
                     scale(pre_invrichness) +
                     scale(pre_natrichness) +
                     scale(BRTE) +
                     scale(CHJU) +
                     scale(N_ug.g.instant_2021Fall) +
                     scale(slm.brte) +
                     scale(slm.chju) +
                     scale(Heatload) +
                     scale(slmr.nat) +
                     scale(slmr.inv) +
                     scale(slm.N.f21) +
                     (1 | Site),
                   data = subset(RRRo_df,
                                 Sprayed=="Yes"),
                   warmup = 1000, iter = 2000, chains = 4, seed = 123,
                   control = list(adapt_delta = 0.999))


#RESILIENCE based on composition
resil_modr_extracover <- brm(LRR.resilience ~
                    scale(pre_invrichness) +
                    scale(pre_natrichness) +
                    scale(BRTE) +
                    scale(CHJU) +
                    scale(N_ug.g.instant_2022Spring) +
                    scale(slm.brte) +
                    scale(slm.chju) +
                    scale(LRR.resistance) +
                    scale(Heatload) +
                    scale(slmr.nat) +
                    scale(slmr.inv) +
                    scale(slm.N.s22) +
                    (1 | Site),
                  data = subset(RRRo_df,
                                Sprayed=="Yes"),
                  warmup = 1000, iter = 2000, chains = 4, seed = 123,
                  control = list(adapt_delta = 0.999, max_treedepth = 12))


#RECOVERY based on composition
recov_modr_extracover <- brm(LRR.recovery ~ 
                    scale(pre_invrichness) +
                    scale(pre_natrichness) +
                    scale(CHJU) +
                    scale(BRTE) +
                    scale(N_ug.g.instant_2022Spring) +
                    scale(Heatload) +
                    scale(slm.brte) +
                    scale(slm.chju) +
                    scale(slmr.nat) +
                    scale(slmr.inv) +
                    scale(slm.N.s22) +
                    (1 | Site), 
                  data = subset(RRRo_df,
                                Sprayed=="Yes"),
                  warmup = 1000, iter = 2000, chains = 4, seed = 123,
                  control = list(adapt_delta = 0.999, max_treedepth = 12))

#

