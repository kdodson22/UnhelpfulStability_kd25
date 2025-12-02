## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 1.1 RRR Cover

## Purpose: This script calculates several summary variables needed to analyze trends in the resistance, resilience, and recovery of disturbed plant communities, based on plant cover
## Author: K. Dodson 
## Date: Updated 12/1/2025

library(tidyverse)
library(vegan)
library(here)


# Format site and plot variables as factors:
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

####### DATA CLEANING - INVASIVE ########

### ABSOLUTE COVER for INVASIVE SPECIES ###
#absolute cover data frame
invabsdf <- df_divcov %>%
  select(Site, Plot, Year, invcover_abs) %>%
  pivot_wider(names_from = "Year", 
              values_from = c("invcover_abs"),
              names_prefix = "invcover_abs_") %>%
  left_join(pretreat, by = c("Site", "Plot"))

# some plots have 0% cover for plant categories that are present-- add the equivalent of ~1/2 of a 'hit'
invabsdf[,3:6] <- replace(invabsdf[,3:6], invabsdf[,3:6] < 0.019, 0.010)

# Add additional soil variables to dataframe:
soildata$Site <- factor(soildata$Site, 
                        levels = c("1","2","3","4","5",
                                   "6","7","8","9","10"))
soildata$Plot <- factor(soildata$Plot, 
                        levels = c("1","2","3","4","5",
                                   "6","7","8","9","10",
                                   "11","12","13"))

invabsdf_soil <- invabsdf  %>% 
  left_join(soildata, by = c("Site","Plot")) %>%
  # Calculate change in N variables between 2021-2022:
  mutate(Fall21_mineralN = N_ug.g.incubated_2021Fall - N_ug.g.instant_2021Fall,
         Spr22_mineralN = N_ug.g.incubated_2022Spring - N_ug.g.instant_2022Spring, 
         DeltaN_21_22 = (N_ug.g.instant_2021Fall - N_ug.g.instant_2022Spring)/
                                                    N_ug.g.instant_2021Fall,
         DeltaNH4_21_22 = (NH4.instant_2021Fall - NH4.instant_2022Spring)/
                                                    NH4.instant_2021Fall,
         DeltaNO3_21_22 = (NO3.instant_2021Fall - NO3.instant_2022Spring)) 


#calculate site-mean variables (Mundlak device) for statistical controls:
mundlaks <- invabsdf_soil %>%
  group_by(Site) %>%
  filter(Sprayed=="Yes") %>%
  summarise(slmc.inv.cov = mean(pre_invcover_abs),
            slmc.inva.f21 = mean(invcover_abs_2021),
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

## Add to data frame:
invabsdf_soil <- invabsdf_soil %>% left_join(mundlaks)



## Calulate RRR Metrics: ###

#calculate means of control plots for each year
controlmeans <- invabsdf_soil %>%
  group_by(Site) %>%
  filter(Sprayed=="No") %>%
  summarise(invcover_abs_2022.control = mean(invcover_abs_2022),
            invcover_abs_2023.control = mean(invcover_abs_2023),
            invcover_abs_2024.control = mean(invcover_abs_2024)) %>%
  select(Site, 
         invcover_abs_2022.control, 
         invcover_abs_2023.control, 
         invcover_abs_2024.control)

#combine
invabsdf_soil <- invabsdf_soil %>% left_join(controlmeans)

#calculate log response ratio style RRR metrics:
# Resistance = compare sprayed and unsprayed in initial sampling after disturbance (2022)
# Resilience = change over time in the comparison of sprayed and unsprayed after disturbance (could be final, could be at intermediate time points) (change in LRR between 2023 and 2022 or between 2024 and 2022)
# Recovery = compare sprayed and unsprayed in final sampling (2024)

invabsdf_soil <- invabsdf_soil %>% 
  mutate(LRR.resistance = log(invcover_abs_2022 / invcover_abs_2022.control),
         
         LRR.recovery = log(invcover_abs_2024 / invcover_abs_2024.control),
         
         LRR.resilience24 = log(invcover_abs_2024/invcover_abs_2024.control) - 
           log(invcover_abs_2022/invcover_abs_2022.control) ,
         
         LRR.resilience23 = log(invcover_abs_2023/invcover_abs_2023.control) - 
           log(invcover_abs_2022/invcover_abs_2022.control) )

# Calculate pre-treatment % cover for focal dominant species:
invabsdf_soil <- invabsdf_soil %>% 
  mutate(Year = as.integer(Year)) %>%
  left_join(brch21, by = c("Site", "Plot", "Year"))

invabsdf_soil <- spp.rel.inv %>% filter(Year == 2021) %>%
  mutate(Site = as.factor(Site),
         Plot = as.factor(Plot), 
         Year = as.integer(Year)) %>%
  select(Site, Plot, Year, BRAR5, BRBR5, CHJU, BRTE, POBU) %>%
  rename("BRTE_invrel" = BRTE,
         "BRAR5_invrel" = BRAR5,
         "BRBR5_invrel" = BRBR5,
         "CHJU_invrel" = CHJU, 
         "POBU_inverel" = POBU) %>%
  left_join(invabsdf_soil, by = c("Site", "Plot", "Year"))


invabsdf_soil$Site <- factor(invabsdf_soil$Site, 
                             levels = c("1","2","3","4","5",
                                        "6","7","8","9","10"))


## Calculate site-level means (mundlak device) for these variables:
invabsdf_soil2 <- invabsdf_soil %>% group_by(Site) %>% mutate(slm.brte = mean(BRTE),
                                                              slm.chju = mean(CHJU), 
                                                              slm.epbr = mean(EPBR3),
                                                              slm.br = mean(Bromus),
                                                              slm.pobu = mean(POBU)) %>% ungroup()







####### DATA CLEANING - NATIVE ########

## repeats same steps as above, but for native species only:

### ABSOLUTE ###
#absolute cover dataframe
natabsdf <- df_divcov %>%
  select(Site, Plot, Year, natcover_abs) %>%
  pivot_wider(names_from = "Year", 
              values_from = c("natcover_abs"),
              names_prefix = "natcover_abs_") %>%
  left_join(pretreat, by = c("Site", "Plot"))

# some plots have 0% cover for plant categories that are present -- add the equivalent of ~1/2 of a 'hit'
natabsdf[,3:6] <- replace(natabsdf[,3:6], natabsdf[,3:6] < 0.019, 0.010)

#add more soil variables:
natabsdf_soil <- natabsdf  %>% 
  left_join(soildata, by = c("Site","Plot")) %>%
  ## Calculate changes in soil N:
  mutate(Fall21_mineralN = N_ug.g.incubated_2021Fall - N_ug.g.instant_2021Fall,
         Spr22_mineralN = N_ug.g.incubated_2022Spring - N_ug.g.instant_2022Spring, 
         DeltaN_21_22 = (N_ug.g.instant_2021Fall - N_ug.g.instant_2022Spring)/N_ug.g.instant_2021Fall,
         DeltaNH4_21_22 = (NH4.instant_2021Fall - NH4.instant_2022Spring)/NH4.instant_2021Fall,
         DeltaNO3_21_22 = (NO3.instant_2021Fall - NO3.instant_2022Spring)) 


#calculate mundlak/site-level means for variables 
mundlaks <- natabsdf_soil %>%
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
natabsdf_soil <- natabsdf_soil %>% left_join(mundlaks)


## Calculate RRR metrics:
#calculate means of control plots for each year
controlmeans <- natabsdf_soil %>%
  group_by(Site) %>%
  filter(Sprayed=="No") %>%
  summarise(natcover_abs_2022.control = mean(natcover_abs_2022),
            natcover_abs_2023.control = mean(natcover_abs_2023),
            natcover_abs_2024.control = mean(natcover_abs_2024)) %>%
  select(Site, natcover_abs_2022.control, natcover_abs_2023.control, natcover_abs_2024.control)

#combine
natabsdf_soil <- natabsdf_soil %>% left_join(controlmeans)

#calculate log response ratio style RRR metrics:
# Resistance = compare sprayed and unsprayed in initial sampling after disturbance (2022)
# Resilience = change over time in the comparison of sprayed and unsprayed after disturbance (could be final, could be at intermediate timepoints) (change in LRR between 2023 and 2022 or between 2024 and 2022)
# Recovery = compare sprayed and unsprayed in final sampling (2024)

natabsdf_soil <- natabsdf_soil %>% 
  mutate(LRR.resistance = log(natcover_abs_2022 / natcover_abs_2022.control),
         
         LRR.recovery = log(natcover_abs_2024 / natcover_abs_2024.control),
         
         LRR.resilience24 = log(natcover_abs_2024 / natcover_abs_2024.control) - 
           log(natcover_abs_2022 / natcover_abs_2022.control) ,
         
         LRR.resilience23 = log(natcover_abs_2023 / natcover_abs_2023.control) - 
           log(natcover_abs_2022 / natcover_abs_2022.control) )

# Calculate pre-treatment % cover for focal dominant species:
natabsdf_soil <- natabsdf_soil %>% 
  mutate(Year = as.integer(Year)) %>%
  left_join(brch21, by = c("Site", "Plot", "Year"))


natabsdf_soil <- invabsdf_soil %>% select(Site, Plot, Sprayed, invcover_abs_2022) %>%
  mutate(Site = as.factor(Site),
         Plot = as.factor(Plot)) %>%
  left_join(natabsdf_soil, by = c("Site", "Plot", "Sprayed")) %>% 
  relocate("invcover_abs_2022", .after = natcover_abs_2024)

natabsdf_soil$Site <- factor(natabsdf_soil$Site, 
                             levels = c("1","2","3","4","5",
                                        "6","7","8","9","10"))

#remove those without native richness & add site-level mean variables for focal dominants:
natabsdf_soil2 <- natabsdf_soil %>% filter(pre_natrichness != 0) %>%
  group_by(Site) %>% mutate(slm.brte = mean(BRTE),
                            slm.chju = mean(CHJU), 
                            slm.epbr = mean(EPBR3),
                            slm.br = mean(Bromus),
                            slm.pobu = mean(POBU)) %>% ungroup()

