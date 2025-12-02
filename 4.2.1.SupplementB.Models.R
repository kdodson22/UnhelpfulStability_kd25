## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 4.2.1. Supplement B Models:

## Purpose:  This script fits an alternate model specification and Supplementary Figure 4: Estimated effect of diversity, dominance, and soil nitrogen and water content on system stability. #####


## Author: K. Dodson 
## Date: Updated 12/1/2025



library(tidyverse)
library(here)
library(brms)

#data
# source(here("1.0.Data.Set-up.R"))
# source(here("1.1.RRR.Cover.R"))
# source(here("1.2.RRR.Composition.R"))


### MODEL TESTING/COMPARISON FOR INCLUSION OF PRE- AND POST-TREATMENT WATER CONTENT ###
## MODELS - Invasive Cover #####
#RESISTANCE
inv_resistmodr_wat <- brm(LRR.resistance ~ 
                            scale(pre_invrichness) +
                            scale(pre_natrichness) +
                            scale(CHJU) + 
                            scale(BRTE) +
                            scale(N_ug.g.instant_2021Fall) +
                            scale(watercontent_g.g.instant_2021Fall) +  
                            scale(slmr.inv) +
                            scale(slmr.nat) +
                            scale(Heatload) +
                            scale(slm.N.21fall) +
                            scale(slm.wc.f21) +
                            (1 | Site), 
                          data = subset(invabsdf_soil,
                                        Sprayed=="Yes"), 
                          warmup = 1000, iter = 2000, chains = 4, seed = 123,
                          control = list(adapt_delta = 0.999, max_treedepth = 12))


#RESILIENCE 2024 
inv_resilience24_modr_wat <- brm(LRR.resilience24 ~ 
                                   scale(pre_invrichness) +
                                   scale(pre_natrichness) +
                                   scale(CHJU) +
                                   scale(BRTE) +
                                   scale(N_ug.g.instant_2022Spring) +
                                   scale(watercontent_g.g.instant_2022Spring) +
                                   scale(LRR.resistance) +
                                   scale(slmr.inv) +
                                   scale(slmr.nat) +
                                   scale(Heatload) +
                                   scale(slm.N.22sp) +
                                   scale(slm.wc.s22) +
                                   (1 | Site), 
                                 data = subset(invabsdf_soil,
                                               Sprayed=="Yes"),
                                 warmup = 1000, iter = 2000, chains = 4, seed = 123,
                                 control = list(adapt_delta = 0.999))


#RECOVERY
inv_recovermodr_wat <- brm(LRR.recovery ~ 
                             scale(pre_invrichness) +
                             scale(pre_natrichness) +
                             scale(CHJU) +
                             scale(BRTE) +
                             scale(N_ug.g.instant_2022Spring) +
                             scale(watercontent_g.g.instant_2022Spring) +
                             scale(slmr.inv) +
                             scale(slmr.nat) +
                             scale(Heatload) +
                             scale(slm.N.22sp) +
                             scale(slm.wc.s22) +
                             (1 | Site), 
                           data = subset(invabsdf_soil,
                                         Sprayed=="Yes"),
                           warmup = 1000, iter = 2000, chains = 4, seed = 123,
                           control = list(adapt_delta = 0.999))


## MODELS - Native Cover #####
# RESISTANCE
nat_resistmodr_wat <- brm(LRR.resistance ~ 
                            scale(pre_invrichness) +
                            scale(pre_natrichness) +
                            scale(CHJU) + 
                            scale(BRTE) +
                            scale(N_ug.g.instant_2021Fall) +
                            scale(watercontent_g.g.instant_2021Fall) +  
                            scale(slmr.inv) +
                            scale(slmr.nat) +
                            scale(Heatload) +
                            scale(slm.N.21fall) +
                            scale(slm.wc.f21) +
                            (1 | Site), 
                          data = subset(natabsdf_soil2,
                                        Sprayed=="Yes"), 
                          warmup = 1000, iter = 2000, chains = 4, seed = 123,
                          control = list(adapt_delta = 0.999, max_treedepth = 12))


#RESILIENCE 2024 
nat_resilience24_modr_wat <- brm(LRR.resilience24 ~ 
                                   scale(pre_invrichness) +
                                   scale(pre_natrichness) +
                                   scale(CHJU) +
                                   scale(BRTE) +
                                   scale(N_ug.g.instant_2022Spring) +
                                   scale(watercontent_g.g.instant_2022Spring) +
                                   scale(LRR.resistance) +
                                   scale(slmr.inv) +
                                   scale(slmr.nat) +
                                   scale(Heatload) +
                                   scale(slm.N.22sp) +
                                   scale(slm.wc.s22) +
                                   (1 | Site), 
                                 data = subset(natabsdf_soil2,
                                               Sprayed=="Yes"),
                                 warmup = 1000, iter = 2000, chains = 4, seed = 123,
                                 control = list(adapt_delta = 0.999, 
                                                max_treedepth = 12))

#RECOVERY
nat_recovermodr_wat <- brm(LRR.recovery ~ 
                             scale(pre_invrichness) +
                             scale(pre_natrichness) +
                             scale(CHJU) +
                             scale(BRTE) +
                             scale(N_ug.g.instant_2022Spring) +
                             scale(watercontent_g.g.instant_2022Spring) +
                             scale(slmr.inv) +
                             scale(slmr.nat) +
                             scale(Heatload) +
                             scale(slm.N.22sp) +
                             scale(slm.wc.s22) +
                             (1 | Site), 
                           data = subset(natabsdf_soil2,
                                         Sprayed=="Yes"),
                           warmup = 1000, iter = 2000, chains = 4, seed = 123,
                           control = list(adapt_delta = 0.999, 
                                          max_treedepth = 12))

## MODELS - Composition #####
#RESISTANCE
resist_modr_wat <- brm(LRR.resistance ~ 
                         scale(pre_invrichness) +
                         scale(pre_natrichness) +
                         scale(BRTE) +
                         scale(CHJU) +
                         scale(N_ug.g.instant_2021Fall) +
                         scale(watercontent_g.g.instant_2021Fall) + 
                         scale(Heatload) +
                         scale(slmr.nat) +
                         scale(slmr.inv) +
                         scale(slm.N.f21) +
                         scale(slm.wc.f21) +
                         (1 | Site),
                       data = subset(RRR_df,
                                     Sprayed=="Yes"),
                       warmup = 1000, iter = 2000, chains = 4, seed = 123,
                       control = list(adapt_delta = 0.999,
                                      max_treedepth = 12))

#RESILIENCE
resil_modr_wat <- brm(LRR.resilience ~
                        scale(pre_invrichness) +
                        scale(pre_natrichness) +
                        scale(BRTE) +
                        scale(CHJU) +
                        scale(N_ug.g.instant_2022Spring) +
                        scale(watercontent_g.g.instant_2022Spring) +
                        scale(LRR.resistance) +
                        scale(Heatload) +
                        scale(slmr.nat) +
                        scale(slmr.inv) +
                        scale(slmc.all) +
                        scale(slm.N.s22) +
                        scale(slm.wc.s22) +
                        (1 | Site),
                      data = subset(RRR_df,
                                    Sprayed=="Yes"),
                      warmup = 1000, iter = 2000, chains = 4, seed = 123,
                      control = list(adapt_delta = 0.999, 
                                     max_treedepth = 12))

# RECOVERY
recov_modr_wat <- brm(LRR.recovery ~ 
                        scale(pre_invrichness) +
                        scale(pre_natrichness) +
                        scale(CHJU) +
                        scale(BRTE) +
                        scale(N_ug.g.instant_2022Spring) +
                        scale(watercontent_g.g.instant_2022Spring) +
                        scale(Heatload) +
                        scale(slmr.nat) +
                        scale(slmr.inv) +
                        scale(slm.N.s22) +
                        scale(slm.wc.s22) +
                        (1 | Site), 
                      data = subset(RRR_df,
                                    Sprayed=="Yes"),
                      warmup = 1000, iter = 2000, chains = 4, seed = 123,
                      control = list(adapt_delta = 0.999, max_treedepth = 12))

