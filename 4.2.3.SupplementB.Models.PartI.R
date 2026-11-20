## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 4.2.1. Supplement B Models for sensitivity checks:

## Purpose: This script presents robustness checks for estimates of species richness and dominant species (Adjustment Set A), while controlling for additional possible confounders:
## Pre-treatment soil nitrogen, water content, and other dominant species #####


## Author: ASW 
## Date: Created 11/2026

library(tidyverse)
library(here)
library(brms)
library(bayesplot)

#data
source(here("1.0.Data.Set-up.R"))
source(here("1.1.RRR.Cover.R"))
source(here("1.2.RRR.Composition.R"))

## MODELS - Invasive Cover #####
#RESISTANCE based on invasive cover
inv_resistmodr_wat <- brm(LRR.resistance ~ 
                        scale(pre_invrichness) +
                        scale(pre_natrichness) +
                        scale(CHJU) + 
                        scale(BRTE) +
                        scale(slm.brte) + 
                        scale(slm.chju) +
                        scale(slmr.inv) +
                        scale(slmr.nat) +
                        scale(Heatload) +
                        scale(N_ug.g.instant_2021Fall) +
                        scale(watercontent_g.g.instant_2021Fall) +  
                        scale(slm.wc.f21) +
                        scale(EPBR3) + 
                        scale(BRAR5)+
                        (1 | Site), 
                      data = subset(invabsdf_soil,
                                    Sprayed=="Yes"), 
                      warmup = 1000, iter = 2000, chains = 4, seed = 123,
                      control = list(adapt_delta = 0.999, max_treedepth = 12))

mcmc_plot(inv_resistmodr_wat)

#RESILIENCE (2024)  based on invasive cover
inv_resilience24_modr_wat <- brm(LRR.resilience24 ~ 
                               scale(pre_invrichness) +
                               scale(pre_natrichness) +
                               scale(CHJU) +
                               scale(BRTE) +
                               scale(slm.brte) +
                               scale(slm.chju) +
                               scale(slmr.inv) +
                               scale(slmr.nat) +
                               scale(Heatload) +
                               scale(LRR.resistance) +
                               scale(N_ug.g.instant_2021Fall) +
                               scale(watercontent_g.g.instant_2021Fall) +  
                               scale(slm.wc.f21) +
                               scale(EPBR3) + 
                               scale(BRAR5)+
                               (1 | Site), 
                             data = subset(invabsdf_soil,
                                           Sprayed=="Yes"),
                             warmup = 1000, iter = 2000, chains = 4, seed = 123,
                             control = list(adapt_delta = 0.999))


mcmc_plot(inv_resilience24_modr_wat)

#RECOVERY based on invasive cover
inv_recovermodr_wat <- brm(LRR.recovery ~ 
                         scale(pre_invrichness) +
                         scale(pre_natrichness) +
                         scale(CHJU) +
                         scale(BRTE) +
                         scale(slm.brte) +
                         scale(slm.chju) +
                         scale(slmr.inv) +
                         scale(slmr.nat) +
                         scale(Heatload) +
                         scale(N_ug.g.instant_2021Fall) +
                         scale(watercontent_g.g.instant_2021Fall) +  
                         scale(slm.wc.f21) +
                         scale(EPBR3) + 
                         scale(BRAR5)+
                         (1 | Site), 
                       data = subset(invabsdf_soil,
                                     Sprayed=="Yes"),
                       warmup = 1000, iter = 2000, chains = 4, seed = 123,
                       control = list(adapt_delta = 0.999))

mcmc_plot(inv_recovermodr_wat)

## MODELS - Native Cover #####
# RESISTANCE based on native cover
nat_resistmodr_wat <- brm(LRR.resistance ~ 
                        scale(pre_invrichness) +
                        scale(pre_natrichness) +
                        scale(CHJU) + 
                        scale(BRTE) +
                        scale(slm.brte) + 
                        scale(slm.chju) +
                        scale(slmr.inv) +
                        scale(slmr.nat) +
                        scale(Heatload) +
                          scale(N_ug.g.instant_2021Fall) +
                          scale(watercontent_g.g.instant_2021Fall) +  
                          scale(slm.wc.f21) +
                          scale(EPBR3) + 
                          scale(BRAR5)+
                        (1 | Site), 
                      data = subset(natabsdf_soil2,
                                    Sprayed=="Yes"), 
                      warmup = 1000, iter = 2000, chains = 4, seed = 123,
                      control = list(adapt_delta = 0.999, max_treedepth = 12))

mcmc_plot(nat_resistmodr_wat)

#RESILIENCE (2024)  based on native cover
nat_resilience24_modr_wat <- brm(LRR.resilience24 ~ 
                               scale(pre_invrichness) +
                               scale(pre_natrichness) +
                               scale(CHJU) +
                               scale(BRTE) +
                               scale(slm.brte) +
                               scale(slm.chju) +
                               scale(slmr.inv) +
                               scale(slmr.nat) +
                               scale(Heatload) +
                               scale(LRR.resistance) +
                                 scale(N_ug.g.instant_2021Fall) +
                                 scale(watercontent_g.g.instant_2021Fall) +  
                                 scale(slm.wc.f21) +
                                 scale(EPBR3) + 
                                 scale(BRAR5)+
                               (1 | Site), 
                             data = subset(natabsdf_soil2,
                                           Sprayed=="Yes"),
                             warmup = 1000, iter = 2000, chains = 4, seed = 123, 
                             control = list(adapt_delta = 0.999, 
                                            max_treedepth = 12))

mcmc_plot(nat_resilience24_modr_wat)

#RECOVERY based on native cover
nat_recovermodr_wat <- brm(LRR.recovery ~ 
                         scale(pre_invrichness) +
                         scale(pre_natrichness) +
                         scale(CHJU) +
                         scale(BRTE) +
                         scale(slm.brte) +
                         scale(slm.chju) +
                         scale(slmr.inv) +
                         scale(slmr.nat) +
                         scale(Heatload) +
                           scale(N_ug.g.instant_2021Fall) +
                           scale(watercontent_g.g.instant_2021Fall) +  
                           scale(slm.wc.f21) +
                           scale(EPBR3) + 
                           scale(BRAR5)+
                         (1 | Site), 
                       data = subset(natabsdf_soil2,
                                     Sprayed=="Yes"),
                       warmup = 1000, iter = 2000, chains = 4, seed = 123,
                       control = list(adapt_delta = 0.999, 
                                      max_treedepth = 13))

mcmc_plot(nat_recovermodr_wat)

#
## MODELS - Composition #####
#RESISTANCE based on composition
resist_modr_wat <- brm(LRR.resistance ~ 
                     scale(pre_invrichness) +
                     scale(pre_natrichness) +
                     scale(CHJU) + 
                     scale(BRTE) +
                     scale(slm.brte) + 
                     scale(slm.chju) +
                     scale(slmr.inv) +
                     scale(slmr.nat) +
                     scale(Heatload) +
                    scale(N_ug.g.instant_2021Fall) +
                    scale(watercontent_g.g.instant_2021Fall) +  
                    scale(slm.wc.f21) +
                    scale(EPBR3) + 
                    scale(BRAR5)+
                     (1 | Site),
                   data = subset(RRR_df,
                                 Sprayed=="Yes"),
                   warmup = 1000, iter = 2000, chains = 4, seed = 123,
                   control = list(adapt_delta = 0.999))

mcmc_plot(resist_modr_wat)

#RESILIENCE based on composition
resil_modr_wat <- brm(LRR.resilience ~
                    scale(pre_invrichness) +
                    scale(pre_natrichness) +
                    scale(CHJU) +
                    scale(BRTE) +
                    scale(slm.brte) +
                    scale(slm.chju) +
                    scale(slmr.inv) +
                    scale(slmr.nat) +
                    scale(Heatload) +
                    scale(LRR.resistance) +
                      scale(N_ug.g.instant_2021Fall) +
                      scale(watercontent_g.g.instant_2021Fall) +  
                      scale(slm.wc.f21) +
                      scale(EPBR3) + 
                      scale(BRAR5)+
                    (1 | Site),
                  data = subset(RRR_df,
                                Sprayed=="Yes"),
                  warmup = 1000, iter = 2000, chains = 4, seed = 123,
                  control = list(adapt_delta = 0.999, max_treedepth = 12))

mcmc_plot(resil_modr_wat)

#RECOVERY based on composition
recov_modr_wat <- brm(LRR.recovery ~ 
                    scale(pre_invrichness) +
                    scale(pre_natrichness) +
                    scale(CHJU) +
                    scale(BRTE) +
                    scale(slm.brte) +
                    scale(slm.chju) +
                    scale(slmr.inv) +
                    scale(slmr.nat) +
                    scale(Heatload) +
                    scale(N_ug.g.instant_2021Fall) +
                    scale(watercontent_g.g.instant_2021Fall) +  
                    scale(slm.wc.f21) +
                    scale(EPBR3) + 
                    scale(BRAR5)+
                    (1 | Site), 
                  data = subset(RRR_df,
                                Sprayed=="Yes"),
                  warmup = 1000, iter = 2000, chains = 4, seed = 123,
                  control = list(adapt_delta = 0.999, max_treedepth = 12))

mcmc_plot(recov_modr_wat)

## Save ####

model_folder <- "data/outputs"
models <- ls(pattern = "modr_wat$")
for(nm in models) {
  saveRDS(get(nm), file = file.path(model_folder, paste0(nm, ".RDS"))
  )
}


