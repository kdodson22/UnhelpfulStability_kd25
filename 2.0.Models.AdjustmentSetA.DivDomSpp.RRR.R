## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 2.0a) Models for RRR using adjustment set A to isolate species richness and dominant species effects

## Purpose: This script fits Bayesian glmms evaluating the effects of diversity and dominant species on the stability of disturbed plant communities. Variable selection and directed acyclic graphs are described in the text.

## Author: K. Dodson 
## Date: Updated 11/8/2026

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
inv_resistmodr <- brm(LRR.resistance ~ 
                        scale(pre_invrichness) +
                        scale(pre_natrichness) +
                        scale(CHJU) + 
                        scale(BRTE) +
                        scale(slm.brte) + 
                        scale(slm.chju) +
                        scale(slmr.inv) +
                        scale(slmr.nat) +
                        scale(Heatload) +
                        #scale(N_ug.g.instant_2021Fall) +
                        (1 | Site), 
                      data = subset(invabsdf_soil,
                                    Sprayed=="Yes"), 
                      warmup = 1000, iter = 2000, chains = 4, seed = 123,
                      control = list(adapt_delta = 0.999, max_treedepth = 12))

mcmc_plot(inv_resistmodr)

#RESILIENCE (2024)  based on invasive cover
inv_resilience24_modr <- brm(LRR.resilience24 ~ 
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
                               #scale(N_ug.g.instant_2021Fall) +
                               #scale(WHC) +
                               (1 | Site), 
                             data = subset(invabsdf_soil,
                                           Sprayed=="Yes"),
                             warmup = 1000, iter = 2000, chains = 4, seed = 123,
                             control = list(adapt_delta = 0.999))


mcmc_plot(inv_resilience24_modr)

#RECOVERY based on invasive cover
inv_recovermodr <- brm(LRR.recovery ~ 
                         scale(pre_invrichness) +
                         scale(pre_natrichness) +
                         scale(CHJU) +
                         scale(BRTE) +
                         scale(slm.brte) +
                         scale(slm.chju) +
                         scale(slmr.inv) +
                         scale(slmr.nat) +
                         scale(Heatload) +
                         (1 | Site), 
                       data = subset(invabsdf_soil,
                                     Sprayed=="Yes"),
                       warmup = 1000, iter = 2000, chains = 4, seed = 123,
                       control = list(adapt_delta = 0.999))

mcmc_plot(inv_recovermodr)

## MODELS - Native Cover #####
# RESISTANCE based on native cover
nat_resistmodr <- brm(LRR.resistance ~ 
                        scale(pre_invrichness) +
                        scale(pre_natrichness) +
                        scale(CHJU) + 
                        scale(BRTE) +
                        scale(slm.brte) + 
                        scale(slm.chju) +
                        scale(slmr.inv) +
                        scale(slmr.nat) +
                        scale(Heatload) +
                        (1 | Site), 
                      data = subset(natabsdf_soil2,
                                    Sprayed=="Yes"), 
                      warmup = 1000, iter = 2000, chains = 4, seed = 123,
                      control = list(adapt_delta = 0.999, max_treedepth = 12))

mcmc_plot(nat_resistmodr)

#RESILIENCE (2024)  based on native cover
nat_resilience24_modr <- brm(LRR.resilience24 ~ 
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
                               #scale(N_ug.g.instant_2021Fall) +
                               #scale(WHC) +
                               (1 | Site), 
                             data = subset(natabsdf_soil2,
                                           Sprayed=="Yes"),
                             warmup = 1000, iter = 2000, chains = 4, seed = 123, 
                             control = list(adapt_delta = 0.999, 
                                            max_treedepth = 12))

mcmc_plot(nat_resilience24_modr)

#RECOVERY based on native cover
nat_recovermodr <- brm(LRR.recovery ~ 
                         scale(pre_invrichness) +
                         scale(pre_natrichness) +
                         scale(CHJU) +
                         scale(BRTE) +
                         scale(slm.brte) +
                         scale(slm.chju) +
                         scale(slmr.inv) +
                         scale(slmr.nat) +
                         scale(Heatload) +
                         (1 | Site), 
                       data = subset(natabsdf_soil2,
                                     Sprayed=="Yes"),
                       warmup = 1000, iter = 2000, chains = 4, seed = 123,
                       control = list(adapt_delta = 0.999, 
                                      max_treedepth = 13))

mcmc_plot(nat_recovermodr)

#
## MODELS - Composition #####
#RESISTANCE based on composition
resist_modr <- brm(LRR.resistance ~ 
                     scale(pre_invrichness) +
                     scale(pre_natrichness) +
                     scale(CHJU) + 
                     scale(BRTE) +
                     scale(slm.brte) + 
                     scale(slm.chju) +
                     scale(slmr.inv) +
                     scale(slmr.nat) +
                     scale(Heatload) +
                     (1 | Site),
                   data = subset(RRR_df,
                                 Sprayed=="Yes"),
                   warmup = 1000, iter = 2000, chains = 4, seed = 123,
                   control = list(adapt_delta = 0.999))

mcmc_plot(resist_modr)

#RESILIENCE based on composition
resil_modr <- brm(LRR.resilience ~
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
                    #scale(N_ug.g.instant_2021Fall) +
                    #scale(WHC) +
                    (1 | Site),
                  data = subset(RRR_df,
                                Sprayed=="Yes"),
                  warmup = 1000, iter = 2000, chains = 4, seed = 123,
                  control = list(adapt_delta = 0.999, max_treedepth = 12))

mcmc_plot(resil_modr)

#RECOVERY based on composition
recov_modr <- brm(LRR.recovery ~ 
                    scale(pre_invrichness) +
                    scale(pre_natrichness) +
                    scale(CHJU) +
                    scale(BRTE) +
                    scale(slm.brte) +
                    scale(slm.chju) +
                    scale(slmr.inv) +
                    scale(slmr.nat) +
                    scale(Heatload) +
                    (1 | Site), 
                  data = subset(RRR_df,
                                Sprayed=="Yes"),
                  warmup = 1000, iter = 2000, chains = 4, seed = 123,
                  control = list(adapt_delta = 0.999, max_treedepth = 12))

mcmc_plot(recov_modr)

## Save ####

model_folder <- "data/outputs"
models <- ls(pattern = "modr$")
for(nm in models) {
  saveRDS(get(nm), file = file.path(model_folder, paste0(nm, ".RDS"))
  )
}


