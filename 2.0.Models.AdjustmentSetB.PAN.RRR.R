## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 2.0b) Models for RRR using adjustment set B to isolate effects of post-treatment PAN on resilience and recovery only

## Purpose: This script fits Bayesian glmms evaluating the effects of plant-available nitrogen on the stability of disturbed plant communities. Variable selection and directed acyclic graphs are described in the text.

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

#RESILIENCE (2024)  based on invasive cover
inv_resilience24_modr_B <- brm(LRR.resilience24 ~ 
                               scale(pre_invrichness) +
                               scale(pre_natrichness) +
                               scale(CHJU) +
                               scale(BRTE) +
                               scale(N_ug.g.instant_2022Spring) +
                                 scale(N_ug.g.instant_2021Fall) +
                               scale(Heatload) +
                               scale(LRR.resistance) +
                               scale(slm.N.22sp) +
                               (1 | Site), 
                             data = subset(invabsdf_soil,
                                           Sprayed=="Yes"),
                             warmup = 1000, iter = 2000, chains = 4, seed = 123,
                             control = list(adapt_delta = 0.999))


mcmc_plot(inv_resilience24_modr_B)

#RECOVERY based on invasive cover
inv_recovermodr_B <- brm(LRR.recovery ~ 
                         scale(pre_invrichness) +
                         scale(pre_natrichness) +
                         scale(CHJU) +
                         scale(BRTE) +
                         scale(N_ug.g.instant_2022Spring) +
                           scale(N_ug.g.instant_2021Fall) +
                         scale(Heatload) +
                         scale(slm.N.22sp) +
                         (1 | Site), 
                       data = subset(invabsdf_soil,
                                     Sprayed=="Yes"),
                       warmup = 1000, iter = 2000, chains = 4, seed = 123,
                       control = list(adapt_delta = 0.999))

mcmc_plot(inv_recovermodr_B)

## MODELS - Native Cover #####

#RESILIENCE (2024)  based on native cover
nat_resilience24_modr_B <- brm(LRR.resilience24 ~ 
                               scale(pre_invrichness) +
                               scale(pre_natrichness) +
                               scale(CHJU) +
                               scale(BRTE) +
                               scale(N_ug.g.instant_2022Spring) +
                                 scale(N_ug.g.instant_2021Fall) +
                               scale(Heatload) +
                               scale(LRR.resistance) +
                               scale(slm.N.22sp) +
                               (1 | Site), 
                             data = subset(natabsdf_soil2,
                                           Sprayed=="Yes"),
                             warmup = 1000, iter = 2000, chains = 4, seed = 123, 
                             control = list(adapt_delta = 0.999, 
                                            max_treedepth = 12))

mcmc_plot(nat_resilience24_modr_B)

#RECOVERY based on native cover
nat_recovermodr_B <- brm(LRR.recovery ~ 
                         scale(pre_invrichness) +
                         scale(pre_natrichness) +
                         scale(CHJU) +
                         scale(BRTE) +
                         scale(N_ug.g.instant_2022Spring) +
                         scale(N_ug.g.instant_2021Fall) +
                         scale(Heatload) +
                         scale(slm.N.22sp) +
                         (1 | Site), 
                       data = subset(natabsdf_soil2,
                                     Sprayed=="Yes"),
                       warmup = 1000, iter = 2000, chains = 4, seed = 123,
                       control = list(adapt_delta = 0.999, 
                                      max_treedepth = 13))

mcmc_plot(nat_recovermodr_B)

#
## MODELS - Composition #####

#RESILIENCE based on composition
resil_modr_B <- brm(LRR.resilience ~
                    scale(pre_invrichness) +
                    scale(pre_natrichness) +
                    scale(CHJU) +
                    scale(BRTE) +
                    scale(N_ug.g.instant_2022Spring) +
                    scale(N_ug.g.instant_2021Fall) +
                    scale(Heatload) +
                    scale(LRR.resistance)+
                    scale(slm.N.s22) +
                    (1 | Site),
                  data = subset(RRR_df,
                                Sprayed=="Yes"),
                  warmup = 1000, iter = 2000, chains = 4, seed = 123,
                  control = list(adapt_delta = 0.999, max_treedepth = 12))

mcmc_plot(resil_modr_B)

#RECOVERY based on composition
recov_modr_B <- brm(LRR.recovery ~ 
                    scale(pre_invrichness) +
                    scale(pre_natrichness) +
                    scale(CHJU) +
                    scale(BRTE) +
                    scale(N_ug.g.instant_2022Spring) +
                    scale(N_ug.g.instant_2021Fall) +
                    scale(Heatload) +
                    scale(slm.N.s22) +
                    (1 | Site), 
                  data = subset(RRR_df,
                                Sprayed=="Yes"),
                  warmup = 1000, iter = 2000, chains = 4, seed = 123,
                  control = list(adapt_delta = 0.999, max_treedepth = 12))

mcmc_plot(recov_modr_B)

## Save ####

model_folder <- "data/outputs"
models <- ls(pattern = "modr_B$")
for(nm in models) {
  saveRDS(get(nm), file = file.path(model_folder, paste0(nm, ".RDS"))
  )
}


