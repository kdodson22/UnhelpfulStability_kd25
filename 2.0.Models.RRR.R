## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 2.0 Models for RRR

## Purpose: This script fits Bayesian glmms evaluating the effects of diversity, dominant species, and plant-available nitrogen on the stability of disturbed plant communities. Variable selection and directed acyclic graphs are described in the text.

## Author: K. Dodson 
## Date: Updated 12/1/2025


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
                        scale(N_ug.g.instant_2021Fall) +
                        scale(slmr.inv) +
                        scale(slmr.nat) +
                        scale(Heatload) +
                        scale(slm.N.21fall) +
                        (1 | Site), 
                      data = subset(invabsdf_soil,
                                    Sprayed=="Yes"), 
                      warmup = 1000, iter = 2000, chains = 4, seed = 123,
                      control = list(adapt_delta = 0.999, max_treedepth = 12))

# summary(inv_resistmodr)
# mcmc_intervals(inv_resistmodr,
#                prob = 0.5,
#                prob_outer = 0.9,
#                regex = "^b") 

#RESILIENCE (2024)  based on invasive cover
inv_resilience24_modr <- brm(LRR.resilience24 ~ 
                               scale(pre_invrichness) +
                               scale(pre_natrichness) +
                               scale(CHJU) +
                               scale(BRTE) +
                               scale(N_ug.g.instant_2022Spring) +
                               scale(LRR.resistance) +
                               scale(slmr.inv) +
                               scale(slmr.nat) +
                               scale(Heatload) +
                               scale(slm.N.22sp) +
                               (1 | Site), 
                             data = subset(invabsdf_soil,
                                           Sprayed=="Yes"),
                             warmup = 1000, iter = 2000, chains = 4, seed = 123,
                             control = list(adapt_delta = 0.999))

# summary(inv_resilience24_modr)
# mcmc_intervals(inv_resilience24_modr,
#                prob = 0.5,
#                prob_outer = 0.9,
#                regex = "^b")

#RECOVERY based on invasive cover
inv_recovermodr <- brm(LRR.recovery ~ 
                         scale(pre_invrichness) +
                         scale(pre_natrichness) +
                         scale(CHJU) +
                         scale(BRTE) +
                         scale(N_ug.g.instant_2022Spring) +
                         scale(slmr.inv) +
                         scale(slmr.nat) +
                         scale(Heatload) +
                         scale(slm.N.22sp) +
                         (1 | Site), 
                       data = subset(invabsdf_soil,
                                     Sprayed=="Yes"),
                       warmup = 1000, iter = 2000, chains = 4, seed = 123,
                       control = list(adapt_delta = 0.999))

# summary(inv_recovermodr)
# mcmc_intervals(inv_recovermodr,
#                prob = 0.5,
#                prob_outer = 0.9,
#                regex = "^b")

## MODELS - Native Cover #####
# RESISTANCE based on native cover
nat_resistmodr <- brm(LRR.resistance ~ 
                        scale(pre_invrichness) +
                        scale(pre_natrichness) +
                        scale(CHJU) + 
                        scale(BRTE) +
                        scale(N_ug.g.instant_2021Fall) +
                        scale(slmr.inv) +
                        scale(slmr.nat) +
                        scale(Heatload) +
                        scale(slm.N.21fall) +
                        (1 | Site), 
                      data = subset(natabsdf_soil2,
                                    Sprayed=="Yes"), 
                      warmup = 1000, iter = 2000, chains = 4, seed = 123,
                      control = list(adapt_delta = 0.999, max_treedepth = 12))

# summary(nat_resistmodr)
# mcmc_intervals(nat_resistmodr,
#                prob = 0.5,
#                prob_outer = 0.9,
#                regex = "^b")

#RESILIENCE (2024)  based on native cover
nat_resilience24_modr <- brm(LRR.resilience24 ~ 
                               scale(pre_invrichness) +
                               scale(pre_natrichness) +
                               scale(CHJU) +
                               scale(BRTE) +
                               scale(N_ug.g.instant_2022Spring) +
                               scale(LRR.resistance) +
                               scale(slmr.inv) +
                               scale(slmr.nat) +
                               scale(Heatload) +
                               scale(slm.N.22sp) +
                               (1 | Site), 
                             data = subset(natabsdf_soil2,
                                           Sprayed=="Yes"),
                             warmup = 1000, iter = 2000, chains = 4, seed = 123, 
                             control = list(adapt_delta = 0.999, 
                                            max_treedepth = 12))

# summary(nat_resilience24_modr)
# mcmc_intervals(nat_resilience24_modr,
#                prob = 0.5,
#                prob_outer = 0.9,
#                regex = "^b")

#RECOVERY based on native cover
nat_recovermodr <- brm(LRR.recovery ~ 
                         scale(pre_invrichness) +
                         scale(pre_natrichness) +
                         scale(CHJU) +
                         scale(BRTE) +
                         scale(N_ug.g.instant_2022Spring) +
                         scale(slmr.inv) +
                         scale(slmr.nat) +
                         scale(Heatload) +
                         scale(slm.N.22sp) +
                         (1 | Site), 
                       data = subset(natabsdf_soil2,
                                     Sprayed=="Yes"),
                       warmup = 1000, iter = 2000, chains = 4, seed = 123,
                       control = list(adapt_delta = 0.999, 
                                      max_treedepth = 12))

# summary(nat_recovermodr)
# mcmc_intervals(nat_recovermodr,
#                prob = 0.5,
#                prob_outer = 0.9,
#                regex = "^b")

## MODELS - Composition #####
#RESISTANCE based on composition
resist_modr <- brm(LRR.resistance ~ 
                     scale(pre_invrichness) +
                     scale(pre_natrichness) +
                     scale(BRTE) +
                     scale(CHJU) +
                     scale(N_ug.g.instant_2021Fall) +
                     scale(Heatload) +
                     scale(slmr.nat) +
                     scale(slmr.inv) +
                     scale(slm.N.f21) +
                     (1 | Site),
                   data = subset(RRR_df,
                                 Sprayed=="Yes"),
                   warmup = 1000, iter = 2000, chains = 4, seed = 123,
                   control = list(adapt_delta = 0.999))

# summary(resist_modr)
# mcmc_intervals(resist_modr,
#                prob = 0.5,
#                prob_outer = 0.9,
#                regex = "^b")

#RESILIENCE based on composition
resil_modr <- brm(LRR.resilience ~
                    scale(pre_invrichness) +
                    scale(pre_natrichness) +
                    scale(BRTE) +
                    scale(CHJU) +
                    scale(N_ug.g.instant_2022Spring) +
                    scale(LRR.resistance) +
                    scale(Heatload) +
                    scale(slmr.nat) +
                    scale(slmr.inv) +
                    scale(slm.N.s22) +
                    (1 | Site),
                  data = subset(RRR_df,
                                Sprayed=="Yes"),
                  warmup = 1000, iter = 2000, chains = 4, seed = 123,
                  control = list(adapt_delta = 0.999, max_treedepth = 12))

# summary(resil_modr)
# mcmc_intervals(resil_modr,
#                prob = 0.5,
#                prob_outer = 0.9,
#                regex = "^b")

#RECOVERY based on composition
recov_modr <- brm(LRR.recovery ~ 
                    scale(pre_invrichness) +
                    scale(pre_natrichness) +
                    scale(CHJU) +
                    scale(BRTE) +
                    scale(N_ug.g.instant_2022Spring) +
                    scale(Heatload) +
                    scale(slmr.nat) +
                    scale(slmr.inv) +
                    scale(slm.N.s22) +
                    (1 | Site), 
                  data = subset(RRR_df,
                                Sprayed=="Yes"),
                  warmup = 1000, iter = 2000, chains = 4, seed = 123,
                  control = list(adapt_delta = 0.999, max_treedepth = 12))

# summary(recov_modr)
# mcmc_intervals(recov_modr,
#                prob = 0.5,
#                prob_outer = 0.9,
#                regex = "^b")