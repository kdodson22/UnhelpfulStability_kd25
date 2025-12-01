## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 2.1 Model checks

## Purpose: This script completes model diagnostics for the model fits in 2.0, including probability of direction for posterior parameter estimates, model fit statistics, and description of priors.

## Author: K. Dodson 
## Date: Updated 12/1/2025

library(tidyverse)
library(here)
library(performance)
library(brms)
library(modelbased)

#data
# source(here("2.0.Models.RRR.R"))

## Extract Priors for supplementary information #####
get_prior(inv_resistmodr)
get_prior(inv_resilience24_modr)
get_prior(inv_recovermodr)

get_prior(nat_resistmodr)
get_prior(nat_resilience24_modr)
get_prior(nat_recovermodr)

get_prior(resist_modr)
get_prior(resil_modr)
get_prior(recov_modr)

## Evaluate Model Fit #####
# Mean absolute error:
performance::mae(inv_resistmodr)
performance::mae(inv_resilience24_modr)
performance::mae(inv_recovermodr)

performance::mae(nat_resistmodr)
performance::mae(nat_resilience24_modr)
performance::mae(nat_recovermodr)

performance::mae(resist_modr)
performance::mae(resil_modr)
performance::mae(recov_modr)

# Bayesian r2
bayes_R2(inv_resistmodr)
bayes_R2(inv_resilience24_modr)
bayes_R2(inv_recovermodr)

bayes_R2(nat_resistmodr)
bayes_R2(nat_resilience24_modr)
bayes_R2(nat_recovermodr)

bayes_R2(resist_modr)
bayes_R2(resil_modr)
bayes_R2(recov_modr)



## Probability of direction and predicted effects on scale of response (marginal effects) #####

#invasive cover resilience and recovery x invasive richness
estimate_prediction(inv_resilience24_modr, by = "pre_invrichness") #marginal effect
estimate_prediction(inv_recovermodr, by = "pre_invrichness")

posterior <- data.frame(inv_recovermodr)
pd.invrich <- length(posterior$b_scalepre_invrichness[posterior$b_scalepre_invrichness>0])/length(posterior$b_scalepre_invrichness)
pd.invrich

#invasive cover resistance, resilience, and recovery x CHJU
estimate_prediction(inv_resistmodr, by = "CHJU")
estimate_prediction(inv_resilience24_modr, by = "CHJU")
estimate_prediction(inv_recovermodr, by = "CHJU")

#native cover resilience x invasive richness
estimate_prediction(nat_resilience24_modr, by = "pre_invrichness")
posterior <- data.frame(nat_resilience24_modr)
pd.invrich <- length(posterior$b_scalepre_invrichness[posterior$b_scalepre_invrichness<0])/length(posterior$b_scalepre_invrichness)
pd.invrich

#compositional resilience and recovery x CHJU 
estimate_prediction(resil_modr, by = "CHJU")
estimate_prediction(recov_modr, by = "CHJU")

#native cover resistance, resilience, and recovery x CHJU 
estimate_prediction(nat_resistmodr, by = "CHJU")
posterior <- data.frame(nat_resistmodr)
pd.chju <- length(posterior$b_scaleCHJU[posterior$b_scaleCHJU<0])/length(posterior$b_scaleCHJU)
pd.chju

estimate_prediction(nat_resilience24_modr, by = "CHJU")
estimate_prediction(nat_recovermodr, by = "CHJU")


#invasive cover resilience and recovery x native richness
estimate_prediction(inv_resilience24_modr, by = "pre_natrichness")
posterior <- data.frame(inv_resilience24_modr)
pd.natrich <- length(posterior$b_scalepre_natrichness[posterior$b_scalepre_natrichness<0])/length(posterior$b_scalepre_natrichness)
pd.natrich

estimate_prediction(inv_recovermodr, by = "pre_natrichness")
posterior <- data.frame(inv_recovermodr)
pd.natrich <- length(posterior$b_scalepre_natrichness[posterior$b_scalepre_natrichness<0])/length(posterior$b_scalepre_natrichness)
pd.natrich

#native cover resistance x native richness
estimate_prediction(nat_resistmodr, by = "pre_natrichness")

#compositional resilience and recovery x native richness
estimate_prediction(resil_modr, by = "pre_natrichness")
estimate_prediction(recov_modr, by = "pre_natrichness")

#invasive cover resilience x PAN (sp22)
estimate_prediction(inv_resilience24_modr, by = "N_ug.g.instant_2022Spring")

#compositional resilience x PAN (sp22)
estimate_prediction(resil_modr, by = "N_ug.g.instant_2022Spring")
estimate_prediction(recov_modr, by = "N_ug.g.instant_2022Spring")
posterior <- data.frame(recov_modr)
pd.Nsp22 <- length(posterior$b_scaleN_ug.g.instant_2022Spring[posterior$b_scaleN_ug.g.instant_2022Spring>0])/length(posterior$b_scaleN_ug.g.instant_2022Spring)
pd.Nsp22


#native cover resistance, resilience, and recovery x BRTE 
estimate_prediction(inv_resistmodr, by = "BRTE")
posterior <- data.frame(inv_resistmodr)
pd.BRTE <- length(posterior$b_scaleBRTE[posterior$b_scaleBRTE>0])/length(posterior$b_scaleBRTE)
pd.BRTE

estimate_prediction(inv_resilience24_modr, by = "BRTE")
estimate_prediction(inv_recovermodr, by = "BRTE")


#native cover resistance, resilience, and recovery x BRTE 
estimate_prediction(nat_resistmodr, by = "BRTE")
posterior <- data.frame(nat_resistmodr)
pd.BRTE <- length(posterior$b_scaleBRTE[posterior$b_scaleBRTE<0])/length(posterior$b_scaleBRTE)
pd.BRTE

estimate_prediction(nat_resilience24_modr, by = "BRTE")
estimate_prediction(nat_recovermodr, by = "BRTE")
