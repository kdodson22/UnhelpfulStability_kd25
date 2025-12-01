## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 2.2 Models for correlation between RRR metrics

## Purpose: This script fits Bayesian glmms for the deviance analysis, evaluating how diversity, dominant species, and plant-available nitrogen relate to the correlation structure/residuals between RRR metrics.

## Author: K. Dodson 
## Date: Updated 12/1/2025


library(tidyverse)
library(marginaleffects)
library(here)
library(brms)

#data
# source(here("1.1.RRR.Cover.R"))
# source(here("1.2.RRR.Composition.R"))

## Build correlation dataframes ##### 
cover_df <- invabsdf_soil %>%  
  mutate(Site = as.integer(Site),
         Plot = as.integer(Plot)) %>%
  select(Site, Plot, Sprayed, LRR.resistance, LRR.resilience24, LRR.recovery) %>%
  rename(ifunc_resistance = LRR.resistance,
         ifunc_resilience = LRR.resilience24,
         ifunc_recovery = LRR.recovery)

cover_df2 <- natabsdf_soil %>%  
  mutate(Site = as.integer(Site),
         Plot = as.integer(Plot), 
         Year = as.integer(Year)) %>%
  filter(pre_natcover_abs != 0) %>%
  select(Site, Plot, Sprayed, LRR.resistance, LRR.resilience24, LRR.recovery) %>%
  rename(nfunc_resistance = LRR.resistance,
         nfunc_resilience = LRR.resilience24,
         nfunc_recovery = LRR.recovery)

composition_df <- RRR_df %>% 
  mutate(Site = as.integer(Site),
         Plot = as.integer(Plot)) %>%
  select(Site, Plot, Sprayed, LRR.resistance, LRR.resilience, LRR.recovery) %>%
  rename(comp_resistance = LRR.resistance,
         comp_resilience = LRR.resilience,
         comp_recovery = LRR.recovery)

#bind
cor_df <- full_join(cover_df, composition_df, by = c("Site", "Plot", "Sprayed"))
cor_df <- full_join(cor_df, cover_df2, by = c("Site", "Plot", "Sprayed"))
cor_df_yes <- cor_df %>% filter(Sprayed == "Yes")


## Estimate correlations between RRR metrics: #######

## Invasive x Composition ##
#recovery
ic_recov_model <- brm(ifunc_recovery ~ comp_recovery 
                      + (1 | Site), 
                      data = cor_df_yes, 
                      warmup = 1000, iter = 2000, chains = 4, 
                      control = list(adapt_delta = 0.999))

ic_recov_coef <- as.data.frame(fixef(ic_recov_model))
ic_recov_r2 <- as.data.frame(bayes_R2(ic_recov_model))

## Invasive x Native ##
#recovery
in_recov_model <- brm(ifunc_recovery ~ nfunc_recovery 
                      + (1 | Site), 
                      data = cor_df_yes, 
                      warmup = 1000, iter = 2000, chains = 4, 
                      control = list(adapt_delta = 0.999))

in_recov_coef <- as.data.frame(fixef(in_recov_model))
in_recov_r2 <- as.data.frame(bayes_R2(in_recov_model))

#calculate residuals
in_recov_resids <- residuals(in_recov_model)
in_recov_df <- cor_df_yes %>% filter(!is.na(nfunc_recovery)) %>% cbind(as.data.frame(in_recov_resids))

##################


########################################


## Deviance Analysis - Invasive x Composition  #####
#create df with recovery residuals 
ic_recov_resids <- residuals(ic_recov_model, summary = T)

ic_resids_df <- cor_df_yes %>% 
  mutate(Site = as.factor(Site)) %>%
  cbind(as.data.frame(ic_recov_resids)) %>%
  left_join(invabsdf_soil, join_by(Site, Plot, Sprayed)) %>%
  rename(residual = Estimate)

ic_resids_df <- ic_resids_df %>% 
  group_by(Site) %>%
  mutate(slm.brte = mean(BRTE),
         slm.chju = mean(CHJU)) %>%
  ungroup()

#model 
recovdiff_mod <-brm(residual ~ 
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
                    data = ic_resids_df,
                    warmup = 1000, iter = 2000, chains = 4, 
                    control = list(adapt_delta = 0.999, 
                                   max_treedepth = 12))

