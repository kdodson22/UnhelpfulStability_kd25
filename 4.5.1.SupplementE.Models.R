## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 4.5.1 Supplement E Models:

## Purpose:  This script generates the functional richness variables and dataframe for mediation analysis of species vs functional richness effects on RRR metrics.

## Author: K. Dodson 
## Date: Updated 6/30/2026

library(tidyverse)
library(here)
library(brms)
library(cmdstanr)

#data
source(here("1.0.Data.Set-up.R"))
source(here("1.1.RRR.Cover.R"))
source(here("1.2.RRR.Composition.R"))
#

#### Functional Richness Data frame ####

#generate functional richness data 
fun.comp.inv <- fun.comp.rel %>% dplyr::select(Site, Plot, Year, EAF, EAG, EPF, EPG, EBF)
fun.comp.nat <- fun.comp.rel %>% dplyr::select(Site, Plot, Year, NAF,NPF,NS,PG)
frall <- fun.comp.rel %>% dplyr::select(Site, Plot, Year)
frall$totalfunctionalrichness <- specnumber(fun.comp.rel[,-c(1:3)]) 
frall$invasivefunctionalrichness <- specnumber(fun.comp.inv[,-c(1:3)]) 
frall$nativefunctionalrichness <- specnumber(fun.comp.nat[,-c(1:3)]) 
frall <- frall %>% mutate(Site = as.factor(Site), Plot = as.factor(Plot))
str(frall)

frall_pre <- frall %>% filter(Year == 2021) %>% dplyr::select(-Year) %>%
  rename(pre_functionalrichness = totalfunctionalrichness,
         pre_invfuncrichness = invasivefunctionalrichness,
         pre_natfuncrichness = nativefunctionalrichness)

ns_pres<-fun.comp.rel %>% filter(Year == 2021) %>% dplyr::select(-Year)%>%
  mutate(Site = as.factor(Site), Plot = as.factor(Plot)) %>%
  rename(pre_NS = NS,
         pre_NAF = NAF,
         pre_NPF = NPF,
         pre_PG = PG)
ns_post<-fun.comp.rel %>% filter(Year == 2022) %>% dplyr::select(-Year)%>%
  mutate(Site = as.factor(Site), Plot = as.factor(Plot)) %>%
  rename(post_NS = NS,
         post_NAF = NAF,
         post_NPF = NPF,
         post_PG = PG)

  

#add to model dataframes
invabsdf_soil3 <- invabsdf_soil %>% 
  left_join(frall_pre, join_by(Site, Plot)) %>%
  left_join(ns_pres, join_by(Site, Plot)) %>%
  left_join(ns_post, join_by(Site, Plot)) %>%
  group_by(Site) %>%
  mutate(slm.tfun = mean(pre_functionalrichness),
         slm.ifun = mean(pre_invfuncrichness),
         slm.nfun = mean(pre_natfuncrichness),
         slm.nspre = mean(pre_NS),
         slm.nspos = mean(post_NS))

natabsdf_soil3 <- natabsdf_soil %>% 
  left_join(frall_pre, join_by(Site, Plot))  %>%
  left_join(ns_pres, join_by(Site, Plot)) %>%
  left_join(ns_post, join_by(Site, Plot)) %>%
  group_by(Site) %>%
  mutate(slm.tfun = mean(pre_functionalrichness),
         slm.ifun = mean(pre_invfuncrichness),
         slm.nfun = mean(pre_natfuncrichness),
         slm.nspre = mean(pre_NS),
         slm.nspos = mean(post_NS))

RRR_df3 <- RRR_df %>% 
  left_join(frall_pre, join_by(Site, Plot))  %>%
  left_join(ns_pres, join_by(Site, Plot)) %>%
  left_join(ns_post, join_by(Site, Plot)) %>%
  group_by(Site) %>%
  mutate(slm.tfun = mean(pre_functionalrichness),
         slm.ifun = mean(pre_invfuncrichness),
         slm.nfun = mean(pre_natfuncrichness),
         slm.nspre = mean(pre_NS),
         slm.nspos = mean(post_NS))
#####


## MODELS - Mediation (a path) ####
#inv fun richness ~ inv spp richness
invrich_apath <- brm(pre_invfuncrichness ~ pre_invrichness + (1 | Site),
                     family = "lognormal",
                     data = subset(invabsdf_soil3,
                                   Sprayed=="Yes"), 
                     warmup = 1000, iter = 2000, chains = 4, seed = 123,
                     control = list(adapt_delta = 0.999, max_treedepth = 15),
                     cores = 3, backend = "cmdstanr")

#nat fun richness ~ nat spp richness
natrich_apath <- brm(pre_natfuncrichness ~ pre_natrichness + (1 | Site),
                     family = "zero_inflated_poisson",
                     data = subset(invabsdf_soil3,
                                   Sprayed=="Yes"), 
                     warmup = 1000, iter = 2000, chains = 4, seed = 123,
                     control = list(adapt_delta = 0.999, max_treedepth = 15),
                     cores = 4, backend = "cmdstanr")
#####


## MODELS - Total Treatment Effect ####
#invasive
#resistance
ate_iresist <- brm(LRR.resistance ~ 
                     scale(pre_invrichness) +
                     scale(pre_natrichness) +
                     
                     scale(slmr.inv) +
                     scale(slmr.nat) +
                     
                     scale(Heatload) +
                     (1|Site),
                   data = subset(invabsdf_soil3,
                                 Sprayed=="Yes"), 
                   warmup = 1000, iter = 2000, chains = 4, seed = 123,
                   control = list(adapt_delta = 0.999, max_treedepth = 15),
                   cores = 4, backend = "cmdstanr")


#resilience 
ate_iresil <- brm(LRR.resilience24 ~ 
                     scale(pre_invrichness) +
                     scale(pre_natrichness) +
                     
                     scale(slmr.inv) +
                     scale(slmr.nat) +
                     
                     scale(Heatload) +
                     (1|Site),
                   data = subset(invabsdf_soil3,
                                 Sprayed=="Yes"), 
                   warmup = 1000, iter = 2000, chains = 4, seed = 123,
                   control = list(adapt_delta = 0.999, max_treedepth = 15),
                   cores = 4, backend = "cmdstanr")

#recovery
ate_ireco <- brm(LRR.recovery ~ 
                    scale(pre_invrichness) +
                    scale(pre_natrichness) +
                    
                    scale(slmr.inv) +
                    scale(slmr.nat) +
                    
                    scale(Heatload) +
                    (1|Site),
                  data = subset(invabsdf_soil3,
                                Sprayed=="Yes"), 
                  warmup = 1000, iter = 2000, chains = 4, seed = 123,
                  control = list(adapt_delta = 0.999, max_treedepth = 15),
                  cores = 4, backend = "cmdstanr")

#native
#resistance
ate_nresist <- brm(LRR.resistance ~ 
                     scale(pre_invrichness) +
                     scale(pre_natrichness) +
                     
                     scale(slmr.inv) +
                     scale(slmr.nat) +
                     
                     scale(Heatload) +
                     (1|Site),
                   data = subset(natabsdf_soil3,
                                 Sprayed=="Yes"), 
                   warmup = 1000, iter = 2000, chains = 4, seed = 123,
                   control = list(adapt_delta = 0.999, max_treedepth = 15),
                   cores = 4, backend = "cmdstanr")

#resilience 
ate_nresil <- brm(LRR.resilience24 ~ 
                    scale(pre_invrichness) +
                    scale(pre_natrichness) +
                    
                    scale(slmr.inv) +
                    scale(slmr.nat) +
                    
                    scale(Heatload) +
                    (1|Site),
                  data = subset(natabsdf_soil3,
                                Sprayed=="Yes"), 
                  warmup = 1000, iter = 2000, chains = 4, seed = 123,
                  control = list(adapt_delta = 0.999, max_treedepth = 15),
                  cores = 4, backend = "cmdstanr")

#recovery
ate_nreco <- brm(LRR.recovery ~ 
                   scale(pre_invrichness) +
                   scale(pre_natrichness) +
                   
                   scale(slmr.inv) +
                   scale(slmr.nat) +
                   
                   scale(Heatload) +
                   (1|Site),
                 data = subset(natabsdf_soil3,
                               Sprayed=="Yes"), 
                 warmup = 1000, iter = 2000, chains = 4, seed = 123,
                 control = list(adapt_delta = 0.999, max_treedepth = 15),
                 cores = 4, backend = "cmdstanr")

#composition
#resistance
ate_cresist <- brm(LRR.resistance ~ 
                     scale(pre_invrichness) +
                     scale(pre_natrichness) +
                     
                     scale(slmr.inv) +
                     scale(slmr.nat) +
                     
                     scale(Heatload) +
                     (1|Site),
                   data = subset(RRR_df3,
                                 Sprayed=="Yes"), 
                   warmup = 1000, iter = 2000, chains = 4, seed = 123,
                   control = list(adapt_delta = 0.999, max_treedepth = 15),
                   cores = 4, backend = "cmdstanr")

#resilience 
ate_cresil <- brm(LRR.resilience ~ 
                    scale(pre_invrichness) +
                    scale(pre_natrichness) +
                    
                    scale(slmr.inv) +
                    scale(slmr.nat) +
                    
                    scale(Heatload) +
                    (1|Site),
                  data = subset(RRR_df3,
                                Sprayed=="Yes"), 
                  warmup = 1000, iter = 2000, chains = 4, seed = 123,
                  control = list(adapt_delta = 0.999, max_treedepth = 15),
                  cores = 4, backend = "cmdstanr")

#recovery
ate_creco <- brm(LRR.recovery ~ 
                   scale(pre_invrichness) +
                   scale(pre_natrichness) +
                   
                   scale(slmr.inv) +
                   scale(slmr.nat) +
                   
                   scale(Heatload) +
                   (1|Site),
                 data = subset(RRR_df3,
                               Sprayed=="Yes"), 
                 warmup = 1000, iter = 2000, chains = 4, seed = 123,
                 control = list(adapt_delta = 0.999, max_treedepth = 15),
                 cores = 4, backend = "cmdstanr")

#####


## MODELS - Invasive Cover #####
#RESISTANCE based on invasive cover
inv_resistmodr_fr <- brm(LRR.resistance ~ 
                        scale(pre_invrichness) +
                        scale(pre_natrichness) +
                        scale(pre_invfuncrichness) +
                        scale(pre_natfuncrichness) +
                          scale(slm.ifun) +
                          scale(slm.nfun) +
                        scale(CHJU) + 
                        scale(BRTE) +
                        scale(N_ug.g.instant_2021Fall) +
                        scale(slmr.inv) +
                        scale(slmr.nat) +
                        scale(Heatload) +
                        scale(slm.N.21fall) +
                        (1 | Site), 
                      data = subset(invabsdf_soil3,
                                    Sprayed=="Yes"), 
                      warmup = 1000, iter = 2000, chains = 4, seed = 123,
                      control = list(adapt_delta = 0.999, max_treedepth = 15),
                      cores = 4, backend = "cmdstanr")

#RESILIENCE (2024)  based on invasive cover
inv_resilience24_modr_fr <- brm(LRR.resilience24 ~ 
                               scale(pre_invrichness) +
                               scale(pre_natrichness) +
                                 scale(pre_invfuncrichness) +
                                 scale(pre_natfuncrichness) +
                                 scale(slm.ifun) +
                                 scale(slm.nfun) +
                               scale(CHJU) +
                               scale(BRTE) +
                               scale(N_ug.g.instant_2022Spring) +
                               scale(LRR.resistance) +
                               scale(slmr.inv) +
                               scale(slmr.nat) +
                               scale(Heatload) +
                               scale(slm.N.22sp) +
                               (1 | Site), 
                             data = subset(invabsdf_soil3,
                                           Sprayed=="Yes"),
                             warmup = 1000, iter = 2000, chains = 4, seed = 123,
                             control = list(adapt_delta = 0.999, max_treedepth = 15),
                             cores = 4, backend = "cmdstanr")


#RECOVERY based on invasive cover
inv_recovermodr_fr <- brm(LRR.recovery ~ 
                         scale(pre_invrichness) +
                         scale(pre_natrichness) +
                           scale(pre_invfuncrichness) +
                           scale(pre_natfuncrichness) +
                           scale(slm.ifun) +
                           # scale(slm.nfun) +     #all slm highly correlated, removal does not change estimates
                         scale(CHJU) +
                         scale(BRTE) +
                         scale(N_ug.g.instant_2022Spring) +
                         scale(slmr.inv) +
                         scale(slmr.nat) +
                         scale(Heatload) +
                         scale(slm.N.22sp) +
                         (1 | Site), 
                       data = subset(invabsdf_soil3,
                                     Sprayed=="Yes"),
                       warmup = 1000, iter = 2000, chains = 4, seed = 123,
                       control = list(adapt_delta = 0.999, max_treedepth = 15),
                       cores = 4, backend = "cmdstanr")


## MODELS - Native Cover #####
# RESISTANCE based on native cover
nat_resistmodr_fr <- brm(LRR.resistance ~ 
                        scale(pre_invrichness) +
                        scale(pre_natrichness) +
                          scale(pre_invfuncrichness) +
                          scale(pre_natfuncrichness) +
                          scale(slm.ifun) +
                          scale(slm.nfun) +
                        scale(CHJU) + 
                        scale(BRTE) +
                        scale(N_ug.g.instant_2021Fall) +
                        scale(slmr.inv) +
                        scale(slmr.nat) +
                        scale(Heatload) +
                        scale(slm.N.21fall) +
                        (1 | Site), 
                      data = subset(natabsdf_soil3,
                                    Sprayed=="Yes"), 
                      warmup = 1000, iter = 2000, chains = 4, seed = 123,
                      control = list(adapt_delta = 0.999, max_treedepth = 15),
                      cores = 4, backend = "cmdstanr")

#RESILIENCE (2024)  based on native cover
nat_resilience24_modr_fr <- brm(LRR.resilience24 ~ 
                               scale(pre_invrichness) +
                               scale(pre_natrichness) +
                                 scale(pre_invfuncrichness) +
                                 scale(pre_natfuncrichness) +
                                 scale(slm.ifun) +
                                 scale(slm.nfun) +
                               scale(CHJU) +
                               scale(BRTE) +
                               scale(N_ug.g.instant_2022Spring) +
                               scale(LRR.resistance) +
                               scale(slmr.inv) +
                               scale(slmr.nat) +
                               scale(Heatload) +
                               scale(slm.N.22sp) +
                               (1 | Site), 
                             data = subset(natabsdf_soil3,
                                           Sprayed=="Yes"),
                             warmup = 1000, iter = 2000, chains = 4, seed = 123, 
                             control = list(adapt_delta = 0.999, max_treedepth = 15),
                             cores = 4, backend = "cmdstanr")

#RECOVERY based on native cover
nat_recovermodr_fr <- brm(LRR.recovery ~ 
                         scale(pre_invrichness) +
                         scale(pre_natrichness) +
                           scale(pre_invfuncrichness) +
                           scale(pre_natfuncrichness) +
                           scale(slm.ifun) +
                           scale(slm.nfun) +
                         scale(CHJU) +
                         scale(BRTE) +
                         scale(N_ug.g.instant_2022Spring) +
                         scale(slmr.inv) +
                         scale(slmr.nat) +
                         scale(Heatload) +
                         scale(slm.N.22sp) +
                         (1 | Site), 
                       data = subset(natabsdf_soil3,
                                     Sprayed=="Yes"),
                       warmup = 1000, iter = 2000, chains = 4, seed = 123,
                       control = list(adapt_delta = 0.999, max_treedepth = 15),
                       cores = 4, backend = "cmdstanr")

## MODELS - Composition #####
#RESISTANCE based on composition
resist_modr_fr <- brm(LRR.resistance ~ 
                     scale(pre_invrichness) +
                     scale(pre_natrichness) +
                       scale(pre_invfuncrichness) +
                       scale(pre_natfuncrichness) +
                       scale(slm.ifun) +
                       scale(slm.nfun) +
                     scale(BRTE) +
                     scale(CHJU) +
                     scale(N_ug.g.instant_2021Fall) +
                     scale(Heatload) +
                     scale(slmr.nat) +
                     scale(slmr.inv) +
                     scale(slm.N.f21) +
                     (1 | Site),
                   data = subset(RRR_df3,
                                 Sprayed=="Yes"),
                   warmup = 1000, iter = 2000, chains = 4, seed = 123,
                   control = list(adapt_delta = 0.999, max_treedepth = 15),
                   cores = 4, backend = "cmdstanr")

#RESILIENCE based on composition
resil_modr_fr <- brm(LRR.resilience ~
                    scale(pre_invrichness) +
                    scale(pre_natrichness) +
                      scale(pre_invfuncrichness) +
                      scale(pre_natfuncrichness) +
                      scale(slm.ifun) +
                      scale(slm.nfun) +
                    scale(BRTE) +
                    scale(CHJU) +
                    scale(N_ug.g.instant_2022Spring) +
                    scale(LRR.resistance) +
                    scale(Heatload) +
                    scale(slmr.nat) +
                    scale(slmr.inv) +
                    scale(slm.N.s22) +
                    (1 | Site),
                  data = subset(RRR_df3,
                                Sprayed=="Yes"),
                  warmup = 1000, iter = 2000, chains = 4, seed = 123,
                  control = list(adapt_delta = 0.999, max_treedepth = 15),
                  cores = 4, backend = "cmdstanr")

#RECOVERY based on composition
recov_modr_fr <- brm(LRR.recovery ~ 
                    scale(pre_invrichness) +
                    scale(pre_natrichness) +
                      scale(pre_invfuncrichness) +
                      scale(pre_natfuncrichness) +
                      scale(slm.ifun) +
                      scale(slm.nfun) +
                    scale(CHJU) +
                    scale(BRTE) +
                    scale(N_ug.g.instant_2022Spring) +
                    scale(Heatload) +
                    scale(slmr.nat) +
                    scale(slmr.inv) +
                    scale(slm.N.s22) +
                    (1 | Site), 
                  data = subset(RRR_df3,
                                Sprayed=="Yes"),
                  warmup = 1000, iter = 2000, chains = 4, seed = 123,
                  control = list(adapt_delta = 0.999, max_treedepth = 15),
                  cores = 4, backend = "cmdstanr")

