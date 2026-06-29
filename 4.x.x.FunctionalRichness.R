library(tidyverse)
library(here)
library(corrplot)
library(brms)
library(cmdstanr)
library(marginaleffects)
library(bayesplot)
library(ggdist)
library(ggeffects)
library(modelr)
library(tidybayes)
library(ggtext)


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


## MODELS - Average Treatment Effect ####
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

#####


## Main Coefficient plot #####
## Extract posterior parameter estimates from each model fit and combine for plotting: 
#Resistance 
ints.inv.resist <- inv_resistmodr_fr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness, 
               b_scalepre_invfuncrichness, 
               b_scalepre_natfuncrichness,
               # b_scalepre_NS,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2021Fall) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Invasive Cover",
         nonzero = ifelse(.lower>0 & 
                            .upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

ints.nat.resist <- nat_resistmodr_fr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scalepre_invfuncrichness, 
               b_scalepre_natfuncrichness,
               # b_scalepre_NS,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2021Fall) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Native Cover",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

ints.comp.resist <- resist_modr_fr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scalepre_invfuncrichness, 
               b_scalepre_natfuncrichness,
               # b_scalepre_NS,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2021Fall) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Composition",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))


resist.posteriors <- rbind(ints.inv.resist, ints.nat.resist, ints.comp.resist)

#Resilience 
ints.inv.resil <- inv_resilience24_modr_fr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scalepre_invfuncrichness, 
               b_scalepre_natfuncrichness,
               # b_scalepost_NS,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Invasive Cover",
         nonzero = ifelse(.lower>0 & 
                            .upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))


ints.nat.resil <- nat_resilience24_modr_fr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scalepre_invfuncrichness, 
               b_scalepre_natfuncrichness,
               # b_scalepost_NS,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Native Cover",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

ints.comp.resil <- resil_modr_fr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scalepre_invfuncrichness, 
               b_scalepre_natfuncrichness,
               # b_scalepost_NS,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Composition",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))


resil.posteriors <- rbind(ints.inv.resil, ints.nat.resil, ints.comp.resil)

#Recovery
ints.inv.reco <- inv_recovermodr_fr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scalepre_invfuncrichness, 
               b_scalepre_natfuncrichness,
               # b_scalepost_NS,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Invasive Cover",
         nonzero = ifelse(.lower>0 & 
                            .upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))


ints.nat.reco <- nat_recovermodr_fr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scalepre_invfuncrichness, 
               b_scalepre_natfuncrichness,
               # b_scalepost_NS,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Native Cover",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

ints.comp.reco <- recov_modr_fr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scalepre_invfuncrichness, 
               b_scalepre_natfuncrichness,
               # b_scalepost_NS,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Composition",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

reco.posteriors <- rbind(ints.inv.reco, ints.nat.reco, ints.comp.reco)


## Organization and Renaming 
resist.posteriors$Metric <- "Resistance"
resil.posteriors$Metric <- "Resilience"
reco.posteriors$Metric <- "Recovery"

#combine 
posteriors <- rbind(resist.posteriors, resil.posteriors, reco.posteriors)
#rename variables
posteriors <- posteriors %>%
  mutate(.variable = recode(.variable, 
                            b_scalepre_invrichness = "Pre-treatment Invasive Species Richness",
                            b_scalepre_natrichness = "Pre-treatment Native Species Richness",
                            b_scalepre_invfuncrichness = "Pre-treatment Invasive Functional Richness", 
                            b_scalepre_natfuncrichness = "Pre-treatment Native Functional Richness",
                            # b_scalepre_NS = "Pre-treatment Native Native Shrub Cover",
                            # b_scalepost_NS = "Post-treatment Native Native Shrub Cover",
                            b_scaleCHJU = "Pre-treatment C. juncea Cover",
                            b_scaleBRTE = "Pre-treatment B. tectorum Cover",
                            b_scaleN_ug.g.instant_2021Fall = "Pre-treatment PAN",
                            b_scaleN_ug.g.instant_2022Spring = "Post-treatment PAN"
  ))

#make response a factor to fix order
posteriors$response <- factor(posteriors$response, 
                              levels = c("Composition", 
                                         "Native Cover",
                                         "Invasive Cover"))
posteriors$Metric <- factor(posteriors$Metric, 
                            levels = c("Resistance", 
                                       "Resilience",
                                       "Recovery"))
posteriors$.variable <- factor(posteriors$.variable,
                               levels = c("Post-treatment PAN", "Pre-treatment PAN",
                                          "Pre-treatment C. juncea Cover", "Pre-treatment B. tectorum Cover",
                                          "Pre-treatment Native Functional Richness", 
                                          "Pre-treatment Native Species Richness", 
                                          "Pre-treatment Invasive Functional Richness", 
                                          "Pre-treatment Invasive Species Richness"
                               )
)

## Plot 
supfig11 <- ggplot(posteriors, aes(y =.variable,
                       color=response,
                       shape = nonzero)) +
  geom_vline(xintercept=0, linetype="dashed", color="grey60") +
  geom_linerange(aes(xmin = .lower, xmax = .upper), lwd = 0.7,
                 position=position_dodge(width = 0.4)) +
  geom_point(aes(x=.value), size=2.25,
             position=position_dodge(width = 0.4)) +
  scale_shape_manual(values=c(21,16), guide = "none") +
  scale_color_manual(breaks = c("Invasive Cover", "Native Cover",
                                "Composition"),
                     name=c("Response variable"),
                     values=c("#EE6677", "#228833","#4477AA")) +
  ylab("") +
  xlab("Estimated effect on response") +
  scale_y_discrete(labels = c("Post-treatment <br> PAN", 
                              "Pre-treatment <br> PAN",
                              "Pre-treatment <br> *C. juncea* Cover", 
                              "Pre-treatment <br> *B. tectorum* Cover",
                              "Pre-treatment <br> Native Functional Richness", 
                              "Pre-treatment <br> Native Species Richness", 
                              "Pre-treatment <br> Invasive Functional Richness", 
                              "Pre-treatment <br> Invasive Species Richness")) +
  theme_bw() + facet_wrap(~Metric) + 
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_markdown(size = 14),
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14), 
        legend.position = "inside",
        legend.justification.inside = c(0.025,0.025)) 

ggsave(plot = supfig11,
      file = "figures/supfig11.png",
       width = 9, height = 7.5, unit = c("in"), dpi = 400)

#####


## Mediation - Draws ####
## Extract Draws 
# apath
invrich_apath_draws <- as_draws_df(invrich_apath)
natrich_apath_draws <- as_draws_df(natrich_apath)

invrich_apath_df <- median_qi(invrich_apath_draws$b_pre_invrichness, 
                              .width=c(0.9)) %>%
  as.data.frame() %>%
  dplyr::select(y,ymin,ymax) %>%
  mutate(term = "theta1", 
         maineffect = "Invasive Species Richness",
         group = "Invasive")

natrich_apath_df <- median_qi(natrich_apath_draws$b_pre_natrichness, 
                              .width=c(0.9)) %>%
  as.data.frame() %>%
  dplyr::select(y,ymin,ymax) %>%
  mutate(term = "theta1",
         maineffect = "Native Species Richness",
         group = "Native")

apath <- bind_rows(invrich_apath_df, natrich_apath_df)

# ate
ate_modlist <- list(invasiveresistance = ate_iresist, 
                    invasiveresilience = ate_iresil, 
                    invasiverecovery = ate_ireco,
                    nativeresistance = ate_nresist, 
                    nativeresilience = ate_nresil, 
                    nativerecovery = ate_nreco,
                    compositionresistance = ate_cresist, 
                    compositionresilience = ate_cresil, 
                    compositionrecovery = ate_creco) 

atelist <- list()
for (nm in names(ate_modlist)) {
  
  df <- ate_modlist[[nm]]
  
  draws <- as_draws_df(df) %>% 
    as.data.frame() %>% 
    dplyr::select(b_scalepre_invrichness, b_scalepre_natrichness)
  
  median_inv <- median_qi(draws$b_scalepre_invrichness, 
                          .width=c(0.9)) %>%
    as.data.frame() %>%
    dplyr::select(y,ymin,ymax) %>%
    mutate(term = "ate",
           maineffect = "Invasive Species Richness")
  
  median_nat <- median_qi(draws$b_scalepre_natrichness, 
                          .width=c(0.9)) %>%
    as.data.frame() %>%
    dplyr::select(y,ymin,ymax) %>%
    mutate(term = "ate",
           maineffect = "Native Species Richness")
  
  median_df <- bind_rows(median_inv, median_nat) %>% as.data.frame()
  
  atelist[[nm]] <- median_df
}

ate <- list_rbind(atelist, names_to = "Model")


# bpath
delta2_modlist <- list(
  invasiveresistance = inv_resistmodr_fr, 
  invasiveresilience = inv_resilience24_modr_fr, 
  invasiverecovery = inv_recovermodr_fr,
  nativeresistance = nat_resistmodr_fr, 
  nativeresilience = nat_resilience24_modr_fr, 
  nativerecovery = nat_recovermodr_fr,
  compositionresistance = resist_modr_fr, 
  compositionresilience = resil_modr_fr, 
  compositionrecovery = recov_modr_fr
)

deltalist <- list()
for(nm in names(delta2_modlist)) {
  
  df <- delta2_modlist[[nm]]
  
  draws <- as_draws_df(df) %>% 
    as.data.frame() %>% 
    dplyr::select(b_scalepre_invfuncrichness, b_scalepre_natfuncrichness)
  
  median_inv <- median_qi(draws$b_scalepre_invfuncrichness, 
                          .width=c(0.9)) %>%
    as.data.frame() %>%
    dplyr::select(y,ymin,ymax) %>%
    mutate(term = "delta2",
           mediator = "Invasive Functional Richness",
           group = "Invasive")
  
  median_nat <- median_qi(draws$b_scalepre_natfuncrichness, 
                          .width=c(0.9)) %>%
    as.data.frame() %>%
    dplyr::select(y,ymin,ymax) %>%
    mutate(term = "delta2",
           mediator = "Native Functional Richness",
           group = "Native")
  
  median_df <- bind_rows(median_inv, median_nat) %>% as.data.frame()
  
  deltalist[[nm]] <- median_df
}


bpath <- list_rbind(deltalist, names_to = "Model")


# pde 
pde_list <- list()
for(nm in names(delta2_modlist)) {
  
  df <- delta2_modlist[[nm]]
  
  draws <- as_draws_df(df) %>% 
    as.data.frame() %>% 
    dplyr::select(b_scalepre_invrichness, b_scalepre_natrichness)
  
  median_inv <- median_qi(draws$b_scalepre_invrichness, 
                          .width=c(0.9)) %>%
    as.data.frame() %>%
    dplyr::select(y,ymin,ymax) %>%
    mutate(term = "pde",
           maineffect = "Invasive Species Richness")
  
  median_nat <- median_qi(draws$b_scalepre_natrichness, 
                          .width=c(0.9)) %>%
    as.data.frame() %>%
    dplyr::select(y,ymin,ymax) %>%
    mutate(term = "pde",
           maineffect = "Native Species Richness")
  
  median_df <- bind_rows(median_inv, median_nat) %>% as.data.frame()
  
  pde_list[[nm]] <- median_df
}

pde <- list_rbind(pde_list, names_to = "Model")




## Combine 
apath <- apath %>% rename(theta1 = y, theta1ymin = ymin, theta1ymax = ymax) %>% dplyr::select(-term)
bpath <- bpath %>% rename(delta2 = y, delta2ymin = ymin, delta2ymax = ymax) %>% dplyr::select(-term)

ite_pre <- bpath %>% left_join(apath, join_by(group)) 

ite <- ite_pre %>% 
  group_by(Model, mediator, maineffect) %>%
  summarise(y = theta1 * delta2,
            ymin = theta1ymin * delta2ymin,
            ymax = theta1ymax * delta2ymax,
            term = "ite") %>%
  as.data.frame()
  

str(ite)
str(ate)
str(pde)


mediation <- bind_rows(ite, ate, pde) %>% 
  mutate(mediator = ifelse(is.na(mediator) & maineffect == "Invasive Species Richness", 
                           "Invasive Functional Richness", 
                           ifelse(is.na(mediator) & maineffect == "Native Species Richness",
                                  "Native Functional Richness", 
                                  mediator
                                  )))

## Mediation Plot ####
str(mediation)

mediation_df <- mediation %>%
  mutate(term = case_when(
    term == "ite" ~ "Indirect effect of functional richness", 
    term == "pde" ~ "Direct effect of species richness",
    term == "ate" ~ "Total effect of species richness"),
    Stability = case_when(
      str_detect(Model, "resistance") ~ "Resistance",
      str_detect(Model, "resilience") ~ "Resilience",
      str_detect(Model, "recovery") ~ "Recovery"),
    Framework = case_when(
      str_detect(Model,"invasive" ) ~ "Invasive Cover",
      str_detect(Model,"native" ) ~ "Native Cover",
      str_detect(Model,"composition" ) ~ "Composition"),
    nonzero = ifelse(ymin>0 & 
                         ymax>0, "nonzero",
                       ifelse(ymin<0 & ymax<0,
                              "nonzero", "contains.zero"))
  )

mediation_df$term <- factor(mediation_df$term, levels = c("Indirect effect of functional richness",
                                             "Direct effect of species richness",
                                             "Total effect of species richness"))
mediation_df$Stability <- factor(mediation_df$Stability, levels = c("Resistance",
                                                          "Resilience",
                                                          "Recovery"))
mediation_df$Framework <- factor(mediation_df$Framework, levels = c("Composition",
                                                                    "Native Cover",
                                                                    "Invasive Cover"))



supfig12 <- mediation_df %>%
ggplot(aes(x = term, color = Framework, shape = nonzero)) +
  geom_hline(yintercept = 0, lwd=1, linetype = "dashed", color = "gray") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax),
                position = position_dodge(width=0.3), width = 0) +
  geom_point(aes(y = y),
             position = position_dodge(width=0.3), size = 2) +
  scale_shape_manual(values = c(21,16), guide = "none") +
  scale_color_manual(breaks = c("Invasive Cover",
                                "Native Cover",
                                "Composition"),
                     values = c("Invasive Cover"="#EE6677",
                                "Native Cover" ="#228833",
                                "Composition" ="#4477AA")) +
  scale_x_discrete(labels = c(
    "Indirect effect of functional richness" = "Indirect effect of <br> functional richness", 
    "Direct effect of species richness" = "Direct effect of <br> species richness",
    "Total effect of species richness" = "Total effect of <br> species richness")) +
  facet_grid(mediator~Stability) +
  coord_flip() +
  theme_bw() + ylab("Estimate") +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y = element_markdown(size = 14),
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14), 
        legend.position = "right"
        )
  
#

ggsave(plot = supfig12,
       file = "figures/supfig12.png",
       width = 9.25, height = 7.5, unit = c("in"), dpi = 400)







#####