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
mediation <- read.csv("data/mediation_analysis.csv")
#

#### Functional Richness ####

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


#### Correlation ####
corrich <- RRR_df3 %>% ungroup() %>%
  dplyr::select(pre_invrichness, pre_totalrichness, pre_natrichness,
                pre_functionalrichness, pre_invfuncrichness, pre_natfuncrichness,
                pre_NS, pre_NAF, pre_PG, pre_NPF, 
                post_NS, post_NAF, post_PG, post_NPF) %>%
  cor()

corrplot(corrich)

#### Modelling


#####



## MODELS - Mediation (a path) ####
#inv fun richness ~ inv spp richness
invrich_apath <- brm(pre_invfuncrichness ~ pre_invrichness + (1 | Site),
                     family = "lognormal",
                     data = subset(invabsdf_soil3,
                                   Sprayed=="Yes"), 
                     warmup = 1000, iter = 2000, chains = 3, seed = 123,
                     control = list(adapt_delta = 0.999, max_treedepth = 15),
                     cores = 3, backend = "cmdstanr")
# plot(invrich_apath, ask=F)
pp_check(invrich_apath)
summary(invrich_apath)


#nat fun richness ~ nat spp richness
natrich_apath <- brm(pre_natfuncrichness ~ pre_natrichness + (1 | Site),
                     family = "zero_inflated_poisson",
                     data = subset(invabsdf_soil3,
                                   Sprayed=="Yes"), 
                     warmup = 1000, iter = 2000, chains = 4, seed = 123,
                     control = list(adapt_delta = 0.999, max_treedepth = 15),
                     cores = 4, backend = "cmdstanr")
pp_check(natrich_apath)
summary(natrich_apath)
#####




## MODELS - Average Treatment Effect ###
#invasive####
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
pp_check(ate_iresist)
summary(ate_iresist)

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
pp_check(ate_iresil)
summary(ate_iresil)

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
pp_check(ate_ireco)
summary(ate_ireco)


#native####
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
pp_check(ate_nresist)
summary(ate_nresist)

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
pp_check(ate_nresil)
summary(ate_nresil)

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
pp_check(ate_nreco)
summary(ate_nreco)
#composition####
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
pp_check(ate_cresist)
summary(ate_cresist)

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
pp_check(ate_cresil)
summary(ate_cresil)

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
pp_check(ate_creco)
summary(ate_creco)
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



## Coefficient plot
## Extract posterior parameter estimates from each model fit and combine for plotting: ####
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


## Organization and Renaming #####
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
                                          # "Post-treatment Native Native Shrub Cover","Pre-treatment Native Native Shrub Cover",
                                          "Pre-treatment Native Functional Richness", 
                                          "Pre-treatment Invasive Functional Richness", 
                                          "Pre-treatment Native Species Richness", 
                                          "Pre-treatment Invasive Species Richness"
                                          )
                               )

## Plot #####
ggplot(posteriors, aes(y =.variable,
                                  color=response,
                                  alpha = nonzero,
                                  shape=response)) +
  geom_vline(xintercept=0, linetype="dashed", color="grey60") +
  geom_linerange(aes(xmin = .lower, xmax = .upper), lwd = 0.7,
                 position=position_dodge(width = 0.4)) +
  geom_point(aes(x=.value, fill = response), size=2.25,
             position=position_dodge(width = 0.4)) +
  scale_shape_manual(breaks = c("Invasive Cover", "Native Cover",
                                "Composition"),
                     name=c("Response variable"),
                     values=c(21, 22, 24)) +
  scale_alpha_manual(values=c(0.25, 1), guide="none") +
  scale_fill_manual(breaks = c("Invasive Cover", "Native Cover",
                               "Composition"),
                    name=c("Response variable"),
                    values=c("#EE6677", "#228833","#4477AA")) +
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
                              # "Post-treatment <br> Native Native Shrub Cover",
                              # "Pre-treatment <br> Native Native Shrub Cover",
                              "Pre-treatment <br> Native Functional Richness", 
                              "Pre-treatment <br> Invasive Functional Richness", 
                              "Pre-treatment <br> Native Species Richness", 
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


# ggsave(plot = figure4,
#       file = "figures/Figure4.png",
#        width = 8.25, height = 7.5, unit = c("in"), dpi = 400)

#####



## Mediation Plot ####
mediation_df <-  mediation %>%
  pivot_longer(cols = c(ATE, TIE, PDE), names_to = "EffectType", values_to = "Magnitude") %>%
  mutate(EffectType = case_when(
    EffectType == "ATE" ~ "Average",
    EffectType == "TIE" ~ "Mediated",
    EffectType == "PDE" ~ "Direct",
  ),
         ModelFrame = str_extract(Model, "^[^_]+"), 
         ModelFrame = case_when(
           ModelFrame == "Invasive" ~ "Invasive Cover",
           ModelFrame == "Native" ~ "Native Cover",
           ModelFrame == "Compositional" ~ "Composition"),
         ModelFrame = fct_relevel(ModelFrame, c("Invasive Cover", "Native Cover", "Composition")),
         StabilityMetric = str_extract(Model, "[^_]+$"),
         StabilityMetric = fct_relevel(StabilityMetric, c("Resistance", "Resilience", "Recovery"))) %>% 
  relocate(ModelFrame, StabilityMetric, .after = Model)

mediation_df %>%
ggplot(aes(EffectType, Magnitude)) +
  geom_col(aes(fill = ModelFrame), position = position_dodge()) +
  scale_fill_manual(values = c(
    "Invasive Cover"="#EE6677", 
    "Native Cover" ="#228833",
    "Composition" ="#4477AA")) +
  labs(x = "Effect", y = "") +
  facet_grid(Mediator~StabilityMetric) +
  coord_flip() +
  theme_bw() +
  theme(axis.title.x = element_text(size = 14),
        axis.text.y = element_markdown(size = 14),
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 11),
        strip.text = element_text(size = 14), 
        legend.position = "inside",
        legend.title = element_blank(),
        legend.justification.inside = c(0.015,0.025)
        ) 









