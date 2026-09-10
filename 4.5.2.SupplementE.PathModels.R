## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 4.5.1 Supplement E Data Formatting:

## Purpose:  This script fits the a-path, b-path, and total effect models:

## Author: K. Dodson + ASW
## Date: Updated 8/28/2026


### Mediation analysis:
## Approach: Fit a-path models, describing X -> M path, b-path models that isolate M->Y, and then total effect models (same as in main text) that isolate overall X -> Y effect.


## A PATH MODELS (X->M) ####

#inv fun richness ~ inv spp richness
invrich_apath <- brm(pre_invfuncrichness | trials(5)~ 
                       scale(pre_invrichness) +
                       scale(pre_natrichness) +
                       scale(CHJU) + 
                       scale(BRTE) +
                       scale(N_ug.g.instant_2021Fall) +
                       scale(slm.brte) +
                       scale(slm.chju) +
                       scale(slmr.inv) +
                       scale(slmr.nat) +
                       scale(Heatload) +
                       (1 | Site),
                     family = "binomial",
                     data = subset(invabsdf_soil3,
                                   Sprayed=="Yes"), 
                     warmup = 1000, iter = 2000, chains = 4, seed = 123,
                     control = list(adapt_delta = 0.999, max_treedepth = 15),
                     cores = 3, backend = "cmdstanr")

#nat fun richness ~ nat spp richness
natrich_apath <- brm(pre_natfuncrichness | trials(4)~ 
                       scale(pre_invrichness) +
                       scale(pre_natrichness) +
                       scale(CHJU) + 
                       scale(BRTE) +
                       scale(slm.brte) +
                       scale(slm.chju) +
                       scale(N_ug.g.instant_2021Fall) +
                       scale(slmr.inv) +
                       scale(slmr.nat) +
                       scale(Heatload) +
                       (1 | Site),
                     family = "binomial",
                     data = subset(invabsdf_soil3,
                                   Sprayed=="Yes"), 
                     warmup = 1000, iter = 2000, chains = 4, seed = 123,
                     control = list(adapt_delta = 0.999, max_treedepth = 15),
                     cores = 4, backend = "cmdstanr")


## Total Treatment Effect  MODELS (X->Y total) #######

## THESE HAVE ALREADY BEEN FIT IN MAIN TEXT, using 2.0.Models.RRR.R




## Indirect effect models/B path models M->Y #####
#RESISTANCE based on invasive cover
inv_resistmodr_fr <- brm(LRR.resistance ~ 
                           scale(pre_invrichness) +
                           scale(pre_natrichness) +
                           scale(pre_invfuncrichness) +
                           scale(pre_natfuncrichness) +
                           scale(slm.ifun) +
                           scale(slm.nfun) +
                           scale(slm.brte) +
                           scale(slm.chju) +
                           scale(CHJU) + 
                           scale(BRTE) +
                           scale(N_ug.g.instant_2021Fall) +
                           scale(slmr.inv) +
                           scale(slmr.nat) +
                           scale(Heatload) +
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
                                  scale(slm.brte) +                        
                                  scale(slm.chju) +
                                  scale(slm.ifun) +
                                  scale(slm.nfun) +
                                  scale(CHJU) +
                                  scale(BRTE) +
                                  scale(N_ug.g.instant_2022Spring) +
                                  scale(LRR.resistance) +
                                  scale(slmr.inv) +
                                  scale(slmr.nat) +
                                  scale(Heatload) +
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
                            scale(slm.nfun) +    
                            scale(CHJU) +
                            scale(BRTE) +
                            scale(slm.brte) +
                            scale(slm.chju) +
                            scale(N_ug.g.instant_2022Spring) +
                            scale(slmr.inv) +
                            scale(slmr.nat) +
                            scale(Heatload) +
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
                           scale(slm.brte) +                        scale(slm.chju) +
                           scale(slm.ifun) +
                           scale(slm.nfun) +
                           scale(CHJU) + 
                           scale(BRTE) +
                           scale(N_ug.g.instant_2021Fall) +
                           scale(slmr.inv) +
                           scale(slmr.nat) +
                           scale(Heatload) +
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
                                  scale(slm.brte) +                        
                                  scale(slm.chju) +
                                  scale(CHJU) +
                                  scale(BRTE) +
                                  scale(N_ug.g.instant_2022Spring) +
                                  scale(LRR.resistance) +
                                  scale(slmr.inv) +
                                  scale(slmr.nat) +
                                  scale(Heatload) +
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
                            scale(slm.brte) +                        
                            scale(slm.chju) +
                            scale(CHJU) +
                            scale(BRTE) +
                            scale(N_ug.g.instant_2022Spring) +
                            scale(slmr.inv) +
                            scale(slmr.nat) +
                            scale(Heatload) +
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
                        scale(slm.brte) +                        
                        scale(slm.chju) +
                        scale(BRTE) +
                        scale(CHJU) +
                        scale(N_ug.g.instant_2021Fall) +
                        scale(Heatload) +
                        scale(slmr.nat) +
                        scale(slmr.inv) +
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
                       scale(slm.brte) +                        
                       scale(slm.chju) +
                       scale(BRTE) +
                       scale(CHJU) +
                       scale(N_ug.g.instant_2022Spring) +
                       scale(LRR.resistance) +
                       scale(Heatload) +
                       scale(slmr.nat) +
                       scale(slmr.inv) +
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
                       scale(slm.brte) +                        
                       scale(slm.chju) +
                       scale(CHJU) +
                       scale(BRTE) +
                       scale(N_ug.g.instant_2022Spring) +
                       scale(Heatload) +
                       scale(slmr.nat) +
                       scale(slmr.inv) +
                       (1 | Site), 
                     data = subset(RRR_df3,
                                   Sprayed=="Yes"),
                     warmup = 1000, iter = 2000, chains = 4, seed = 123,
                     control = list(adapt_delta = 0.999, max_treedepth = 15),
                     cores = 4, backend = "cmdstanr")

