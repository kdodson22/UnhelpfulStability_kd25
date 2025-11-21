library(tidyverse)
library(here)
library(brms)

#data
# source(here("1.0.Data.Set-up.R"))
# source(here("1.1.RRR.Cover.R"))
# source(here("1.2.RRR.Composition.R"))
# source(here("2.2.Models.Correlation.R"))

## MODELS: Resistance x Resilience
#invasive cover  
i_resistxresil_model <- brm(ifunc_resilience ~ ifunc_resistance 
                            + (1 | Site), 
                            data = cor_df_yes, 
                            warmup = 1000, iter = 2000, chains = 4, 
                            control = list(adapt_delta = 0.999))

i_resistxresil_coef <- as.data.frame(fixef(i_resistxresil_model))
i_resistxresil_r2 <- as.data.frame(bayes_R2(i_resistxresil_model))

#native cover
n_resistxresil_model <- brm(nfunc_resilience ~ nfunc_resistance
                            + (1 | Site), 
                            data = cor_df_yes, 
                            warmup = 1000, iter = 2000, chains = 4, 
                            control = list(adapt_delta = 0.999))

n_resistxresil_coef <- as.data.frame(fixef(n_resistxresil_model))
n_resistxresil_r2 <- as.data.frame(bayes_R2(n_resistxresil_model))

#composition
c_resistxresil_model <- brm(comp_resilience ~ comp_resistance
                            + (1 | Site), 
                            data = cor_df_yes, 
                            warmup = 1000, iter = 2000, chains = 4, 
                            control = list(adapt_delta = 0.999))

c_resistxresil_coef <- as.data.frame(fixef(c_resistxresil_model))
c_resistxresil_r2 <- as.data.frame(bayes_R2(c_resistxresil_model))

## MODELS: Invasive vs Compositional vs Native Resistance and Resilience
## Invasive vs Compositional
#resistance 
ic_resist_model <- brm(ifunc_resistance ~ comp_resistance 
                       + (1 | Site), 
                       data = cor_df_yes, 
                       warmup = 1000, iter = 2000, chains = 4, 
                       control = list(adapt_delta = 0.999))

ic_resist_coef <- as.data.frame(fixef(ic_resist_model))
ic_resist_r2 <- as.data.frame(bayes_R2(ic_resist_model))

#resilience
ic_resil_model <- brm(ifunc_resilience ~ comp_resilience 
                      + (1 | Site), 
                      data = cor_df_yes, 
                      warmup = 1000, iter = 2000, chains = 4, 
                      control = list(adapt_delta = 0.999))

ic_resil_coef <- as.data.frame(fixef(ic_resil_model))
ic_resil_r2 <- as.data.frame(bayes_R2(ic_resil_model))

#Invasive vs Native
#resistance 
in_resist_model <- brm(ifunc_resistance ~ nfunc_resistance 
                       + (1 | Site), 
                       data = cor_df_yes, 
                       warmup = 1000, iter = 2000, chains = 4, 
                       control = list(adapt_delta = 0.999))

in_resist_coef <- as.data.frame(fixef(in_resist_model))
in_resist_r2 <- as.data.frame(bayes_R2(in_resist_model))

#resilience
in_resil_model <- brm(ifunc_resilience ~ nfunc_resilience 
                      + (1 | Site), 
                      data = cor_df_yes, 
                      warmup = 1000, iter = 2000, chains = 4, 
                      control = list(adapt_delta = 0.999))

in_resil_coef <- as.data.frame(fixef(in_resil_model))
in_resil_r2 <- as.data.frame(bayes_R2(in_resil_model))
