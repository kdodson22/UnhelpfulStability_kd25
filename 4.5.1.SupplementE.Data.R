## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 4.5.1 Supplement E Data Formatting:

## Purpose:  This script generates the functional richness variables and dataframe for mediation analysis of species vs functional richness effects on RRR metrics.

## Author: K. Dodson 
## Date: Updated 8/7/2026

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

natabsdf_soil3 <- natabsdf_soil2 %>% 
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
