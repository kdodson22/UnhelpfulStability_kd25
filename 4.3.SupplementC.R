## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 4.3. Supplement C

## Purpose:  This script completes posterior predictive checks for Bayesian glmms and plots the effect sizes for statistical control variables (not for causal interpretation) from models examining drivers of RRR:

## Author: K. Dodson 
## Date: Updated 12/1/2025

library(tidyverse)
library(here)
library(ggeffects)
library(cowplot)
library(bayesplot)
library(ggtext)
library(ggdist)

#data
# source(here("2.0.Models.RRR.R"))

## SupFig 6. Posterior parameter estimtates for site-level mean control variables and heat load #####
## posterior draws 
#invasive
invresist.intercept <- as.data.frame(inv_resistmodr) %>%
  dplyr::select(Intercept,
                sigma,
                b_scaleslmr.inv,
                b_scaleslmr.nat,
                b_scaleHeatload,
                b_scaleslm.N.21fall) %>%
  rename("Site-Mean Invasive Richness" = b_scaleslmr.inv,
         "Site-Mean Native Richness" = b_scaleslmr.nat,
         "Heat load" = b_scaleHeatload,
         "Site-Mean Pre-treatment PAN" = b_scaleslm.N.21fall) %>%
  pivot_longer(cols = `Intercept`:`Site-Mean Pre-treatment PAN`, 
               names_to = "parameter", 
               values_to = "estimate") %>%
  mutate(Response = "Resistance")

invresil.intercept <- as.data.frame(inv_resilience24_modr) %>%
  dplyr::select(Intercept,
                sigma,
                b_scaleslmr.inv,
                b_scaleslmr.nat,
                b_scaleHeatload,
                b_scaleslm.N.22sp) %>%
  rename("Site-Mean Invasive Richness" = b_scaleslmr.inv,
         "Site-Mean Native Richness" = b_scaleslmr.nat,
         "Heat load" = b_scaleHeatload,
         "Site-Mean Post-treatment PAN" = b_scaleslm.N.22sp) %>%
  pivot_longer(cols = `Intercept`:`Site-Mean Post-treatment PAN`, 
               names_to = "parameter", 
               values_to = "estimate") %>%
  mutate(Response = "Resilience")

invrecov.intercept <- as.data.frame(inv_recovermodr) %>%
  dplyr::select(Intercept,
                sigma,
                b_scaleslmr.inv,
                b_scaleslmr.nat,
                b_scaleHeatload,
                b_scaleslm.N.22sp) %>%
  rename("Site-Mean Invasive Richness" = b_scaleslmr.inv,
         "Site-Mean Native Richness" = b_scaleslmr.nat,
         "Heat load" = b_scaleHeatload,
         "Site-Mean Post-treatment PAN" = b_scaleslm.N.22sp) %>%
  pivot_longer(cols = `Intercept`:`Site-Mean Post-treatment PAN`, 
               names_to = "parameter", 
               values_to = "estimate") %>%
  mutate(Response = "Recovery")


#native
natresist.intercept <- as.data.frame(nat_resistmodr) %>%
  dplyr::select(Intercept,
                sigma,
                b_scaleslmr.inv,
                b_scaleslmr.nat,
                b_scaleHeatload,
                b_scaleslm.N.21fall) %>%
  rename("Site-Mean Invasive Richness" = b_scaleslmr.inv,
         "Site-Mean Native Richness" = b_scaleslmr.nat,
         "Heat load" = b_scaleHeatload,
         "Site-Mean Pre-treatment PAN" = b_scaleslm.N.21fall) %>%
  pivot_longer(cols = `Intercept`:`Site-Mean Pre-treatment PAN`, 
               names_to = "parameter", 
               values_to = "estimate") %>%
  mutate(Response = "Resistance")

natresil.intercept <- as.data.frame(nat_resilience24_modr) %>%
  dplyr::select(Intercept,
                sigma,
                b_scaleslmr.inv,
                b_scaleslmr.nat,
                b_scaleHeatload,
                b_scaleslm.N.22sp) %>%
  rename("Site-Mean Invasive Richness" = b_scaleslmr.inv,
         "Site-Mean Native Richness" = b_scaleslmr.nat,
         "Heat load" = b_scaleHeatload,
         "Site-Mean Post-treatment PAN" = b_scaleslm.N.22sp) %>%
  pivot_longer(cols = `Intercept`:`Site-Mean Post-treatment PAN`, 
               names_to = "parameter", 
               values_to = "estimate") %>%
  mutate(Response = "Resilience")

natrecov.intercept <- as.data.frame(nat_recovermodr) %>%
  dplyr::select(Intercept,
                sigma,
                b_scaleslmr.inv,
                b_scaleslmr.nat,
                b_scaleHeatload,
                b_scaleslm.N.22sp) %>%
  rename("Site-Mean Invasive Richness" = b_scaleslmr.inv,
         "Site-Mean Native Richness" = b_scaleslmr.nat,
         "Heat load" = b_scaleHeatload,
         "Site-Mean Post-treatment PAN" = b_scaleslm.N.22sp) %>%
  pivot_longer(cols = `Intercept`:`Site-Mean Post-treatment PAN`, 
               names_to = "parameter", 
               values_to = "estimate") %>%
  mutate(Response = "Recovery")

#compositon
resist.intercept <- as.data.frame(resist_modr) %>%
  dplyr::select(Intercept,
                sigma,
                b_scaleslmr.inv,
                b_scaleslmr.nat,
                b_scaleHeatload,
                b_scaleslm.N.f21) %>%
  rename("Site-Mean Invasive Richness" = b_scaleslmr.inv,
         "Site-Mean Native Richness" = b_scaleslmr.nat,
         "Heat load" = b_scaleHeatload,
         "Site-Mean Pre-treatment PAN" = b_scaleslm.N.f21) %>%
  pivot_longer(cols = `Intercept`:`Site-Mean Pre-treatment PAN`, 
               names_to = "parameter", 
               values_to = "estimate") %>%
  mutate(Response = "Resistance")

resil.intercept <- as.data.frame(resil_modr) %>%
  dplyr::select(Intercept,
                sigma,
                b_scaleslmr.inv,
                b_scaleslmr.nat,
                b_scaleHeatload,
                b_scaleslm.N.s22) %>%
  rename("Site-Mean Invasive Richness" = b_scaleslmr.inv,
         "Site-Mean Native Richness" = b_scaleslmr.nat,
         "Heat load" = b_scaleHeatload,
         "Site-Mean Post-treatment PAN" = b_scaleslm.N.s22) %>%
  pivot_longer(cols = `Intercept`:`Site-Mean Post-treatment PAN`, 
               names_to = "parameter", 
               values_to = "estimate") %>%
  mutate(Response = "Resilience")

recov.intercept <- as.data.frame(recov_modr) %>%
  dplyr::select(Intercept,
                sigma,
                b_scaleslmr.inv,
                b_scaleslmr.nat,
                b_scaleHeatload,
                b_scaleslm.N.s22) %>%
  rename("Site-Mean Invasive Richness" = b_scaleslmr.inv,
         "Site-Mean Native Richness" = b_scaleslmr.nat,
         "Heat load" = b_scaleHeatload,
         "Site-Mean Post-treatment PAN" = b_scaleslm.N.s22) %>%
  pivot_longer(cols = `Intercept`:`Site-Mean Post-treatment PAN`, 
               names_to = "parameter", 
               values_to = "estimate") %>%
  mutate(Response = "Recovery")

## combine 
inv.intercepts <- rbind(invresist.intercept, invresil.intercept, invrecov.intercept)
nat.intercepts <- rbind(natresist.intercept, natresil.intercept, natrecov.intercept)
com.intercepts <- rbind(resist.intercept, resil.intercept, recov.intercept)

inv.intercepts$Type <- "Invasive Cover"
nat.intercepts$Type <- "Native Cover"
com.intercepts$Type <- "Composition"

intercepts.df <- inv.intercepts %>% full_join(nat.intercepts) %>% full_join(com.intercepts)

intercepts.df$Response <- factor(intercepts.df$Response,
                                 levels = c( "Resistance", "Resilience", "Recovery"))
intercepts.df$parameter <- factor(intercepts.df$parameter,
                                  levels = c("Site-Mean Post-treatment PAN",
                                             "Site-Mean Pre-treatment PAN",
                                             "Heat load",
                                             "Site-Mean Native Richness",
                                             "Site-Mean Invasive Richness",
                                             "sigma",
                                             "Intercept"))

## plot 
supfig6 <- ggplot(intercepts.df,
       aes(x=estimate, y=parameter, color=Type)) +
  geom_vline(xintercept=0, color="grey60", linetype=2)+
  stat_pointinterval(.width = c(0.50, 0.90), position=position_dodge(width=0.6)) + 
  ylab("") + xlab("Estimated effect on Response") + 
  scale_color_manual(breaks = c("Invasive Cover", "Native Cover", "Composition"),
                     name=c("Response variable"),
                     values=c("#EE6677", "#228833","#4477AA")) +
  scale_y_discrete(labels = c("Site-Mean \n Post-treatment PAN",
                              "Site-Mean \n Pre-treatment PAN",
                              "Heat load",
                              "Site-Mean \n Native Richness",
                              "Site-Mean \n Invasive Richness",
                              "sigma",
                              "Intercept")) +
  facet_grid(~Response) +
  theme_bw() + 
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12), 
        legend.position = "inside",
        legend.justification.inside = c(0.025,0.025) ) 

supfig6

# ggsave(plot = supfig6,
#        file = "figures/supfig6.png",
#        width = 8, height = 8, unit = c("in"), dpi = 450)


## SupFig 7. Posterior predictive checks. #####
pp_theme <-  theme(legend.position = "none",
                   aspect.ratio = 1/1,
                   axis.text = element_text(12))

pp1 <- pp_check(inv_resistmodr) + pp_theme
pp2 <- pp_check(inv_resilience24_modr)  + pp_theme
pp3 <- pp_check(inv_recovermodr)  + pp_theme
pp4 <- pp_check(nat_resistmodr) + pp_theme
pp5 <- pp_check(nat_resilience24_modr) + pp_theme
pp6 <- pp_check(nat_recovermodr) + pp_theme
pp7 <- pp_check(resist_modr) + pp_theme
pp8 <- pp_check(resil_modr) + pp_theme
pp9 <- pp_check(recov_modr) + theme(aspect.ratio = 1/1,
                                    axis.text = element_text(12))

supfig7 <- plot_grid(pp1, pp2, pp3,
          pp4, pp5, pp6,
          pp7, pp8, pp9,
          align = "hv",
          labels = c("a","b"," c",
                     "d","e"," f",
                     "g","h"," i"),
          nrow=3,
          hjust=-2)
supfig7

ggsave(plot = supfig7,
       file = "figures/supfig7.png",
       width = 8, height = 6.5, unit = c("in"), dpi = 450)

## SupFig 8.  Relationship between stability metrics within invasive plant cover stability framework. #####


## you need to run 4.4.1. Supplement D Models for this to work.
## Invasive cover
iresist_df <- datagrid(model = i_resistxresil_model,
                       ifunc_resistance = seq_range(cor_df_yes$ifunc_resistance, 
                                                    100))
iresist_pred <- predictions(i_resistxresil_model, 
                            newdata = iresist_df,
                            conf_level = 0.9)
iresist_pred <- iresist_pred %>%
  select(estimate:conf.high, 
         ifunc_resistance)

## Native cover
nresist_df <- datagrid(model = n_resistxresil_model,
                       nfunc_resistance = seq_range(cor_df_yes$nfunc_resistance, 
                                                    100))
nresist_pred <- predictions(n_resistxresil_model, 
                            newdata = nresist_df,
                            conf_level = 0.9)
nresist_pred <- nresist_pred %>%
  select(estimate:conf.high, 
         nfunc_resistance)


## Composition
cresist_df <- datagrid(model = c_resistxresil_model,
                       comp_resistance = seq_range(cor_df_yes$comp_resistance, 
                                                   100))
cresist_pred <- predictions(c_resistxresil_model, 
                            newdata = cresist_df,
                            conf_level = 0.9)
cresist_pred <- cresist_pred %>%
  select(estimate:conf.high, 
         comp_resistance)


## Plots 
i_resistxresil_plot <- ggplot(iresist_pred, aes(x = ifunc_resistance)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha=0.15, colour = NA) + 
  geom_line(aes(y=estimate), linewidth=1) +
  geom_point(data = cor_df_yes, aes(ifunc_resistance, ifunc_resilience)) +
  xlab("Resistance") +
  ylab("Resilience") +
  theme_bw() +
  labs(caption = paste("R\u00b2 = ", round(i_resistxresil_r2[1,1], 2),
                       "\n \u03b2 =", round(i_resistxresil_coef[2,1], 2))) +
  corrtheme

n_resistxresil_plot <- ggplot(nresist_pred, aes(x = nfunc_resistance)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha=0.15, colour = NA) + 
  geom_line(aes(y=estimate), linewidth=1) +
  geom_point(data = cor_df_yes, aes(nfunc_resistance, nfunc_resilience)) +
  xlab("Resistance") +
  ylab("Resilience") +
  theme_bw() +
  labs(caption = paste("R\u00b2 = ", round(n_resistxresil_r2[1,1], 2),
                       "\n \u03b2 =", round(n_resistxresil_coef[2,1], 2))) +
  corrtheme

c_resistxresil_plot <- ggplot(cresist_pred, aes(x = comp_resistance)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha=0.15, colour = NA) + 
  geom_line(aes(y=estimate), linewidth=1) +
  geom_point(data = cor_df_yes, aes(comp_resistance, comp_resilience)) +
  xlab("Resistance") +
  ylab("Resilience") +
  theme_bw() +
  labs(caption = paste("R\u00b2 = ", round(c_resistxresil_r2[1,1], 2),
                       "\n \u03b2 =", round(c_resistxresil_coef[2,1], 2))) +
  corrtheme

supfig8 <- plot_grid(i_resistxresil_plot, n_resistxresil_plot, c_resistxresil_plot,
                     labels = c("a", "b", "c"), nrow = 1, align = "hv")

ggsave(supfig8, file = "figures/supfig8.png",
       width = 8, height = 4, unit = c("in"), dpi = 450)
