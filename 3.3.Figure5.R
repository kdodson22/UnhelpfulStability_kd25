## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 3.3 Figure 5.

## Purpose: This script generates Figure 5, showing marginal effects of diversity, dominant species, and soil resources on RRR metrics.

## Author: K. Dodson 
## Date: Updated 12/1/2025


library(tidyverse)
library(marginaleffects)
library(here)
library(corrplot)
library(bayesplot)
library(ggdist)
library(ggeffects)
library(ggtext)
library(modelr)
library(tidybayes)
library(cowplot)

#data
# source(here("2.0.Models.RRR.R"))

## Each of the panels below illustrates the marginal effect of a variable of interest on a particular RRR metric, using the predictions function from marginaleffects. This function calculates the effect of variation in the variable of interest on the response, while holding other variables at their means.

## Pre-treatment Invasive Richness ~ Resilience #####
#Invasive Cover Model: 
if_resil_invrich_df <- datagrid(model = inv_resilience24_modr,
                                pre_invrichness = seq_range(invabsdf_soil$pre_invrichness, 100))
if_resil_invrich_pred <- predictions(inv_resilience24_modr, 
                                     newdata = if_resil_invrich_df,
                                     conf_level = 0.5)
if_resil_invrich_pred <- if_resil_invrich_pred %>%
  select(estimate:conf.high, 
         pre_invrichness) %>%
  mutate(mod = "Invasive Cover")

#Native Cover Model: 
nf_resil_invrich_df <- datagrid(model = nat_resilience24_modr,
                                pre_invrichness = seq_range(natabsdf_soil$pre_invrichness, 100))
nf_resil_invrich_pred <- predictions(nat_resilience24_modr, 
                                     newdata = nf_resil_invrich_df,
                                     conf_level = 0.5)
nf_resil_invrich_pred <- nf_resil_invrich_pred %>%
  select(estimate:conf.high, 
         pre_invrichness) %>%
  mutate(mod = "Native Cover")

#Compositional Model:  
cc_resil_invrich_df <- datagrid(model = resil_modr,
                                pre_invrichness = seq_range(RRR_df$pre_invrichness, 100))
cc_resil_invrich_pred <- predictions(resil_modr, 
                                     newdata = cc_resil_invrich_df,
                                     conf_level = 0.5)
cc_resil_invrich_pred <- cc_resil_invrich_pred %>%
  select(estimate:conf.high, 
         pre_invrichness) %>%
  mutate(mod = "Composition")

#Combine 
resil_invrich_preds <- rbind(if_resil_invrich_pred, nf_resil_invrich_pred, cc_resil_invrich_pred)


## Pre-treatment Invasive Richness ~ Recovery #####
#Invasive Cover Model 
if_recov_invrich_df <- datagrid(model = inv_recovermodr,
                                pre_invrichness = seq_range(invabsdf_soil$pre_invrichness, 100))
if_recov_invrich_pred <- predictions(inv_recovermodr, 
                                     newdata = if_recov_invrich_df,
                                     conf_level = 0.5)
if_recov_invrich_pred <- if_recov_invrich_pred %>%
  select(estimate:conf.high, 
         pre_invrichness) %>%
  mutate(mod = "Invasive Cover")

#Native Cover Model
nf_recov_invrich_df <- datagrid(model = nat_recovermodr,
                                pre_invrichness = seq_range(natabsdf_soil$pre_invrichness, 100))
nf_recov_invrich_pred <- predictions(nat_recovermodr, 
                                     newdata = nf_recov_invrich_df,
                                     conf_level = 0.5)
nf_recov_invrich_pred <- nf_recov_invrich_pred %>%
  select(estimate:conf.high, 
         pre_invrichness) %>%
  mutate(mod = "Native Cover")

#Compositional Model 
cc_recov_invrich_df <- datagrid(model = recov_modr,
                                pre_invrichness = seq_range(RRR_df$pre_invrichness, 100))
cc_recov_invrich_pred <- predictions(recov_modr, 
                                     newdata = cc_recov_invrich_df,
                                     conf_level = 0.5)
cc_recov_invrich_pred <- cc_recov_invrich_pred %>%
  select(estimate:conf.high, 
         pre_invrichness) %>%
  mutate(mod = "Composition")

#combine 
recov_invrich_preds <- rbind(if_recov_invrich_pred, nf_recov_invrich_pred, cc_recov_invrich_pred)





## Pre-treatment Native Richness ~ Recovery #####
#Invasive Cover Model
if_resil_natrich_df <- datagrid(model = inv_resilience24_modr,
                                pre_natrichness = seq_range(invabsdf_soil$pre_natrichness, 100))
if_resil_natrich_pred <- predictions(inv_resilience24_modr,
                                     newdata = if_resil_natrich_df,
                                     conf_level = 0.5)
if_resil_natrich_pred <- if_resil_natrich_pred %>%
  select(estimate:conf.high,
         pre_natrichness) %>%
  mutate(mod = "Invasive Cover")

#Native Cover Model
nf_resil_natrich_df <- datagrid(model = nat_resilience24_modr,
                                pre_natrichness = seq_range(natabsdf_soil$pre_natrichness, 100))
nf_resil_natrich_pred <- predictions(nat_resilience24_modr,
                                     newdata = nf_resil_natrich_df,
                                     conf_level = 0.5)
nf_resil_natrich_pred <- nf_resil_natrich_pred %>%
  select(estimate:conf.high,
         pre_natrichness) %>%
  mutate(mod = "Native Cover")

#Compositional Model
cc_resil_natrich_df <- datagrid(model = resil_modr,
                                pre_natrichness = seq_range(RRR_df$pre_natrichness, 100))
cc_resil_natrich_pred <- predictions(resil_modr,
                                     newdata = cc_resil_natrich_df,
                                     conf_level = 0.5)
cc_resil_natrich_pred <- cc_resil_natrich_pred %>%
  select(estimate:conf.high,
         pre_natrichness) %>%
  mutate(mod = "Composition")

#combine
resil_natrich_preds <- rbind(if_resil_natrich_pred, nf_resil_natrich_pred, cc_resil_natrich_pred)

## B. tectorum ~ Recovery #####
#Invasive Cover Model
if_recov_brte_df <- datagrid(model = inv_recovermodr,
                             BRTE = seq_range(invabsdf_soil$BRTE, 100))
if_recov_brte_pred <- predictions(inv_recovermodr, 
                                  newdata = if_recov_brte_df,
                                  conf_level = 0.5)
if_recov_brte_pred <- if_recov_brte_pred %>%
  select(estimate:conf.high, 
         BRTE) %>%
  mutate(mod = "Invasive Cover")

#Native Cover Model
nf_recov_brte_df <- datagrid(model = nat_recovermodr,
                             BRTE = seq_range(natabsdf_soil$BRTE, 100))
nf_recov_brte_pred <- predictions(nat_recovermodr, 
                                  newdata = nf_recov_brte_df,
                                  conf_level = 0.5)
nf_recov_brte_pred <- nf_recov_brte_pred %>%
  select(estimate:conf.high, 
         BRTE) %>%
  mutate(mod = "Native Cover")

#Compositional Model
cc_recov_brte_df <- datagrid(model = recov_modr,
                             BRTE = seq_range(RRR_df$BRTE, 100))
cc_recov_brte_pred <- predictions(recov_modr, 
                                  newdata = cc_recov_brte_df,
                                  conf_level = 0.5)
cc_recov_brte_pred <- cc_recov_brte_pred %>%
  select(estimate:conf.high, 
         BRTE) %>%
  mutate(mod = "Composition")

#combine 
recov_brte_preds <- rbind(if_recov_brte_pred, nf_recov_brte_pred, cc_recov_brte_pred)


## C. juncea ~ Resistance #####
#Invasive Cover Model
if_resist_chju_df <- datagrid(model = inv_resistmodr,
                              CHJU = seq_range(invabsdf_soil$CHJU, 100))
if_resist_chju_pred <- predictions(inv_resistmodr, 
                                   newdata = if_resist_chju_df,
                                   conf_level = 0.5)
if_resist_chju_pred <- if_resist_chju_pred %>%
  select(estimate:conf.high, 
         CHJU) %>%
  mutate(mod = "Invasive Cover")

#Native Cover Model 
nf_resist_chju_df <- datagrid(model = nat_resistmodr,
                              CHJU = seq_range(natabsdf_soil$CHJU, 100))
nf_resist_chju_pred <- predictions(nat_resistmodr, 
                                   newdata = nf_resist_chju_df,
                                   conf_level = 0.5)
nf_resist_chju_pred <- nf_resist_chju_pred %>%
  select(estimate:conf.high, 
         CHJU) %>%
  mutate(mod = "Native Cover")

#Compositional Model
cc_resist_chju_df <- datagrid(model = resist_modr,
                              CHJU = seq_range(RRR_df$CHJU, 100))
cc_resist_chju_pred <- predictions(resist_modr, 
                                   newdata = cc_resist_chju_df,
                                   conf_level = 0.5)
cc_resist_chju_pred <- cc_resist_chju_pred %>%
  select(estimate:conf.high, 
         CHJU) %>%
  mutate(mod = "Composition")

#combine 
resist_chju_preds <- rbind(if_resist_chju_pred, nf_resist_chju_pred, cc_resist_chju_pred)


## C. juncea ~ Recovery #####
#Invasive Cover Model
if_recov_chju_df <- datagrid(model = inv_recovermodr,
                             CHJU = seq_range(invabsdf_soil$CHJU, 100))
if_recov_chju_pred <- predictions(inv_recovermodr, 
                                  newdata = if_recov_chju_df,
                                  conf_level = 0.5)
if_recov_chju_pred <- if_recov_chju_pred %>%
  select(estimate:conf.high, 
         CHJU) %>%
  mutate(mod = "Invasive Cover")

#Native Cover Model
nf_recov_chju_df <- datagrid(model = nat_recovermodr,
                             CHJU = seq_range(natabsdf_soil$CHJU, 100))
nf_recov_chju_pred <- predictions(nat_recovermodr, 
                                  newdata = nf_recov_chju_df,
                                  conf_level = 0.5)
nf_recov_chju_pred <- nf_recov_chju_pred %>%
  select(estimate:conf.high, 
         CHJU) %>%
  mutate(mod = "Native Cover")

#Compositional Model
cc_recov_chju_df <- datagrid(model = recov_modr,
                             CHJU = seq_range(RRR_df$CHJU, 100))
cc_recov_chju_pred <- predictions(recov_modr, 
                                  newdata = cc_recov_chju_df,
                                  conf_level = 0.5)
cc_recov_chju_pred <- cc_recov_chju_pred %>%
  select(estimate:conf.high, 
         CHJU) %>%
  mutate(mod = "Composition")

#combine 
recov_chju_preds <- rbind(if_recov_chju_pred, nf_recov_chju_pred, cc_recov_chju_pred)

## POST-PAN ~ Resilience #####
#INVFUN ~ recovery
if_resil_postN_df <- datagrid(model = inv_resilience24_modr,
                              N_ug.g.instant_2022Spring = seq_range(invabsdf_soil$N_ug.g.instant_2022Spring, 100))
if_resil_postN_pred <- predictions(inv_resilience24_modr, 
                                   newdata = if_resil_postN_df,
                                   conf_level = 0.5)
if_resil_postN_pred <- if_resil_postN_pred %>%
  select(estimate:conf.high, 
         N_ug.g.instant_2022Spring) %>%
  mutate(mod = "Invasive Cover")

#NATFUN ~ recovery 
nf_resil_postN_df <- datagrid(model = nat_resilience24_modr,
                              N_ug.g.instant_2022Spring = seq_range(natabsdf_soil$N_ug.g.instant_2022Spring, 100))
nf_resil_postN_pred <- predictions(nat_resilience24_modr, 
                                   newdata = nf_resil_postN_df,
                                   conf_level = 0.5)
nf_resil_postN_pred <- nf_resil_postN_pred %>%
  select(estimate:conf.high, 
         N_ug.g.instant_2022Spring) %>%
  mutate(mod = "Native Cover")


#COMCOM ~ recovery 
cc_resil_postN_df <- datagrid(model = resil_modr,
                              N_ug.g.instant_2022Spring = seq_range(RRR_df$N_ug.g.instant_2022Spring, 100))
cc_resil_postN_pred <- predictions(resil_modr, 
                                   newdata = cc_resil_postN_df,
                                   conf_level = 0.5)
cc_resil_postN_pred <- cc_resil_postN_pred %>%
  select(estimate:conf.high, 
         N_ug.g.instant_2022Spring) %>%
  mutate(mod = "Composition")

#combine 
resil_postN_preds <- rbind(if_resil_postN_pred, nf_resil_postN_pred, cc_resil_postN_pred)


## POST-PAN ~ Recovery #####
#INVFUN ~ recovery
if_recov_postN_df <- datagrid(model = inv_recovermodr,
                              N_ug.g.instant_2022Spring = seq_range(invabsdf_soil$N_ug.g.instant_2022Spring, 100))
if_recov_postN_pred <- predictions(inv_recovermodr, 
                                   newdata = if_recov_postN_df,
                                   conf_level = 0.5)
if_recov_postN_pred <- if_recov_postN_pred %>%
  select(estimate:conf.high, 
         N_ug.g.instant_2022Spring) %>%
  mutate(mod = "Invasive Cover")

#NATFUN ~ recovery 
nf_recov_postN_df <- datagrid(model = nat_recovermodr,
                              N_ug.g.instant_2022Spring = seq_range(natabsdf_soil$N_ug.g.instant_2022Spring, 100))
nf_recov_postN_pred <- predictions(nat_recovermodr, 
                                   newdata = nf_recov_postN_df,
                                   conf_level = 0.5)
nf_recov_postN_pred <- nf_recov_postN_pred %>%
  select(estimate:conf.high, 
         N_ug.g.instant_2022Spring) %>%
  mutate(mod = "Native Cover")


#COMCOM ~ recovery 
cc_recov_postN_df <- datagrid(model = recov_modr,
                              N_ug.g.instant_2022Spring = seq_range(RRR_df$N_ug.g.instant_2022Spring, 100))
cc_recov_postN_pred <- predictions(recov_modr, 
                                   newdata = cc_recov_postN_df,
                                   conf_level = 0.5)
cc_recov_postN_pred <- cc_recov_postN_pred %>%
  select(estimate:conf.high, 
         N_ug.g.instant_2022Spring) %>%
  mutate(mod = "Composition")

#combine 
recov_postN_preds <- rbind(if_recov_postN_pred, nf_recov_postN_pred, cc_recov_postN_pred)


## Marginal Effects Plots #####

#set plotting theme 
metheme <- theme(aspect.ratio = 1/1,
                 legend.position = "none",
                 axis.text.x = ggtext::element_markdown(size = 12),
                 axis.text.y = element_text(size = 12),
                 axis.title.x = ggtext::element_markdown(size = 14),
                 axis.title.y = element_text(size = 14))

# Pre-treatment Invasive Richness ~ Resilience
me_5a <- ggplot(resil_invrich_preds, aes( x = pre_invrichness,
                                          fill = mod, color = mod)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha=0.15, colour = NA) + 
  geom_line(aes(y=estimate), linewidth=1) +
  scale_color_manual(breaks = c("Invasive Cover", 
                                "Native Cover",
                                "Composition"),
                     name=c("Response variable"),
                     values=c("#EE6677", "#228833","#4477AA")) +
  scale_fill_manual(breaks = c("Invasive Cover", 
                               "Native Cover",
                               "Composition"),
                    name=c("Response variable"),
                    values=c("#EE6677", "#228833","#4477AA")) +
  geom_hline(yintercept=0, linetype="dashed") +
  xlab("Pre-treatment<br>Invasive  Richness") +
  ylab("Resilience") +
  theme_bw() + metheme

# Pre-treatment Invasive Richness ~ Recovery
me_5b <- ggplot(recov_invrich_preds, aes( x = pre_invrichness,
                                          fill = mod, color = mod)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha=0.15, colour = NA) + 
  geom_line(aes(y=estimate), linewidth=1) +
  scale_color_manual(breaks = c("Invasive Cover", 
                                "Native Cover",
                                "Composition"),
                     name=c("Response variable"),
                     values=c("#EE6677", "#228833","#4477AA")) +
  scale_fill_manual(breaks = c("Invasive Cover", 
                               "Native Cover",
                               "Composition"),
                    name=c("Response variable"),
                    values=c("#EE6677", "#228833","#4477AA")) +
  geom_hline(yintercept=0, linetype="dashed") +
  xlab("Pre-treatment<br>Invasive  Richness") +
  ylab("Recovery") +
  theme_bw() + metheme

# Pre-treatment Native Richness ~ Resilience
me_5c <- ggplot(resil_natrich_preds, aes( x = pre_natrichness,
                                          fill = mod, color = mod)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha=0.15, colour = NA) + 
  geom_line(aes(y=estimate), linewidth=1) +
  scale_color_manual(breaks = c("Invasive Cover", 
                                "Native Cover",
                                "Composition"),
                     name=c("Response variable"),
                     values=c("#EE6677", "#228833","#4477AA")) +
  scale_fill_manual(breaks = c("Invasive Cover", 
                               "Native Cover",
                               "Composition"),
                    name=c("Response variable"),
                    values=c("#EE6677", "#228833","#4477AA")) +
  geom_hline(yintercept=0, linetype="dashed") +
  xlab("Pre-treatment<br>Native  Richness") +
  ylab("Resilience") + 
  theme_bw() + metheme


# B. tectorum ~ Recovery
me_5d <- ggplot(recov_brte_preds, aes( x = BRTE,
                                       fill = mod, color = mod)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha=0.15, colour = NA) + 
  geom_line(aes(y=estimate), linewidth=1) +
  scale_color_manual(breaks = c("Invasive Cover", 
                                "Native Cover",
                                "Composition"),
                     name=c("Response variable"),
                     values=c("#EE6677", "#228833","#4477AA")) +
  scale_fill_manual(breaks = c("Invasive Cover", 
                               "Native Cover",
                               "Composition"),
                    name=c("Response variable"),
                    values=c("#EE6677", "#228833","#4477AA")) +
  geom_hline(yintercept=0, linetype="dashed") +
  xlab("Pre-treatment<br>*B. tectorum*  Cover (%)") +
  ylab("Recovery") +
  theme_bw() + metheme

# C. juncea ~ Resistance
me_5e <- ggplot(resist_chju_preds, aes( x = CHJU,
                                        fill = mod, color = mod)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha=0.15, colour = NA) + 
  geom_line(aes(y=estimate), linewidth=1) +
  scale_color_manual(breaks = c("Invasive Cover", 
                                "Native Cover",
                                "Composition"),
                     name=c("Response variable"),
                     values=c("#EE6677", "#228833","#4477AA")) +
  scale_fill_manual(breaks = c("Invasive Cover", 
                               "Native Cover",
                               "Composition"),
                    name=c("Response variable"),
                    values=c("#EE6677", "#228833","#4477AA")) +
  geom_hline(yintercept=0, linetype="dashed") +
  xlab("Pre-treatment<br>*C. juncea*  Cover (%)") +
  ylab("Resistance") +
  theme_bw() + metheme

# C. juncea ~ Recovery
me_5f <- ggplot(recov_chju_preds, aes( x = CHJU,
                                       fill = mod, color = mod)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha=0.15, colour = NA) + 
  geom_line(aes(y=estimate), linewidth=1) +
  scale_color_manual(breaks = c("Invasive Cover", 
                                "Native Cover",
                                "Composition"),
                     name=c("Response variable"),
                     values=c("#EE6677", "#228833","#4477AA")) +
  scale_fill_manual(breaks = c("Invasive Cover", 
                               "Native Cover",
                               "Composition"),
                    name=c("Response variable"),
                    values=c("#EE6677", "#228833","#4477AA")) +
  geom_hline(yintercept=0, linetype="dashed") +
  xlab("Pre-treatment<br>*C. juncea*  Cover (%)") +
  ylab("Recovery") +
  theme_bw() + metheme

# Post-treatment PAN ~ Resilience
me_5g <- ggplot(resil_postN_preds, aes( x = N_ug.g.instant_2022Spring,
                                        fill = mod, color = mod)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha=0.15, colour = NA) + 
  geom_line(aes(y=estimate), linewidth=1) +
  scale_color_manual(breaks = c("Invasive Cover", 
                                "Native Cover",
                                "Composition"),
                     name=c("Response variable"),
                     values=c("#EE6677", "#228833","#4477AA")) +
  scale_fill_manual(breaks = c("Invasive Cover", 
                               "Native Cover",
                               "Composition"),
                    name=c("Response variable"),
                    values=c("#EE6677", "#228833","#4477AA")) +
  geom_hline(yintercept=0, linetype="dashed") +
  xlab("Post-treatment PAN <br>") +
  ylab("Resilience") +
  theme_bw() + metheme

# Post-Treatment PAN ~ Recovery
me_5h <- ggplot(recov_postN_preds, aes( x = N_ug.g.instant_2022Spring,
                                        fill = mod, color = mod)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha=0.15, colour = NA) + 
  geom_line(aes(y=estimate), linewidth=1) +
  scale_color_manual(breaks = c("Invasive Cover", 
                                "Native Cover",
                                "Composition"),
                     name=c("Response variable"),
                     values=c("#EE6677", "#228833","#4477AA")) +
  scale_fill_manual(breaks = c("Invasive Cover", 
                               "Native Cover",
                               "Composition"),
                    name=c("Response variable"),
                    values=c("#EE6677", "#228833","#4477AA")) +
  geom_hline(yintercept=0, linetype="dashed") +
  xlab("Post-treatment PAN <br>") +
  ylab("Recovery") +
  theme_bw() + metheme


#get legend 
me_legend <- get_legend(me_5a +
                          guides(color = guide_legend(nrow = 3)) +
                          theme(legend.position = "right",
                                legend.title = element_blank(),
                                legend.text = element_text(size = 12)))

# Combine panels:
figure5me <- plot_grid(me_5a, me_5b, me_5c, 
                       me_5d, me_5e, me_5f, 
                       me_legend, me_5g, me_5h,  
                       nrow = 3, axis = "tblr", align = "hv",
                       labels = c("a","b","c","d","e","f","","g","h"))
figure5me


# ggsave(figure5me, file = "figures/Figure 5.png",
#        width = 9, height = 7.9, dpi = 450)
