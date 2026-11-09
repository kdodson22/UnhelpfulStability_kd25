## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 4.2.3 Supplement B Models:

## Purpose: This script fits an alternate model specification: Estimated effect of diversity, dominance, and soil nitrogen on system stability when restoration-associated native plantings are included in measured of post-treatment native cover and composition

## Author: KED
## Updated: 11/8/2026
######

library(tidyverse)
library(marginaleffects)
library(here)
library(corrplot)
library(bayesplot)
library(ggeffects)
library(modelr)
library(tidybayes)
library(ggtext)

# source(here("4.2.3.SupplementB.Models.R"))

## SupFig x.  Estimated effect of diversity, dominance, and soil nitrogen on system stability when native plantings are included. ####
#Resistance 
intsextra.inv.resist <- inv_resistmodr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2021Fall) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Invasive Cover",
         nonzero = ifelse(.lower>0 & 
                            .upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

intsextra.nat.resist <- nat_resistmodr_extracover %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2021Fall) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Native Cover (ext)",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

intsextra.comp.resist <- resist_modr_extracover %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2021Fall) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Composition (ext)",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))


resist.posteriors <- rbind(intsextra.inv.resist, intsextra.nat.resist, intsextra.comp.resist)

#Resilience 
intsextra.inv.resil <- inv_resilience24_modr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Invasive Cover",
         nonzero = ifelse(.lower>0 & 
                            .upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))


intsextra.nat.resil <- nat_resilience24_modr_extracover %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Native Cover (ext)",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

intsextra.comp.resil <- resil_modr_extracover %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Composition (ext)",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

resil.posteriors <- rbind(intsextra.inv.resil, intsextra.nat.resil, intsextra.comp.resil)

#Recovery
intsextra.inv.reco <- inv_recovermodr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Invasive Cover",
         nonzero = ifelse(.lower>0 & 
                            .upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))


intsextra.nat.reco <- nat_recovermodr_extracover %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Native Cover (ext)",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

intsextra.comp.reco <- recov_modr_extracover %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Composition (ext)",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

reco.posteriors <- rbind(intsextra.inv.reco, intsextra.nat.reco, intsextra.comp.reco)

resist.posteriors$Metric <- "Resistance"
resil.posteriors$Metric <- "Resilience"
reco.posteriors$Metric <- "Recovery"

#combine 
posteriors <- rbind(resist.posteriors, resil.posteriors, reco.posteriors)
#rename variables
posteriors <- posteriors %>%
  mutate(.variable = recode(.variable, 
                            b_scalepre_invrichness = "Pre-treatment Invasive Richness",
                            b_scalepre_natrichness = "Pre-treatment Native Richness",
                            b_scaleCHJU = "Pre-treatment C. juncea Cover",
                            b_scaleBRTE = "Pre-treatment B. tectorum Cover",
                            b_scaleN_ug.g.instant_2021Fall = "Pre-treatment PAN",
                            b_scaleN_ug.g.instant_2022Spring = "Post-treatment PAN"
  ))

#make response a factor to fix order
posteriors$response <- factor(posteriors$response, 
                              levels = c("Composition (ext)", 
                                         "Native Cover (ext)",
                                         "Invasive Cover"))
posteriors$Metric <- factor(posteriors$Metric, 
                            levels = c("Resistance", 
                                       "Resilience",
                                       "Recovery"))
posteriors$.variable <- factor(posteriors$.variable,
                               levels = c("Post-treatment PAN", "Pre-treatment PAN",
                                          "Pre-treatment C. juncea Cover", "Pre-treatment B. tectorum Cover",
                                          "Pre-treatment Native Richness", "Pre-treatment Invasive Richness"))

## Plot #####
supfigx <- ggplot(posteriors, aes(y =.variable,
                                   color=response,
                                   alpha = nonzero,
                                   shape=response)) +
  geom_vline(xintercept=0, linetype="dashed", color="grey60") +
  geom_linerange(aes(xmin = .lower, xmax = .upper), linewidth = 0.7,
                 position=position_dodge(width = 0.5)) +
  geom_point(aes(x=.value, fill = response), size=2.25,
             position=position_dodge(width = 0.5)) +
  scale_shape_manual(breaks = c("Invasive Cover", "Native Cover (ext)",
                                "Composition (ext)"),
                     name=c("Response variable"),
                     values=c(21, 22, 24)) +
  scale_alpha_manual(values=c(0.5, 1), guide="none") +
  scale_fill_manual(breaks = c("Invasive Cover", "Native Cover (ext)",
                               "Composition (ext)"),
                    name=c("Response variable"),
                    values=c("#EE6677", "#228833","#4477AA")) +
  scale_color_manual(breaks = c("Invasive Cover", "Native Cover (ext)",
                                "Composition (ext)"),
                     name=c("Response variable"),
                     values=c("#EE6677", "#228833","#4477AA")) +
  ylab("") +
  xlab("Estimated effect on response") +
  scale_y_discrete(labels = c("Post-treatment<br>PAN   ", 
                              "Pre-treatment<br>PAN  ",
                              "Pre-treatment <br> *C. juncea* Cover", 
                              "Pre-treatment <br> *B. tectorum* Cover",
                              "Pre-treatment <br> Native Richness", 
                              "Pre-treatment <br> Invasive Richness")) +
  theme_bw() + facet_wrap(~Metric, scales="free_x") + 
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_markdown(size = 14),
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14), 
        legend.position = "none",
        legend.justification.inside = c(0.025,0.025)) 

supfigx
