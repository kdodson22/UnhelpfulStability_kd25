## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 4.6.3 Supplement F Figures:

## Purpose: This script creates supplemental figure 15: Estimated effect of diversity, dominance, and soil nitrogen on system stability when restoration-associated native plantings are included in measured of post-treatment native cover and composition, and supplemental figure 16:  

## Author: KED
## Updated: 11/11/2026
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

## SupFig x.  Estimated effect of diversity, dominance, and soil nitrogen on native cover and compositional stability when native plantings are included. ####

## Native Cover
#original models 
nat_resistmodr <- readRDS("data/outputs/nat_resistmodr.RDS")
nat_resilience24_modr <- readRDS("data/outputs/nat_resilience24_modr.RDS")
nat_recovermodr <- readRDS("data/outputs/nat_recovermodr.RDS")

#Resistance 
##excluded plantings 
ints.nat.resist <- nat_resistmodr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2021Fall) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Native Cover",
         plantings = "Plantings excluded",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))
##included plantings
intsextra.nat.resist <- nat_resistmodr_extracover %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2021Fall) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Native Cover (ext)",
         plantings = "Plantings included",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

n.resist.posteriors <- rbind(ints.nat.resist, intsextra.nat.resist)

#Resilience
##excluded plantings
ints.nat.resil <- nat_resilience24_modr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Native Cover",
         plantings = "Plantings excluded",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

##included plantings
intsextra.nat.resil <- nat_resilience24_modr_extracover %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Native Cover (ext)",
         plantings = "Plantings included",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

n.resil.posteriors <- rbind(ints.nat.resil, intsextra.nat.resil)


#Recovery
##excluded plantings
ints.nat.reco <- nat_recovermodr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Native Cover",
         plantings = "Plantings excluded",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

##included plantings
intsextra.nat.reco <- nat_recovermodr_extracover %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Native Cover (ext)",
         plantings = "Plantings included",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

n.reco.posteriors <- rbind(ints.nat.reco, intsextra.nat.reco)

#add metric, 
n.resist.posteriors$Metric <- "Resistance"
n.resil.posteriors$Metric <- "Resilience"
n.reco.posteriors$Metric <- "Recovery"
#then combine 
n.posteriors <- rbind(n.resist.posteriors, n.resil.posteriors, n.reco.posteriors)
#rename variables
n.posteriors <- n.posteriors %>%
  mutate(.variable = recode(.variable, 
                            b_scalepre_invrichness = "Pre-treatment Invasive Richness",
                            b_scalepre_natrichness = "Pre-treatment Native Richness",
                            b_scaleCHJU = "Pre-treatment C. juncea Cover",
                            b_scaleBRTE = "Pre-treatment B. tectorum Cover",
                            b_scaleN_ug.g.instant_2021Fall = "Pre-treatment PAN",
                            b_scaleN_ug.g.instant_2022Spring = "Post-treatment PAN"
  ))

#make fix ordering of factors
n.posteriors$plantings <- factor(n.posteriors$plantings, 
                              levels = c("Plantings included",
                                         "Plantings excluded"))
n.posteriors$Metric <- factor(n.posteriors$Metric, 
                            levels = c("Resistance", 
                                       "Resilience",
                                       "Recovery"))
n.posteriors$.variable <- factor(n.posteriors$.variable,
                               levels = c("Post-treatment PAN", "Pre-treatment PAN",
                                          "Pre-treatment C. juncea Cover", "Pre-treatment B. tectorum Cover",
                                          "Pre-treatment Native Richness", "Pre-treatment Invasive Richness"))

supfig15a <- ggplot(n.posteriors, aes(y =.variable,
                                   color=plantings,
                                   alpha = nonzero,
                                   shape=plantings)) +
  geom_vline(xintercept=0, linetype="dashed", color="grey60") +
  geom_linerange(aes(xmin = .lower, xmax = .upper), linewidth = 0.7,
                 position=position_dodge(width = 0.5)) +
  geom_point(aes(x=.value, fill = plantings), size=2.25,
             position=position_dodge(width = 0.5)) +
  
  scale_shape_manual(breaks = c("Plantings excluded",
                                "Plantings included"),
                     name=c("Response variable"),
                     values=c(22,23)) +
  scale_alpha_manual(values=c(0.5, 1), guide="none") +
  scale_fill_manual(breaks = c("Plantings excluded",
                               "Plantings included"),
                    name=c("Response variable"),
                    values=c("#228833","#85d872ff")) +
  scale_color_manual(breaks = c("Plantings excluded",
                                "Plantings included"),
                     name=c("Response variable"),
                     values=c("#228833","#85d872ff")) +
  
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
        axis.text.x = element_text(size = 10.5),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14), 
        legend.position = "inside",
        legend.justification.inside = c(0.025,0.025)) 
#




## Composition
#original models
resist_modr <- readRDS("data/outputs/resist_modr.RDS")
resil_modr<- readRDS("data/outputs/resil_modr.RDS")
recov_modr<- readRDS("data/outputs/recov_modr.RDS")

#Resistance
##plantings excluded
ints.comp.resist <- resist_modr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2021Fall) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Composition",
         plantings = "Plantings excluded",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

##plantings included
intsextra.comp.resist <- resist_modr_extracover %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2021Fall) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Composition (ext)",
         plantings = "Plantings included",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

c.resist.posteriors <- rbind(ints.comp.resist, intsextra.comp.resist)

#Resilience
##plantings excluded
ints.comp.resil <- resil_modr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Composition",
         plantings = "Plantings excluded",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

##plantings included
intsextra.comp.resil <- resil_modr_extracover %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Composition (ext)",
         plantings = "Plantings included",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

c.resil.posteriors <- rbind(ints.comp.resil, intsextra.comp.resil)
#Recovery
##plantings excluded
ints.comp.reco <- recov_modr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Composition",
         plantings = "Plantings excluded",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))
##plantings included
intsextra.comp.reco <- recov_modr_extracover %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Composition (ext)",
         plantings = "Plantings included",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

c.reco.posteriors <- rbind(ints.comp.reco, intsextra.comp.reco)


#add metric, 
c.resist.posteriors$Metric <- "Resistance"
c.resil.posteriors$Metric <- "Resilience"
c.reco.posteriors$Metric <- "Recovery"
#then combine 
c.posteriors <- rbind(c.resist.posteriors, c.resil.posteriors, c.reco.posteriors)
#rename variables
c.posteriors <- c.posteriors %>%
  mutate(.variable = recode(.variable, 
                            b_scalepre_invrichness = "Pre-treatment Invasive Richness",
                            b_scalepre_natrichness = "Pre-treatment Native Richness",
                            b_scaleCHJU = "Pre-treatment C. juncea Cover",
                            b_scaleBRTE = "Pre-treatment B. tectorum Cover",
                            b_scaleN_ug.g.instant_2021Fall = "Pre-treatment PAN",
                            b_scaleN_ug.g.instant_2022Spring = "Post-treatment PAN"
  ))

#make fix ordering of factors
c.posteriors$plantings <- factor(c.posteriors$plantings, 
                                 levels = c("Plantings included",
                                            "Plantings excluded"))
c.posteriors$Metric <- factor(c.posteriors$Metric, 
                              levels = c("Resistance", 
                                         "Resilience",
                                         "Recovery"))
c.posteriors$.variable <- factor(c.posteriors$.variable,
                                 levels = c("Post-treatment PAN", "Pre-treatment PAN",
                                            "Pre-treatment C. juncea Cover", "Pre-treatment B. tectorum Cover",
                                            "Pre-treatment Native Richness", "Pre-treatment Invasive Richness"))

supfig15b <- ggplot(c.posteriors, aes(y =.variable,
                                      color=plantings,
                                      alpha = nonzero,
                                      shape=plantings)) +
  geom_vline(xintercept=0, linetype="dashed", color="grey60") +
  geom_linerange(aes(xmin = .lower, xmax = .upper), linewidth = 0.7,
                 position=position_dodge(width = 0.5)) +
  geom_point(aes(x=.value, fill = plantings), size=2.25,
             position=position_dodge(width = 0.5)) +
  
  scale_shape_manual(breaks = c("Plantings excluded",
                                "Plantings included"),
                     name=c("Response variable"),
                     values=c(24,25)) +
  scale_alpha_manual(values=c(0.5, 1), guide="none") +
  scale_fill_manual(breaks = c("Plantings excluded",
                               "Plantings included"),
                    name=c("Response variable"),
                    values=c("#4477AA","#a1a1e2ff")) +
  scale_color_manual(breaks = c("Plantings excluded",
                                "Plantings included"),
                     name=c("Response variable"),
                     values=c("#4477AA","#a1a1e2ff")) +
  
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
        axis.text.x = element_text(size = 10.5),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14), 
        legend.position = "inside",
        legend.justification.inside = c(0.025,0.025)) 
#

#combine/stack plots 
supfig15 <- plot_grid(supfig15a, supfig15b, 
          labels = c("a", "b"),
          nrow=2)

ggsave(plot = supfig15,
       file = "figures/supfig15.png",
       width = 9.5, height = 9, unit = c("in"), dpi = 400)
