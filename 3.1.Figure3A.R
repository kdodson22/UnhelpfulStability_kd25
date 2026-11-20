## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 3.1 Figure 3A

## Purpose: This script generates Figure 3A, showing posterior parameter estimates for each model

## Author: K. Dodson 
## Date: Updated 11/20/2026 by ASW

library(tidyverse)
library(marginaleffects)
library(here)
library(corrplot)
library(bayesplot)
library(ggdist)
library(ggeffects)
library(modelr)
library(tidybayes)
library(ggtext)

#data
# source(here("2.0.Models.RRR.R"))

# Reload the adjustment set A models:
model_folder <- "data/outputs"
mlist <- list.files(model_folder, pattern = "\\modr.RDS", full.names = T)
models <- setNames(lapply(mlist, readRDS), tools::file_path_sans_ext(basename(mlist)))
for (name in names(models)) {
  assign(name, models[[name]], envir = .GlobalEnv)
}

# Adjustment set B models are labeled with B extension:
mlist <- list.files(model_folder, pattern = "\\modr_B.RDS", full.names = T)
models_B <- setNames(lapply(mlist, readRDS), tools::file_path_sans_ext(basename(mlist)))
for (name in names(models_B)) {
  assign(name, models_B[[name]], envir = .GlobalEnv)
}

## Extract posterior parameter estimates from each model fit and combine for plotting: ####
#Resistance 
ints.inv.resist <- inv_resistmodr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               #b_scaleN_ug.g.instant_2021Fall
               ) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Invasive Cover",
         adjustment = "Plant Composition Hypotheses",
         nonzero = ifelse(.lower>0 & 
                            .upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

ints.nat.resist <- nat_resistmodr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               #b_scaleN_ug.g.instant_2021Fall
               ) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Native Cover",
         adjustment = "Plant Composition Hypotheses",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

ints.comp.resist <- resist_modr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               #b_scaleN_ug.g.instant_2021Fall
               ) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Composition",
         adjustment = "Plant Composition Hypotheses",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))


resist.posteriors <- rbind(ints.inv.resist, ints.nat.resist, ints.comp.resist)

#Resilience 
ints.inv.resil <- inv_resilience24_modr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE
               ) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Invasive Cover",
         adjustment = "Plant Composition Hypotheses",
         nonzero = ifelse(.lower>0 & 
                            .upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

ints.inv.resil.B <- inv_resilience24_modr_B %>%
  gather_draws(b_scaleN_ug.g.instant_2022Spring
  ) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Invasive Cover",
         adjustment = "Resource\nHypothesis",
         nonzero = ifelse(.lower>0 & 
                            .upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))


ints.nat.resil <- nat_resilience24_modr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE
               ) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Native Cover",
         adjustment = "Plant Composition Hypotheses",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

ints.nat.resil.B <- nat_resilience24_modr_B %>%
  gather_draws(b_scaleN_ug.g.instant_2022Spring
  ) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Native Cover",
         adjustment = "Resource\nHypothesis",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

ints.comp.resil <- resil_modr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE
               ) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Composition",
         adjustment="Plant Composition Hypotheses",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

ints.comp.resil.B <- resil_modr_B %>%
  gather_draws(b_scaleN_ug.g.instant_2022Spring
  ) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Composition",
         adjustment="Resource\nHypothesis",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))


resil.posteriors <- rbind(ints.inv.resil, ints.nat.resil, ints.comp.resil,
                          ints.inv.resil.B, ints.nat.resil.B, ints.comp.resil.B)

#Recovery
ints.inv.reco <- inv_recovermodr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE
               ) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Invasive Cover",
         adjustment="Plant Composition Hypotheses",
         nonzero = ifelse(.lower>0 & 
                            .upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

ints.inv.reco.B <- inv_recovermodr_B %>%
  gather_draws(b_scaleN_ug.g.instant_2022Spring
  ) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Invasive Cover",
         adjustment="Resource\nHypothesis",
         nonzero = ifelse(.lower>0 & 
                            .upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))


ints.nat.reco <- nat_recovermodr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               #b_scaleN_ug.g.instant_2022Spring
               ) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Native Cover",
         adjustment="Plant Composition Hypotheses",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

ints.nat.reco.B <- nat_recovermodr_B %>%
  gather_draws(b_scaleN_ug.g.instant_2022Spring
  ) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Native Cover",
         adjustment="Resource\nHypothesis",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

ints.comp.reco <- recov_modr %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               #b_scaleN_ug.g.instant_2022Spring
               ) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Composition",
         adjustment="Plant Composition Hypotheses",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

ints.comp.reco.B <- recov_modr_B %>%
  gather_draws(b_scaleN_ug.g.instant_2022Spring
  ) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Composition",
         adjustment="Resource\nHypothesis",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))


reco.posteriors <- rbind(ints.inv.reco, ints.nat.reco, ints.comp.reco,
                         ints.inv.reco.B, ints.nat.reco.B, ints.comp.reco.B)


## Organization and Renaming #####
resist.posteriors$Metric <- "Resistance"
resil.posteriors$Metric <- "Resilience"
reco.posteriors$Metric <- "Recovery"

#combine 
posteriors <- rbind(resist.posteriors, resil.posteriors, reco.posteriors)
#rename variables
posteriors <- posteriors %>%
  mutate( .variable = recode(.variable, 
                            b_scalepre_invrichness = "Pre-treatment<br>Invasive Richness",
                            b_scalepre_natrichness = "Pre-treatment<br>Native Richness",
                            b_scaleCHJU = "Pre-treatment<br>*C. juncea* Cover",
                            b_scaleBRTE = "Pre-treatment<br>*B. tectorum* Cover",
                            b_scaleN_ug.g.instant_2022Spring = "Post-treatment<br>PAN"
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
                               levels = c("Post-treatment<br>PAN", 
                                          "Pre-treatment<br>*C. juncea* Cover", 
                                          "Pre-treatment<br>*B. tectorum* Cover",
                                          "Pre-treatment<br>Native Richness", 
                                          "Pre-treatment<br>Invasive Richness"))

## Plot #####
figure3A <- ggplot(posteriors, aes(y =.variable,
                                  color=response,
                                  alpha = nonzero,
                                  shape=response)) +
  geom_vline(xintercept=0, linetype="dashed", color="grey60") +
  geom_linerange(aes(xmin = .lower, xmax = .upper), linewidth = 0.7,
                 position=position_dodge(width = 0.5)) +
  geom_point(aes(x=.value, fill = response), size=2.25,
             position=position_dodge(width = 0.5)) +
  scale_shape_manual(breaks = c("Invasive Cover", "Native Cover",
                                "Composition"),
                     name=c("Response variable"),
                     values=c(21, 22, 24)) +
  scale_alpha_manual(values=c(0.5, 1), guide="none") +
  scale_fill_manual(breaks = c("Invasive Cover", "Native Cover",
                               "Composition"),
                    name=c("Response variable"),
                    values=c("#EE6677", "#228833","#4477AA")) +
  scale_color_manual(breaks = c("Invasive Cover", "Native Cover",
                                "Composition"),
                     name=c("Response variable"),
                     values=c("#EE6677", "#228833","#4477AA")) +
  ylab("") +
  xlab("Estimated effect on response") + #xlim(-0.6,0.68) +
  theme_bw() + facet_grid(adjustment~Metric, scales=c("free"), space="free_y") + 
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_markdown(size = 14),
        axis.text.x = element_text(size = 10.5),
        legend.text = element_text(size = 14),
        strip.text.x = element_text(size = 14), 
        strip.background.y = element_blank(),
        strip.text.y = element_blank(),
        legend.position = "none",
        legend.justification.inside = c(0.025,0.025)) 

figure3A

ggsave(plot = figure3A,
       file = "figures/Figure3A.png",
       width = 7.5, height = 4.5, unit = c("in"), dpi = 400)

