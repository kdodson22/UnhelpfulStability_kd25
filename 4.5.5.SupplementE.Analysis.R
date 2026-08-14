## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 4.5.5 Supplement E Analysis of how turnover relates to patterns of native and invasive functional recovery.

## Purpose: This script tests the hypothesese that:
# a) increases in invasive richness may promote turnover of species, causing functional stability to be maintained, but with some compositional turnover
# b) increases in native richness promote turnover of species, causing reduced compositional stability of the whole community.

## Author: A. Simler-W
## Date: Updated 8/6/2026

library(here)
library(tidyverse)
library(marginaleffects)
library(brms)
library(cowplot)
library(bayesplot)
library(tidybayes)
library(modelr)

#data
# source(here("1.0.Data.Set-up.R"))
# source(here("1.1.RRR.Cover.R"))
# source(here("1.2.RRR.Composition.R"))
# source(here("4.5.4.SupplementF.Data.R"))


### make the turnover data into wideformat to separate losses and gains into their own columns:
turnover_df_wide <- turnover_df %>% pivot_wider(values_from=value,
                                names_from=c(dynamic, invasive_q)) %>%
                    separate(Plot, into = c("Site", "Plot"), sep = "_")

### make "loss" a positive value, rather than a negative one:
turnover_df_wide <- RRR_df %>% left_join(turnover_df_wide, by=c("Site", "Plot")) %>%
                    mutate(loss_invasive=abs(loss_invasive),
                           loss_notinvasive=abs(loss_notinvasive))


### MODEL: 
## Do invasive richness and native richness change the probability that a single invasive species is "lost" from the community?
lossesmod_inv <- brm(loss_invasive|trials(pre_invrichness) ~
                     scale(pre_invrichness) +
                     scale(pre_natrichness) +
                     scale(CHJU) +
                     scale(BRTE) +
                     scale(N_ug.g.instant_2022Spring) +
                     scale(Heatload) +
                       scale(slm.brte) +
                       scale(slm.chju) +
                     scale(slmr.nat) +
                     scale(slmr.inv) +
                     scale(slm.N.s22) +
                     (1 | Site), 
                   family="binomial",
                   data = subset(turnover_df_wide,
                                 Sprayed=="Yes" & pre_invrichness>0),
                   warmup = 1000, iter = 2000, chains = 4, seed = 123,
                   control = list(adapt_delta = 0.999, max_treedepth = 12))

mcmc_plot(lossesmod_inv)



## MODEL:
## Do invasive richness and native richness change the probability that a single NATIVE species is "lost" from the community?
lossesmod_native <- brm(loss_notinvasive|trials(pre_natrichness) ~
                     scale(pre_invrichness) +
                     scale(pre_natrichness) +
                     scale(CHJU) +
                     scale(BRTE) +
                     scale(N_ug.g.instant_2022Spring) +
                    scale(slm.brte) +
                      scale(slm.chju) +
                     scale(Heatload) +
                     scale(slmr.nat) +
                     scale(slmr.inv) +
                     scale(slm.N.s22) +
                     (1 | Site), 
                   family="binomial",
                   data = subset(turnover_df_wide,
                                 Sprayed=="Yes" & pre_natrichness>0),
                   warmup = 1000, iter = 2000, chains = 4, seed = 123,
                   control = list(adapt_delta = 0.999, max_treedepth = 12))

mcmc_plot(lossesmod_native)




## PLOT IT ######
my_labels <- c("Intercept", "Pre-treatment\ninvasive richness", 
               "Pre-treatment\nnative richness", 
               "Pre-treatment\nC.juncea cover",
               "Pre-treatment\nB.tectorum cover",
               "Post-treatment PAN",
               "Site-mean B. tectorum",
               "Site-mean C. juncea",
               "Heat load",
               "Site-mean Invasive richness",
               "Site-mean Native richness",
               "Site-mean PAN",
               "Std. deviation\nfor site-level varying intercept")



turnover_plot_a <- mcmc_plot(lossesmod_inv) + 
  xlab("Effect on probability of invasive species loss\nfollowing disturbance")+ 
  scale_y_discrete(labels = rev(my_labels), limits = rev)

turnover_plot_a

turnover_plot_b<- mcmc_plot(lossesmod_native) + 
  xlab("Effect on probability of native species loss\nfollowing disturbance")+ 
  scale_y_discrete(labels = rev(my_labels), limits = rev)

turnover_plot_b


coefplot <- plot_grid(turnover_plot_a, turnover_plot_b, labels=c("a", "b"))

coefplot

 ggsave(plot = coefplot,
        file = "figures/supfig13.png",
        width = 10, height = 5, unit = c("in"), dpi = 400)


