## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 4.2. Supplement B Figures:

## Purpose:  This script generates Supplementary Figure 4: Estimated effect of diversity, dominance, and soil nitrogen and water content on system stability. #####

## Author: K. Dodson 
## Date: Updated 12/1/2025


library(tidyverse)
library(here)
library(ggeffects)
library(cowplot)



## Extract posterior parameter estimates from each model fit and combine for plotting:

# resistance 
ints.inv.resist.wat <- inv_resistmodr_wat %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2021Fall,
               b_scalewatercontent_g.g.instant_2021Fall) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Invasive Cover",
         nonzero = ifelse(.lower>0 & 
                            .upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))


ints.nat.resist.wat <- nat_resistmodr_wat %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2021Fall,
               b_scalewatercontent_g.g.instant_2021Fall) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Native Cover",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

ints.comp.resist.wat <- resist_modr_wat %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2021Fall,
               b_scalewatercontent_g.g.instant_2021Fall) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Composition",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))


resist.posteriors.wat <- rbind(ints.inv.resist.wat, ints.nat.resist.wat, ints.comp.resist.wat)

# resilience 
ints.inv.resil.wat <- inv_resilience24_modr_wat %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring,
               b_scalewatercontent_g.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Invasive Cover",
         nonzero = ifelse(.lower>0 & 
                            .upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))


ints.nat.resil.wat <- nat_resilience24_modr_wat %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring,
               b_scalewatercontent_g.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Native Cover",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

ints.comp.resil.wat <- resil_modr_wat %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring,
               b_scalewatercontent_g.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Composition",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))


resil.posteriors.wat <- rbind(ints.inv.resil.wat, ints.nat.resil.wat, ints.comp.resil.wat)

# recovery 
ints.inv.reco.wat <- inv_recovermodr_wat %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring,
               b_scalewatercontent_g.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Invasive Cover",
         nonzero = ifelse(.lower>0 & 
                            .upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))


ints.nat.reco.wat <- nat_recovermodr_wat %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring,
               b_scalewatercontent_g.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Native Cover",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

ints.comp.reco.wat <- recov_modr_wat %>%
  gather_draws(b_scalepre_invrichness,
               b_scalepre_natrichness,
               b_scaleCHJU,
               b_scaleBRTE,
               b_scaleN_ug.g.instant_2022Spring,
               b_scalewatercontent_g.g.instant_2022Spring) %>%
  median_qi(.width=c(0.9)) %>%
  mutate(response = "Composition",
         nonzero = ifelse(.lower>0 &.upper>0, "nonzero",
                          ifelse(.lower<0 & .upper<0,
                                 "nonzero", "contains.zero")))

reco.posteriors.wat <- rbind(ints.inv.reco.wat, ints.nat.reco.wat, ints.comp.reco.wat)



#give name to each 
resist.posteriors.wat$Metric <- "Resistance"
resil.posteriors.wat$Metric <- "Resilience"
reco.posteriors.wat$Metric <- "Recovery"

#combine 
posteriors.wat <- rbind(resist.posteriors.wat, resil.posteriors.wat, reco.posteriors.wat)
#rename variables
posteriors.wat <-posteriors.wat %>%
  mutate(.variable = recode(.variable, 
                            b_scalepre_invrichness = "Pre-treatment Invasive Richness",
                            b_scalepre_natrichness = "Pre-treatment Native Richness",
                            b_scaleCHJU = "Pre-treatment C. juncea Cover",
                            b_scaleBRTE = "Pre-treatment B. tectorum Cover",
                            b_scaleN_ug.g.instant_2021Fall = "Pre-treatment PAN",
                            b_scaleN_ug.g.instant_2022Spring = "Post-treatment PAN",
                            b_scalewatercontent_g.g.instant_2021Fall = "Pre-treatment WC",
                            b_scalewatercontent_g.g.instant_2022Spring = "Post-treatment WC"
  ))

#make response a factor to fix order
posteriors.wat$response <- factor(posteriors.wat$response, 
                                  levels = c("Composition", 
                                             "Native Cover",
                                             "Invasive Cover"))
posteriors.wat$Metric <- factor(posteriors.wat$Metric, 
                                levels = c("Resistance", 
                                           "Resilience",
                                           "Recovery"))
posteriors.wat$.variable <- factor(posteriors.wat$.variable,
                                   levels = c("Post-treatment WC", "Pre-treatment WC",
                                              "Post-treatment PAN", "Pre-treatment PAN",
                                              "Pre-treatment C. juncea Cover", "Pre-treatment B. tectorum Cover",
                                              "Pre-treatment Native Richness", "Pre-treatment Invasive Richness")) 


#ggplot
supfig4 <- 
ggplot(posteriors.wat, aes(y =.variable,
                           color=response, 
                           alpha=nonzero, shape=nonzero)) +
  geom_vline(xintercept=0, linetype="dashed", color="grey60") +
  ## make the line interval
  geom_linerange(aes(xmin = .lower, xmax = .upper), size = 0.7,
                 position=position_dodge(width = 0.4)) +
  ## make the point interval
  geom_point(aes(x=.value), size=2.25, fill="white",,
             position=position_dodge(width = 0.4)) + 
  scale_shape_manual(values=c(21, 16), guide="none") + 
  scale_alpha_manual(values=c(0.5, 1), guide="none") +
  scale_color_manual(breaks = c("Invasive Cover", "Native Cover",
                                "Composition"),
                     name=c("Response variable"),
                     values=c("#EE6677", "#228833","#4477AA")) +
  ylab("") +
  xlab("Estimated effect on response") +
  scale_y_discrete(labels = c("Post-treatment <br> WC",
                              "Pre-treatment <br> WC",
                              "Post-treatment <br> PAN", 
                              "Pre-treatment <br> PAN",
                              "Pre-treatment <br> *C. juncea* Cover", 
                              "Pre-treatment <br> *B. tectorum* Cover",
                              "Pre-treatment <br> Native Richness", 
                              "Pre-treatment <br> Invasive Richness")) +
  theme_bw() + facet_wrap(~Metric) + 
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_markdown(size = 12, hjust = 0.5),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12), 
        legend.position = "inside",
        legend.justification.inside = c(0.025,0.025) ) 

supfig4

# ggsave(plot = supfig4,
#        file = "figures/supfig4.png",
#        width = 8, height = 7.75, unit = c("in"), dpi = 350)


## SupFig 5. Soil water content treatment effect. #####
supfig5 <-
soildata2 %>%
  mutate(Site = as.integer(Site),
         Plot = as.integer(Plot)) %>%
  left_join(sitedata, by = join_by(Site, Plot)) %>%
  filter(Year_Season == "2021Fall" |
           Year_Season == "2022Spring") %>%
  group_by(Year_Season, Sprayed) %>%
  summarise(WC = mean(watercontent_g.g.instant),
            sd = sd(watercontent_g.g.instant)) %>%
  ggplot(aes(Year_Season, WC)) +
  geom_point(aes(color = Sprayed), size = 4,
             position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(x = Year_Season,
                    ymin = WC - sd, ymax = WC + sd, 
                    color = Sprayed), width = 0.25,
                position = position_dodge(width = 0.25))+
  geom_path(aes(group = Sprayed, color = Sprayed),
            position = position_dodge(width = 0.25)) +
  scale_x_discrete(labels = c("Fall 2021
Pre-treatment",
                              "Spring 2022
Post-treatment")) +
  scale_color_manual(name = "Treatment",
                     labels = c("Control", "Treatment"),
                     values = c("#4477AA", "#CCBB44")) +
  xlab("") + ylab("Soil Water Content 
(g water / g soil)") + ylim(0, 0.08) +
  theme_bw() + theme(aspect.ratio = 1/1,
                     legend.title = element_blank(),
                     legend.position = "inside",
                     legend.position.inside = c(0.8, 0.9),
                     axis.text.x = element_text(size = 16),
                     axis.text.y = element_text(size = 12),
                     legend.text = element_text(size = 16),
                     axis.title =  element_text(size = 16))

 ggsave(plot = supfig5,
       file = "figures/supfig5.png",
       width = 6, height = 6, unit = c("in"), dpi = 450)

