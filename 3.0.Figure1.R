## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 3.0 Figure 1 c-e

## Purpose: This script generates Figure 1, showing summary statistics of resistance, resilisence, and recovery metrics for sprayed plots 

## Author: K. Dodson 
## Date: Updated 8/7/2026


library(tidyverse)
library(here)
library(cowplot)

#data
# source(here("1.0.Data.Set-up.R"))
# source(here("1.1.RRR.Cover.R"))
# source(here("1.2.RRR.Composition.R"))


## Figure 1 ##########

## RRR Summary Statistics - Figure 1 #####
sumtheme <- theme(axis.text.x = element_text(size = 14, angle = 45, vjust = 0.6),
                  axis.title.y = element_text(size = 16)) 

p1 <- invabsdf_soil %>% 
  select(Site, Plot, Sprayed, LRR.resistance, LRR.resilience24, LRR.recovery) %>%
  pivot_longer(cols = LRR.resistance:LRR.recovery, names_to = "Metric", values_to = "Score") %>%
  group_by(Sprayed, Metric) %>%
  mutate(MeanScore = mean(Score),
         SEScore = sd(Score),
         Metric = as.factor(Metric)) %>%
  mutate(Metric = fct_relevel(Metric, c("LRR.resistance", "LRR.resilience24", "LRR.recovery"))) %>%
  filter(Sprayed == "Yes") %>%
  ggplot() +
  geom_jitter(aes(Metric, Score),
              size = 1, color = "#EE6677", alpha = 0.5, width = 0.25) +
  geom_point(aes(Metric, MeanScore), 
             size = 4, color= "#EE6677") +
  geom_errorbar(aes(x = Metric , ymin = MeanScore - SEScore, ymax = MeanScore + SEScore),
                width = 0.2, color = "#ab0d20ff", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
  scale_x_discrete(labels = c("Resistance", "Resilience", "Recovery")) +
  ylab("Stability Score") +
  xlab("") +
  theme_bw() +
  sumtheme +
  ylim(-5,5)

p2 <- natabsdf_soil %>% 
  select(Site, Plot, Sprayed, LRR.resistance, LRR.resilience24, LRR.recovery) %>%
  pivot_longer(cols = LRR.resistance:LRR.recovery, names_to = "Metric", values_to = "Score") %>%
  group_by(Sprayed, Metric) %>%
  mutate(MeanScore = mean(Score),
         SEScore = sd(Score),
         Metric = as.factor(Metric)) %>%
  mutate(Metric = fct_relevel(Metric, c("LRR.resistance", "LRR.resilience24", "LRR.recovery"))) %>%
  filter(Sprayed == "Yes") %>%
  ggplot() +
  geom_jitter(aes(Metric, Score),
              size = 1, color = "#228833", alpha = 0.5, width = 0.25) +
  geom_point(aes(Metric, MeanScore), 
             size = 4, color= "#228833") +
  geom_errorbar(aes(x = Metric , ymin = MeanScore - SEScore, ymax = MeanScore + SEScore),
                width = 0.2, color = "#044a10ff", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
  scale_x_discrete(labels = c("Resistance", "Resilience", "Recovery")) +
  ylab("") +
  xlab("") +
  theme_bw() +
  sumtheme +
  ylim(-5,5)

p3 <- RRR_df %>% 
  select(Site, Plot, Sprayed, LRR.resistance, LRR.resilience, LRR.recovery) %>%
  pivot_longer(cols = LRR.resistance:LRR.recovery, names_to = "Metric", values_to = "Score") %>%
  group_by(Sprayed, Metric) %>%
  mutate(MeanScore = mean(Score),
         SEScore = sd(Score),
         Metric = as.factor(Metric)) %>%
  mutate(Metric = fct_relevel(Metric, c("LRR.resistance", "LRR.resilience", "LRR.recovery"))) %>%
  filter(Sprayed == "Yes") %>%
  ggplot() +
  geom_jitter(aes(Metric, Score),
              size = 1, color = "#4477AA", alpha = 0.5, width = 0.25) +
  geom_point(aes(Metric, MeanScore), 
             size = 4, color= "#4477AA") +
  geom_errorbar(aes(x = Metric , ymin = MeanScore - SEScore, ymax = MeanScore + SEScore),
                width = 0.2, color = "#113356ff", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
  scale_x_discrete(labels = c("Resistance", "Resilience", "Recovery")) +
  ylab("") +
  xlab("") +
  theme_bw() +
  sumtheme +
  ylim(-5,5)


figure1ce <- plot_grid(p1, p2, p3, 
                       nrow = 1, align = "hv")

# ggsave(figure1ce, file = "figures/figure1ce.png",
#        width = 9.87, height = 4, units = c("in"), dpi = 300)
