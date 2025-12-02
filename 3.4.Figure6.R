## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 3.4 Figure 6.

## Purpose: This script generates Figure 6, showing marginal effects of diversity, dominant species, and soil resources in the deviance analysis, as well as correlations between RRR metrics.

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
# source(here("2.2.Models.Correlation.R"))

## Marginal Effects - Posterior Predictions #####
#get extra metadata
recov_cor_df <- cor_df_yes %>% 
  mutate(Site = as.factor(Site)) %>%
  left_join(invabsdf_soil, join_by(Site, Plot, Sprayed))

## Invasive x Native Recovery
inrecov_df <- datagrid(model = in_recov_model,
                       nfunc_recovery = seq_range(cor_df_yes$nfunc_recovery, 100))
inrecov_pred <- predictions(in_recov_model, 
                            newdata = inrecov_df,
                            conf_level = 0.9)
inrecov_pred <- inrecov_pred %>%
  select(estimate:conf.high, 
         nfunc_recovery)

## Invasive x Compositional Recovery
icrecov_df <- datagrid(model = ic_recov_model,
                       comp_recovery = seq_range(cor_df_yes$comp_recovery, 100))
icrecov_pred <- predictions(ic_recov_model, 
                            newdata = icrecov_df,
                            conf_level = 0.9)
icrecov_pred <- icrecov_pred %>%
  select(estimate:conf.high, 
         comp_recovery)

##Deviance Models 
#invasive richness
recovdiff_invrich <- datagrid(model = recovdiff_mod,
                              pre_invrichness = seq_range(recov_cor_df$pre_invrichness, 100))
recovdiff_invrich_pred <- predictions(recovdiff_mod, 
                                      newdata = recovdiff_invrich,
                                      conf_level = 0.5)
recovdiff_invrich_pred <- recovdiff_invrich_pred %>%
  select(estimate:conf.high, 
         pre_invrichness) %>%
  mutate(mod = "Recovery Difference")


#native richness
recovdiff_natrich <- datagrid(model = recovdiff_mod,
                              pre_natrichness = seq_range(recov_cor_df$pre_natrichness, 100))
recovdiff_natrich_pred <- predictions(recovdiff_mod, 
                                      newdata = recovdiff_natrich,
                                      conf_level = 0.5)
recovdiff_natrich_pred <- recovdiff_natrich_pred %>%
  select(estimate:conf.high, 
         pre_natrichness) %>%
  mutate(mod = "Recovery Difference")


#BRTE
recovdiff_brte <- datagrid(model = recovdiff_mod,
                           BRTE = seq_range(recov_cor_df$BRTE, 100))
recovdiff_brte_pred <- predictions(recovdiff_mod, 
                                   newdata = recovdiff_brte,
                                   conf_level = 0.5)
recovdiff_brte_pred <- recovdiff_brte_pred %>%
  select(estimate:conf.high, 
         BRTE) %>%
  mutate(mod = "Recovery Difference")

#CHJU
recovdiff_chju <- datagrid(model = recovdiff_mod,
                           CHJU = seq_range(recov_cor_df$CHJU, 100))
recovdiff_chju_pred <- predictions(recovdiff_mod, 
                                   newdata = recovdiff_chju,
                                   conf_level = 0.5)
recovdiff_chju_pred <- recovdiff_chju_pred %>%
  select(estimate:conf.high, 
         CHJU) %>%
  mutate(mod = "Recovery Difference")

#PAN
recovdiff_pan <- datagrid(model = recovdiff_mod,
                          N_ug.g.instant_2022Spring = seq_range(recov_cor_df$N_ug.g.instant_2022Spring, 100))
recovdiff_pan_pred <- predictions(recovdiff_mod, 
                                  newdata = recovdiff_pan,
                                  conf_level = 0.5)
recovdiff_pan_pred <- recovdiff_pan_pred %>%
  select(estimate:conf.high, 
         N_ug.g.instant_2022Spring) %>%
  mutate(mod = "Recovery Difference")

## Richness
#combine richness dfs 
recov_invrich_c <- recovdiff_invrich_pred %>% 
  rename(Richness = pre_invrichness) %>%
  mutate(RichType = "Invasive Richness")

recov_natrich_c <- recovdiff_natrich_pred %>% 
  rename(Richness = pre_natrichness) %>%
  mutate(RichType = "Native Richness")

recov_rich_c <- rbind(recov_invrich_c, recov_natrich_c)

## Dominance
#combine dominance dfs
recov_brte_c <- recovdiff_brte_pred %>% 
  rename(Cover = BRTE) %>%
  mutate(Species = "B. tectorum")

recov_chju_c <- recovdiff_chju_pred %>% 
  rename(Cover = CHJU) %>%
  mutate(Species = "C. juncea")

recov_dom_c <- rbind(recov_brte_c, recov_chju_c)


## Marginal Effects - Plots #####
in_recov_color <-
  ggplot(inrecov_pred, aes(x = nfunc_recovery)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha=0.15, colour = NA) +
  geom_line(aes(y=estimate), linewidth=1) +
  geom_point(data = in_recov_df, aes(nfunc_recovery, ifunc_recovery), size = 2) +
  ylab(" 
Invasive Cover Recovery") +
  xlab("Native Cover Recovery") +
  theme_bw() +
  ylim(-3.25, 0.25) +
  annotate("text", x = 1.47, y = -2.855,
           label = paste("R\u00b2 = ", round(in_recov_r2[1,1], 2), 
                         "\n \u03b2 =", round(in_recov_coef[2,1], 2)),
           size = 6) +
  theme(aspect.ratio = 1/1,
        axis.text = element_markdown(size = 14),
        axis.title = element_markdown(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_markdown(size = 16),
        plot.caption = element_text(size = 14),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA))


ic_recov_color <-
  ggplot(icrecov_pred, aes(x = comp_recovery)) +
  annotate("label", x = -1.1, y = -0.8, 
           label = "More 
invasive 
recovery
than
expected",
           fill = NA, hjust = 1, size = 4.5,   label.size = NA) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha=0.15, colour = NA) + 
  annotate("segment", x = -1, y = -1.8, yend = 0, 
           arrow = arrow(length = unit(0.5, "cm"), angle = 35), 
           color = "black", linewidth = 2) +
  geom_line(aes(y=estimate), linewidth=1) +
  geom_point(data = ic_resids_df, aes(comp_recovery, ifunc_recovery, color = residual), size = 2) +
  scale_color_gradient(low = "#4477AA", high = "#CCBB44") +
  ylab(" 
Invasive Cover Recovery") +
  xlab("Compositional Recovery") +
  labs(color = "Residuals") +
  theme_bw() +
  ylim(-3.25, 0.25) +
  annotate("text", x = 0.25, y = -2.855,
           label = paste("R\u00b2 = ", round(ic_recov_r2[1,1], 2), 
                         "\n \u03b2 =", round(ic_recov_coef[2,1], 2)),
           size = 6) +
  theme(aspect.ratio = 1/1,
        axis.text = element_markdown(size = 14),
        axis.title = element_markdown(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_markdown(size = 16),
        plot.caption = element_text(size = 14),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA))


rich_resid_p <-
  ggplot(recov_rich_c, aes(x = Richness, group = RichType)) +
  annotate("label", x = 3.4, y = 0.5, 
           label = "More 
invasive 
recovery
than
expected",
           fill = NA, hjust = 1, size = 4.5,   label.size = NA) +
  annotate("segment", x = 3.65, y = 0, yend = 0.85, 
           arrow = arrow(length = unit(0.5, "cm"), angle = 35), 
           color = "black", size = 2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = RichType),
              alpha=0.15) + 
  geom_line(aes(y=estimate, colour = RichType), linewidth=1) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(breaks = c("Invasive Richness", 
                                "Native Richness"),
                     name=c(""),
                     values=c("#EE6677", "#228833")) +
  scale_fill_manual(breaks = c("Invasive Richness", 
                               "Native Richness"),
                    name=c(""),
                    values=c("#EE6677", "#228833")) +
  xlab("Pre-treatment <br> Richness") +
  ylab("Residual for 
invasive recovery ~ compositional recovery") +
  theme_bw() +
  ylim(-0.75, 1)  +
  theme(aspect.ratio = 1/1,
        axis.text = element_markdown(size = 14),
        axis.title.x = element_markdown(size = 16),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_markdown(size = 14),
        legend.position = "inside",
        legend.position.inside = c(0.6, 0.15),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA)
  )

#plot dominance marginal effects together 
dom_resid_p <-
  ggplot(recov_dom_c, aes(x = Cover, group = Species)) +
  annotate("label", x = .34, y = .5,
           label = "More 
invasive 
recovery
than
expected",
           fill = NA, hjust = 1, size = 4.5,   label.size = NA) +
  annotate("segment", x = 0.365, y = 0, yend = 0.85, 
           arrow = arrow(length = unit(0.5, "cm"), angle = 35), 
           color = "black", size = 2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Species),
              alpha=0.15) + 
  geom_line(aes(y=estimate, colour = Species), linewidth=1) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(breaks = c("B. tectorum", 
                                "C. juncea"),
                     name=c(""),
                     values=c("#66CCEE", "#AA3377")) +
  scale_fill_manual(breaks = c("B. tectorum", 
                               "C. juncea"),
                    name=c(""),
                    values=c("#66CCEE", "#AA3377")) +
  xlab("Pre-treatment Cover <br> of Dominant Invaders") +
  ylab("Residual for 
invasive recovery ~ compositional recovery") +
  theme_bw() + 
  ylim(-0.75, 1)  +
  theme(aspect.ratio = 1/1,
        axis.text = element_text(size = 14),
        axis.title.x = element_markdown(size = 16),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_markdown(size = 14, face = "italic"),
        legend.position = "inside",
        legend.position.inside = c(0.7,0.15),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA)
  )

#grid with both plots
figure6 <- plot_grid(in_recov_color, ic_recov_color,
          rich_resid_p, dom_resid_p, 
          nrow = 2, align = "hv", labels = c("a", "b", "c", "d"),
          label_size = 18)
figure6

# ggsave(figure6, file = "figures/figure6.png",
#        width = 11, height = 9.5, unit = c("in"), dpi = 450, bg="transparent")
