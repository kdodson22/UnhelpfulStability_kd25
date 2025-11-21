library(tidyverse)
library(here)
library(ggeffects)
library(cowplot)
library(bayesplot)
library(modelr)
library(ggtext)
library(ggdist)

#data
# source(here("2.2 Correlation models.R"))

## SupFig9. Estimated effect on response of site-level mean control variables and heat load #####
## posterior draws 
recovresid.intercept <- as.data.frame(recovdiff_mod) %>%
  dplyr::select(Intercept,
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
  mutate(Response = "Residual")

recovresid.intercept$parameter <- factor(recovresid.intercept$parameter,
                                  levels = c("Site-Mean Post-treatment PAN",
                                             "Heat load",
                                             "Site-Mean Native Richness",
                                             "Site-Mean Invasive Richness",
                                             "Intercept"))
supfig9 <- ggplot(recovresid.intercept,
       aes(x=estimate, y=parameter)) +
  geom_vline(xintercept=0, color="grey60", linetype=2)+
  stat_pointinterval(.width = c(0.50, 0.90), position=position_dodge(width=0.6)) + 
  ylab("") + xlab("Estimated effect on Response") + 
  scale_y_discrete(labels = c("Site-Mean \n Post-treatment PAN",
                              "Site-Mean \n Pre-treatment PAN",
                              "Heat load",
                              "Site-Mean \n Native Richness",
                              "Site-Mean \n Invasive Richness",
                              "Intercept")) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 12)) 

supfig9 

# ggsave(supfig9, file = "figures/supfig9.png",
#        width = 5, height = 7.5, unit = c("in"), dpi = 450)

##


## SupFig 10. Correlations across frameworks for resistance and resilience. #####
## Invasive x Composition
# RESISTANCE 
icresist_df <- datagrid(model = ic_resist_model,
                        comp_resistance = seq_range(cor_df_yes$comp_resistance, 100))
icresist_pred <- predictions(ic_resist_model, 
                             newdata = icresist_df,
                             conf_level = 0.9)
icresist_pred <- icresist_pred %>%
  select(estimate:conf.high, 
         comp_resistance) 

# RESILIENCE
icresil_df <- datagrid(model = ic_resil_model,
                       comp_resilience = seq_range(cor_df_yes$comp_resilience, 100))
icresil_pred <- predictions(ic_resil_model, 
                            newdata = icresil_df,
                            conf_level = 0.9)
icresil_pred <- icresil_pred %>%
  select(estimate:conf.high, 
         comp_resilience) 

## Invasive x Native 
# RESISTANCE 
inresist_df <- datagrid(model = in_resist_model,
                        nfunc_resistance = seq_range(cor_df_yes$nfunc_resistance, 100))
inresist_pred <- predictions(in_resist_model, 
                             newdata = inresist_df,
                             conf_level = 0.9)
inresist_pred <- inresist_pred %>%
  select(estimate:conf.high, 
         nfunc_resistance) 

# RESILIENCE 
inresil_df <- datagrid(model = in_resil_model,
                       nfunc_resilience = seq_range(cor_df_yes$nfunc_resilience, 100))
inresil_pred <- predictions(in_resil_model, 
                            newdata = inresil_df,
                            conf_level = 0.9)
inresil_pred <- inresil_pred %>%
  select(estimate:conf.high, 
         nfunc_resilience)

## Plots
corrtheme <- theme(aspect.ratio = 1/1,
                   axis.title = element_text(size = 14),
                   plot.caption = element_text(size = 14)) 

icresist_plot <- ggplot(icresist_pred, aes(x = comp_resistance)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha=0.15, colour = NA) + 
  geom_line(aes(y=estimate), linewidth=1) +
  geom_point(data = cor_df_yes, aes(comp_resistance, ifunc_resistance)) +
  ylab("Invasive Cover") +
  xlab("Compositional") +
  theme_bw() +
  labs(caption = paste("R\u00b2 = ", round(ic_resist_r2[1,1], 2),
                       "\n \u03b2 =", round(ic_resist_coef[2,1], 2))) +
  corrtheme

icresil_plot <- ggplot(icresil_pred, aes(x = comp_resilience)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha=0.15, colour = NA) + 
  geom_line(aes(y=estimate), linewidth=1) +
  geom_point(data = cor_df_yes, aes(comp_resilience, ifunc_resilience)) +
  ylab("Invasive Cover") +
  xlab("Compositional") +
  theme_bw() +
  labs(caption = paste("R\u00b2 = ", round(ic_resil_r2[1,1], 2),
                       "\n \u03b2 =", round(ic_resil_coef[2,1], 2))) +
  corrtheme

inresist_plot <- ggplot(inresist_pred, aes(x = nfunc_resistance)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha=0.15, colour = NA) + 
  geom_line(aes(y=estimate), linewidth=1) +
  geom_point(data = cor_df_yes, aes(nfunc_resistance, ifunc_resistance)) +
  ylab("Invasive Cover") +
  xlab("Native Cover") +
  theme_bw()  +
  labs(caption = paste("R\u00b2 = ", round(in_resist_r2[1,1], 2),
                       "\n \u03b2 =", round(in_resist_coef[2,1], 2))) +
  corrtheme

inresil_plot <- ggplot(inresil_pred, aes(x = nfunc_resilience)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha=0.15, colour = NA) + 
  geom_line(aes(y=estimate), linewidth=1) +
  geom_point(data = cor_df_yes, aes(nfunc_resilience, ifunc_resilience)) +
  ylab("Invasive Cover") +
  xlab("Native Cover") +
  theme_bw() +
  labs(caption = paste("R\u00b2 = ", round(in_resil_r2[1,1], 2),
                       "\n \u03b2 =", round(in_resil_coef[2,1], 2))) +
  corrtheme

supfig10 <- plot_grid(icresist_plot, icresil_plot, 
          inresist_plot, inresil_plot, 
          labels = c("a", "b", 
                     "c", "d"),
          nrow = 2)

supfig10

# ggsave(supfig10, file = "figures/supfig10.png",
#        width = 8, height = 7.5, unit = c("in"), dpi = 450)