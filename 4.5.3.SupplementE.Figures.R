## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 4.5.2 Supplement E Mediated effects figures:

## Purpose:  This script generates the coefficient plots for parsing species vs functional richness effects on RRR metrics, and supplementary figure 13.

## Author: K. Dodson + A. Simler-W
## Date: Updated 8/7/26

## Mediated effects plots -- illustrate total (X->Y), indirect (M->Y), and direct effects (X->Y, conditional on M) ####

## Instead of using the bootstrapping approach, we will communicate uncertainty around estimates using Bayesian posterior draws (yay!)

library(dplyr)
library(tidyr)
library(tidybayes)
library(marginaleffects)
library(ggplot2)

## Plotting mediated effects: #####
# apath - The apaths are on a different link function scale than the main model, unfortunately.

## Need a work around to create "effect" on identity scale:

## 1. Calculate a-path effects ########

get_a_path <- function(model, variable, x0, x1) {
  med <- datagrid(model = model, FUN_numeric = median)
  d0 <- med %>% mutate("{variable}" := x0)
  d1 <- med %>% mutate("{variable}" := x1)
  
  pred0 <- posterior_epred(model, newdata = d0)
  pred1 <- posterior_epred(model, newdata = d1)
  rowMeans(pred1) - rowMeans(pred0)
}

a_inv <- get_a_path(
  model = invrich_apath,
  variable = "pre_invrichness",
  x0 = 2,
  x1 = 4
)

a_nat <- get_a_path(
  model = natrich_apath,
  variable = "pre_natrichness",
  x0 = 1,
  x1 = 3
)


## 2. Function to calculate mediated effects across all the different models #####

get_effects <- function(function_model, total_model,
                        a_path, variable,
                        model_name) {
  
  draws <- as_draws_df(function_model)
  draws_total <- as_draws_df(total_model)
  
  # Names of the mediator and "direct terms"
  mediator_term <- paste0(
    "b_scalepre_",
    ifelse(variable == "Invasive Richness",
           "invfuncrichness", "natfuncrichness"))
  
  direct_term <- paste0(
    "b_scalepre_",
    ifelse(variable == "Invasive Richness",
           "invrichness", "natrichness"))
  
  indirect <- draws[[mediator_term]] * a_path
  direct   <- draws[[direct_term]]
  total    <- draws_total[[direct_term]]
  
  tibble(indirect = indirect,
         direct = direct,
         total = total) %>%
    mutate( Model = model_name, Variable = variable) %>%
    pivot_longer(
      cols = c(indirect, direct, total), names_to = "Effect", values_to = "value")
}


## 3. Organize all the different models' info so we can plot together: ########

model_info <- tribble(
  ~Response,                  ~Model,       ~function_model,                ~total_model,
  
  "Response=Invasive Cover",  "Resistance",  inv_resistmodr_fr,           inv_resistmodr,
  "Response=Invasive Cover",  "Resilience",  inv_resilience24_modr_fr,    inv_resilience24_modr,
  "Response=Invasive Cover",  "Recovery",    inv_recovermodr_fr,          inv_recovermodr,
  
  "Response=Native Cover",    "Resistance",  nat_resistmodr_fr,           nat_resistmodr,
  "Response=Native Cover",    "Resilience",  nat_resilience24_modr_fr,    nat_resilience24_modr,
  "Response=Native Cover",    "Recovery",    nat_recovermodr_fr,          nat_recovermodr,
  
  "Response=Composition",    "Resistance",  resist_modr_fr,              resist_modr,
  "Response=Composition",    "Resilience",  resil_modr_fr,               resil_modr,
  "Response=Composition",    "Recovery",    recov_modr_fr,               recov_modr
)



## 4. Loop over all models and both richness predictors ########

effects <- purrr::map_dfr(seq_len(nrow(model_info)), function(i) {
  
  row <- model_info[i, ]
  
  bind_rows(
    get_effects( function_model = row$function_model[[1]],
                 total_model = row$total_model[[1]],
                 a_path = a_inv,
                 variable = "Invasive Richness",
                 model_name = row$Model),
    
    get_effects(
      function_model = row$function_model[[1]],
      total_model = row$total_model[[1]],
      a_path = a_nat,
      variable = "Native Richness",
      model_name = row$Model)) %>%
    mutate(Response = row$Response)})


## Set factors 

effects <- effects %>%
  mutate(Model = factor(Model,
                        levels = c("Resistance", "Resilience", "Recovery")),
         Response = factor(
           Response,
           levels = c("Response=Invasive Cover","Response=Native Cover","Response=Composition" ) ),
         Variable = factor( Variable,
                            levels = c("Native Richness", "Invasive Richness")))



## 6. Identify effects whose credible interval excludes zero ######

sig_flags <- effects %>% group_by(Model, Variable, Effect, Response) %>%
  summarise(excludes_zero = quantile(value, .05) > 0 |quantile(value, .95) < 0,
            .groups = "drop")



## 7. Plot ####

supfig13 <- effects %>%
  left_join( sig_flags, by = c("Model", "Variable", "Effect", "Response")) %>%
  ggplot(aes( y = Variable, x = value, color = Effect, alpha = excludes_zero)) +
  geom_vline(
    xintercept = 0, lwd = 1,linetype = "dashed", color = "grey60") +
  stat_pointinterval(
    .width = c(.5, .9),
    position = position_dodge(width = 0.5)) +
  scale_alpha_manual(values = c(0.4, 1)) +
  facet_grid(Response ~ Model, scales = "free_x") +
  xlab("Estimate") +
  scale_color_manual(
    name = "Effect type",
    values = c("#d4430f","#2c6cbf", "#492e63"),
    labels = c("Direct effect of species richness",
               "Indirect effect via functional richness",
               "Total effect of richness" )) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_blank(),
    axis.text.y = element_markdown(size = 14),
    axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 12),
    legend.position = "bottom"
  ) +
  guides(alpha = "none")

supfig13

ggsave(plot = supfig13,
       file = "figures/supfig13.png",
       width = 12, height = 7.5, unit = c("in"), dpi = 400)

