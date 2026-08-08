## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 4.5.3 Supplement E Mediated effects figures:

## Purpose:  This script generates the coefficient plots for parsing species vs functional richness effects on RRR metrics.

## Author: K. Dodson 
## Date: Updated 8/7/2026

## Mediated effects plots -- illustrate total (X->Y), indirect (M->Y), and direct effects (X->Y, conditional on M) ####


## Plotting mediated effects: #####
# apath - extract draws:
invrich_apath_draws <- as_draws_df(invrich_apath)
natrich_apath_draws <- as_draws_df(natrich_apath)

## only need to extract A paths once for all models below:
a_inv   <- invrich_apath_draws$b_scalepre_invrichness #A path effect of X on M
a_nat   <- natrich_apath_draws$b_scalepre_natrichness #A path effect of X on M

## organized below by: Response variable, stability type, and then focal effect:


## Invasive cover responses:#######

## Invasive cover Resistance: #######
draws <- as_draws_df(inv_resistmodr_fr )# focal model = Resilience of invasive cover
draws_total <- as_draws_df(inv_resistmodr) # model from main text

## Effect of invasive richness on invasive cover resistance:
b  <- draws$b_scalepre_invfuncrichness # B path effect of M on Y
indirect <- b * a_inv  # indirect effect
direct <- draws$b_scalepre_invrichness   # direct effect, per draw
total <-  draws_total$b_scalepre_invrichness  # total effect

resist_inv_effs <- bind_cols(indirect, direct, total)
resist_inv_effs <- resist_inv_effs %>% 
  rename_with(~ c("indirect","direct", "total")) %>%
  mutate(Model = "Resistance",
         Variable = "Invasive Richness") %>% 
  pivot_longer(cols=indirect:total, names_to = "Effect")


## Effect of native richness on invasive cover resistance:
b  <- draws$b_scalepre_natfuncrichness # B path effect of M on Y
indirect <- b * a_nat  # indirect effect
direct <- draws$b_scalepre_natrichness   # direct effect, per draw
total <-  draws_total$b_scalepre_natrichness  # total effect

resist_nat_effs <- bind_cols(indirect, direct, total)
resist_nat_effs <- resist_nat_effs %>% 
  rename_with(~ c("indirect","direct", "total")) %>%
  mutate(Model = "Resistance",
         Variable = "Native Richness") %>% 
  pivot_longer(cols=indirect:total, names_to = "Effect")



## Invasive cover Resilience: #######
draws <- as_draws_df(inv_resilience24_modr_fr) # focal model = Resilience of invasive cover
draws_total <- as_draws_df(inv_resilience24_modr) 

## Effect of invasive richness on invasive cover resil:
b  <- draws$b_scalepre_invfuncrichness # B path effect of M on Y
indirect <- b * a_inv  # indirect effect
direct <- draws$b_scalepre_invrichness   # direct effect, per draw
total <-  draws_total$b_scalepre_invrichness  # total effect

resil_inv_effs <- bind_cols(indirect, direct, total)
resil_inv_effs <- resil_inv_effs %>% 
  rename_with(~ c("indirect","direct", "total")) %>%
  mutate(Model = "Resilience",
         Variable = "Invasive Richness") %>% 
  pivot_longer(cols=indirect:total, names_to = "Effect")



## Effect of native richness on invasive cover resil:
b  <- draws$b_scalepre_natfuncrichness # B path effect of M on Y
indirect <- b * a_nat  # indirect effect
direct <- draws$b_scalepre_natrichness   # direct effect, per draw
total <-  draws_total$b_scalepre_natrichness  # total effect

resil_nat_effs <- bind_cols(indirect, direct, total)
resil_nat_effs <- resil_nat_effs %>% 
  rename_with(~ c("indirect","direct", "total")) %>%
  mutate(Model = "Resilience",
         Variable = "Native Richness") %>% 
  pivot_longer(cols=indirect:total, names_to = "Effect")


## Invasive cover Recovery: #######
draws <- as_draws_df(inv_recovermodr_fr) # focal model = Resilience of invasive cover
draws_total <- as_draws_df(inv_recovermodr) 

## Effect of invasive richness on invasive cover recovery:
b  <- draws$b_scalepre_invfuncrichness # B path effect of M on Y
indirect <- b * a_inv  # indirect effect
direct <- draws$b_scalepre_invrichness   # direct effect, per draw
total <-  draws_total$b_scalepre_invrichness  # total effect

recov_inv_effs <- bind_cols(indirect, direct, total)
recov_inv_effs <- recov_inv_effs %>% 
  rename_with(~ c("indirect","direct", "total")) %>%
  mutate(Model = "Recovery",
         Variable = "Invasive Richness") %>% 
  pivot_longer(cols=indirect:total, names_to = "Effect")



## Effect of native richness on invasive cover recovery:
b  <- draws$b_scalepre_natfuncrichness # B path effect of M on Y
indirect <- b * a_nat  # indirect effect
direct <- draws$b_scalepre_natrichness   # direct effect, per draw
total <-  draws_total$b_scalepre_natrichness  # total effect

recov_nat_effs <- bind_cols(indirect, direct, total)
recov_nat_effs <- recov_nat_effs %>% 
  rename_with(~ c("indirect","direct", "total")) %>%
  mutate(Model = "Recovery",
         Variable = "Native Richness") %>% 
  pivot_longer(cols=indirect:total, names_to = "Effect")


effects_invcovRRR <- bind_rows(recov_inv_effs, recov_nat_effs,
                               resil_inv_effs, resil_nat_effs,
                               resist_inv_effs, resist_nat_effs)
effects_invcovRRR$Response <- "Response=Invasive Cover"




## Native cover responses:#######

## Native cover Resistance: #######
draws <- as_draws_df(nat_resistmodr_fr )# focal model = Resilience of invasive cover
draws_total <- as_draws_df(nat_resistmodr) # model from main text

## Effect of invasive richness on invasive cover resistance:
b  <- draws$b_scalepre_invfuncrichness # B path effect of M on Y
indirect <- b * a_inv  # indirect effect
direct <- draws$b_scalepre_invrichness   # direct effect, per draw
total <-  draws_total$b_scalepre_invrichness  # total effect

resist_inv_effs <- bind_cols(indirect, direct, total)
resist_inv_effs <- resist_inv_effs %>% 
  rename_with(~ c("indirect","direct", "total")) %>%
  mutate(Model = "Resistance",
         Variable = "Invasive Richness") %>% 
  pivot_longer(cols=indirect:total, names_to = "Effect")


## Effect of native richness on invasive cover resistance:
b  <- draws$b_scalepre_natfuncrichness # B path effect of M on Y
indirect <- b * a_nat  # indirect effect
direct <- draws$b_scalepre_natrichness   # direct effect, per draw
total <-  draws_total$b_scalepre_natrichness  # total effect

resist_nat_effs <- bind_cols(indirect, direct, total)
resist_nat_effs <- resist_nat_effs %>% 
  rename_with(~ c("indirect","direct", "total")) %>%
  mutate(Model = "Resistance",
         Variable = "Native Richness") %>% 
  pivot_longer(cols=indirect:total, names_to = "Effect")



## Native cover Resilience: #######
draws <- as_draws_df(nat_resilience24_modr_fr) # focal model = Resilience of invasive cover
draws_total <- as_draws_df(nat_resilience24_modr) 

## Effect of invasive richness on invasive cover resistance:
b  <- draws$b_scalepre_invfuncrichness # B path effect of M on Y
indirect <- b * a_inv  # indirect effect
direct <- draws$b_scalepre_invrichness   # direct effect, per draw
total <-  draws_total$b_scalepre_invrichness  # total effect

resil_inv_effs <- bind_cols(indirect, direct, total)
resil_inv_effs <- resil_inv_effs %>% 
  rename_with(~ c("indirect","direct", "total")) %>%
  mutate(Model = "Resilience",
         Variable = "Invasive Richness") %>% 
  pivot_longer(cols=indirect:total, names_to = "Effect")



## Effect of native richness on invasive cover resistance:
b  <- draws$b_scalepre_natfuncrichness # B path effect of M on Y
indirect <- b * a_nat  # indirect effect
direct <- draws$b_scalepre_natrichness   # direct effect, per draw
total <-  draws_total$b_scalepre_natrichness  # total effect

resil_nat_effs <- bind_cols(indirect, direct, total)
resil_nat_effs <- resil_nat_effs %>% 
  rename_with(~ c("indirect","direct", "total")) %>%
  mutate(Model = "Resilience",
         Variable = "Native Richness") %>% 
  pivot_longer(cols=indirect:total, names_to = "Effect")


## Native cover Recovery: #######
draws <- as_draws_df(nat_recovermodr_fr) # focal model = Resilience of invasive cover
draws_total <- as_draws_df(nat_recovermodr) 

## Effect of invasive richness on invasive cover recovery:
b  <- draws$b_scalepre_invfuncrichness # B path effect of M on Y
indirect <- b * a_inv  # indirect effect
direct <- draws$b_scalepre_invrichness   # direct effect, per draw
total <-  draws_total$b_scalepre_invrichness  # total effect

recov_inv_effs <- bind_cols(indirect, direct, total)
recov_inv_effs <- recov_inv_effs %>% 
  rename_with(~ c("indirect","direct", "total")) %>%
  mutate(Model = "Recovery",
         Variable = "Invasive Richness") %>% 
  pivot_longer(cols=indirect:total, names_to = "Effect")



## Effect of native richness on invasive cover recovery:
b  <- draws$b_scalepre_natfuncrichness # B path effect of M on Y
indirect <- b * a_nat  # indirect effect
direct <- draws$b_scalepre_natrichness   # direct effect, per draw
total <-  draws_total$b_scalepre_natrichness  # total effect

recov_nat_effs <- bind_cols(indirect, direct, total)
recov_nat_effs <- recov_nat_effs %>% 
  rename_with(~ c("indirect","direct", "total")) %>%
  mutate(Model = "Recovery",
         Variable = "Native Richness") %>% 
  pivot_longer(cols=indirect:total, names_to = "Effect")


effects_natcovRRR <- bind_rows(recov_inv_effs, recov_nat_effs,
                               resil_inv_effs, resil_nat_effs,
                               resist_inv_effs, resist_nat_effs)
effects_natcovRRR$Response <- "Response=Native Cover"



## Compositional responses:#######

## Compositional Resistance: #######
draws <- as_draws_df(resist_modr_fr )# focal model = Resilience of invasive cover
draws_total <- as_draws_df(resist_modr) # model from main text

## Effect of invasive richness on invasive cover resistance:
b  <- draws$b_scalepre_invfuncrichness # B path effect of M on Y
indirect <- b * a_inv  # indirect effect
direct <- draws$b_scalepre_invrichness   # direct effect, per draw
total <-  draws_total$b_scalepre_invrichness  # total effect

resist_inv_effs <- bind_cols(indirect, direct, total)
resist_inv_effs <- resist_inv_effs %>% 
  rename_with(~ c("indirect","direct", "total")) %>%
  mutate(Model = "Resistance",
         Variable = "Invasive Richness") %>% 
  pivot_longer(cols=indirect:total, names_to = "Effect")


## Effect of native richness on invasive cover resistance:
b  <- draws$b_scalepre_natfuncrichness # B path effect of M on Y
indirect <- b * a_nat  # indirect effect
direct <- draws$b_scalepre_natrichness   # direct effect, per draw
total <-  draws_total$b_scalepre_natrichness  # total effect

resist_nat_effs <- bind_cols(indirect, direct, total)
resist_nat_effs <- resist_nat_effs %>% 
  rename_with(~ c("indirect","direct", "total")) %>%
  mutate(Model = "Resistance",
         Variable = "Native Richness") %>% 
  pivot_longer(cols=indirect:total, names_to = "Effect")



## Native cover Resilience: #######
draws <- as_draws_df(resil_modr_fr) # focal model = Resilience of invasive cover
draws_total <- as_draws_df(resil_modr) 

## Effect of invasive richness on invasive cover resistance:
b  <- draws$b_scalepre_invfuncrichness # B path effect of M on Y
indirect <- b * a_inv  # indirect effect
direct <- draws$b_scalepre_invrichness   # direct effect, per draw
total <-  draws_total$b_scalepre_invrichness  # total effect

resil_inv_effs <- bind_cols(indirect, direct, total)
resil_inv_effs <- resil_inv_effs %>% 
  rename_with(~ c("indirect","direct", "total")) %>%
  mutate(Model = "Resilience",
         Variable = "Invasive Richness") %>% 
  pivot_longer(cols=indirect:total, names_to = "Effect")



## Effect of native richness on invasive cover resistance:
b  <- draws$b_scalepre_natfuncrichness # B path effect of M on Y
indirect <- b * a_nat  # indirect effect
direct <- draws$b_scalepre_natrichness   # direct effect, per draw
total <-  draws_total$b_scalepre_natrichness  # total effect

resil_nat_effs <- bind_cols(indirect, direct, total)
resil_nat_effs <- resil_nat_effs %>% 
  rename_with(~ c("indirect","direct", "total")) %>%
  mutate(Model = "Resilience",
         Variable = "Native Richness") %>% 
  pivot_longer(cols=indirect:total, names_to = "Effect")


## Native cover Recovery: #######
draws <- as_draws_df(recov_modr_fr) # focal model = Resilience of invasive cover
draws_total <- as_draws_df(recov_modr) 

## Effect of invasive richness on invasive cover recovery:
b  <- draws$b_scalepre_invfuncrichness # B path effect of M on Y
indirect <- b * a_inv  # indirect effect
direct <- draws$b_scalepre_invrichness   # direct effect, per draw
total <-  draws_total$b_scalepre_invrichness  # total effect

recov_inv_effs <- bind_cols(indirect, direct, total)
recov_inv_effs <- recov_inv_effs %>% 
  rename_with(~ c("indirect","direct", "total")) %>%
  mutate(Model = "Recovery",
         Variable = "Invasive Richness") %>% 
  pivot_longer(cols=indirect:total, names_to = "Effect")



## Effect of native richness on invasive cover recovery:
b  <- draws$b_scalepre_natfuncrichness # B path effect of M on Y
indirect <- b * a_nat  # indirect effect
direct <- draws$b_scalepre_natrichness   # direct effect, per draw
total <-  draws_total$b_scalepre_natrichness  # total effect

recov_nat_effs <- bind_cols(indirect, direct, total)
recov_nat_effs <- recov_nat_effs %>% 
  rename_with(~ c("indirect","direct", "total")) %>%
  mutate(Model = "Recovery",
         Variable = "Native Richness") %>% 
  pivot_longer(cols=indirect:total, names_to = "Effect")


effects_compRRR <- bind_rows(recov_inv_effs, recov_nat_effs,
                             resil_inv_effs, resil_nat_effs,
                             resist_inv_effs, resist_nat_effs)
effects_compRRR$Response <- "Response=Composition"


effects <- bind_rows(effects_invcovRRR, effects_natcovRRR, effects_compRRR)

effects$Model <- factor(effects$Model, levels = c("Resistance","Resilience", "Recovery"))
effects$Response <- factor(effects$Response, levels = c("Response=Invasive Cover",
                                                        "Response=Native Cover", 
                                                        "Response=Composition"))

effects$Variable <- factor(effects$Variable, levels = c("Native Richness",
                                                        "Invasive Richness"))


sig_flags <- effects %>%
  group_by(Model, Variable, Effect, Response) %>%
  summarise(excludes_zero = quantile(value, .05) > 0 | quantile(value, .95) < 0,
            .groups = "drop")

supfig12 <- effects %>%
  left_join(sig_flags, by = c("Model", "Variable", "Effect", "Response")) %>%
  ggplot(aes(y = Variable, x = value, color = Effect, alpha=excludes_zero)) +
  geom_vline(xintercept = 0, lwd = 1, linetype = "dashed", color = "grey60") +
  stat_pointinterval(.width = c(.5, .9),
                     position = position_dodge(width = 0.5)) +
  scale_alpha_manual(values=c(0.4, 1))+
  facet_grid(Response~Model, scales="free_x") +
  xlab("Estimate") +
  scale_color_manual(name="Effect type", 
                     values=c("#d4430f", "#2c6cbf", "#492e63"),
                     labels=c("Direct effect of species richness",
                              "Indirect effect via functional richness",
                              "Total effect of richness")) +
  theme_bw() + 
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y = element_markdown(size = 14),
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 12), 
        legend.position = "bottom"
  ) + guides(alpha="none")

supfig12



# ggsave(plot = supfig12,
#        file = "figures/supfig12.png",
#        width = 9.25, height = 7.5, unit = c("in"), dpi = 400)


