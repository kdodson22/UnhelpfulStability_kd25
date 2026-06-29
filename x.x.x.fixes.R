library(tidyverse)
library(here)
library(brms)
library(marginaleffects)
library(khroma)
library(patchwork)
library(rtry)

#data
source(here("1.0.Data.Set-up.R"))

#### Species Turnover ####

# presence dataframe for all plots for all years
species_presence <- spp.rel.all %>%
  left_join(pretreat %>% dplyr::select(Site, Plot, Sprayed) %>% 
              mutate(Site = as.integer(Site), Plot = as.integer(Plot)), 
            join_by(Site, Plot)) %>%
  mutate(Treatment = ifelse(Sprayed == "Yes", "Sprayed", "Control"),
         Sprayed = ifelse(Year == 2021, "No", Sprayed)) %>%
  # mutate(ARTR2 = sum(ARTR2 + OARTR), 
  #        ERNA10 = sum(ERNA10 + OERNA),
  #        ELEL5 = sum(ELEL5 + OELEL),
  #        ACMI2 = sum(ACMI2 + OACMI),
  #        PSSP6 = sum(PSSP6 + OPSSP)) %>% dplyr::select(-OARTR, -OERNA, -OPSSP,-OELEL,-OACMI) %>%
  pivot_longer(cols = c(AMSIN:TAOF), names_to = "Species", values_to = "Cover") %>%
  mutate(Present = ifelse(Cover > 0, 1, 0)) %>%
  dplyr::select(-Cover) %>% 
  filter(Present == 1) %>%
  left_join(plantattribs %>% dplyr::select(Species, Scientific, Native_Exotic_Planted, Invasive), join_by(Species)) %>%
  mutate(Plot = paste (Site, Plot, sep = "_"))


species_splitplots <- split(species_presence, species_presence$Plot)


species_plotslist <- list()
for(nm in names(species_splitplots)){
  
  df <- species_splitplots[[nm]]
  
  #separate out by year
  #invasive
  yr0.inv <- df %>% filter(Year == 2021 & Invasive == "Y")
  yr1.inv <- df %>% filter(Year == 2022 & Invasive == "Y")
  yr2.inv <- df %>% filter(Year == 2023 & Invasive == "Y")
  yr3.inv <- df %>% filter(Year == 2024 & Invasive == "Y")
  #native
  yr0.nat <- df %>% filter(Year == 2021 & Invasive == "N")
  yr1.nat <- df %>% filter(Year == 2022 & Invasive == "N")
  yr2.nat <- df %>% filter(Year == 2023 & Invasive == "N")
  yr3.nat <- df %>% filter(Year == 2024 & Invasive == "N")
  
  #pull vectors of unique species for each year
  yr0.invspp <- unique(yr0.inv$Species)
  yr1.invspp <- unique(yr1.inv$Species)
  yr2.invspp <- unique(yr2.inv$Species)
  yr3.invspp <- unique(yr3.inv$Species)
  
  yr0.natspp <- unique(yr0.nat$Species)
  yr1.natspp <- unique(yr1.nat$Species)
  yr2.natspp <- unique(yr2.nat$Species)
  yr3.natspp <- unique(yr3.nat$Species)
  
  
  #sum
  yr1_inv_persist <- sum(yr1.invspp %in% yr0.invspp)
  yr1_inv_gain <- sum(!yr1.invspp %in% yr0.invspp)
  yr1_inv_loss <- sum(!yr0.invspp %in% yr1.invspp)
  yr2_inv_persist <- sum(yr2.invspp %in% yr0.invspp)
  yr2_inv_gain <- sum(!yr2.invspp %in% yr0.invspp)
  yr2_inv_loss <- sum(!yr0.invspp %in% yr2.invspp)
  yr3_inv_persist <- sum(yr3.invspp %in% yr0.invspp)
  yr3_inv_gain <- sum(!yr3.invspp %in% yr0.invspp)
  yr3_inv_loss <- sum(!yr0.invspp %in% yr3.invspp)
  
  yr1_nat_persist <- sum(yr1.natspp %in% yr0.natspp)
  yr1_nat_gain <- sum(!yr1.natspp %in% yr0.natspp)
  yr1_nat_loss <- sum(!yr0.natspp %in% yr1.natspp)
  yr2_nat_persist <- sum(yr2.natspp %in% yr0.natspp)
  yr2_nat_gain <- sum(!yr2.natspp %in% yr0.natspp)
  yr2_nat_loss <- sum(!yr0.natspp %in% yr2.natspp)
  yr3_nat_persist <- sum(yr3.natspp %in% yr0.natspp)
  yr3_nat_gain <- sum(!yr3.natspp %in% yr0.natspp)
  yr3_nat_loss <- sum(!yr0.natspp %in% yr3.natspp)
  
  
  yrx_spp <- data.frame(
    cumulative = c(yr1_inv_persist, yr1_inv_gain, yr1_inv_loss,
                   yr2_inv_persist, yr2_inv_gain, yr2_inv_loss,
                   yr3_inv_persist, yr3_inv_gain, yr3_inv_loss,
                   yr1_nat_persist, yr1_nat_gain, yr1_nat_loss,
                   yr2_nat_persist, yr2_nat_gain, yr2_nat_loss,
                   yr3_nat_persist, yr3_nat_gain, yr3_nat_loss),
    invasive_q = c(rep("invasive", 9), rep("notinvasive", 9)),
    year = rep(c("yr1", "yr2", "yr3"), each = 3, times = 2),
    dynamic = rep(c("persist", "gain", "loss"), each = 1, times = 3)
  )
  
  species_plotslist[[nm]] <- yrx_spp 
  
}


turnoverspp_plot <- list_rbind(species_plotslist, names_to = "Plot")
turnoverspp_plot <- turnoverspp_plot %>% left_join(species_presence %>% 
                                 dplyr::select(Plot, Treatment) %>% distinct(Plot, Treatment), 
                               join_by(Plot))

my.cols <- colour("highcontrast")(3)
my.cols[2] <- "gray90"
names(my.cols) <- c('gain',"persist","loss")
# "#004488" "gray90" "#BB5566"

turnoverspp_plot %>%
  rename(value = cumulative) %>%
  mutate(value=case_when(dynamic=='loss'~value*(-1),
                         .default=value)) %>%
  group_by(Treatment, invasive_q, year, dynamic) %>% summarise(value = mean(value), .groups = "drop") %>%
  mutate(dynamic = fct_relevel(dynamic,c('gain',"persist","loss"))) %>%
  ggplot(aes(x = as.factor(year), y = value)) +
  geom_col(aes(fill=dynamic)) +
  # geom_point(aes(color = inv.dyn, shape = invasive_q), position = position_dodge(width = 0.2)) +
  # geom_boxplot(aes(fill = invasive_q)) +
  scale_fill_manual(values = my.cols,labels=c("Gain","Persist","Loss")) +
  xlab("") + ylab("Average Species (n; by plot)") + theme_bw() +
  theme(legend.position = "bottom",
        # legend.position.inside = c(.1,.85) ,
        legend.title = element_blank())  + facet_grid(Treatment~invasive_q)


turnoverspp_plot %>%
  filter(year == "yr3") %>%
  rename(value = cumulative) %>%
  mutate(value=case_when(dynamic=='loss'~value*(-1),
                         .default=value)) %>%
  group_by(Treatment, invasive_q, dynamic) %>% summarise(value = mean(value), .groups = "drop") %>%
  mutate(dynamic = fct_relevel(dynamic,c('gain',"persist","loss"))) %>%
  ggplot(aes(x = invasive_q, y = value)) +
  geom_col(aes(fill=dynamic)) +
  # geom_point(aes(color = inv.dyn, shape = invasive_q), position = position_dodge(width = 0.2)) +
  # geom_boxplot(aes(fill = invasive_q)) +
  scale_fill_manual(values = my.cols,labels=c("Gain","Persist","Loss")) +
  xlab("") + ylab("Average Species (n; by plot)") + theme_bw() +
  theme(legend.position = "bottom",
        # legend.position.inside = c(.1,.85) ,
        legend.title = element_blank())  + facet_grid(~Treatment)


turnoverspp_plot %>%
  filter(year == "yr3") %>%
  rename(value = cumulative) %>%
  mutate(value=case_when(dynamic=='loss'~value*(-1),
                         .default=value)) %>%
  # group_by(Treatment, invasive_q, dynamic) %>% summarise(value = mean(value), .groups = "drop") %>%
  mutate(inv.dyn = paste(invasive_q, dynamic, sep = "-")) %>%
  mutate(inv.dyn = fct_relevel(inv.dyn,
                               c("invasive-gain", "notinvasive-gain",
                               "invasive-persist", "notinvasive-persist",
                               "invasive-loss", "notinvasive-loss"))
         ) %>%
  ggplot(aes(x = Treatment, y = value)) +
  geom_boxplot(aes(fill = inv.dyn, alpha = inv.dyn)) +
  scale_fill_manual(values = c("invasive-gain" = "#004488",
                               "invasive-persist" = "#DDAA33",
                               "invasive-loss" = "#BB5566",
                               "notinvasive-gain" = "#004488",
                               "notinvasive-persist" = "#DDAA33",
                               "notinvasive-loss" = "#BB5566")) +
  scale_alpha_manual(values = rep(c(1,0.5),3)) +
  xlab("") + ylab("Average Species (n; by plot)") + theme_bw() +
  theme(legend.position = "bottom",
        # legend.position.inside = c(.1,.85) ,
        legend.title = element_blank()) 

#####

## Turnover model & Marginal Effects Plot####
turnover_df <- turnoverspp_plot %>%
  filter(year == "yr3") %>%
  rename(value = cumulative) %>%
  mutate(value=case_when(dynamic=='loss'~value*(-1),
                         .default=value)) 
turnmod <- brm(value ~ invasive_q * dynamic * Treatment + (1|Plot),
               data = turnover_df, 
               warmup = 500, iter = 1000, chains = 3, seed = 123,
               control = list(adapt_delta = 0.999, max_treedepth = 12),
               cores=3, backend="cmdstanr")
# plot(turnmod, ask = F)
# pp_check(turnmod)
# bayes_R2(turnmod)
# summary(turnmod)
# mcmc_plot(turnmod, variable = "^b_", regex = T) + theme_bw()


turnover_mod_df <- datagrid(model = turnmod,
                            Treatment = c("Control", "Sprayed"),
                            invasive_q = c("invasive", "notinvasive"),
                            dynamic = c("gain", "persist", "loss"))

turnover_mod_pred <- predictions(turnmod, 
                                     newdata = turnover_mod_df,
                                     conf_level = 0.9) %>%
  select(estimate:conf.high, 
         dynamic, Treatment, invasive_q) 

turnover_pred_df <- turnover_mod_pred %>%
  mutate(invasive_q = case_when(
    invasive_q == "invasive" ~ "Invasive Species",
    invasive_q == "notinvasive" ~ "Non-Invasive Species"
  ))
  

supfig13 <- turnover_pred_df %>%
  filter(Treatment == "Sprayed") %>%
ggplot(aes(x = invasive_q, color = dynamic, group = Treatment)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0, lwd = 1, position = position_dodge(width = 0.25)) + 
  geom_point(aes(y=estimate, shape = dynamic), 
             size = 3, position = position_dodge(width = 0.25)) +
  geom_hline(yintercept=0, linetype="dashed", color="gray",lwd=1) +
  scale_color_manual(values = c(
    "gain" = "#004488",
    "persist" = "gray",
    "loss" = "#BB5566"
  )) +
  scale_shape_manual(values = c(
    "gain" = 17,
    "persist" = 15,
    "loss" = 16
  )) +
  theme_bw() + labs(y="Number of Species",x="") + 
  theme(axis.text = element_text(size = 14),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        strip.text = element_text(size = 14),
        legend.position = "bottom",
        legend.position.inside = c(0.1,0.85))

ggsave(plot = supfig13,
       file = "figures/supfig13.png",
       width = 5, height = 6, unit = c("in"), dpi = 400)

#####
# #### Try Plant Trait Database ####
# 
# # Try Data
# try <- rtry_import("data/34607.txt")
# try <- rtry_remove_dup(try)
# tryspplist <- unique(try$AccSpeciesName)
# ourspplist <- species_presence %>% 
#   mutate(GenusSpecies = word(Scientific, 1, 2)) %>%
#   distinct(GenusSpecies) 
# ourspplist <- ourspplist$GenusSpecies
# 
# # species overlaps
# sppintry <- species_presence %>% 
#   mutate(GenusSpecies = word(Scientific, 1, 2)) %>%
#   # distinct(GenusSpecies) %>%
#   filter(GenusSpecies %in% tryspplist) %>%
#   distinct(Species)
# 
# cuttry <- try %>% 
#   filter(AccSpeciesName %in% ourspplist) %>%
#   filter(!is.na(TraitID))
# 
# # rtryexplore <- 
#   rtry_explore(cuttry,
#              AccSpeciesName, DataName,
#              TraitID, TraitName,
#              sortBy = desc(Count))
# 
# cuttry %>%
#   group_by(TraitID, TraitName) %>% distinct(AccSpeciesName) %>% 
#   count() %>% arrange(desc(n)) %>% 
#   rename(Number.of.species.with.Data = n)
# 
# 
# spp.rel.all %>% 
#   pivot_longer(cols = c(AMSIN:TAOF), names_to = "Species", values_to = "RelCov") %>%
#   mutate(Try_Question = ifelse(Species %in% sppintry$Species, "Yes", "No"),
#          Plot = paste(Site, Plot, sep = "_")) %>%
#   filter(RelCov != 0) %>%
#   group_by(Plot, Year, Try_Question) %>%
#   summarise(maxCov = max(RelCov),
#             avgCov = mean(RelCov),
#             minCov = min(RelCov)) %>%
#   group_by(Year, Try_Question) %>%
#   summarise(maxCov = max(maxCov),
#             avgCov = mean(avgCov),
#             minCov = min(minCov),
#             percenttotal = avgCov * 100) 
# 
# 

#####








