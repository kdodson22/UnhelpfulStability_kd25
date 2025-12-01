## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 4.1 Supplement A:

## Purpose: This script generates material for supplement A, including: Site climate description, trends in relative cover of dominant species across the study period, and a rank abundance plot for plant species observed

## Author: K. Dodson 
## Date: Updated 12/1/2025

library(tidyverse)
library(here)
library(ggeffects)
library(cowplot)

#data
# source(here("1.0.Data.Set-up.R"))

## Calculate precipitation data from PRISM for sites: #####
#get year and month from 'ID'
prism <- prism %>%
  mutate(Year = substr(ID, 4, 7),
         Month = substr(ID, 8, 9)) 

#calculate 'Water Year' (Oct-Sept) and calculate annual ppt
prismannuals <- prism %>%
  mutate(
    Year = as.integer(Year),
    Month = as.integer(Month),
    WaterYear = ifelse(Month >= 10, Year + 1, Year)
  ) %>%
  group_by(Site, Plot, WaterYear) %>%
  summarise(
    Annual_ppt_mm = sum(ppt_mm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Site, WaterYear) %>%
  summarise(Sitemean_annual_ppt_mm = mean(Annual_ppt_mm)) %>%
  group_by(WaterYear) %>%
  summarise(Mean_annual_ppt_mm = mean(Sitemean_annual_ppt_mm))




## SupFig 1. B. tectorum and C. juncea relative cover pre- and post-treatment.  #####
brte_cov <- spp.comp.rel %>%
  rownames_to_column(var = "ObsID") %>%
  separate(col = "ObsID", 
           into = c("Site", "Plot","Year"), 
           sep = "-") %>%
  select(Site, Plot, Year, BRTE)

brte_cov <- pretreat %>% select(Site, Plot, Sprayed) %>%
  mutate(Site = as.character(Site),
         Plot = as.character(Plot)) %>%
  right_join(brte_cov, by = c("Site", "Plot"))

chju_cov <- spp.comp.rel %>%
  rownames_to_column(var = "ObsID") %>%
  separate(col = "ObsID", 
           into = c("Site", "Plot","Year"), 
           sep = "-") %>%
  select(Site, Plot, Year, CHJU)

chju_cov <- pretreat %>% select(Site, Plot, Sprayed) %>%
  mutate(Site = as.character(Site),
         Plot = as.character(Plot)) %>%
  right_join(chju_cov, by = c("Site", "Plot"))

dominants <- full_join(brte_cov, chju_cov) %>%
  pivot_longer(cols = c(5:6), names_to = "Species", values_to = "Cover")

supfig1 <- dominants %>%
  filter(Sprayed == "Yes") %>%
  mutate(Cover = Cover*100) %>%
  group_by(Year, Species) %>%
  mutate(mean = mean(Cover),
         sd = sd(Cover),
         Year = as.numeric(Year)) %>%
  ungroup() %>%
ggplot() +
  geom_point(aes(Year, mean, group = Species, color = Species), 
             position = position_dodge(width = 0.25), size = 4) +
  
  geom_errorbar(aes(Year, ymin = mean - sd, ymax = mean + sd, 
                    group = Species, color = Species),
                position = position_dodge(width = 0.25),
                width = 0.2, linewidth = 1) +
  
  geom_line(aes(Year, mean,
                group = Species, color = Species), 
            position = position_dodge(width = 0.25),
            linewidth = 1) +
  
  scale_color_manual(values = c("#66CCEE", "#AA3377"), labels = c("B. tectorum", "C. juncea"))+
  
  geom_vline(xintercept = 2021.5, 
             linetype = "dashed", color = "darkgray", linewidth = 0.75) +
  annotate("text", label = "restoration 
treatment", 
           x = 2021.55, y = 95, color = "darkgray", size = 5, hjust = 0) +
  ylab("Relative Cover (%)") + ylim(NA, 100) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(face = "italic", size = 14),
        legend.title = element_text(size = 16),
        legend.position = "inside",
        legend.position.inside = c(0.8, 0.88))

supfig1

 ggsave(supfig1, file = "figures/supfig1.png",
       width = 5.5, height = 5, unit = c("in"), dpi = 450)


## SupFig 3. Ranked species rareness based on relative abundance. #####
# SE function
se <- function(x) {
  sd(x) / sqrt(length(x))
}

# String to search for and subset by
search_string <- "2021"
rowstokeep <- grepl(search_string, rownames(spp.comp.rel))
spp.comp.rel2 <- spp.comp.rel[rowstokeep, ] 
spp.comp.rel2$Obs.ID = NULL

# Calculate mean and SE of rel abundance for each species
species_total_abundance <- colMeans(spp.comp.rel2) 
se_values <- sapply(spp.comp.rel2, se)

ranks <- data.frame(species_total_abundance, se_values)
ranks <- ranks %>% filter(species_total_abundance>0)

ranks$species_ranks <- rank(-ranks$species_total_abundance, ties.method = "min")
ranks$Species <- rownames(ranks)

ranks <- ranks %>% left_join(plantattribs) %>%
  group_by(species_ranks) %>%
  mutate(count = n(),
         Species = ifelse(count>1, paste0("Tie:", count, " spp"), Species)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(Invasive = ifelse(count>1, "NA", Invasive))

Otherspecies <- ranks %>% filter(species_ranks> 40)
Otherspecies <- Otherspecies$Scientific

ranks$Species <- ifelse(ranks$species_ranks == 40, "14 other species", ranks$Species)
ranks$Invasive <- ifelse(ranks$species_ranks == 40, "aggregated", ranks$Invasive)
ranks$PlantType <- ifelse(ranks$Invasive=="Y", " Invasive", 
                          ifelse(ranks$Invasive=="N", " Native", "Aggregated"))

#fix species names
ranks <- ranks %>% 
  mutate(Scientific = sub("^([A-Za-z-]+\\s+[a-z-]+).*", "\\1", Scientific))

ranks$Scientific <- ifelse(ranks$species_ranks == 40, "14 other species", ranks$Scientific)

ranks <- ranks %>%
  mutate(Scientific = ifelse(Scientific == "Lupinus L.", 
                             "Lupinus", 
                             ifelse(Scientific == "Madia Molina",
                                    "Madia", 
                                    ifelse(Scientific == "Amsinckia Lehm.",
                                           "Amsinckia", Scientific)
                                    )
                             )
         )
         

supfig3 <-
  ggplot(subset(ranks, species_ranks <41),
       aes(x=fct_reorder(Scientific, species_ranks), y=species_total_abundance, color=PlantType)) + 
  geom_point(size = 3) + 
  geom_pointrange(aes(ymin = species_total_abundance - 1.96*se_values, 
                      ymax =  species_total_abundance + 1.96*se_values))+
  scale_color_manual(name="", values=c("#EE6677", "#228833", "#66CCEE"))+
  xlab("Species in order of rank") +
  ylab("Relative abundance") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, 
                                   face = "italic", size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16), legend.position = "inside",
        legend.position.inside = c(0.9, 0.88),
        legend.title = element_blank())

supfig3

# ggsave(supfig3, file = "figures/supfig3.png",
#        width = 11, height = 7.5, unit = c("in"), dpi = 450)
