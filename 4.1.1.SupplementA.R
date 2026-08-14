## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 4.1.1 Supplement A:

## Purpose: This script generates material for supplement A, including: Map showing study location and descriptive statistics for variables used in this study, Site climate description, trends in relative cover of dominant species across the study period, and a rank abundance plot for plant species observed

## Author: K. Dodson 
## Date: Updated 8/7/2026

library(tidyverse)
library(here)
library(cowplot)
library(sp)
library(ggmap)
library(grid)
library(sf)
library(ggspatial)
library(maps)
library(biscale)
library(ggeffects)

#data
# source(here("1.0.Data.Set-up.R"))
# source(here("1.1.RRR.Cover.R"))
# source(here("1.2.RRR.Composition.R"))

## SupFig 1. BRWMA Map and other associated metadata #####
## BRWMA Map - Figure 1a 
# Idaho inset
#pull out state outlines & select Idaho
states <- map_data("state")
id_df <- subset(states, region == "idaho")

id_base <- ggplot(id_df, mapping = aes(long, lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color="white", fill = "grey") 
id_base + theme_nothing()

#box around study area
boxes <- data.frame(maxlat = 43.5, minlat = 43,
                    maxlong = -115.5, minlong = -116.25, id="1")
boxes<-transform(boxes, laby=(maxlat +minlat )/2, labx=(maxlong+minlong )/2)

id_plot <- 
  ggplot() +
  geom_polygon(data = id_df, aes(long, lat, group = group),
               fill = "white", color = "black") +
  coord_fixed(1.3) +
  geom_point(aes(x = -116, y = 43.55), size = 6, fill = "#371415ff", shape = 21) + 
  annotate("text", x = -114.3, y = 43.55, label = "BRWMA", size = 5, color = "black") +
  theme_nothing() 

id_plot  

# ggsave(id_plot, file = "figures/IdahoMap.png", dpi = 350)

#BRWMA and our sites
#combine with site-plot data 
mypoints <- invabsdf %>% mutate(Site = as.numeric(Site), Plot = as.numeric(Plot)) %>%
  rename(Latitude = Lat, Longitude = Long) %>%
  left_join(mypoints, join_by(Site, Plot, Longitude, Latitude)) 

#establish study region 
box <- make_bbox(lon = mypoints$Longitude, lat = mypoints$Latitude, f = 0.2)
box

#make lat/long points into geometry  
mypoints_sf <- mypoints %>% st_as_sf(coords = c("Longitude", "Latitude"))
mypoints_sfcrs <- st_set_crs(mypoints_sf, 4979) 
mypoints_points <- mypoints_sf %>% 
  extract(geometry, c('lon', 'lat'), '\\((.*), (.*)\\)', convert = TRUE) %>%
  relocate(c("lat", "lon"), .after = Plot)

#upload BRMWA polygon
brwma_poly <- st_read("data/BRWMA_shapefiles", layer="boiseriver_wma")
brwma_poly <- st_transform(brwma_poly["Name"], "+proj=longlat", CRS=4979) 
head(st_coordinates(brwma_poly))

#download stadia maps -- need to insert key for script to work from here:
ggmap::register_stadiamaps(key = "insert-key-here")
region_map <- get_stadiamap(box, zoom = 14, maptype="stamen_terrain")


#bivariate calculations
richness_biclass <- bi_class(mypoints_points, 
                             x = pre_natrichness, y = pre_invrichness, 
                             style = "quantile", dim = 3)

#make legend
break_vals <- bi_class_breaks(mypoints_points, 
                              x = pre_natrichness, y = pre_invrichness,  
                              dig_lab = c(x = 4, y = 5), style = "quantile",
                              split = TRUE)

custom_pal3 <- c(
  "1-1" = "#d3d3d3", # low x, low y
  "2-1" = "#7ca883",
  "3-1" = "#228833", # high x, low y
  "1-2" = "#d69ca4",
  "2-2" = "#7d7d66", # medium x, medium y
  "3-2" = "#1f5c24",
  "1-3" = "#EE6677", # low x, high y
  "2-3" = "#7f4a43",
  "3-3" = "#203618" # high x, high y
)

richlegend <- bi_legend(pal = custom_pal3,
                        dim = 3,
                        xlab = "Native Species Richness",
                        ylab = "Invasive Species Richness",
                        breaks = break_vals,
                        size = 14)

#map
map <- ggmap(region_map, darken = c(0.15, "white"))  +
  geom_sf(data = brwma_poly, inherit.aes = FALSE,
          fill = "pink", color = "pink", alpha = 0.25) +
  geom_point(data = richness_biclass, inherit.aes = FALSE,
             aes(lon, lat, 
                 fill = bi_class), 
             position = position_jitter(width = 0.001, height = 0.001), 
             shape = 21, size = 5) +
  annotation_scale(location = "tl", style = "ticks", text_cex = 1) +
  annotation_north_arrow(pad_y = unit(1, "cm"), location = "tl") +
  bi_scale_fill(pal = custom_pal3, dim = 3) +
  theme_bw() + 
  theme(axis.text = element_text(size = 14),
        legend.position = "none") +
  xlab("") + ylab("") 

#alltogether
finalmap <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(richlegend, 0.635, 0.61, 0.375, 0.375) +
  draw_plot(id_plot, 0.05, 0.065, 0.35, 0.35)

fig1a <- plot_grid(finalmap, labels = c("a"))

# ggsave(fig1a, file = "figures/fig1a.png", dpi = 500,
#        width = 11, height = 8.5)




## Descriptive statistics - Figure 1b-c 
# DOMINANCE #
dominance_pplot <- 
  RRR_df %>% 
  select(Site, Sprayed, BRTE, CHJU) %>%
  pivot_longer(cols = c(3,4), names_to = "Species", values_to = "Cover") %>%
  ggplot() +
  geom_histogram(aes(Cover, fill = Species, group = Species), 
                 position = "dodge", binwidth = 0.1) +
  theme_bw() +
  labs(x = "Pre-treatment Cover (%)",
       y = "
# of Plots") +
  scale_fill_manual(labels = c("B. tectorum", "C juncea"), values = c("#66CCEE", "#AA3377")) +
  labs(fill = "") +
  ylim(0, 45) +
  theme(aspect.ratio = 1/1,
        legend.position = "inside",
        legend.position.inside = c(0.7, 0.85),
        legend.title = element_blank(),
        legend.text = element_text(face = "italic", size = 16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 17.5))

# NITROGEN # 
nitrogen_pplot <- 
  RRR_df %>%
  mutate(Sprayed = factor(Sprayed, levels = c("Yes", "No"))) %>%
  ggplot(aes(Sprayed, N_ug.g.instant_2022Spring, fill = Sprayed, color = Sprayed)) +
  geom_boxplot(alpha = 0.75) +
  geom_jitter(aes(color = Sprayed)) +
  theme_bw() +
  ylab("Post-treatment PAN 
(\U00B5g mineral N/g soil)") +
  xlab("") +
  scale_x_discrete(labels = c("Treatment", "Control")) +
  scale_color_manual(name = "Treatment",
                     labels = c("Treatment", "Control"),
                     values = c("#CCBB44", "#4477AA")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Treatment", "Control"),
                    values = c("#CCBB44", "#4477AA")) +
  theme(aspect.ratio = 1/1,
        legend.position = "none",
        axis.title = element_text(size = 17.5),
        axis.text = element_text(size = 17.5)) 


figure1bc <- plot_grid(dominance_pplot, nitrogen_pplot,
                       ncol=1, align = "hv")

# ggsave(figure1bc, file = "figures/figureSupplementary1.png",
#        width = 3.75, height = 7, units = c("in"), dpi = 400)

## SupFig 2. B. tectorum and C. juncea relative cover pre- and post-treatment.  #####
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

supfig2 <- dominants %>%
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

supfig2

 # ggsave(supfig2, file = "figures/supfig2.png",
 #       width = 5.5, height = 5, unit = c("in"), dpi = 450)


## SupFig 4. Ranked species rareness based on relative abundance. #####
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
         

supfig4 <-
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

supfig4

# ggsave(supfig4, file = "figures/supfig4.png",
#        width = 11, height = 7.5, unit = c("in"), dpi = 450)

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



