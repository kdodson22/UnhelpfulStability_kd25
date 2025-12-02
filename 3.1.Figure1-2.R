## Resistance and resilience to restoration: Plant diversity and soil resources promote the post-disturbance stability of invaded communities #####

## 3.1 Figures 1-2

## Purpose: This script generates Figures 1 and 2, showing study location and descriptive statistics for variables used in this study

## Author: K. Dodson 
## Date: Updated 12/1/2025


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

#data
# source(here("1.0.Data.Set-up.R"))
# source(here("1.1.RRR.Cover.R"))
# source(here("1.2.RRR.Composition.R"))

## Figure 1 ######

## BRWMA Map - Figure 1a #####
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




## Descriptive statistics - Figure 1b-c #####
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

 # ggsave(figure1bc, file = "figures/figure1bc.png",
 #        width = 3.75, height = 7, units = c("in"), dpi = 400)


## Figure 2 ##########

## RRR Summary Statistics - Figure 2 #####
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


figure2ac <- plot_grid(p1, p2, p3, 
          nrow = 1, align = "hv")

 # ggsave(figure2ac, file = "figures/figure2ac.png",
 #        width = 9.87, height = 4, units = c("in"), dpi = 300)
