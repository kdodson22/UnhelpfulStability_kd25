library(ecotraj)
library(tidyverse)
library(vegan)
library(here)

#data
# source(here("1.0.Data.Set-up.R"))

## DATA PREP #####
#create 'metadata' df with site, plot, date, and treatment info
meta1 <- spp.comp %>% 
  dplyr::select(Site, Plot, Year) %>%
  mutate(ObsID = paste(Site, Plot, Year, sep = "-")) 

treat1 <- sitedata %>%
  filter(Plot <= 13) %>%
  dplyr::select(Site, Plot, Sprayed)

meta <- left_join(meta1, treat1, by = c("Site", "Plot"))
meta <- meta %>% mutate(Year = as.numeric(Year))

## 2021 -> 2022 Distance ###### 
#create metadata df
plots1 <- meta %>% 
  filter(Year == 2021 | Year == 2022)

#filter correct years from spp.comp.rel
spp.comp.rel$Obs.ID <- row.names(spp.comp.rel)
scr.1 <- spp.comp.rel %>% 
  separate(Obs.ID, into = c("Site", "Plot", "Year")) %>% 
  relocate(c("Site", "Plot", "Year"), .before = "AMSIN") %>%
  mutate(Year = as.numeric(Year),
         Site = as.numeric(Site),
         Plot = as.numeric(Plot)) %>%
  filter(Year == 2021 | Year == 2022)


#bray-curtis dissimilarity
bray1 <- vegdist(scr.1[,-c(1:3)], method = "bray")

#as matrix
braymat1 <- as.matrix(bray1)
#as data frame
braydf1 <- as.data.frame(braymat1)

#combine
distmatrix1 <- cbind(plots1, braydf1)

#group by site-plot combo
distmat1 <- distmatrix1 %>%
  mutate("Site-Plot" = paste(Site, Plot, Sprayed, sep = "-")) %>%
  relocate("Site-Plot", .after = Year)

#LENGTHS (segments between points & sum; within year/site/plot combo)
tr_lengths1 <- trajectoryLengths(braymat1,
                                 distmat1$`Site-Plot`,
                                 distmat1$Year)

tr_lengths_df1 <- as.data.frame(tr_lengths1)
tr_lengths_df1$ObsID <- rownames(tr_lengths_df1)
tr_lengths_df1 <- tr_lengths_df1 %>% 
  separate(ObsID, into = c("Site", "Plot", "Sprayed"), sep = "-") %>%
  mutate(Site = as.factor(Site),
         Plot = as.factor(Plot))%>%
  relocate(c("Site", "Plot", "Sprayed"), .before = "S1") 

tr_lengths_df1$S1 <- replace(tr_lengths_df1$S1,tr_lengths_df1$S1==1, 0.99)

## 2021 -> 2024 Distance ##### 
#create metadata df
plots2 <- meta %>% 
  filter(Year == 2021 | Year == 2024)

#filter correct years from spp.comp.rel
scr.2 <- spp.comp.rel %>%
  separate(Obs.ID, into = c("Site", "Plot", "Year")) %>%
  relocate(c("Site", "Plot", "Year"), .before = "AMSIN") %>%
  mutate(Year = as.numeric(Year),
         Site = as.numeric(Site),
         Plot = as.numeric(Plot)) %>%
  filter(Year == 2021 | Year == 2024)

#bray-curtis dissimilarity
bray2 <- vegdist(scr.2[,-c(1:4)], method = "bray")

#as matrix
braymat2 <- as.matrix(bray2)
#as data frame
braydf2 <- as.data.frame(braymat2)

#combine
distmatrix2 <- cbind(plots2, braydf2)

#group by site-plot combo
distmat2 <- distmatrix2 %>%
  mutate("Site-Plot" = paste(Site, Plot, Sprayed, sep = "-")) %>%
  relocate("Site-Plot", .after = Year)

#LENGTHS (segments between points & sum; within year/site/plot combo)
tr_lengths2 <- trajectoryLengths(braymat2,
                                 distmat2$`Site-Plot`,
                                 distmat2$Year)

tr_lengths_df2 <- as.data.frame(tr_lengths2)
tr_lengths_df2$ObsID <- rownames(tr_lengths_df2)
tr_lengths_df2 <- tr_lengths_df2 %>% 
  separate(ObsID, into = c("Site", "Plot", "Sprayed"), sep = "-") %>%
  mutate(Site = as.factor(Site),
         Plot = as.factor(Plot))%>%
  relocate(c("Site", "Plot", "Sprayed"), .before = "S1") 

## Create dataframe #####
pretreat$Site <- factor(pretreat$Site, 
                        levels = c("1","2","3","4","5",
                                   "6","7","8","9","10"))
pretreat$Plot <- factor(pretreat$Plot, 
                        levels = c("1","2","3","4","5",
                                   "6","7","8","9","10",
                                   "11","12","13"))

df_divcov$Site <- factor(df_divcov$Site, 
                         levels = c("1","2","3","4","5",
                                    "6","7","8","9","10"))
df_divcov$Plot <- factor(df_divcov$Plot, 
                         levels = c("1","2","3","4","5",
                                    "6","7","8","9","10",
                                    "11","12","13"))

#calculate means of control lengths for two interest year pairings
controlmeanlengths <- tr_lengths_df1 %>%
  select(-Trajectory) %>%
  rename("Y21_Y22" = S1) %>% 
  filter(Sprayed == "No") %>%
  group_by(Site) %>%
  summarise("Y21_Y22_conmean" = mean(Y21_Y22))

controlmeanlengths <- tr_lengths_df2 %>%
  select(-Trajectory) %>%
  rename("Y21_Y24" = S1) %>% 
  filter(Sprayed == "No") %>%
  group_by(Site) %>%
  summarise("Y21_Y24_conmean" = mean(Y21_Y24)) %>%
  left_join(controlmeanlengths)

#create dataframe with all lengths 
RRR_df <- tr_lengths_df1 %>%
  select(-Trajectory) %>%
  rename("Y21_Y22" = S1) %>% 
  left_join(tr_lengths_df2) %>%
  select(-Trajectory) %>%
  rename("Y21_Y24" = S1) %>% 
  left_join(controlmeanlengths)

#join with pretreatment & calculate LRRs
RRR_df <- df_divcov %>% filter(Year == 2021) %>% 
  left_join(RRR_df) %>% 
  mutate(LRR.resistance = log((1-Y21_Y22) / (1-Y21_Y22_conmean)),
         
         LRR.resilience = log((1-Y21_Y24) / (1-Y21_Y24_conmean)) - 
           log((1-Y21_Y22) / (1-Y21_Y22_conmean)),
         
         LRR.recovery = log((1-Y21_Y24) / (1-Y21_Y24_conmean)))

#add soil data
RRR_df <- RRR_df %>% left_join(soildata, by = c("Site", "Plot")) 

#munlacks
RRR_df <- RRR_df %>% 
  group_by(Site) %>%
  rename("pre_natcover_abs" = natcover_abs,
         "pre_invcover_abs" = invcover_abs,
         "pre_natcover_rel" = natcover_rel,
         "pre_invcover_rel" = invcover_rel,
         "pre_totplantcov" = totplantcov ) %>%
  select(-totaldiversity, -natdiversity, -invdiversity) %>%
  mutate(slmc.all = mean(pre_totplantcov),
         slmc.invr = mean(pre_invcover_rel),
         slmc.natr = mean(pre_natcover_rel),
         slmc.inva = mean(pre_invcover_abs),
         slmc.nata = mean(pre_natcover_abs),
         slmd.all = mean(pre_totaldiversity),
         slmd.nat = mean(pre_natdiversity),
         slmd.inv = mean(pre_invdiversity),
         slmr.all = mean(pre_totalrichness),
         slmr.inv = mean(pre_invrichness),
         slmr.nat = mean(pre_natrichness),
         slm.No3.f21 = mean(NO3.instant_2021Fall),
         slm.Nh4.f21 = mean(NH4.instant_2021Fall),
         slm.N.f21 = mean(N_ug.g.instant_2021Fall),
         slm.N.s22 = mean(N_ug.g.instant_2022Spring),
         slm.No3.s22 = mean(NO3.instant_2022Spring),
         slm.Nh4.s22 = mean(NH4.instant_2022Spring),
         slm.wc.f21 = mean(watercontent_g.g.instant_2021Fall),
         slm.wc.s22 = mean(watercontent_g.g.instant_2022Spring),
         slm.whc.f21 = mean(WHC_g.g.instant_2021Fall),
         slm.whc.s22 = mean(WHC_g.g.instant_2022Spring))

#dominants 
RRR_df <- RRR_df %>% 
  mutate(Site = as.integer(Site),
         Plot = as.integer(Plot), 
         Year = as.integer(Year)) %>%
  left_join(brch21, by = c("Site", "Plot")) %>%
  select(-Year.x, -Year.y, -Depth, -Size) %>%
  relocate("Season", .after = "Plot")

#Mineralization & Delta N
RRR_df <- RRR_df %>%
  mutate(Fall21_mineralN = N_ug.g.incubated_2021Fall - N_ug.g.instant_2021Fall,
         Spr22_mineralN = N_ug.g.incubated_2022Spring - N_ug.g.instant_2022Spring, 
         DeltaN_21_22 = (N_ug.g.instant_2021Fall - N_ug.g.instant_2022Spring)/N_ug.g.instant_2021Fall,
         DeltaNH4_21_22 = (NH4.instant_2021Fall - NH4.instant_2022Spring)/NH4.instant_2021Fall,
         DeltaNO3_21_22 = (NO3.instant_2021Fall - NO3.instant_2022Spring)/NO3.instant_2021Fall)


RRR_df$Site <- factor(RRR_df$Site, 
                      levels = c("1","2","3","4","5",
                                 "6","7","8","9","10"))
RRR_df$Plot <- factor(RRR_df$Plot, 
                      levels = c("1","2","3","4","5",
                                 "6","7","8","9","10",
                                 "11","12","13"))
RRR_df <- RRR_df %>% relocate(c("LRR.resistance", "LRR.resilience", "LRR.recovery",
                                "Y21_Y22", "Y21_Y24", "Y21_Y22_conmean", "Y21_Y24_conmean"),
                              .after = Sprayed)

RRR_df2 <- RRR_df %>% group_by(Site) %>% mutate(slm.brte = mean(BRTE),
                                                slm.chju = mean(CHJU), 
                                                slm.epbr = mean(EPBR3),
                                                slm.br = mean(Bromus),
                                                slm.pobu = mean(POBU)) %>% ungroup()
