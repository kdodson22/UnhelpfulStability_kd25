## from targettrajectories NW_soildata 


### NITROGEN PROBING ####
# Also, Kay, can you probe 

# how correlated the N pulse measures are within-plot are over time? 
# If a given plot's pulse magnitude is strongly correlated in, say Year 1 Spring v. Year 1 Fall v. Year 2 spring, then it shouldn't matter much whether we "missed" Fall Year 0 pulses because the spring year 1 covariate is likely to to be correlated with that omitted observation too. (Highly correlated variables capture the same info, so we should get similar effect sizes).



NW_soildata %>%
  filter(Sprayed == "Yes") %>%
  mutate(Plot = paste(Site, Plot, sep = "_")) %>%
  ggplot(aes(Sampletime, totalN)) + 
  geom_point(aes(color = Plot)) + 
  geom_line(aes(color = Plot, group = Plot)) + 
  theme_bw() +
  facet_wrap(~Site) + theme(legend.position = 'none')


tmp_soildata <- NW_soildata %>% 
  # filter(Sprayed == 'Yes') %>%
  mutate(Sampletime = fct_relevel(Sampletime, 
                                  "Fall 2021","Spring 2022","Fall 2022", "Spring 2023",
                                  "Fall 2023","Spring 2024", "Fall 2024"))


soilpantest <- brm(totalN ~ Sampletime * Sprayed + (1 | Plot),
                   data = subset(tmp_soildata, Sampletime != "Fall 2021"),
                   family = "gamma"
)
summary(soilpantest)
mcmc_plot(soilpantest)

conditional_effects(soilpantest)

tmp_soildata %>%
  dplyr::select(Site, Plot, Sprayed, Sampletime, totalN) %>%
  pivot_wider(names_from = Sampletime, values_from = totalN) %>%
  ggplot() +
  geom_point(aes(x=`Spring 2022`, y = `Fall 2022`)) + 
  theme_bw() + theme(aspect.ratio = 1/1)

C1 <- tmp_soildata %>% 
  filter(!is.na(totalN) & Sprayed == "Yes") %>%
  dplyr::select(Site, Plot, Sprayed, Sampletime, totalN) %>%
  pivot_wider(names_from = Sampletime, values_from = totalN) %>%
  dplyr::select(`Fall 2021`, `Spring 2022`, `Fall 2022`, `Spring 2023`, `Fall 2023`, `Spring 2024`, `Fall 2024`) 
cor.test(C1$`Spring 2022`, C1$`Fall 2022`)



C <- tmp_soildata %>%
  filter(!is.na(totalN) & Sprayed == "Yes") %>%
  dplyr::select(Site, Plot, Sprayed, Sampletime, totalN) %>%
  pivot_wider(names_from = Sampletime, values_from = totalN) %>%
  dplyr::select(`Fall 2021`, `Spring 2022`, `Fall 2022`, `Spring 2023`, `Fall 2023`, `Spring 2024`, `Fall 2024`)%>%
  drop_na(1:7) %>%
  cor()
corrplot(C)
