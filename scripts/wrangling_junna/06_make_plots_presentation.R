# here are scripts used for final presentation of our projects
# 5/19/2025
#----------------
# housekeeping
rm(list = ls())
librarian::shelf(googledrive, vegan, lme4, car, dplyr, tidyr, lattice, lavaan, lmerTest, 
                 piecewiseSEM, semPlot, ggplot2, codyn, ggpubr, performance, emmeans)

combined_dss <- read.csv('scripts/wrangling_junna/synthesized_data/combined_dss.csv')

# diversity-stability within one trophic level
# combined_dss %>% filter(!site_new %in% c('sbc_fish', 'mcr_fish')) %>%
combined_dss %>% filter(!site_new %in% c('gce_invert')) %>%
  filter(ecosystem == 'aquatic') %>%
  ggplot(aes(x=prod_richness, y=prod_stability, color = site)) +
  geom_point() +
  geom_smooth(method='lm') +
#  stat_cor() +
  ylim(1, 12) +
  labs(x='Producer Richness', y='Producer Aggregate Stability', color='LTER Site', title='Aqutic ecosystems') + 
  theme_classic() +
  theme(legend.position = c(0.75, 0.75))
ggsave('figures/May_present/producer_richness_agg_stability_aquatic.png', width=5, height=4)


combined_dss %>% filter(!site_new %in% c('sbc_fish', 'mcr_fish')) %>%
  filter(ecosystem == 'terrestrial') %>%
  ggplot(aes(x=prod_richness, y=prod_stability, color = site)) +
  geom_point() +
  geom_smooth(method='lm') +
  #  stat_cor() +
  ylim(1, 12) +
  labs(x='Producer Richness', y='Producer Aggregate Stability', color='LTER Site', title='Terrestrial ecosystems') + 
  theme_classic() +
  theme(legend.position = c(0.15, 0.85))
ggsave('figures/May_present/producer_richness_agg_stability_terrestrial.png', width=5, height=4)

#------------
combined_dss  %>% # filter(!site_new %in% c('sbc_fish', 'mcr_fish')) %>%
  filter(ecosystem == 'aquatic') %>%
  ggplot(aes(x=con_richness, y=con_stability, color = site_new)) +
  geom_point() +
  geom_smooth(method='lm') +
  #  stat_cor() +
  ylim(0, 6) +
  labs(x='Consumer Richness', y='Consumer Aggregate Stability', color='LTER Site', title='Aqutic ecosystems') + 
  theme_classic() +
  theme(legend.position = 'right') # c(0.25, 0.7)
ggsave('figures/May_present/consumer_richness_agg_stability_aquatic.png', width=5, height=4)

#------------
combined_dss  %>% # filter(!site_new %in% c('sbc_fish', 'mcr_fish')) %>%
  filter(ecosystem == 'terrestrial') %>%
  ggplot(aes(x=con_richness, y=con_stability, color = site_new)) +
  geom_point() +
  geom_smooth(method='lm') +
  #  stat_cor() +
  ylim(0, 4) +
  labs(x='Consumer Richness', y='Consumer Aggregate Stability', color='LTER Site', title='Terrestrial ecosystems') + 
  theme_classic() +
  theme(legend.position = c(0.75, 0.85)) # 
ggsave('figures/May_present/consumer_richness_agg_stability_terrestrial.png', width=5, height=4)


#--cross trophic level relationship
combined_dss  %>% # filter(!site_new %in% c('sbc_fish', 'mcr_fish')) %>%
  filter(ecosystem == 'aquatic') %>%
  ggplot(aes(x=prod_richness, y=con_stability, color = site_new)) +
  geom_point() +
  geom_smooth(method='lm') +
  #  stat_cor() +
#  ylim(0, 4) +
  labs(x='Producer Richness', y='Consumer Aggregate Stability', color='LTER Site', title='Aquatic ecosystems') + 
  theme_classic() +
  theme(legend.position = c(0.75, 0.85)) #

ggsave('figures/May_present/producer_richness_consumer_agg_stability_aquatic.png', width=5, height=4)




