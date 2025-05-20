# 
# Comparison between aggregated stability and compositional stability
# Junna, 5/17/2025
rm(list = ls())
librarian::shelf(googledrive, vegan, lme4, car, dplyr, tidyr, lattice, lavaan, lmerTest, 
                 piecewiseSEM, semPlot, ggplot2, codyn, ggpubr, performance, emmeans)

tmp <- tempfile(fileext = ".csv")
drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1xa-ypKd_ovsRov_Ol_uvE7ZI2rCc5SSj"), type='csv')

drive_download(drive_folder[drive_folder$name=='combined_agg_dss.csv', ], path = tmp, overwrite = TRUE)
combined_agg_dss <- read.csv(tmp)

drive_download(drive_folder[drive_folder$name=='combined_comp_stability.csv', ], path = tmp, overwrite = TRUE)
combined_comp_stability <- read.csv(tmp)

# use consistent site names: lower case and simple name
combined_agg_dss$site[combined_agg_dss$site == 'KNZ'] <- 'knz'
combined_agg_dss$site[combined_agg_dss$site == 'KBS'] <- 'kbs'
combined_agg_dss$site[combined_agg_dss$site == 'CDR_oldField'] <- 'cdr-of'
combined_agg_dss$site[combined_agg_dss$site == 'GCE'] <- 'gce'
combined_agg_dss$site[combined_agg_dss$site == 'USVI'] <- 'usvi'
#
combined_comp_stability$site[combined_comp_stability$site=='cdr_of'] <- 'cdr-of'

#
combined_dss <- merge(combined_agg_dss, combined_comp_stability, all=TRUE)
combined_dss <- combined_dss %>% arrange(site_new, plot)

write.csv(row.names = F, combined_dss, 'scripts/wrangling_junna/synthesized_data/combined_dss.csv')

# Making figures for presentation. 
# Producer aggregated stability and compositional stability
combined_dss %>%  filter(!site_new %in% c('sbc_invert', 'sbc_fish', 'usvi_fish', 'mcr_fish')) %>% 
  # usvi_fish: its producer is very stable? why? 
  ggplot(aes(x=prod_stability, y=prod_comp_stability, colour = site)) +
  geom_point() +
  geom_smooth(method='lm') +
  stat_cor() +
  ylim(1, 12) +
  labs(x='Producer Aggregate Stability', y='Producer Compositional Stability', colour='LTER Site') + 
  theme_classic() +
  theme(legend.position = c(0.75, 0.75))
ggsave('figures/May_present/producer_agg_comp_stability.png', width=5, height=4)


# producer aggregated stability and compositional stability
combined_dss %>% filter(!site_new %in% c('gce_invert', 'mcr_invert')) %>% 
  ggplot(aes(x=con_stability, y=con_comp_stability, colour = site)) +
  geom_point() +
  geom_smooth(method='lm') +
  stat_cor() +
  ylim(0.5, 5) +
  # stat_cor(method = "pearson", 
  #          aes(group = site_new,
  #              label.x = 0.8,
  #              lable.y = 0.5 + as.numeric(factor(site_new)) * 0.25)) +
  labs(x='Consumer Aggregate Stability', y='Consumer Compositional Stability', colour='LTER Site') + 
  theme_classic() +
  theme(legend.position = c(0.75, 0.75))
ggsave('figures/May_present/consumer_agg_comp_stability.png', width=5, height=4)


# combined_dss %>% filter(site_new %in% c('usvi_fish')) %>% 
#   ggplot(aes(x=prod_stability, y=prod_richness, colour = site_new)) +
#   geom_point() +
#   geom_smooth(method='lm') +
#   stat_cor()


