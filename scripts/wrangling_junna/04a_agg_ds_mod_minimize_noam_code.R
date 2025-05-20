# read in aggregate stability metrics, Junna
rm(list = ls())
librarian::shelf(googledrive, vegan, lme4, car, dplyr, tidyr, lattice, lavaan, lmerTest, 
                 piecewiseSEM, semPlot, ggplot2, codyn, ggpubr, performance, emmeans)

# retrieve functions from 00_functions.r
source('scripts/00_functions_minimize.R')

# get data from google drive
drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1xa-ypKd_ovsRov_Ol_uvE7ZI2rCc5SSj"), type='csv')
tmp <- tempfile(fileext = ".csv")

# using Junna's data
drive_download(drive_folder[drive_folder$name=='combined_dss.csv',], path = tmp, overwrite = TRUE)
combined_dss <- read.csv(tmp)

# work on aquatic ecosystems, aggregate stability---------
combined_dss_aquatic <- combined_dss %>% filter(ecosystem == 'aquatic') %>% 
  rename('site_origin' = 'site', 'site' = 'site_new')
#
model_stability(df = combined_dss_aquatic,
                ecosystem_type = "combined",
                stability_metric = "aggregate",
                diversity_metric = "richness",
                prod_diversity_col = "prod_richness",
                con_diversity_col = "con_richness",
                prod_stability_col = "prod_stability",
                con_stability_col = "con_stability",
                multi_stability_col = NULL,
                transformation = "z")

# view figures
basic_sem_plot_combined_richness_aggregate
plot_combined_aggregate_prod_richness_stability
plot_combined_aggregate_con_richness_stability
# plot_combined_aggregate_multi_richness_stability
plot_combined_aggregate_stability_richness_correlations

# work on terrestrial ecosystems, aggregate stability---------
combined_dss_terrestrial <- combined_dss %>% filter(ecosystem == 'terrestrial') %>% 
  rename('site_origin' = 'site', 'site' = 'site_new')
#
model_stability(df = combined_dss_terrestrial,
                ecosystem_type = "combined",
                stability_metric = "aggregate",
                diversity_metric = "richness",
                prod_diversity_col = "prod_richness",
                con_diversity_col = "con_richness",
                prod_stability_col = "prod_stability",
                con_stability_col = "con_stability",
                multi_stability_col = NULL,
                transformation = "z")

# view figures
basic_sem_plot_combined_richness_aggregate
plot_combined_aggregate_prod_richness_stability
plot_combined_aggregate_con_richness_stability
# plot_combined_aggregate_multi_richness_stability
plot_combined_aggregate_stability_richness_correlations


# work on aquatic ecosystems, compositional stability---------
combined_dss_aquatic_comp <- na.omit(combined_dss_aquatic)
model_stability(df = combined_dss_aquatic_comp,
                ecosystem_type = "aquatic",
                stability_metric = "compositional",
                diversity_metric = "richness",
                prod_diversity_col = "prod_richness",
                con_diversity_col = "con_richness",
                prod_stability_col = "prod_comp_stability",
                con_stability_col = "con_comp_stability",
                multi_stability_col = NULL,
                transformation = "z")
# view figures
basic_sem_plot_aquatic_richness_compositional
plot_aquatic_compositional_prod_richness_stability
plot_aquatic_compositional_con_richness_stability
# plot_aquatic_compositional_multi_richness_stability
plot_aquatic_compositional_stability_richness_correlations

# work on terrestrial ecosystems, compositional stability---------
combined_dss_terrestrial_comp <- na.omit(combined_dss_terrestrial)
model_stability(df = combined_dss_terrestrial_comp,
                ecosystem_type = "terrestrial",
                stability_metric = "compositional",
                diversity_metric = "richness",
                prod_diversity_col = "prod_richness",
                con_diversity_col = "con_richness",
                prod_stability_col = "prod_comp_stability",
                con_stability_col = "con_comp_stability",
                multi_stability_col = NULL,
                transformation = "z")
# view figures
basic_sem_plot_terrestrial_richness_compositional
plot_terrestrial_compositional_prod_richness_stability
plot_terrestrial_compositional_con_richness_stability
# plot_terrestrial_compositional_multi_richness_stability
plot_terrestrial_compositional_stability_richness_correlations


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# read Noam's master_stability.csv file
drive_download(drive_folder[drive_folder$name=='master_stability.csv'], path = tmp, overwrite = TRUE)
master_stability <- read.csv(tmp)
# the master table missed gce_invert

# work on aquatic ecosystems, aggregate stability---------
master_stability_aquatic <- subset(master_stability, ecosystem == 'aquatic')
model_stability(df = master_stability_aquatic,
                ecosystem_type = "combined",
                stability_metric = "aggregate",
                diversity_metric = "richness",
                prod_diversity_col = "prod_richness",
                con_diversity_col = "con_richness",
                prod_stability_col = "prod_stability",
                con_stability_col = "con_stability",
                multi_stability_col = NULL,
                transformation = "z")

# view figures
basic_sem_plot_combined_richness_aggregate
plot_combined_aggregate_prod_richness_stability
plot_combined_aggregate_con_richness_stability
# plot_combined_aggregate_multi_richness_stability
plot_combined_aggregate_stability_richness_correlations


# work on terrestrial ecosystems, aggregate stability---------
master_stability_terrestrial <- subset(master_stability, ecosystem == 'terrestrial')
model_stability(df = master_stability_terrestrial,
                ecosystem_type = "combined",
                stability_metric = "aggregate",
                diversity_metric = "richness",
                prod_diversity_col = "prod_richness",
                con_diversity_col = "con_richness",
                prod_stability_col = "prod_stability",
                con_stability_col = "con_stability",
                multi_stability_col = NULL,
                transformation = "z")


# view figures
basic_sem_plot_combined_richness_aggregate
plot_combined_aggregate_prod_richness_stability
plot_combined_aggregate_con_richness_stability
# plot_combined_aggregate_multi_richness_stability
plot_combined_aggregate_stability_richness_correlations
