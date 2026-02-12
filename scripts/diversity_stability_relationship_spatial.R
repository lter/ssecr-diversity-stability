# We used spatial method to estimate diversity-stability relationship. 
# Author: Junna Wang, 12/12/2025


library(librarian)
# Install missing packages and load needed libraries
shelf(tidyverse, googlesheets4, googledrive, readxl, lmerTest, semPlot, ggplot2, codyn, ggpubr, performance, emmeans, lme4, DHARMa)
rm(list=ls())

# source('scripts/00_functions_minimize.R')

tmp <- tempfile(fileext = ".csv")
drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1xa-ypKd_ovsRov_Ol_uvE7ZI2rCc5SSj"), type='csv')
drive_download(drive_folder[drive_folder$name == "combined_agg_dss_12082025.csv",], path = tmp, overwrite = TRUE)
mar_terr <- read.csv(tmp)

#-------------------------------
source(file.path('scripts', 'wrangling_junna', 'function_confounding.R'))
# Noam's suggestion to use: MCR fish and SBC Invert. 
aqua <- mar_terr %>% filter(ecosystem == 'aquatic') %>% filter(!site_new %in% c("sbc_fish", "mcr_invert"))
terr <- mar_terr %>% filter(ecosystem == 'terrestrial')

aqua <- control_confounder_spatial(aqua)
terr <- control_confounder_spatial(terr)

#-------------------aquatic producer community
# use model D -- group mean centered path model in Byrnes and Dee, 2025
mod_mar_prod1 <- lmer(data = aqua, prod_stability_log_zscore ~ prod_richness_log_zscore_devi + con_richness_log_zscore_devi + prod_richness_log_zscore_site_mean + con_richness_log_zscore_site_mean + (1|site_new))
summary(mod_mar_prod1)
# 
plot(DHARMa::simulateResiduals(mod_mar_prod1))
plot(residuals(mod_mar_prod1), predict(mod_mar_prod1))

#----
# use model A -- fixed effects transformation path model
mod_mar_prod2 <- lm(data = aqua, prod_stability_log_zscore_devi ~ prod_richness_log_zscore_devi + con_richness_log_zscore_devi)
summary(mod_mar_prod2)

plot(DHARMa::simulateResiduals(mod_mar_prod2))         
plot(residuals(mod_mar_prod2), predict(mod_mar_prod2))
# Models A and D are equivalent. 

#-------------------aquatic consumer community
# use model D
mod_mar_con1 <- lmer(data = aqua, con_stability_log_zscore ~ prod_richness_log_zscore_devi + con_richness_log_zscore_devi + prod_richness_log_zscore_site_mean + con_richness_log_zscore_site_mean + (1|site_new))
summary(mod_mar_con1)
# 
plot(DHARMa::simulateResiduals(mod_mar_con1)) 
plot(residuals(mod_mar_con1), predict(mod_mar_con1))

#-------------------
# use model A
mod_mar_con2 <- lm(data = aqua, con_stability_log_zscore_devi ~ prod_richness_log_zscore_devi + con_richness_log_zscore_devi)  
summary(mod_mar_con2)
# 
plot(DHARMa::simulateResiduals(mod_mar_con2))  
plot(residuals(mod_mar_con2), predict(mod_mar_con2))
# Models A and D are equivalent.

# cross-tropical correlation of richness and of stability
summary(lm(data = aqua, con_stability_log_zscore_devi ~ prod_stability_log_zscore_devi))
summary(lm(data = aqua, prod_richness_log_zscore ~ con_richness_log_zscore + site_new))
summary(lm(data = aqua, con_richness_log_zscore ~ prod_richness_log_zscore + site_new))
summary(lm(data = aqua, prod_richness_log_zscore ~ herb_richness_log_zscore + site_new))

# what if using herbivore for consumers?----This yields almost the same results. 
#------------------------------------------------------------------------------------------------------------
# model A
mod_mar_prod3 <- lm(data = aqua, prod_stability_log_zscore_devi ~ prod_richness_log_zscore_devi + herb_richness_log_zscore_devi)
summary(mod_mar_prod3)
plot(DHARMa::simulateResiduals(mod_mar_prod3))   
plot(residuals(mod_mar_prod3), predict(mod_mar_prod3))

# model A
mod_mar_herb2 <- lm(data = aqua, herb_stability_log_zscore_devi ~ prod_richness_log_zscore_devi + herb_richness_log_zscore_devi)
summary(mod_mar_herb2)
plot(DHARMa::simulateResiduals(mod_mar_herb2))
plot(residuals(mod_mar_herb2), predict(mod_mar_herb2))

#-----------------------------------------------------terrestrial ecosystems--------------------------------
# producer stability
# use model D -- group mean centered path model in Byrnes and Dee, 2025
mod_terr_prod1 <- lmer(data = terr, prod_stability_log_zscore ~ prod_richness_log_zscore_devi + con_richness_log_zscore_devi + prod_richness_log_zscore_site_mean + con_richness_log_zscore_site_mean + (1|site_new))
summary(mod_terr_prod1)
# 
plot(DHARMa::simulateResiduals(mod_terr_prod1))  # 
plot(residuals(mod_terr_prod1), predict(mod_terr_prod1))

#---------------------------
# consumer stability
mod_terr_con1 <- lmer(data = terr, con_stability_log_zscore ~ prod_richness_log_zscore_devi + con_richness_log_zscore_devi + prod_richness_log_zscore_site_mean + con_richness_log_zscore_site_mean + (1|site_new))
summary(mod_terr_con1)
# 
plot(DHARMa::simulateResiduals(mod_terr_con1))  # 
plot(residuals(mod_terr_con1), predict(mod_terr_con1))

#----------------------------
# use herbivore rather than consumers
mod_terr_prod3 <- lmer(data = terr, prod_stability_log_zscore ~ prod_richness_log_zscore_devi + herb_richness_log_zscore_devi + prod_richness_log_zscore_site_mean + herb_richness_log_zscore_site_mean + (1|site_new))
summary(mod_terr_prod3)

plot(DHARMa::simulateResiduals(mod_terr_prod3))  # 
plot(residuals(mod_terr_prod3), predict(mod_terr_prod3))
#
mod_terr_herb1 <- lm(data = terr, herb_stability_log_zscore_devi ~ prod_richness_log_zscore_devi + herb_richness_log_zscore_devi)
summary(mod_terr_herb1)
