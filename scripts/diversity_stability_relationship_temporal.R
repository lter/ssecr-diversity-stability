# We have tried two temporal methods. 
# One is to use non-overlapping moving window to calculate diversity and stability metrics, where auto-correlation in residuals is not considered;
# the other is use overlapping moving window to calculate diversity and stability metrics, where auto-correlation in the residuals is considered. 
# Author: Junna Wang, 2/12/2026


#-----------------------------------non-overlapping moving window----------------------------------
library(librarian)
# Install missing packages and load needed libraries
shelf(tidyverse, googlesheets4, googledrive, readxl, lmerTest, semPlot, ggplot2, codyn, ggpubr, performance, emmeans, lme4, DHARMa, glmmTMB, brms, nlme)
rm(list=ls())

source(file.path('scripts', '00_functions_minimize.R'))
source(file.path('scripts', 'wrangling_junna', 'function_confounding.R'))


tmp <- tempfile(fileext = ".csv")
drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"), type='csv')


# sites: use ecosystem to differentiate aquatic and terrestrial. do not use mcr-invert and sbc_fish
sites_aquatic <- c("usvi_fish", "sbc_invert", "aims", "mcr_fish", "ntl_trout", "ntl_madison", "adk", "gce")  # 8 sites:  
sites_terrestrial <- c("cdr_of", "kbs", "knz", "jrn", "bex_he", "bex_ae", "bex_se")                   # 7 sites
sites <- c(sites_aquatic, sites_terrestrial)

# number of years to aggregate to calculate stability; this can be changed. cannot use <=3 and >=7 
duration = 5

for (isite in 1:length(sites)) {
  
  drive_download(drive_folder[drive_folder$name == paste0(sites[isite], "_producers_wide_sub.csv"),], path = tmp, overwrite = TRUE)
  producer <- read.csv(tmp)
  
  drive_download(drive_folder[drive_folder$name == paste0(sites[isite], "_consumers_wide_sub.csv"),], path = tmp, overwrite = TRUE)
  consumer <- read.csv(tmp)
  
  drive_download(drive_folder[drive_folder$name == paste0(sites[isite], "_herbivore_wide_sub.csv"),], path = tmp, overwrite = TRUE)
  herbivore <- read.csv(tmp)
  
  years <- sort(unique(producer$year))
  
  nyears <- floor(length(years) / duration) * duration
  
  period <- data.frame(start = seq(1, nyears, by = duration), end = seq(duration, nyears, by = duration))
  # do we want to add one more row
  if (length(years) - nyears >= duration / 2) {
    period <- rbind(period, data.frame(start = length(years) - duration + 1, end = length(years)))
  }
  period$start <- years[period$start]
  period$end <- years[period$end]
  period$years <- paste(period$start, period$end, sep = "_")
  #
  for (i in 1:nrow(period)) {
    producer_period <- producer %>% filter(between(year, period$start[i], period$end[i]))
    consumer_period <- consumer %>% filter(between(year, period$start[i], period$end[i]))
    herbivore_period <- herbivore %>% filter(between(year, period$start[i], period$end[i]))
    
    calculate_agg_stability(producer_data = producer_period, consumer_data = consumer_period, 
                            herbivore_data = herbivore_period, site_name=sites[isite])
    # pull the output dataframe
    agg_ds_site <- get(paste0(sites[isite], "_aggregate_dss"))
    
    if (i == 1 & isite == 1) {
      agg_ds_allsites <- data.frame(agg_ds_site, period = period$years[i], site_new = sites[isite])
    } else {
      agg_ds_allsites <- rbind(agg_ds_allsites, data.frame(agg_ds_site, period = period$years[i], site_new = sites[isite]))
    }
  }
  #
}
#
agg_ds_allsites$site_period <- paste(agg_ds_allsites$site, agg_ds_allsites$period, sep="_")

# rename plot to ensure every plot is unique across these sites
agg_ds_allsites$plot_new <- paste(agg_ds_allsites$site_new, agg_ds_allsites$plot, sep="_")


agg_ds_aquatic <- agg_ds_allsites %>% filter(ecosystem == 'aquatic')
agg_ds_terrestrial <- agg_ds_allsites %>% filter(ecosystem == 'terrestrial')

agg_ds_aquatic <- control_confounder_temporal(agg_ds_aquatic)
agg_ds_terrestrial <- control_confounder_temporal(agg_ds_terrestrial)


#--------------------------------test for different ecosystems------------------------------------------------
# choose the ecosystem you want to work on
agg_ds_ecosystem <- agg_ds_aquatic
# agg_ds_ecosystem <- agg_ds_terrestrial


#-------------------For producer stability
# use a linear mixed model (a variation on the Two-way Mundlak model design; Box 2 in Byrnes and Dee, 2025)
mod_prod <- lmer(data=agg_ds_ecosystem, prod_stability_log_zscore ~ prod_richness_log_zscore_plot_mean_devi + prod_richness_log_zscore_plot_mean + prod_richness_log_zscore_site_period_mean + con_richness_log_zscore_plot_mean_devi + con_richness_log_zscore_plot_mean + con_richness_log_zscore_site_period_mean + (1|plot_new) + (1|site_period))
summary(mod_prod)
plot(DHARMa::simulateResiduals(mod_prod))


# res <- residuals(mod_prod)
# agg_ds_ecosystem$res_lmer <- res
# plots_all <- unique(agg_ds_ecosystem$plot_new)
# for (plot in plots_all) {
#   res_plot <- agg_ds_ecosystem$res_lmer[agg_ds_ecosystem$plot_new == plot]
#   if (length(res_plot) > 2) {
#     plot(acf(res_plot))
#   }
# }

# use a linear model (using fixed effects to address confounding effects)
summary(lm(data=agg_ds_ecosystem, prod_stability_log_zscore ~ prod_richness_log_zscore + con_richness_log_zscore + plot_new + site_period))
# essentially, the linear mixed model and the linear model are equivalent. 

#-------
# use herbivore data instead of consumer data
summary(lm(data=agg_ds_ecosystem, prod_stability_log_zscore ~ prod_richness_log_zscore + herb_richness_log_zscore + plot_new + site_period))

mod_prod3 <- lmer(data=agg_ds_ecosystem, prod_stability_log_zscore ~ prod_richness_log_zscore_plot_mean_devi + prod_richness_log_zscore_plot_mean + prod_richness_log_zscore_site_period_mean + herb_richness_log_zscore_plot_mean_devi + herb_richness_log_zscore_plot_mean + herb_richness_log_zscore_site_period_mean + (1|plot_new) + (1|site_period))
summary(mod_prod3)
plot(DHARMa::simulateResiduals(mod_prod3))

#-------------------For consumer stability
# use a linear mixed model (a variation on the Two-way Mundlak model design; Box 2 in Byrnes and Dee, 2025)
mod_con <- lmer(data=agg_ds_ecosystem, con_stability_log_zscore ~ prod_richness_log_zscore_plot_mean_devi + prod_richness_log_zscore_plot_mean + prod_richness_log_zscore_site_period_mean + con_richness_log_zscore_plot_mean_devi + con_richness_log_zscore_plot_mean + con_richness_log_zscore_site_period_mean + (1|plot_new) + (1|site_period))
summary(mod_con)
plot(DHARMa::simulateResiduals(mod_con))

# use a linear model (using fixed effects to address confounding effects)
summary(lm(data=agg_ds_ecosystem, con_stability_log_zscore ~ prod_richness_log_zscore + con_richness_log_zscore + plot_new + site_period))

#-------------------For herbivore stability
# use a linear mixed model (a variation on the Two-way Mundlak model design; Box 2 in Byrnes and Dee, 2025)
mod_herb <- lmer(data=agg_ds_ecosystem, herb_stability_log_zscore ~ prod_richness_log_zscore_plot_mean_devi + prod_richness_log_zscore_plot_mean + prod_richness_log_zscore_site_period_mean + herb_richness_log_zscore_plot_mean_devi + herb_richness_log_zscore_plot_mean + herb_richness_log_zscore_site_period_mean + (1|plot_new) + (1|site_period))
summary(mod_herb)
plot(DHARMa::simulateResiduals(mod_herb))

# use a linear model (using fixed effects to address confounding effects)
summary(lm(data=agg_ds_ecosystem, herb_stability_log_zscore ~ prod_richness_log_zscore + herb_richness_log_zscore + plot_new + site_period))




#--------------------------------Overlapping moving window: NEED TO ADDRESS AUTOCORRELATION----------------------------------
library(librarian)
# Install missing packages and load needed libraries
shelf(tidyverse, googlesheets4, googledrive, readxl, lmerTest, semPlot, ggplot2, codyn, ggpubr, performance, emmeans, lme4, DHARMa, glmmTMB, brms, nlme)
rm(list=ls())

source(file.path('scripts', '00_functions_minimize.R'))
source(file.path('scripts', 'wrangling_junna', 'function_confounding.R'))


tmp <- tempfile(fileext = ".csv")
drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"), type='csv')


# sites: use ecosystem to differentiate aquatic and terrestrial. do not use mcr-invert and sbc_fish
sites_aquatic <- c("usvi_fish", "sbc_invert", "aims", "mcr_fish", "ntl_trout", "ntl_madison", "adk", "gce")  # 8 sites:  
sites_terrestrial <- c("cdr_of", "kbs", "knz", "jrn", "bex_he", "bex_ae", "bex_se")                   # 7 sites
sites <- c(sites_aquatic, sites_terrestrial)

# number of years to aggregate to calculate stability; this can be changed.
duration = 5
# interval (number of years) between two consecutive moving windows. 
interval = 2
# IMPORTANT: when interval = 1, that is for a 10-year time series we use years 1-5, 2-6, 3-7, 4-8, 5-9, and 6-10 for calculating diversity and stability metric. Strong autocorrelation (phi = ~0.7) exists in these metrics, particularly for diversity metrics
# when interval = 2, that is for a 10-year time series we only use years 1-5, 3-7, 5-9 for calculating diversity and stability metric. Autocorrelation could be dramatically reduced (phi = ~0.4)

for (isite in 1:length(sites)) {
  #
  drive_download(drive_folder[drive_folder$name == paste0(sites[isite], "_producers_wide_sub.csv"),], path = tmp, overwrite = TRUE)
  producer <- read.csv(tmp)
  #
  drive_download(drive_folder[drive_folder$name == paste0(sites[isite], "_consumers_wide_sub.csv"),], path = tmp, overwrite = TRUE)
  consumer <- read.csv(tmp)
  #
  drive_download(drive_folder[drive_folder$name == paste0(sites[isite], "_herbivore_wide_sub.csv"),], path = tmp, overwrite = TRUE)
  herbivore <- read.csv(tmp)
  #
  years <- sort(unique(producer$year))
  nyears <- length(years)
  period <- data.frame(start = seq(1, nyears - duration + 1, by = interval), end = seq(duration, nyears, by = interval))
  period <- period %>% rowwise() %>% mutate(mid_year = round(mean(years[start:end], na.rm = T), 2))
  period$start <- years[period$start]
  period$end <- years[period$end]
  period$years <- paste(period$start, period$end, sep = "_")
  #
  for (i in 1:nrow(period)) {
    producer_period <- producer %>% filter(between(year, period$start[i], period$end[i]))
    consumer_period <- consumer %>% filter(between(year, period$start[i], period$end[i]))
    herbivore_period <- herbivore %>% filter(between(year, period$start[i], period$end[i]))
    
    calculate_agg_stability(producer_data = producer_period, consumer_data = consumer_period, 
                            herbivore_data = herbivore_period, site_name=sites[isite])
    # pull the output dataframe
    agg_ds_site <- get(paste0(sites[isite], "_aggregate_dss"))
    
    if (i == 1 & isite == 1) {
      agg_ds_allsites <- data.frame(agg_ds_site, period = period$mid_year[i], site_new = sites[isite])
    } else {
      agg_ds_allsites <- rbind(agg_ds_allsites, data.frame(agg_ds_site, period = period$mid_year[i], site_new = sites[isite]))
    }
  }
  #
}
# We have 2681 site_period in total if aggregated by 6 years. 
agg_ds_allsites$site_period <- paste(agg_ds_allsites$site, agg_ds_allsites$period, sep="_")

# rename plot to ensure every plot is unique across these sites. 
agg_ds_allsites$plot_new <- paste(agg_ds_allsites$site_new, agg_ds_allsites$plot, sep="_")


# look at the site level data------------------------------------------------------------------------------------------
# Autocorrelation of consumers < autocorrelation of producers; 
agg_ds_allsites_experiment <- agg_ds_allsites %>% filter(site_new %in% c("knz") & plot == '002c')     # 'ntl_madison', 'ntl_trout' , "bex_ae", "bex_se"
# 'MO', plot == 'ME'
acf(agg_ds_allsites_experiment$prod_richness)   # Richness is strongly auto-correlated, but stability is not
acf(agg_ds_allsites_experiment$prod_stability)

ggplot(data = agg_ds_allsites_experiment, aes(x=prod_richness, y=prod_stability, shape = plot, color = period)) +
  geom_point()

# summary(lm(data = agg_ds_allsites_experiment, prod_stability ~ prod_richness + con_richness + plot + site_period))

# library(nlme)
# # gls does not support random effects. 
# fit <- gls(
#   prod_stability ~ prod_richness + con_richness + plot + site_period,
#   data = agg_ds_allsites_experiment,
#   correlation = corARMA(p = 2, q = 0, form = ~ period | plot)
# )
# summary(fit)
# coef(fit)
# intervals(fit)
# 
# x = agg_ds_allsites_experiment$con_richness
# cor(x[-length(x)], x[-1])

####-------------------------------------------------Checking End------------------------------------------------------

agg_ds_aquatic <- agg_ds_allsites %>% filter(ecosystem == 'aquatic')
agg_ds_terrestrial <- agg_ds_allsites %>% filter(ecosystem == 'terrestrial')

agg_ds_aquatic <- control_confounder_temporal(agg_ds_aquatic)
agg_ds_terrestrial <- control_confounder_temporal(agg_ds_terrestrial)

#--------------------------------test for different ecosystems------------------------------------------------
agg_ds_ecosystem <- agg_ds_aquatic
# agg_ds_ecosystem <- agg_ds_terrestrial


#-------------------For producer stability
# a few options: 
# 1) Two-way Mundlak model design and ignore autocorrelation. 
fit_prod_glmm_noar <- glmmTMB(
  prod_stability_log_zscore ~ prod_richness_log_zscore_site_period_mean_devi +
    con_richness_log_zscore_site_period_mean_devi +
    prod_richness_log_zscore_plot_mean +
    prod_richness_log_zscore_site_period_mean +
    con_richness_log_zscore_plot_mean +
    con_richness_log_zscore_site_period_mean + 
    (1 | plot_new) + (1 | site_period),
  data = agg_ds_ecosystem,
  family = gaussian()
)
summary(fit_prod_glmm_noar)
# result
#                                               Estimate Std. Error z value Pr(>|z|)
# prod_richness_log_zscore_site_period_mean_devi  0.32368    0.12655   2.558   0.0105 *
# con_richness_log_zscore_site_period_mean_devi  -0.20943    0.22289  -0.940   0.3474 

# 2) Two-way Mundlak model design and adding autocorrelation using glmmTMB (Frequentist option)
fit_prod_glmm_ar <- glmmTMB(
  prod_stability_log_zscore ~ prod_richness_log_zscore_site_period_mean_devi +
    con_richness_log_zscore_site_period_mean_devi +
    prod_richness_log_zscore_plot_mean +
    prod_richness_log_zscore_site_period_mean +
    con_richness_log_zscore_plot_mean +
    con_richness_log_zscore_site_period_mean + 
    (1 | plot_new) + (1 | site_period) +
    ar1(site_period + 0 | plot_new),
  data = agg_ds_ecosystem,
  family = gaussian()
)
summary(fit_prod_glmm_ar)
# the result is similar, but it found the coefficient of autocorrelation (phi) is ~0.43 if interval = 2
#                                               Estimate Std. Error z value Pr(>|z|)
# prod_richness_log_zscore_site_period_mean_devi  0.37124    0.14780   2.512   0.0120 *
# con_richness_log_zscore_site_period_mean_devi  -0.30878    0.25344  -1.218   0.2231 

AIC(fit_prod_glmm_noar, fit_prod_glmm_ar)
#                    df      AIC
# fit_prod_glmm_noar 10 2109.098
# fit_prod_glmm_ar   12 2017.844
# AIC suggests we should use the model ar(1) structure, if the interval = 2. 


# 3) Two-way Mundlak model design and adding autocorrelation using brm (Bayesian option)
fit_prod_brm_ar <- brm(
  formula = bf(
    prod_stability_log_zscore ~
      prod_richness_log_zscore_site_period_mean_devi +
      con_richness_log_zscore_site_period_mean_devi +
      prod_richness_log_zscore_plot_mean +
      prod_richness_log_zscore_site_period_mean +
      con_richness_log_zscore_plot_mean +
      con_richness_log_zscore_site_period_mean +
      (1 | plot_new) +
      (1 | site_period) ,
    autocor = cor_ar(~ period | plot_new, p = 1)
  ),
  data = agg_ds_ecosystem,
  family = gaussian(),
  chains = 4, cores = 4, iter = 2000,
  control = list(adapt_delta = 0.90)
)
summary(fit_prod_brm_ar)
# the brm also estimate the coefficient of autocorrelation (phi) is ~0.42, but it estimates very small effect size, likely because autocorrelation eats up most variations. 
#                                                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# prod_richness_log_zscore_site_period_mean_devi     0.07      0.13    -0.18     0.32 1.00     1612     2538
# con_richness_log_zscore_site_period_mean_devi      0.04      0.24    -0.43     0.51 1.00     1781     2472

# Based on this comparison, I would recommend using interval = 2 and autocorrelation structure added by glmmTMB. 




#-------------------For consumer stability
# a few options: 
# 1) Two-way Mundlak model design and ignore autocorrelation. 
fit_con_glmm_noar <- glmmTMB(
  con_stability_log_zscore ~ prod_richness_log_zscore_site_period_mean_devi +
    con_richness_log_zscore_site_period_mean_devi +
    prod_richness_log_zscore_plot_mean +
    prod_richness_log_zscore_site_period_mean +
    con_richness_log_zscore_plot_mean +
    con_richness_log_zscore_site_period_mean + 
    (1 | plot_new) + (1 | site_period),
  data = agg_ds_ecosystem,
  family = gaussian()
)
summary(fit_con_glmm_noar)
# result
#                                               Estimate Std. Error z value Pr(>|z|)
# prod_richness_log_zscore_site_period_mean_devi -0.03345    0.17571  -0.190 0.849021    
# con_richness_log_zscore_site_period_mean_devi   1.01617    0.30780   3.301 0.000962 ***

# 2) Two-way Mundlak model design and adding autocorrelation using glmmTMB (Frequentist option)
fit_con_glmm_ar <- glmmTMB(
  con_stability_log_zscore ~ prod_richness_log_zscore_site_period_mean_devi +
    con_richness_log_zscore_site_period_mean_devi +
    prod_richness_log_zscore_plot_mean +
    prod_richness_log_zscore_site_period_mean +
    con_richness_log_zscore_plot_mean +
    con_richness_log_zscore_site_period_mean + 
    (1 | plot_new) + (1 | site_period) +
    ar1(site_period + 0 | plot_new),
  data = agg_ds_ecosystem,
  family = gaussian()
)
summary(fit_con_glmm_ar)
# consistent with results for producers, the result is similar, but it found the coefficient of autocorrelation (phi) is ~0.43 
#                                               Estimate Std. Error z value Pr(>|z|)
# prod_richness_log_zscore_site_period_mean_devi -0.07114    0.20151  -0.353   0.7241  
# con_richness_log_zscore_site_period_mean_devi   0.85238    0.34737   2.454   0.0141 *

AIC(fit_con_glmm_noar, fit_con_glmm_ar)
#                    df      AIC
# fit_con_glmm_noar 10 2704.962
# fit_con_glmm_ar   12 2611.199
# consistent with results for producers, AIC suggests we use the model with ar(1) structure, if the interval = 2. 


# 3) Two-way Mundlak model design and adding autocorrelation using brm (Bayesian option)
fit_con_brm_ar <- brm(
  formula = bf(
    con_stability_log_zscore ~
      prod_richness_log_zscore_site_period_mean_devi +
      con_richness_log_zscore_site_period_mean_devi +
      prod_richness_log_zscore_plot_mean +
      prod_richness_log_zscore_site_period_mean +
      con_richness_log_zscore_plot_mean +
      con_richness_log_zscore_site_period_mean +
      (1 | plot_new) +
      (1 | site_period) ,
    autocor = cor_ar(~ period | plot_new, p = 1)
  ),
  data = agg_ds_ecosystem,
  family = gaussian(),
  chains = 4, cores = 4, iter = 2000,
  control = list(adapt_delta = 0.90)
)
summary(fit_con_brm_ar)
# the brm also estimate the coefficient of autocorrelation (phi) is ~0.43, but it estimates smaller effect size and large uncertainties. 
#                                                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# prod_richness_log_zscore_site_period_mean_devi    -0.07      0.17    -0.41     0.27 1.00     2072     2439
# con_richness_log_zscore_site_period_mean_devi     -0.44      0.32    -1.07     0.18 1.00     1961     2710

# Based on the comparison among the three methods for both producer and consumer communities, I would recommend using autocorrelation structure added by glmmTMB. 
# I also suggest to use interval = 2, because of too strong residual autocorrelation when interval = 1 (phi = ~0.7), which sometimes affect model convergence. This is open to our discussion. 



