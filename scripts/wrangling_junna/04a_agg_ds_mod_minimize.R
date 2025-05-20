# read in aggregate stability metrics, Junna
rm(list = ls())
librarian::shelf(googledrive, vegan, lme4, car, dplyr, tidyr, lattice, lavaan, lmerTest, 
                 piecewiseSEM, semPlot, ggplot2, codyn, ggpubr, performance, emmeans)

# retrieve functions from 00_functions.r
source('scripts/00_functions_minimize.R')
tmp <- tempfile(fileext = ".csv")

drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1xa-ypKd_ovsRov_Ol_uvE7ZI2rCc5SSj"), type='csv')
drive_download(drive_folder[1,], path = tmp, overwrite = TRUE)
mar <- read.csv(tmp)

drive_download(drive_folder[2,], path = tmp, overwrite = TRUE)
terr <- read.csv(tmp)

#-------------------------------------------------------------------------------
# z_standard
terr_z <- terr
for (site in unique(terr$site)) {
  for (j in 7:21) {
    terr_z[terr_z$site==site, j] <- z_standard(terr_z[terr_z$site==site, j])
  }
}

# after z-standardized it, we do not need intercept and we only need random slope. 
mod <- lmer(data = terr_z, prod_stability ~ 0 + prod_richness + con_richness + (prod_richness + 0|site) + (con_richness + 0|site))
isSingular(mod, tol = 1e-4) 
summary(mod)
# Estimate Std. Error      df t value Pr(>|t|)
# prod_richness  -0.6478     0.4260  1.7034  -1.521    0.288
# con_richness    0.4106     0.4242  2.1171   0.968    0.430
plot(mod)
qqnorm(resid(mod))
# there is one outlier in the residual. 

mod <- lmer(data = terr_z, con_stability ~ 0 + prod_richness + con_richness + (con_richness + 0|site))
isSingular(mod, tol = 1e-4) 
summary(mod)
# Estimate Std. Error       df t value Pr(>|t|)
# prod_richness  0.07767    0.21631 29.38903   0.359    0.722
# con_richness   0.21855    0.35923  2.34032   0.608    0.597
plot(mod)
qqnorm(resid(mod))

ggplot(terr, aes(y=prod_stability, x=prod_richness, col=site)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(title='terrestrial-producer')

ggplot(terr, aes(y=con_stability, x=con_richness, col=site)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(title='terrestrial-invert')

#------------------------------------------marine ecosystems
mar_z <- mar
for (site in unique(mar$site_new)) {
  for (j in 7:21) {
    mar_z[mar_z$site_new==site, j] <- z_standard(mar_z[mar_z$site_new==site, j])
  }
}

mar_invert <- mar[mar$site_new %in% c("gce_invert", "usvi_invert", "sbc_invert", "mcr_invert"), ]
mar_fish <- mar[mar$site_new %in% c("usvi_fish", "sbc_fish", "mcr_fish", "aims_fish"), ]

mar_z_invert <- mar_z[mar_z$site_new %in% c("gce_invert", "usvi_invert", "sbc_invert", "mcr_invert"), ]
mar_z_fish <- mar_z[mar_z$site_new %in% c("usvi_fish", "sbc_fish", "mcr_fish", "aims_fish"), ]


# after z-standardized it, we do not need intercept and we only need random slope. 
mod <- lmer(data = mar_z_invert, prod_stability ~ 0 + prod_richness + con_richness + (prod_richness + 0|site/habitat_fine))
isSingular(mod, tol = 1e-4) 
summary(mod)
# Estimate Std. Error      df t value Pr(>|t|)
# prod_richness   0.1255     0.1722  1.2204   0.729    0.580
# con_richness    0.1402     0.1156 69.0778   1.213    0.229
plot(mod)
qqnorm(resid(mod))

# nested random effect -- habitat_fine was not added, because it is unnecessary, causing singular issue 
mod <- lmer(data = mar_z_invert, con_stability ~ 0 + prod_richness + con_richness + (con_richness + 0|site))
isSingular(mod, tol = 1e-4) 
summary(mod)
# Estimate Std. Error      df t value Pr(>|t|)    
# prod_richness   0.1195     0.0993 80.8828   1.204    0.232  
# con_richness    0.4567     0.1249  3.0673   3.656    0.034 *
plot(mod)
qqnorm(resid(mod))

## we do not need random effects for producer_fish data, because the slope cross these sites are so similar after z-standardize. 
# mod <- lmer(data = mar_z_fish, prod_stability ~ 0 + prod_richness + con_richness + (prod_richness + 0|site))
# isSingular(mod, tol = 1e-4) 
mod <- lm(data = mar_z_fish, prod_stability ~ 0 + prod_richness + con_richness)
summary(mod)
# Estimate Std. Error      df t value Pr(>|t|)
# prod_richness  0.41518    0.07591   5.469  1.7e-07 ***
# con_richness  -0.06092    0.07591  -0.803    0.423 
plot(mod)
qqnorm(resid(mod))


## we do not need random effects for producer_fish data, because the slope cross these sites are so similar after z-standardize. 
# mod <- lmer(data = mar_z_fish, con_stability ~ 0 + prod_richness + con_richness + (con_richness + 0|site))
# isSingular(mod, tol = 1e-4) 
mod <- lm(data = mar_z_fish, con_stability ~ 0 + prod_richness + con_richness)
summary(mod)
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)
# prod_richness  0.13645    0.07964   1.713   0.0886 .
# con_richness   0.20121    0.07964   2.526   0.0125 *
plot(mod)
qqnorm(resid(mod))

# plot of z-standarized data
ggplot(mar_z_invert, aes(y=prod_stability, x=prod_richness, col=site)) +
  geom_point() +
  geom_smooth(method='lm')

ggplot(mar_z_invert, aes(y=con_stability, x=con_richness, col=site)) +
  geom_point() +
  geom_smooth(method='lm')

ggplot(mar_z_fish, aes(y=prod_stability, x=prod_richness, col=site)) +
  geom_point() +
  geom_smooth(method='lm')

ggplot(mar_z_fish, aes(y=con_stability, x=con_richness, col=site)) +
  geom_point() +
  geom_smooth(method='lm')


# plot of raw data
ggplot(mar_invert, aes(y=prod_stability, x=prod_richness, col=site)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(title='marine-producer')

ggplot(mar_fish, aes(y=prod_stability, x=prod_richness, col=site)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(title='marine-producer')


ggplot(mar_invert, aes(y=con_stability, x=con_richness, col=site)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(title='marine-invert')

ggplot(mar_fish, aes(y=con_stability, x=con_richness, col=site)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(title='marine-fish')


##these structure
# model_stability(df = terr_z,
#                 ecosystem_type = "terrestrial",
#                 stability_metric = "aggregate",
#                 diversity_metric = "richness",
#                 prod_diversity_col = "prod_richness",
#                 con_diversity_col = "con_richness",
#                 prod_stability_col = "prod_stability",
#                 con_stability_col = "con_stability",
#                 multi_stability_col = "multitroph_stability",
#                 z_standard = FALSE)
# # view figures
# basic_sem_plot_terrestrial_richness_aggregate
# plot_terrestrial_aggregate_prod_richness_stability
# plot_terrestrial_aggregate_con_richness_stability
# plot_terrestrial_aggregate_multi_richness_stability
# plot_terrestrial_aggregate_stability_richness_correlations



