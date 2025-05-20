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

mod <- lmer(data = terr_z, con_stability ~ 0 + prod_richness + con_richness + (con_richness + 0|site))
isSingular(mod, tol = 1e-4) 
summary(mod)
# Estimate Std. Error       df t value Pr(>|t|)
# prod_richness  0.07767    0.21631 29.38903   0.359    0.722
# con_richness   0.21855    0.35923  2.34032   0.608    0.597


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
mar_fish <- mar[mar$site_new %in% c("usvi_fish", "sbc_fish", "mcr_fish"), ]

mar_z_invert <- mar_z[mar_z$site_new %in% c("gce_invert", "usvi_invert", "sbc_invert", "mcr_invert"), ]
mar_z_fish <- mar_z[mar_z$site_new %in% c("usvi_fish", "sbc_fish", "mcr_fish"), ]


# after z-standardized it, we do not need intercept and we only need random slope. 
mod <- lmer(data = mar_z_invert, prod_stability ~ 0 + prod_richness + con_richness + (prod_richness + 0|site/habitat_fine))
isSingular(mod, tol = 1e-4) 
summary(mod)
# Estimate Std. Error      df t value Pr(>|t|)
# prod_richness   0.1255     0.1722  1.2204   0.729    0.580
# con_richness    0.1402     0.1156 69.0778   1.213    0.229

mod <- lmer(data = mar_z_invert, con_stability ~ 0 + prod_richness + con_richness + (con_richness + 0|site/habitat_fine))
isSingular(mod, tol = 1e-4) 
summary(mod)
# Estimate Std. Error      df t value Pr(>|t|)    
# prod_richness   0.1439     0.1099 69.4739   1.309   0.1947  
# con_richness    0.3991     0.1310  2.1384   3.047   0.0856 .

## we cannot adding habitat_fine to mar_z_fish
mod <- lmer(data = mar_z_fish, prod_stability ~ 0 + prod_richness + con_richness + (prod_richness + 0|site))
isSingular(mod, tol = 1e-4) 
summary(mod)
# Estimate Std. Error      df t value Pr(>|t|)
# prod_richness   0.3930     0.1219 57.0000   3.223   0.0021 **
# con_richness   -0.0223     0.1219 57.0000  -0.183   0.8555

# this one has singularity issue. 
mod <- lmer(data = mar_z_fish, con_stability ~ 0 + prod_richness + con_richness + (con_richness + 0|site))
isSingular(mod, tol = 1e-4) 
summary(mod)
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)
# prod_richness  0.09277    0.13043 57.00000   0.711    0.480
# con_richness   0.14871    0.13043 57.00000   1.140    0.259


ggplot(mar_z_invert, aes(y=prod_stability, x=prod_richness, col=site)) +
  geom_point() +
  geom_smooth(method='lm')

ggplot(mar_z_invert, aes(y=con_stability, x=con_richness, col=site)) +
  geom_point() +
  geom_smooth(method='lm')

ggplot(mar_invert, aes(y=prod_stability, x=prod_richness, col=site)) +
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





